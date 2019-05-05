{-# LANGUAGE NoMonoLocalBinds, NoMonomorphismRestriction #-}
module Classgen.Module where

import           Control.Lens
import           Control.Monad.State
import           Data.HashMap.Strict                      ( HashMap )
import qualified Data.HashMap.Strict           as HM
import qualified Data.Vector                   as V
import           Data.Maybe                               ( fromMaybe )
import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import           Data.Set                                 ( Set )
import qualified Data.Set                      as S
import qualified Language.Haskell.Exts         as HS
import           Language.Haskell.Exts.QQ
import           Text.Casing
import           Classgen.Spec

--TODO add singleton functions

data ClassgenState = ClassgenState
  { _csModules :: !(HashMap Text (HS.Module ()))
  , _csMainDecls :: !([HS.Decl ()])
  , _csMethods :: !(HashMap Text (Set Text))
  } deriving (Show, Eq)

makeLensesWith abbreviatedFields ''ClassgenState

addClass :: MonadState ClassgenState m => GodotClass -> m ()
addClass cls = do
  methods    <- mkMethods cls
  properties <- mkProperties cls
  signals    <- mkSignals cls
  let dataType = if isCoreType (cls ^. name) then [] else mkDataType cls
  mainDecls %= (++ (dataType ++ properties ++ methods))
  modules %= HM.insert
    (cls ^. name)
    (HS.Module () (Just classModuleHead) [] classImports (classDecls ++ signals)
    )
 where
  classDecls   = mkConstants cls ++ mkEnums cls
  classImports = map
    (\n -> HS.ImportDecl ()
                         (HS.ModuleName () n)
                         False
                         False
                         False
                         Nothing
                         Nothing
                         Nothing
    )
    [ "Data.Coerce"
    , "Foreign.C"
    , "Godot.Internal.Dispatch"
    , "System.IO.Unsafe"
    , "Godot.Gdnative.Internal"
    , "Godot.Gdnative.Types"
    , "Godot.Api.Auto"
    ]
  classModuleHead = HS.ModuleHead () classModuleName Nothing Nothing
  classModuleName =
    HS.ModuleName ()
      $  "Godot."
      ++ (pascal $ T.unpack (cls ^. apiType))
      ++ "."
      ++ ("Godot" ++ T.unpack (cls ^. name))

resolveMethods :: MonadState ClassgenState m => m ()
resolveMethods = do
  mtds <- use methods
  let decls = concatMap resolveMethod (S.toList $ S.unions $ HM.elems mtds)
      moduleHead = HS.ModuleHead () (HS.ModuleName () "Godot.Methods") Nothing Nothing
      imports = map
        (\n -> HS.ImportDecl () (HS.ModuleName () n) False False False Nothing Nothing Nothing )
        [ "Godot.Internal.Dispatch"
        , "Godot.Api.Auto"
        , "Data.Text"
        ]
  modules %= HM.insert "Methods" (HS.Module () (Just moduleHead) [] imports decls)
  where
    escapeName "import" = "import'"
    escapeName "instance" = "instance'"
    escapeName n = n

    resolveMethod method =
      let methodName = T.unpack method
          methodNameVar = HS.Var () $ HS.UnQual () $ HS.Ident () methodName
          methodNamePromoted = HS.TyPromoted () $ HS.PromotedString () methodName methodName
          methodCtx = HS.CxSingle () $ HS.ClassA () (HS.UnQual () $ HS.Ident () "Method")
           [ methodNamePromoted
           , [ty|cls|]
           , [ty|sig|] ]
      in
        [ HS.TypeSig () [HS.Ident () $ escapeName methodName] $ HS.TyForall () Nothing (Just methodCtx) [ty|cls -> sig|]
        , HS.PatBind () (HS.PVar () $ HS.Ident () $ escapeName methodName)
            (HS.UnGuardedRhs () (HS.App () [hs|runMethod|] (HS.TypeApp () methodNamePromoted))) Nothing ]


mkProperties :: MonadState ClassgenState m => GodotClass -> m [HS.Decl ()]
mkProperties cls = concat <$> mapM mkProperty (V.toList $ cls ^. properties)
 where
  mkProperty prop = do
    get <- mkGetter prop
    set <- mkSetter prop
    return (get ++ set)
  mkGetter prop
    | T.null (prop ^. getter) = return []
    | otherwise = mkMethod cls $ GodotMethod (prop ^. getter)
                                             (prop ^. type')
                                             False
                                             False
                                             True
                                             False
                                             False
                                             False
                                             False
                                             mempty
  mkSetter prop
    | T.null (prop ^. setter) = return []
    | otherwise = mkMethod cls $ GodotMethod
      (prop ^. setter)
      (PrimitiveType VoidType)
      False
      False
      False
      False
      False
      False
      False
      (V.singleton $ GodotArgument (prop ^. name) (prop ^. type') Nothing)


godotObjectTy :: HS.Type ()
godotObjectTy = HS.TyCon () $ HS.UnQual () $ HS.Ident () "GodotObject"

sigCon :: HS.Exp ()
sigCon = HS.Con () $ HS.UnQual () $ HS.Ident () "Signal"

sigTy :: HS.Type ()
sigTy = HS.TyCon () $ HS.UnQual () $ HS.Ident () "Signal"

clsAsName :: GodotClass -> HS.Name ()
clsAsName cls = HS.Ident () ("Godot" ++ T.unpack (cls ^. name))

clsTy :: GodotClass -> HS.Type ()
clsTy = HS.TyCon () . HS.UnQual () . clsAsName

baseClsTy :: GodotClass -> HS.Type ()
baseClsTy cls = HS.TyCon () . HS.UnQual () $ HS.Ident
  ()
  ("Godot" ++ T.unpack (cls ^. baseClass))


intTy = HS.TyCon () . HS.UnQual () $ HS.name "Int"

mkDataType cls =
  [ HS.DataDecl
      ()
      (HS.NewType ())
      Nothing
      (HS.DHead () $ clsAsName cls)
      [ HS.QualConDecl () Nothing Nothing
          $ HS.ConDecl () (clsAsName cls) [godotObjectTy]
      ]
      [HS.Deriving () (Just $ HS.DerivNewtype ()) [asVariantRule, asHsVariantRule, typeableRule]]
    ]
    ++ if T.null (cls ^. baseClass)
         then []
         else
           [ HS.InstDecl
               ()
               Nothing
               (HS.IRule
                 ()
                 Nothing
                 Nothing
                 (HS.IHApp
                   ()
                   (HS.IHCon () (HS.UnQual () (HS.Ident () "HasBaseClass")))
                   (clsTy cls)
                 )
               )
               (Just
                 [ HS.InsType
                   ()
                   (HS.TyApp
                     ()
                     (HS.TyCon () (HS.UnQual () (HS.Ident () "BaseClass")))
                     (clsTy cls)
                   )
                   (baseClsTy cls)
                 , HS.InsDecl
                   ()
                   (HS.PatBind
                     ()
                     (HS.PVar () (HS.Ident () "super"))
                     (HS.UnGuardedRhs
                       ()
                       (HS.Var () (HS.UnQual () (HS.Ident () "coerce")))
                     )
                     Nothing
                   )
                 ]
               )
           , HS.InstDecl
               ()
               Nothing
               (HS.IRule
                 ()
                 Nothing
                 Nothing
                 (HS.IHApp
                   ()
                   (HS.IHApp
                    ()
                    (HS.IHCon () (HS.UnQual () (HS.Ident () "GodotFFI")))
                    (clsTy cls)
                   )
                   (clsTy cls)
                 )
               )
               (Just
                 [ HS.InsDecl
                   ()
                   (HS.PatBind
                     ()
                     (HS.PVar () (HS.Ident () "toLowLevel"))
                     (HS.UnGuardedRhs
                       ()
                       (HS.Var () (HS.UnQual () (HS.Ident () "return")))
                     )
                     Nothing
                   )
                 , HS.InsDecl
                    ()
                    (HS.PatBind
                      ()
                      (HS.PVar () (HS.Ident () "fromLowLevel"))
                      (HS.UnGuardedRhs
                        ()
                        (HS.Var () (HS.UnQual () (HS.Ident () "return")))
                      )
                      Nothing
                    )
                 ]
               )
           ]
 where
  asVariantRule = HS.IRule () Nothing Nothing
    $ HS.IHCon () (HS.UnQual () $ HS.Ident () "AsVariant")
  asHsVariantRule = HS.IRule () Nothing Nothing
    $ HS.IHCon () (HS.UnQual () $ HS.Ident () "AsHsVariant")
  typeableRule = HS.IRule () Nothing Nothing
    $ HS.IHCon () (HS.UnQual () $ HS.Ident () "Typeable")

mkSignals :: MonadState ClassgenState m => GodotClass -> m [HS.Decl ()]
mkSignals cls = return $ concatMap mkSignal (V.toList $ cls ^. signals)
 where
  mkSignal sig =
    let sigStr  = T.unpack (sig ^. name)
        sigName = HS.Ident () sigStr
    in  [ HS.TypeSig () [sigName] (HS.TyApp () sigTy (clsTy cls))
        , HS.PatBind
          ()
          (HS.PVar () sigName)
          (HS.UnGuardedRhs () $ HS.App () sigCon $ HS.Lit () $ HS.String
            ()
            sigStr
            sigStr
          )
          Nothing
        ]

mkConstants :: GodotClass -> [HS.Decl ()]
mkConstants cls = concatMap mkConstant (HM.toList $ cls ^. constants)
 where
  mkConstant (cname, cval) =
    let constName = HS.Ident () $ T.unpack cname
    in  [ HS.PatSynSig () [constName] Nothing Nothing Nothing intTy
        , HS.PatSyn ()
                    (HS.PVar () constName)
                    (HS.intP $ fromIntegral cval)
                    HS.ImplicitBidirectional
        ]

mkEnums cls = [] -- i don't think enums have a point

mkMethods :: MonadState ClassgenState m => GodotClass -> m [HS.Decl ()]
mkMethods cls = concat <$> mapM (mkMethod cls) (V.toList $ cls ^. methods)

mkMethod
  :: MonadState ClassgenState m => GodotClass -> GodotMethod -> m [HS.Decl ()]
mkMethod cls method = do
  mtds <- use methods
  if (method ^. name) `S.member` (HM.lookupDefault mempty (cls ^. name) mtds)
    then return []
    else do
      methods
        %= HM.insertWith S.union (cls ^. name) (S.singleton $ method ^. name)
      if (method ^. hasVarargs)
        then return []
        else do
          when (T.null $ method ^. name)
            $ error (show cls ++ "\n" ++ show method)
          return
            $ [ HS.PatBind ()
                           (HS.PVar () clsMethodBindName)
                           clsMethodBindRhs
                           Nothing
              , HS.InlineSig () False Nothing (HS.UnQual () clsMethodBindName)
              , HS.InstDecl () Nothing instRule (Just instDecls)
              ]
 where
  instRule = HS.IRule () Nothing Nothing instHead
  instHead = foldl
    (HS.IHApp ())
    methodClsTy
    [ HS.TyPromoted () $ HS.PromotedString () methodName methodName
    , clsTy cls
    , HS.TyParen () methodSig
    ]
  instDecls      = [HS.InsDecl () runMethodDecl]
  runMethodDecl  = HS.FunBind () [runMethodMatch]
  runMethodMatch = HS.Match
    ()
    runMethodName
    (HS.PVar () (HS.Ident () "cls") : map (HS.PVar ()) argNames)
    runMethodRhs
    Nothing

  clsMethodBindName =
    HS.Ident () $ "bind" ++ (T.unpack (cls ^. name)) ++ "_" ++ methodName
  clsMethodBindVar = HS.Var () $ HS.UnQual () clsMethodBindName

  clsMethodBindRhs = HS.UnGuardedRhs ()
    [hs|
      unsafePerformIO $ withCString $(HS.strE (T.unpack (cls ^. name))) $
        \clsNamePtr -> withCString $(HS.strE methodName) $
          \methodNamePtr -> godot_method_bind_get_method clsNamePtr methodNamePtr |]

  -- this is actually killing me.
  runMethodRhs     = HS.UnGuardedRhs () $ HS.App
    ()
    (HS.App
      ()
      (HS.Var () (HS.UnQual () (HS.Ident () "withHsVariantArray")))
      (HS.List () $ map
        ( HS.App () (HS.Var () (HS.UnQual () (HS.Ident () "toHsVariant")))
        . HS.Var ()
        . HS.UnQual ()
        )
        argNames
      )
    )
    [hs|
      \(arrPtr, len) -> godot_method_bind_call $(clsMethodBindVar) (coerce cls) arrPtr len >>=
        \(err, vt) -> do
          throwIfErr err
          res <- fromLowLevel =<< fromGodotVariant vt
          godot_variant_destroy vt
          return res |]

  argNames =
    map (HS.Ident () . ("arg" ++) . show) [1 .. length (method ^. arguments)]

  methodClsTy = HS.IHCon () $ HS.UnQual () $ HS.Ident () "Method"
  methodSig = foldr (HS.TyFun ()) (HS.TyApp () [ty|IO|] (toHsType $ method ^. returnType)) $
    fmap (argToHsType) (method ^. arguments)

  argToHsType (GodotArgument _ ty _) = toHsType ty
  -- TODO: default values
--    argToHsType (GodotArgument _ ty (Just _))
--      = HS.TyApp () (HS.TyCon () $ HS.UnQual () $ HS.Ident () "Maybe") $ toHsType ty

  methodName    = T.unpack (method ^. name)
  runMethodName = HS.Ident () "runMethod"

  {-
  instance Method "methodName" ClsName (generated -> signature) where
    runMethod cls arg1..argn = withVariantArray [asVariant arg1, ..., asVariant argn] $ 
      \(arrPtr, len) -> godot_method_bind_call clsMethodNameBind (coerce cls) arrPtr len >>=
      \(res, err) -> throwIfErr err >> return (coerce res)
  -}

toHsType :: GType -> HS.Type ()
toHsType (PrimitiveType VoidType ) = [ty| () |]
toHsType (PrimitiveType BoolType ) = [ty| Bool |]
toHsType (PrimitiveType IntType  ) = [ty| Int |]
toHsType (PrimitiveType FloatType) = [ty| Float |]
toHsType (EnumType _)              = [ty| Int |]
toHsType (CustomType ty)           =
  HS.TyCon () $ HS.UnQual () $ HS.Ident () $ "Godot" ++ T.unpack ty
toHsType (CoreType ty)             =
  HS.TyCon () $ HS.UnQual () $ HS.Ident () $ T.unpack $ renameType ty
 where
  renameType :: Text -> Text
  renameType "String" = "Text"
  renameType "Object" = "GodotObject"
  renameType "Variant" = "(Variant 'HaskellTy)"
  renameType x        = fromMaybe x $ T.stripPrefix "Pool" $ x
