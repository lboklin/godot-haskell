{-#LANGUAGE TemplateHaskell#-}
module Godot.Gdnative.Internal.TH where

import           Data.Maybe
import           Godot.Gdnative.Internal.Gdnative
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Foreign
import           Control.Monad                            ( liftM2 )


data InstanceTy = InstanceTy
  { instName :: String
  , libTy :: String
  , toVt :: String
  , fromVt :: String
  }

showInstance :: InstanceTy -> Type -> String
showInstance instTy gty = unwords ["TypeOf", libTy instTy, typeName gty]
  where typeName (ConT name) = nameBase name


generateAsVariantInstances :: Q [Dec]
generateAsVariantInstances =
  generateInstances $ InstanceTy "AsVariant" "GodotTy" "toVariant" "fromVariant"

generateAsHsVariantInstances :: Q [Dec]
generateAsHsVariantInstances = generateInstances
  $ InstanceTy "AsHsVariant" "HaskellTy" "toHsVariant" "fromHsVariant"

generateInstances :: InstanceTy -> Q [Dec]
generateInstances instTy = do
  (TyConI (DataD _ _ bndrs _ cons _)) <- lookupTypeName "Variant"
    >>= \(Just x) -> reify x
  let tys = mapMaybe getCon cons
  mapM mkInstance tys
 where
  tySynTy :: Type -> Q Type
  tySynTy gty = do
    tof <- conT $ mkName "TypeOf"
    lty <- liftM2 SigT (conT $ mkName $ libTy instTy) (conT $ mkName "LibType")
    reifyInstances (mkName "TypeOf") [lty, gty] >>= \insts -> case insts of
      [TySynInstD _ (TySynEqn _ ty)] -> return ty
      [] -> error $ "No type instance for " ++ showInstance instTy gty
      _ -> error $ "Multiple type instances for " ++ showInstance instTy gty

  getCon :: Con -> Maybe (Name, Q Type)
  getCon (NormalC name [(_, AppT (AppT _ _) gty)]) = Just (name, tySynTy gty)
  getCon (NormalC name [(_, ty)]) = Just (name, return ty)
  getCon _ = Nothing

  mkInstance :: (Name, Q Type) -> Q Dec
  mkInstance (name, qTy) = qTy >>= \ty -> instanceD
    (cxt [])
    (appT (conT $ mkName $ instName instTy) (return ty))
    [ valD (varP $ mkName $ toVt instTy)
           (normalB $ conE $ mkName $ nameBase name)
           []
    , valD (varP $ mkName $ fromVt instTy)
        (normalB [| \var -> case var of
                      $(conP (mkName $ nameBase name) [varP $ mkName "x"]) -> Just x
                      _ -> Nothing |])
        []
    ]
