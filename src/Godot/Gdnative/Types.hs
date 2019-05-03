{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns, FunctionalDependencies, TypeFamilies, TypeInType, LambdaCase #-}
module Godot.Gdnative.Types where

import           Control.Exception

import qualified Data.ByteString               as B
import qualified Data.ByteString.Unsafe        as B

import qualified Data.Map.Strict               as M
import           Data.Text                                ( Text )
import qualified Data.Text.Encoding            as T
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Function                            ( (&) )

import           Data.Typeable

import           Foreign
import           Foreign.C

import           Linear

import           Godot.Gdnative.Internal
import           Godot.Gdnative.Internal.TH

import           System.IO.Unsafe                         ( unsafePerformIO )

import           Data.Vector                              ( Vector )
import qualified Data.Vector                   as Vec

data LibType = GodotTy | HaskellTy

type family TypeOf (x :: LibType) a

data Variant (ty :: LibType)
  = VariantNil
  | VariantBool !Bool
  | VariantInt !Int
  | VariantReal !Float
  | VariantString !(TypeOf ty GodotString)
  | VariantVector2 !(TypeOf ty GodotVector2)
  | VariantRect2 !(TypeOf ty GodotRect2)
  | VariantVector3 !(TypeOf ty GodotVector3)
  | VariantTransform2d !(TypeOf ty GodotTransform2d)
  | VariantPlane !(TypeOf ty GodotPlane)
  | VariantQuat !(TypeOf ty GodotQuat)
  | VariantAabb !(TypeOf ty GodotAabb)
  | VariantBasis !(TypeOf ty GodotBasis)
  | VariantTransform !(TypeOf ty GodotTransform)
  | VariantColor !(TypeOf ty GodotColor)
  | VariantNodePath !(TypeOf ty GodotNodePath)
  | VariantRid !(TypeOf ty GodotRid)
  | VariantObject !(TypeOf ty GodotObject)
  | VariantDictionary !(TypeOf ty GodotDictionary)
  | VariantArray !(TypeOf ty GodotArray)
  | VariantPoolByteArray !(TypeOf ty GodotPoolByteArray)
  | VariantPoolIntArray !(TypeOf ty GodotPoolIntArray)
  | VariantPoolRealArray !(TypeOf ty GodotPoolRealArray)
  | VariantPoolStringArray !(TypeOf ty GodotPoolStringArray)
  | VariantPoolVector2Array !(TypeOf ty GodotPoolVector2Array)
  | VariantPoolVector3Array !(TypeOf ty GodotPoolVector3Array)
  | VariantPoolColorArray !(TypeOf ty GodotPoolColorArray)


class AsHsVariant a where
  toHsVariant :: a -> Variant 'HaskellTy
  fromHsVariant :: Variant 'HaskellTy -> Maybe a

class AsVariant a where
  toVariant :: a -> Variant 'GodotTy
  fromVariant :: Variant 'GodotTy -> Maybe a

-- |GodotFFI is a relation between low-level and high-level
-- |Godot types, and conversions between them.
class (AsVariant low, AsHsVariant high) => GodotFFI low high | low -> high, high -> low where
  fromLowLevel :: low -> IO high
  toLowLevel :: high -> IO low

toGodotVariant
  :: forall low high . GodotFFI low high => high -> Proxy low -> IO GodotVariant
toGodotVariant high _ = do
  low <- toLowLevel high :: IO low
  let vt = toVariant low :: Variant 'GodotTy
  toLowLevel vt

fromGodotVariant :: forall a . (Typeable a, AsVariant a) => GodotVariant -> IO a
fromGodotVariant var = do
  res <- fromVariant <$> fromLowLevel var
  case res of
    Just x  -> x `seq` return x
    Nothing -> do
      haveTy <- godot_variant_get_type var
      let expTy = typeOf (undefined :: a)
      error
        $  "Error in API: couldn't fromVariant. have: "
        ++ show haveTy
        ++ ", expected: "
        ++ show expTy


type instance TypeOf 'HaskellTy (Variant 'GodotTy) = Variant 'HaskellTy
type instance TypeOf 'GodotTy (Variant 'HaskellTy) = Variant 'GodotTy
instance GodotFFI (Variant 'GodotTy) (Variant 'HaskellTy) where
  toLowLevel highVt = case highVt of
    VariantNil -> return VariantNil
    VariantBool b -> return $ VariantBool b
    VariantInt int -> return $ VariantInt int
    VariantReal float -> return $ VariantReal float
    VariantString high -> VariantString <$> toLowLevel high
    VariantVector2 high -> VariantVector2 <$> toLowLevel high
    VariantRect2 high -> VariantRect2 <$> toLowLevel high
    VariantVector3 high -> VariantVector3 <$> toLowLevel high
    VariantTransform2d high -> VariantTransform2d <$> toLowLevel high
    VariantPlane high -> VariantPlane <$> toLowLevel high
    VariantQuat high -> VariantQuat <$> toLowLevel high
    VariantAabb high -> VariantAabb <$> toLowLevel high
    VariantBasis high -> VariantBasis <$> toLowLevel high
    VariantTransform high -> VariantTransform <$> toLowLevel high
    VariantColor high -> VariantColor <$> toLowLevel high
    VariantNodePath high -> VariantNodePath <$> toLowLevel high
    VariantRid high -> VariantRid <$> toLowLevel high
    VariantObject high -> VariantObject <$> toLowLevel high
    VariantDictionary high -> VariantDictionary <$> toLowLevel high
    VariantArray high -> VariantArray <$> toLowLevel high
    VariantPoolByteArray high -> VariantPoolByteArray <$> toLowLevel high
    VariantPoolIntArray high -> VariantPoolIntArray <$> toLowLevel high
    VariantPoolRealArray high -> VariantPoolRealArray <$> toLowLevel high
    VariantPoolStringArray high -> VariantPoolStringArray <$> toLowLevel high
    VariantPoolVector2Array high -> VariantPoolVector2Array <$> toLowLevel high
    VariantPoolVector3Array high -> VariantPoolVector3Array <$> toLowLevel high
    VariantPoolColorArray high -> VariantPoolColorArray <$> toLowLevel high

  fromLowLevel lowVt = case lowVt of
    VariantNil -> return VariantNil
    VariantBool b -> return $ VariantBool b
    VariantInt int -> return $ VariantInt int
    VariantReal float -> return $ VariantReal float
    VariantString low -> VariantString <$> fromLowLevel low
    VariantVector2 low -> VariantVector2 <$> fromLowLevel low
    VariantRect2 low -> VariantRect2 <$> fromLowLevel low
    VariantVector3 low -> VariantVector3 <$> fromLowLevel low
    VariantTransform2d low -> VariantTransform2d <$> fromLowLevel low
    VariantPlane low -> VariantPlane <$> fromLowLevel low
    VariantQuat low -> VariantQuat <$> fromLowLevel low
    VariantAabb low -> VariantAabb <$> fromLowLevel low
    VariantBasis low -> VariantBasis <$> fromLowLevel low
    VariantTransform low -> VariantTransform <$> fromLowLevel low
    VariantColor low -> VariantColor <$> fromLowLevel low
    VariantNodePath low -> VariantNodePath <$> fromLowLevel low
    VariantRid low -> VariantRid <$> fromLowLevel low
    VariantObject low -> VariantObject <$> fromLowLevel low
    VariantDictionary low -> VariantDictionary <$> fromLowLevel low
    VariantArray low -> VariantArray <$> fromLowLevel low
    VariantPoolByteArray low -> VariantPoolByteArray <$> fromLowLevel low
    VariantPoolIntArray low -> VariantPoolIntArray <$> fromLowLevel low
    VariantPoolRealArray low -> VariantPoolRealArray <$> fromLowLevel low
    VariantPoolStringArray low -> VariantPoolStringArray <$> fromLowLevel low
    VariantPoolVector2Array low -> VariantPoolVector2Array <$> fromLowLevel low
    VariantPoolVector3Array low -> VariantPoolVector3Array <$> fromLowLevel low
    VariantPoolColorArray low -> VariantPoolColorArray <$> fromLowLevel low

type instance TypeOf 'HaskellTy GodotVariant = Variant 'GodotTy
type instance TypeOf 'GodotTy (Variant 'GodotTy) = GodotVariant
type instance TypeOf 'GodotTy GodotVariant = GodotVariant
instance GodotFFI GodotVariant (Variant 'GodotTy) where
  fromLowLevel var = godot_variant_get_type var >>= \case
      GodotVariantTypeNil -> return VariantNil
      GodotVariantTypeBool -> (VariantBool . (/= 0)) <$> godot_variant_as_bool var
      GodotVariantTypeInt -> (VariantInt . fromIntegral) <$> godot_variant_as_int var
      GodotVariantTypeReal -> (VariantReal . realToFrac) <$> godot_variant_as_real var
      GodotVariantTypeString -> VariantString <$> godot_variant_as_string var
      GodotVariantTypeVector2 -> VariantVector2 <$> godot_variant_as_vector2 var
      GodotVariantTypeRect2 -> VariantRect2 <$> godot_variant_as_rect2 var
      GodotVariantTypeVector3 -> VariantVector3 <$> godot_variant_as_vector3 var
      GodotVariantTypeTransform2d -> VariantTransform2d <$> godot_variant_as_transform2d var
      GodotVariantTypePlane -> VariantPlane <$> godot_variant_as_plane var
      GodotVariantTypeQuat -> VariantQuat <$> godot_variant_as_quat var
      GodotVariantTypeAabb -> VariantAabb <$> godot_variant_as_aabb var
      GodotVariantTypeBasis -> VariantBasis <$> godot_variant_as_basis var
      GodotVariantTypeTransform -> VariantTransform <$> godot_variant_as_transform var
      GodotVariantTypeColor -> VariantColor <$> godot_variant_as_color var
      GodotVariantTypeNodePath -> VariantNodePath <$> godot_variant_as_node_path var
      GodotVariantTypeRid -> VariantRid <$> godot_variant_as_rid var
      GodotVariantTypeObject -> VariantObject <$> godot_variant_as_object var
      GodotVariantTypeDictionary -> VariantDictionary <$> godot_variant_as_dictionary var
      GodotVariantTypeArray -> VariantArray <$> godot_variant_as_array var
      GodotVariantTypePoolByteArray -> VariantPoolByteArray <$> godot_variant_as_pool_byte_array var
      GodotVariantTypePoolIntArray -> VariantPoolIntArray <$> godot_variant_as_pool_int_array var
      GodotVariantTypePoolRealArray -> VariantPoolRealArray <$> godot_variant_as_pool_real_array var
      GodotVariantTypePoolStringArray -> VariantPoolStringArray <$> godot_variant_as_pool_string_array var
      GodotVariantTypePoolVector2Array -> VariantPoolVector2Array <$> godot_variant_as_pool_vector2_array var
      GodotVariantTypePoolVector3Array -> VariantPoolVector3Array <$> godot_variant_as_pool_vector3_array var
      GodotVariantTypePoolColorArray -> VariantPoolColorArray <$> godot_variant_as_pool_color_array var

  toLowLevel VariantNil = godot_variant_new_nil
  toLowLevel (VariantBool b) = godot_variant_new_bool . fromIntegral $ fromEnum b
  toLowLevel (VariantInt i) = godot_variant_new_int (fromIntegral i)
  toLowLevel (VariantReal r) = godot_variant_new_real (realToFrac r)
  toLowLevel (VariantString x) = godot_variant_new_string x
  toLowLevel (VariantVector2 x) = godot_variant_new_vector2 x
  toLowLevel (VariantRect2 x) = godot_variant_new_rect2 x
  toLowLevel (VariantVector3 x) = godot_variant_new_vector3 x
  toLowLevel (VariantTransform2d x) = godot_variant_new_transform2d x
  toLowLevel (VariantPlane x) = godot_variant_new_plane x
  toLowLevel (VariantQuat x) = godot_variant_new_quat x
  toLowLevel (VariantAabb x) = godot_variant_new_aabb x
  toLowLevel (VariantBasis x) = godot_variant_new_basis x
  toLowLevel (VariantTransform x) = godot_variant_new_transform x
  toLowLevel (VariantColor x) = godot_variant_new_color x
  toLowLevel (VariantNodePath x) = godot_variant_new_node_path x
  toLowLevel (VariantRid x) = godot_variant_new_rid x
  toLowLevel (VariantObject x) = godot_variant_new_object x
  toLowLevel (VariantDictionary x) = godot_variant_new_dictionary x
  toLowLevel (VariantArray x) = godot_variant_new_array x
  toLowLevel (VariantPoolByteArray x) = godot_variant_new_pool_byte_array x
  toLowLevel (VariantPoolIntArray x) = godot_variant_new_pool_int_array x
  toLowLevel (VariantPoolRealArray x) = godot_variant_new_pool_real_array x
  toLowLevel (VariantPoolStringArray x) = godot_variant_new_pool_string_array x
  toLowLevel (VariantPoolVector2Array x) = godot_variant_new_pool_vector2_array x
  toLowLevel (VariantPoolVector3Array x) = godot_variant_new_pool_vector3_array x
  toLowLevel (VariantPoolColorArray x) = godot_variant_new_pool_color_array x

withVariantArray
  :: [Variant 'GodotTy] -> ((Ptr (Ptr GodotVariant), CInt) -> IO a) -> IO a
withVariantArray vars mtd = allocaArray (length vars)
  $ \arrPtr -> withVars vars 0 arrPtr mtd
 where
  withVars (x : xs) n arrPtr mtd = do
    vt  <- toLowLevel x
    res <- withGodotVariant vt $ \vtPtr -> do
      poke (advancePtr arrPtr n) vtPtr
      withVars xs (n + 1) arrPtr mtd
    godot_variant_destroy vt
    return res
  withVars [] n arrPtr mtd = mtd (arrPtr, fromIntegral n)

throwIfErr :: GodotVariantCallError -> IO ()
throwIfErr err = case variantCallErrorError err of
  GodotCallErrorCallOk -> return ()
  _                    -> throwIO err


-- C types

type instance TypeOf 'HaskellTy CBool = Bool
type instance TypeOf 'GodotTy CBool = CBool
type instance TypeOf 'GodotTy Bool = CBool
instance GodotFFI CBool Bool where
  fromLowLevel = return . toBool
  toLowLevel = return . fromBool

type instance TypeOf 'HaskellTy CFloat = Float
type instance TypeOf 'GodotTy CFloat = CFloat
type instance TypeOf 'GodotTy Float = CFloat
instance GodotFFI CFloat Float where
  fromLowLevel = return . realToFrac
  toLowLevel = return . realToFrac

type instance TypeOf 'HaskellTy CInt = Int
type instance TypeOf 'GodotTy CInt = CInt
type instance TypeOf 'GodotTy Int = CInt
instance GodotFFI CInt Int where
  fromLowLevel = return . fromEnum
  toLowLevel = return . toEnum

type instance TypeOf 'HaskellTy () = ()
type instance TypeOf 'GodotTy () = ()
instance GodotFFI () () where
  fromLowLevel = return . const ()
  toLowLevel = return . const ()


-- Built-in Godot types

type instance TypeOf 'HaskellTy GodotString = Text
type instance TypeOf 'GodotTy GodotString = GodotString
type instance TypeOf 'GodotTy Text = GodotString
instance GodotFFI GodotString Text where
  fromLowLevel str = godot_string_utf8 str >>= \cstr -> T.decodeUtf8 <$> fromCharString cstr
    where
      fromCharString cstr = do
        len <- godot_char_string_length cstr
        sptr <- godot_char_string_get_data cstr
        B.packCStringLen (sptr, fromIntegral len)
  toLowLevel txt = B.unsafeUseAsCStringLen bstr $ \(ptr, len) ->
    godot_string_chars_to_utf8_with_len ptr (fromIntegral len)
    where
      bstr = T.encodeUtf8 txt

type Vector2 = V2 Float
type instance TypeOf 'HaskellTy GodotVector2 = Vector2
type instance TypeOf 'GodotTy GodotVector2 = GodotVector2
type instance TypeOf 'GodotTy Vector2 = GodotVector2
instance GodotFFI GodotVector2 Vector2 where
  fromLowLevel v = V2
                   <$> (realToFrac <$> godot_vector2_get_x v)
                   <*> (realToFrac <$> godot_vector2_get_y v)
  toLowLevel (V2 x y) = godot_vector2_new (realToFrac x) (realToFrac y)


type Vector3 = V3 Float
type instance TypeOf 'HaskellTy GodotVector3 = Vector3
type instance TypeOf 'GodotTy GodotVector3 = GodotVector3
type instance TypeOf 'GodotTy Vector3 = GodotVector3
instance GodotFFI GodotVector3 Vector3 where
  fromLowLevel v = V3
                   <$> (realToFrac <$> godot_vector3_get_axis v GodotVector3AxisX)
                   <*> (realToFrac <$> godot_vector3_get_axis v GodotVector3AxisY)
                   <*> (realToFrac <$> godot_vector3_get_axis v GodotVector3AxisZ)
  toLowLevel (V3 x y z) = godot_vector3_new (realToFrac x) (realToFrac y) (realToFrac z)

type Quat = Quaternion Float
type instance TypeOf 'HaskellTy GodotQuat = Quat
type instance TypeOf 'GodotTy GodotQuat = GodotQuat
type instance TypeOf 'GodotTy Quat = GodotQuat
instance GodotFFI GodotQuat Quat where
  fromLowLevel q = Quaternion
                   <$> (realToFrac <$> godot_quat_get_w q)
                   <*> (V3
                        <$> (realToFrac <$> godot_quat_get_x q)
                        <*> (realToFrac <$> godot_quat_get_y q)
                        <*> (realToFrac <$> godot_quat_get_z q))
  toLowLevel (Quaternion w (V3 x y z)) = godot_quat_new (realToFrac x)
                                                        (realToFrac y)
                                                        (realToFrac z)
                                                        (realToFrac w)

type Rect2 = M22 Float
type instance TypeOf 'HaskellTy GodotRect2 = Rect2
type instance TypeOf 'GodotTy GodotRect2 = GodotRect2
type instance TypeOf 'GodotTy Rect2 = GodotRect2
instance GodotFFI GodotRect2 Rect2 where
  fromLowLevel r = V2
                   <$> (fromLowLevel =<< godot_rect2_get_position r)
                   <*> (fromLowLevel =<< godot_rect2_get_size r)
  toLowLevel (V2 pos size) = do pos' <- toLowLevel pos
                                size' <- toLowLevel size
                                godot_rect2_new_with_position_and_size pos' size'

type AABB = M23 Float
type instance TypeOf 'HaskellTy GodotAabb = AABB
type instance TypeOf 'GodotTy GodotAabb = GodotAabb
type instance TypeOf 'GodotTy AABB = GodotAabb
instance GodotFFI GodotAabb AABB where
  fromLowLevel aabb = V2
                      <$> (fromLowLevel =<< godot_aabb_get_position aabb)
                      <*> (fromLowLevel =<< godot_aabb_get_size aabb)
  toLowLevel (V2 pos size) = do pos'  <- toLowLevel pos
                                size' <- toLowLevel size
                                godot_aabb_new pos' size'

-- Axes X, Y and Z are represented by the int constants 0, 1 and 2 respectively (at least for Vector3):
-- https://godot.readthedocs.io/en/latest/classes/class_vector3.html?highlight=axis#numeric-constants
type Basis = M33 Float
type instance TypeOf 'HaskellTy GodotBasis = Basis
type instance TypeOf 'GodotTy GodotBasis = GodotBasis
type instance TypeOf 'GodotTy Basis = GodotBasis
instance GodotFFI GodotBasis Basis where
  fromLowLevel b = V3
                   <$> (llAxis 0)
                   <*> (llAxis 1)
                   <*> (llAxis 2)
                 where llAxis axis = fromLowLevel =<< godot_basis_get_axis b axis
  toLowLevel (V3 x y z) = do x' <- toLowLevel x
                             y' <- toLowLevel y
                             z' <- toLowLevel z
                             godot_basis_new_with_rows x' y' z'

data Transform = TF { _tfBasis :: Basis, _tfPosition :: V3 Float }
type instance TypeOf 'HaskellTy GodotTransform = Transform
type instance TypeOf 'GodotTy GodotTransform = GodotTransform
type instance TypeOf 'GodotTy Transform = GodotTransform
instance GodotFFI GodotTransform Transform where
  fromLowLevel tf = TF
                    <$> (fromLowLevel =<< godot_transform_get_basis tf)
                    <*> (fromLowLevel =<< godot_transform_get_origin tf)
  toLowLevel (TF basis orig) = do basis' <- toLowLevel basis
                                  orig'  <- toLowLevel orig
                                  godot_transform_new basis' orig'

data Transform2 = TF2 { _tf2Rotation :: Float, _tf2Position :: V2 Float }
type instance TypeOf 'HaskellTy GodotTransform2d = Transform2
type instance TypeOf 'GodotTy GodotTransform2d = GodotTransform2d
type instance TypeOf 'GodotTy Transform2 = GodotTransform2d
instance GodotFFI GodotTransform2d Transform2 where
  fromLowLevel tf = TF2
                    <$> (fromLowLevel =<< godot_transform2d_get_rotation tf)
                    <*> (fromLowLevel =<< godot_transform2d_get_origin tf)
  toLowLevel (TF2 rot orig) = do rot' <- toLowLevel rot
                                 orig'  <- toLowLevel orig
                                 godot_transform2d_new rot' orig'

data Plane = Plane Float Float Float Float
type instance TypeOf 'HaskellTy GodotPlane = Plane
type instance TypeOf 'GodotTy GodotPlane = GodotPlane
type instance TypeOf 'GodotTy Plane = GodotPlane
instance GodotFFI GodotPlane Plane where
  fromLowLevel pl = do
    V3 a b c <- fromLowLevel =<< godot_plane_get_normal pl
    d <- fromLowLevel =<< godot_plane_get_d pl
    return $ Plane a b c d
  toLowLevel (Plane a b c d) = do
    [a', b', c', d'] <- mapM toLowLevel [a, b, c, d] :: IO [CFloat]
    godot_plane_new_with_reals a' b' c' d'

-- This should perhaps be better modeled - FilePath?
newtype NodePath = NodePath Text
type instance TypeOf 'HaskellTy GodotNodePath = NodePath
type instance TypeOf 'GodotTy GodotNodePath = GodotNodePath
type instance TypeOf 'GodotTy NodePath = GodotNodePath
instance GodotFFI GodotNodePath NodePath where
  fromLowLevel np = NodePath <$> (fromLowLevel =<< godot_node_path_get_name np 0)
  toLowLevel (NodePath np) = godot_node_path_new =<< toLowLevel np

type Color = AlphaColour Double
type instance TypeOf 'HaskellTy GodotColor = Color
type instance TypeOf 'GodotTy GodotColor = GodotColor
type instance TypeOf 'GodotTy Color = GodotColor
instance GodotFFI GodotColor Color where
  fromLowLevel c = do
    r <- realToFrac <$> godot_color_get_r c
    g <- realToFrac <$> godot_color_get_g c
    b <- realToFrac <$> godot_color_get_b c
    a <- realToFrac <$> godot_color_get_a c
    return $ withOpacity (sRGB r g b) a
  toLowLevel rgba =
    let RGB r g b = toSRGB (rgba `over` black)
    in  godot_color_new_rgba
          (realToFrac r)
          (realToFrac g)
          (realToFrac b)
          (realToFrac $ alphaChannel rgba)

type instance TypeOf 'HaskellTy GodotObject = GodotObject
type instance TypeOf 'GodotTy GodotObject = GodotObject
instance GodotFFI GodotObject GodotObject where
  fromLowLevel = return
  toLowLevel = return

data RID = RID GodotRid Int
type instance TypeOf 'HaskellTy GodotRid = RID
type instance TypeOf 'GodotTy GodotRid = GodotRid
type instance TypeOf 'GodotTy RID = GodotRid
instance GodotFFI GodotRid RID where
  fromLowLevel rid = RID rid <$> fromIntegral <$> godot_rid_get_id rid
  toLowLevel (RID rid _) = return rid


-- Godot Arrays

type Array = Vector (Variant 'HaskellTy)
type instance TypeOf 'HaskellTy GodotArray = Array
type instance TypeOf 'GodotTy GodotArray = GodotArray
type instance TypeOf 'GodotTy Array = GodotArray
instance GodotFFI GodotArray Array where
  fromLowLevel low = fromLowLevelArray godot_array_size godot_array_get low
    >>= mapM fromLowLevel
  toLowLevel high = mapM toLowLevel high
    >>= toLowLevelArray godot_array_new godot_array_append

type ByteArray = Vector Word8
type instance TypeOf 'HaskellTy GodotPoolByteArray = ByteArray
type instance TypeOf 'GodotTy GodotPoolByteArray = GodotPoolByteArray
type instance TypeOf 'GodotTy ByteArray = GodotPoolByteArray
instance GodotFFI GodotPoolByteArray ByteArray where
  fromLowLevel vs = do
    size <- fromIntegral <$> godot_pool_byte_array_size vs
    let maybeNext n v = if n == (size - 1) then Nothing else Just (v, n + 1)
    let variantAt n = maybeNext n <$> (godot_pool_byte_array_get vs n)
    Vec.unfoldrM variantAt 0
  toLowLevel vs = do
    array <- godot_pool_byte_array_new
    mapM_ (godot_pool_byte_array_append array) (vs)
    return array

type IntArray = Vector Int
type instance TypeOf 'HaskellTy GodotPoolIntArray = IntArray
type instance TypeOf 'GodotTy GodotPoolIntArray = GodotPoolIntArray
type instance TypeOf 'GodotTy IntArray = GodotPoolIntArray
instance GodotFFI GodotPoolIntArray IntArray where
  fromLowLevel = fromLowLevelArray godot_pool_int_array_size godot_pool_int_array_get
  toLowLevel = toLowLevelArray godot_pool_int_array_new godot_pool_int_array_append

type RealArray = Vector Float
type instance TypeOf 'HaskellTy GodotPoolRealArray = RealArray
type instance TypeOf 'GodotTy GodotPoolRealArray = GodotPoolRealArray
type instance TypeOf 'GodotTy RealArray = GodotPoolRealArray
instance GodotFFI GodotPoolRealArray RealArray where
  fromLowLevel = fromLowLevelArray godot_pool_real_array_size godot_pool_real_array_get
  toLowLevel = toLowLevelArray godot_pool_real_array_new godot_pool_real_array_append

type StringArray = Vector Text
type instance TypeOf 'HaskellTy GodotPoolStringArray = Vector Text
type instance TypeOf 'GodotTy GodotPoolStringArray = GodotPoolStringArray
type instance TypeOf 'GodotTy (Vector Text) = GodotPoolStringArray
instance GodotFFI GodotPoolStringArray StringArray where
  fromLowLevel = fromLowLevelArray godot_pool_string_array_size godot_pool_string_array_get
  toLowLevel = toLowLevelArray godot_pool_string_array_new godot_pool_string_array_append

type Vector2Array = Vector (V2 Float)
type instance TypeOf 'HaskellTy GodotPoolVector2Array = Vector2Array
type instance TypeOf 'GodotTy GodotPoolVector2Array = GodotPoolVector2Array
type instance TypeOf 'GodotTy Vector2Array = GodotPoolVector2Array
instance GodotFFI GodotPoolVector2Array Vector2Array where
  fromLowLevel = fromLowLevelArray godot_pool_vector2_array_size godot_pool_vector2_array_get
  toLowLevel = toLowLevelArray godot_pool_vector2_array_new godot_pool_vector2_array_append

type Vector3Array = Vector (V3 Float)
type instance TypeOf 'HaskellTy GodotPoolVector3Array = Vector3Array
type instance TypeOf 'GodotTy GodotPoolVector3Array = GodotPoolVector3Array
type instance TypeOf 'GodotTy Vector3Array = GodotPoolVector3Array
instance GodotFFI GodotPoolVector3Array Vector3Array where
  fromLowLevel = fromLowLevelArray godot_pool_vector3_array_size godot_pool_vector3_array_get
  toLowLevel = toLowLevelArray godot_pool_vector3_array_new godot_pool_vector3_array_append

type ColorArray = Vector (AlphaColour Double)
type instance TypeOf 'HaskellTy GodotPoolColorArray = ColorArray
type instance TypeOf 'GodotTy GodotPoolColorArray = GodotPoolColorArray
type instance TypeOf 'GodotTy ColorArray = GodotPoolColorArray
instance GodotFFI GodotPoolColorArray ColorArray where
  fromLowLevel = fromLowLevelArray godot_pool_color_array_size godot_pool_color_array_get
  toLowLevel = toLowLevelArray godot_pool_color_array_new godot_pool_color_array_append

fromLowLevelArray
  :: GodotFFI low high
  => (arr -> IO CInt)
  -> (arr -> CInt -> IO low)
  -> arr
  -> IO (Vector high)
fromLowLevelArray sizef getf vs = do
  size <- fromIntegral <$> sizef vs
  let maybeNext n v = if n == (size - 1) then Nothing else Just (v, n + 1)
  let variantAt n = maybeNext n <$> (getf vs n >>= fromLowLevel)
  Vec.unfoldrM variantAt 0

toLowLevelArray
  :: GodotFFI low high
  => IO arr
  -> (arr -> low -> IO ())
  -> Vector high
  -> IO arr
toLowLevelArray constr appendf vs = do
  array <- constr
  mapM_ (\x -> toLowLevel x >>= appendf array) (vs)
  return array

type Dictionary = M.Map Text (Variant 'HaskellTy)
type instance TypeOf 'HaskellTy GodotDictionary = Dictionary
type instance TypeOf 'GodotTy GodotDictionary = GodotDictionary
type instance TypeOf 'GodotTy Dictionary = GodotDictionary
instance GodotFFI GodotDictionary Dictionary where
  fromLowLevel dict = do
    let fromVt (VariantString str) = str
    vtKeys <- fromLowLevel =<< godot_dictionary_keys dict
    vtVals <- fromLowLevel =<< godot_dictionary_values dict
    let keys = fmap fromVt $ Vec.toList vtKeys
        vals = Vec.toList vtVals
    return $ M.fromList $ zip keys vals
  toLowLevel m = do
    dict <- godot_dictionary_new
    flip mapM_ (M.toList m) $ \(k, v) -> do
      vtKey <- toGodotVariant k (Proxy @GodotString)
      vtVal <- toLowLevel =<< toLowLevel v
      godot_dictionary_set dict vtKey vtVal
    return dict


-- Variants


instance AsVariant (Variant 'GodotTy) where
  toVariant = id
  fromVariant = Just

instance AsVariant (Variant 'HaskellTy) where
  toVariant v = let !res = unsafePerformIO $ toLowLevel v in res
  fromVariant v = let !res = unsafePerformIO $ fromLowLevel v in Just res

instance AsVariant GodotVariant where
  toVariant v = let !res = unsafePerformIO $ fromLowLevel v in res
  fromVariant v = let !res = unsafePerformIO $ toLowLevel v in Just res

instance AsVariant () where
  toVariant _ = VariantNil
  fromVariant VariantNil = Just ()
  fromVariant _ = Nothing

instance AsVariant CBool where
  fromVariant (VariantBool b) = Just $ fromBool b
  fromVariant _ = Nothing
  toVariant = VariantBool . toBool

instance AsVariant CInt where
  fromVariant (VariantInt a) = Just $ toEnum a
  fromVariant _ = Nothing
  toVariant = VariantInt . fromEnum

instance AsVariant CFloat where
  fromVariant (VariantReal a) = Just $ realToFrac a
  fromVariant _ = Nothing
  toVariant = VariantReal . realToFrac

$(generateAsVariantInstances)

instance AsHsVariant (Variant 'GodotTy) where
  toHsVariant v = let !res = unsafePerformIO $ fromLowLevel v in res
  fromHsVariant v = let !res = unsafePerformIO $ toLowLevel v in Just res

instance AsHsVariant (Variant 'HaskellTy) where
  toHsVariant = id
  fromHsVariant = Just

instance AsHsVariant () where
  fromHsVariant VariantNil = Just ()
  fromHsVariant _ = Nothing
  toHsVariant _ = VariantNil

$(generateAsHsVariantInstances)
