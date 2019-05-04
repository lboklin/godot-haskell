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
  -- mapHsVariant :: forall b. (a -> b) -> Variant 'HaskellTy -> Variant 'HaskellTy

class AsVariant a where
  toVariant :: a -> Variant 'GodotTy
  fromVariant :: Variant 'GodotTy -> Maybe a
  -- mapVariant :: forall b. (a -> b) -> Variant 'GodotTy -> Variant 'GodotTy

-- |GodotFFI is a relation between low-level and high-level
-- |Godot types, and conversions between them.
class (Typeable low, AsVariant low, AsHsVariant high) =>
  GodotFFI low high | low -> high, high -> low
 where
  fromLowLevel :: low -> IO high
  toLowLevel :: high -> IO low


type instance TypeOf 'HaskellTy (Variant 'GodotTy) = Variant 'HaskellTy
type instance TypeOf 'HaskellTy GodotVariant       = Variant 'HaskellTy
type instance TypeOf 'GodotTy (Variant 'HaskellTy) = Variant 'GodotTy
type instance TypeOf 'GodotTy (Variant 'GodotTy)   = GodotVariant
type instance TypeOf 'GodotTy GodotVariant         = GodotVariant


type instance TypeOf 'HaskellTy () = ()
type instance TypeOf 'GodotTy () = ()

type instance TypeOf 'HaskellTy GodotString = Text
type instance TypeOf 'GodotTy GodotString = GodotString
type instance TypeOf 'GodotTy Text = GodotString

type Vector2 = V2 Float
type instance TypeOf 'HaskellTy GodotVector2 = Vector2
type instance TypeOf 'GodotTy GodotVector2 = GodotVector2
type instance TypeOf 'GodotTy Vector2 = GodotVector2

type Vector3 = V3 Float
type instance TypeOf 'HaskellTy GodotVector3 = Vector3
type instance TypeOf 'GodotTy GodotVector3 = GodotVector3
type instance TypeOf 'GodotTy Vector3 = GodotVector3

type Quat = Quaternion Float
type instance TypeOf 'HaskellTy GodotQuat = Quat
type instance TypeOf 'GodotTy GodotQuat = GodotQuat
type instance TypeOf 'GodotTy Quat = GodotQuat

type Rect2 = M22 Float
type instance TypeOf 'HaskellTy GodotRect2 = Rect2
type instance TypeOf 'GodotTy GodotRect2 = GodotRect2
type instance TypeOf 'GodotTy Rect2 = GodotRect2

type AABB = M23 Float
type instance TypeOf 'HaskellTy GodotAabb = AABB
type instance TypeOf 'GodotTy GodotAabb = GodotAabb
type instance TypeOf 'GodotTy AABB = GodotAabb
type Basis = M33 Float
type instance TypeOf 'HaskellTy GodotBasis = Basis
type instance TypeOf 'GodotTy GodotBasis = GodotBasis
type instance TypeOf 'GodotTy Basis = GodotBasis


data Transform = TF { _tfBasis :: Basis, _tfPosition :: V3 Float }
type instance TypeOf 'HaskellTy GodotTransform = Transform
type instance TypeOf 'GodotTy GodotTransform = GodotTransform
type instance TypeOf 'GodotTy Transform = GodotTransform

data Transform2D = TF2D { _tf2Rotation :: Float, _tf2Position :: V2 Float }
type instance TypeOf 'HaskellTy GodotTransform2d = Transform2D
type instance TypeOf 'GodotTy GodotTransform2d = GodotTransform2d
type instance TypeOf 'GodotTy Transform2D = GodotTransform2d

-- | A plane with normal vector (a, b, c) and distance from the plane (d)
data Plane = Plane Float Float Float Float
type instance TypeOf 'HaskellTy GodotPlane = Plane
type instance TypeOf 'GodotTy GodotPlane = GodotPlane
type instance TypeOf 'GodotTy Plane = GodotPlane

-- This should perhaps be better modeled - FilePath?
newtype NodePath = NodePath Text
type instance TypeOf 'HaskellTy GodotNodePath = NodePath
type instance TypeOf 'GodotTy GodotNodePath = GodotNodePath
type instance TypeOf 'GodotTy NodePath = GodotNodePath

type Color = AlphaColour Double
type instance TypeOf 'HaskellTy GodotColor = Color
type instance TypeOf 'GodotTy GodotColor = GodotColor
type instance TypeOf 'GodotTy Color = GodotColor

type instance TypeOf 'HaskellTy GodotObject = GodotObject
type instance TypeOf 'GodotTy GodotObject = GodotObject

data RID = RID GodotRid Int
type instance TypeOf 'HaskellTy GodotRid = RID
type instance TypeOf 'GodotTy GodotRid = GodotRid
type instance TypeOf 'GodotTy RID = GodotRid

type Array = Vector (Variant 'HaskellTy)
type instance TypeOf 'HaskellTy GodotArray = Array
type instance TypeOf 'GodotTy GodotArray = GodotArray
type instance TypeOf 'GodotTy Array = GodotArray

type ByteArray = Vector Word8
type instance TypeOf 'HaskellTy GodotPoolByteArray = ByteArray
type instance TypeOf 'GodotTy GodotPoolByteArray = GodotPoolByteArray
type instance TypeOf 'GodotTy ByteArray = GodotPoolByteArray

type IntArray = Vector Int
type instance TypeOf 'HaskellTy GodotPoolIntArray = IntArray
type instance TypeOf 'GodotTy GodotPoolIntArray = GodotPoolIntArray
type instance TypeOf 'GodotTy IntArray = GodotPoolIntArray

type RealArray = Vector Float
type instance TypeOf 'HaskellTy GodotPoolRealArray = RealArray
type instance TypeOf 'GodotTy GodotPoolRealArray = GodotPoolRealArray
type instance TypeOf 'GodotTy RealArray = GodotPoolRealArray

type StringArray = Vector Text
type instance TypeOf 'HaskellTy GodotPoolStringArray = Vector Text
type instance TypeOf 'GodotTy GodotPoolStringArray = GodotPoolStringArray
type instance TypeOf 'GodotTy (Vector Text) = GodotPoolStringArray

type Vector2Array = Vector (V2 Float)
type instance TypeOf 'HaskellTy GodotPoolVector2Array = Vector2Array
type instance TypeOf 'GodotTy GodotPoolVector2Array = GodotPoolVector2Array
type instance TypeOf 'GodotTy Vector2Array = GodotPoolVector2Array

type Vector3Array = Vector (V3 Float)
type instance TypeOf 'HaskellTy GodotPoolVector3Array = Vector3Array
type instance TypeOf 'GodotTy GodotPoolVector3Array = GodotPoolVector3Array
type instance TypeOf 'GodotTy Vector3Array = GodotPoolVector3Array

type ColorArray = Vector (AlphaColour Double)
type instance TypeOf 'HaskellTy GodotPoolColorArray = ColorArray
type instance TypeOf 'GodotTy GodotPoolColorArray = GodotPoolColorArray
type instance TypeOf 'GodotTy ColorArray = GodotPoolColorArray

type Dictionary = M.Map Text (Variant 'HaskellTy)
type instance TypeOf 'HaskellTy GodotDictionary = Dictionary
type instance TypeOf 'GodotTy GodotDictionary = GodotDictionary
type instance TypeOf 'GodotTy Dictionary = GodotDictionary

toGodotVariant :: forall a . (Typeable a, AsVariant a) => a -> IO GodotVariant
toGodotVariant low = do
  -- low <- toLowLevel high :: IO low
  let vt = toVariant low :: Variant 'GodotTy
  toLowLevel vt

fromGodotVariant :: forall a . (Typeable a, AsVariant a) => GodotVariant -> IO a
fromGodotVariant var = do
  low <-
    fromVariant <$> (fromLowLevel var :: IO (Variant 'GodotTy)) :: IO (Maybe a)
  case low of
    Just x  -> return x
    Nothing -> do
      haveTy <- godot_variant_get_type var
      let expTy = typeOf (undefined :: a)
      error
        $  "Error in API: couldn't fromVariant. have: "
        ++ show haveTy
        ++ ", expected: "
        ++ show expTy


instance GodotFFI (Variant 'GodotTy) (Variant 'HaskellTy) where
  toLowLevel highVt = case highVt of
    VariantNil                   -> return VariantNil
    VariantBool             high -> return $ VariantBool high
    VariantInt              high -> return $ VariantInt  high
    VariantReal             high -> return $ VariantReal high
    VariantString           high -> VariantString           <$> toLowLevel high
    VariantVector2          high -> VariantVector2          <$> toLowLevel high
    VariantRect2            high -> VariantRect2            <$> toLowLevel high
    VariantVector3          high -> VariantVector3          <$> toLowLevel high
    VariantTransform2d      high -> VariantTransform2d      <$> toLowLevel high
    VariantPlane            high -> VariantPlane            <$> toLowLevel high
    VariantQuat             high -> VariantQuat             <$> toLowLevel high
    VariantAabb             high -> VariantAabb             <$> toLowLevel high
    VariantBasis            high -> VariantBasis            <$> toLowLevel high
    VariantTransform        high -> VariantTransform        <$> toLowLevel high
    VariantColor            high -> VariantColor            <$> toLowLevel high
    VariantNodePath         high -> VariantNodePath         <$> toLowLevel high
    VariantRid              high -> VariantRid              <$> toLowLevel high
    VariantObject           high -> VariantObject           <$> toLowLevel high
    VariantDictionary       high -> VariantDictionary       <$> toLowLevel high
    VariantArray            high -> VariantArray            <$> toLowLevel high
    VariantPoolByteArray    high -> VariantPoolByteArray    <$> toLowLevel high
    VariantPoolIntArray     high -> VariantPoolIntArray     <$> toLowLevel high
    VariantPoolRealArray    high -> VariantPoolRealArray    <$> toLowLevel high
    VariantPoolStringArray  high -> VariantPoolStringArray  <$> toLowLevel high
    VariantPoolVector2Array high -> VariantPoolVector2Array <$> toLowLevel high
    VariantPoolVector3Array high -> VariantPoolVector3Array <$> toLowLevel high
    VariantPoolColorArray   high -> VariantPoolColorArray   <$> toLowLevel high

  fromLowLevel lowVt = case lowVt of
    VariantNil                  -> return VariantNil
    VariantBool             low -> return $ VariantBool low
    VariantInt              low -> return $ VariantInt  low
    VariantReal             low -> return $ VariantReal low
    VariantString           low -> VariantString           <$> fromLowLevel low
    VariantVector2          low -> VariantVector2          <$> fromLowLevel low
    VariantRect2            low -> VariantRect2            <$> fromLowLevel low
    VariantVector3          low -> VariantVector3          <$> fromLowLevel low
    VariantTransform2d      low -> VariantTransform2d      <$> fromLowLevel low
    VariantPlane            low -> VariantPlane            <$> fromLowLevel low
    VariantQuat             low -> VariantQuat             <$> fromLowLevel low
    VariantAabb             low -> VariantAabb             <$> fromLowLevel low
    VariantBasis            low -> VariantBasis            <$> fromLowLevel low
    VariantTransform        low -> VariantTransform        <$> fromLowLevel low
    VariantColor            low -> VariantColor            <$> fromLowLevel low
    VariantNodePath         low -> VariantNodePath         <$> fromLowLevel low
    VariantRid              low -> VariantRid              <$> fromLowLevel low
    VariantObject           low -> VariantObject           <$> fromLowLevel low
    VariantDictionary       low -> VariantDictionary       <$> fromLowLevel low
    VariantArray            low -> VariantArray            <$> fromLowLevel low
    VariantPoolByteArray    low -> VariantPoolByteArray    <$> fromLowLevel low
    VariantPoolIntArray     low -> VariantPoolIntArray     <$> fromLowLevel low
    VariantPoolRealArray    low -> VariantPoolRealArray    <$> fromLowLevel low
    VariantPoolStringArray  low -> VariantPoolStringArray  <$> fromLowLevel low
    VariantPoolVector2Array low -> VariantPoolVector2Array <$> fromLowLevel low
    VariantPoolVector3Array low -> VariantPoolVector3Array <$> fromLowLevel low
    VariantPoolColorArray   low -> VariantPoolColorArray   <$> fromLowLevel low

instance GodotFFI GodotVariant (Variant 'GodotTy) where
  fromLowLevel var = godot_variant_get_type var >>= \case
      GodotVariantTypeNil         -> return VariantNil
      GodotVariantTypeBool        -> (VariantBool . (/= 0))      <$> godot_variant_as_bool var
      GodotVariantTypeInt         -> (VariantInt . fromIntegral) <$> godot_variant_as_int var
      GodotVariantTypeReal        -> (VariantReal . realToFrac)  <$> godot_variant_as_real var
      GodotVariantTypeString      -> VariantString      <$> godot_variant_as_string var
      GodotVariantTypeVector2     -> VariantVector2     <$> godot_variant_as_vector2 var
      GodotVariantTypeRect2       -> VariantRect2       <$> godot_variant_as_rect2 var
      GodotVariantTypeVector3     -> VariantVector3     <$> godot_variant_as_vector3 var
      GodotVariantTypeTransform2d -> VariantTransform2d <$> godot_variant_as_transform2d var
      GodotVariantTypePlane       -> VariantPlane       <$> godot_variant_as_plane var
      GodotVariantTypeQuat        -> VariantQuat        <$> godot_variant_as_quat var
      GodotVariantTypeAabb        -> VariantAabb        <$> godot_variant_as_aabb var
      GodotVariantTypeBasis       -> VariantBasis       <$> godot_variant_as_basis var
      GodotVariantTypeTransform   -> VariantTransform   <$> godot_variant_as_transform var
      GodotVariantTypeColor       -> VariantColor       <$> godot_variant_as_color var
      GodotVariantTypeNodePath    -> VariantNodePath    <$> godot_variant_as_node_path var
      GodotVariantTypeRid         -> VariantRid         <$> godot_variant_as_rid var
      GodotVariantTypeObject      -> VariantObject      <$> godot_variant_as_object var
      GodotVariantTypeDictionary  -> VariantDictionary  <$> godot_variant_as_dictionary var
      GodotVariantTypeArray       -> VariantArray       <$> godot_variant_as_array var
      GodotVariantTypePoolByteArray    -> VariantPoolByteArray
        <$> godot_variant_as_pool_byte_array    var
      GodotVariantTypePoolIntArray     -> VariantPoolIntArray
        <$> godot_variant_as_pool_int_array     var
      GodotVariantTypePoolRealArray    -> VariantPoolRealArray
        <$> godot_variant_as_pool_real_array    var
      GodotVariantTypePoolStringArray  -> VariantPoolStringArray
        <$> godot_variant_as_pool_string_array  var
      GodotVariantTypePoolVector2Array -> VariantPoolVector2Array
        <$> godot_variant_as_pool_vector2_array var
      GodotVariantTypePoolVector3Array -> VariantPoolVector3Array
        <$> godot_variant_as_pool_vector3_array var
      GodotVariantTypePoolColorArray   -> VariantPoolColorArray
        <$> godot_variant_as_pool_color_array   var

  toLowLevel vt = case vt of
    VariantNil                -> godot_variant_new_nil
    VariantBool             x -> godot_variant_new_bool $ fromIntegral $ fromEnum x
    VariantInt              x -> godot_variant_new_int  $ fromIntegral            x
    VariantReal             x -> godot_variant_new_real $ realToFrac              x
    VariantString           x -> godot_variant_new_string                         x
    VariantVector2          x -> godot_variant_new_vector2                        x
    VariantRect2            x -> godot_variant_new_rect2                          x
    VariantVector3          x -> godot_variant_new_vector3                        x
    VariantTransform2d      x -> godot_variant_new_transform2d                    x
    VariantPlane            x -> godot_variant_new_plane                          x
    VariantQuat             x -> godot_variant_new_quat                           x
    VariantAabb             x -> godot_variant_new_aabb                           x
    VariantBasis            x -> godot_variant_new_basis                          x
    VariantTransform        x -> godot_variant_new_transform                      x
    VariantColor            x -> godot_variant_new_color                          x
    VariantNodePath         x -> godot_variant_new_node_path                      x
    VariantRid              x -> godot_variant_new_rid                            x
    VariantObject           x -> godot_variant_new_object                         x
    VariantDictionary       x -> godot_variant_new_dictionary                     x
    VariantArray            x -> godot_variant_new_array                          x
    VariantPoolByteArray    x -> godot_variant_new_pool_byte_array                x
    VariantPoolIntArray     x -> godot_variant_new_pool_int_array                 x
    VariantPoolRealArray    x -> godot_variant_new_pool_real_array                x
    VariantPoolStringArray  x -> godot_variant_new_pool_string_array              x
    VariantPoolVector2Array x -> godot_variant_new_pool_vector2_array             x
    VariantPoolVector3Array x -> godot_variant_new_pool_vector3_array             x
    VariantPoolColorArray   x -> godot_variant_new_pool_color_array               x

withHsVariantArray
  :: [Variant 'HaskellTy] -> ((Ptr (Ptr GodotVariant), CInt) -> IO a) -> IO a
withHsVariantArray vars mtd = allocaArray (length vars) $ \arrPtr -> do
  vars' <- mapM toLowLevel vars
  withVars vars' 0 arrPtr mtd
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

instance AsHsVariant GodotVariant where
  toHsVariant v = let !res = unsafePerformIO $ fromLowLevel =<< fromLowLevel v in res
  fromHsVariant v = let !res = unsafePerformIO $ toLowLevel =<< toLowLevel v in Just res

instance AsHsVariant () where
  fromHsVariant VariantNil = Just ()
  fromHsVariant _ = Nothing
  toHsVariant _ = VariantNil

$(generateAsHsVariantInstances)


instance GodotFFI CBool Bool where
  fromLowLevel = return . toBool
  toLowLevel = return . fromBool

instance GodotFFI CFloat Float where
  fromLowLevel = return . realToFrac
  toLowLevel = return . realToFrac

instance GodotFFI CInt Int where
  fromLowLevel = return . fromEnum
  toLowLevel = return . toEnum


instance GodotFFI () () where
  fromLowLevel = return . const ()
  toLowLevel = return . const ()


-- Built-in Godot types

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

instance GodotFFI GodotVector2 Vector2 where
  fromLowLevel v = V2
                   <$> (realToFrac <$> godot_vector2_get_x v)
                   <*> (realToFrac <$> godot_vector2_get_y v)
  toLowLevel (V2 x y) = godot_vector2_new (realToFrac x) (realToFrac y)


instance GodotFFI GodotVector3 Vector3 where
  fromLowLevel v = V3
                   <$> (realToFrac <$> godot_vector3_get_axis v GodotVector3AxisX)
                   <*> (realToFrac <$> godot_vector3_get_axis v GodotVector3AxisY)
                   <*> (realToFrac <$> godot_vector3_get_axis v GodotVector3AxisZ)
  toLowLevel (V3 x y z) = godot_vector3_new (realToFrac x) (realToFrac y) (realToFrac z)

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

instance GodotFFI GodotRect2 Rect2 where
  fromLowLevel r = V2
                   <$> (fromLowLevel =<< godot_rect2_get_position r)
                   <*> (fromLowLevel =<< godot_rect2_get_size r)
  toLowLevel (V2 pos size) = do pos' <- toLowLevel pos
                                size' <- toLowLevel size
                                godot_rect2_new_with_position_and_size pos' size'

instance GodotFFI GodotAabb AABB where
  fromLowLevel aabb = V2
                      <$> (fromLowLevel =<< godot_aabb_get_position aabb)
                      <*> (fromLowLevel =<< godot_aabb_get_size aabb)
  toLowLevel (V2 pos size) = do pos'  <- toLowLevel pos
                                size' <- toLowLevel size
                                godot_aabb_new pos' size'

-- Axes X, Y and Z are represented by the int constants 0, 1 and 2 respectively (at least for Vector3):
-- https://godot.readthedocs.io/en/latest/classes/class_vector3.html?highlight=axis#numeric-constants
instance GodotFFI GodotBasis Basis where
  fromLowLevel b = V3 <$> (llAxis 0) <*> (llAxis 1) <*> (llAxis 2)
    where llAxis axis = fromLowLevel =<< godot_basis_get_axis b axis
  toLowLevel (V3 x y z) = do
    x' <- toLowLevel x
    y' <- toLowLevel y
    z' <- toLowLevel z
    godot_basis_new_with_rows x' y' z'

instance GodotFFI GodotTransform Transform where
  fromLowLevel tf = do
    basis <- fromLowLevel =<< godot_transform_get_basis tf
    origin <- fromLowLevel =<< godot_transform_get_origin tf
    return $ TF basis origin
  toLowLevel (TF basis orig) = do
    basis' <- toLowLevel basis
    orig' <- toLowLevel orig
    godot_transform_new basis' orig'

instance GodotFFI GodotTransform2d Transform2D where
  fromLowLevel tf = do
    rot <- fromLowLevel =<< godot_transform2d_get_rotation tf
    origin <- fromLowLevel =<< godot_transform2d_get_origin tf
    return $ TF2D rot origin
  toLowLevel (TF2D rot orig) = do
    rot' <- toLowLevel rot
    orig' <- toLowLevel orig
    godot_transform2d_new rot' orig'

instance GodotFFI GodotPlane Plane where
  fromLowLevel pl = do
    V3 a b c <- fromLowLevel =<< godot_plane_get_normal pl
    d <- fromLowLevel =<< godot_plane_get_d pl
    return $ Plane a b c d
  toLowLevel (Plane a b c d) = do
    [a', b', c', d'] <- mapM toLowLevel [a, b, c, d] :: IO [CFloat]
    godot_plane_new_with_reals a' b' c' d'


instance GodotFFI GodotNodePath NodePath where
  fromLowLevel np = do
    nm <- fromLowLevel =<< godot_node_path_get_name np 0
    return $ NodePath nm
  toLowLevel (NodePath np) = godot_node_path_new =<< toLowLevel np

instance GodotFFI GodotColor Color where
  fromLowLevel c = do
    [r, g, b, a] <- mapM (\f -> realToFrac <$> f c)
      [ godot_color_get_r, godot_color_get_g, godot_color_get_b, godot_color_get_a]
    return $ withOpacity (sRGB r g b) a
  toLowLevel rgba =
    let RGB r g b = toSRGB (rgba `over` black)
        [r', g', b', a'] = realToFrac <$> [r, g, b, alphaChannel rgba]
    in  godot_color_new_rgba r' g' b' a'

instance GodotFFI GodotObject GodotObject where
  fromLowLevel = return
  toLowLevel = return

instance GodotFFI GodotRid RID where
  fromLowLevel rid = RID rid <$> fromIntegral <$> godot_rid_get_id rid
  toLowLevel (RID rid _) = return rid


-- Godot Arrays

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

instance GodotFFI GodotArray Array where
  fromLowLevel low =
    fromLowLevelArray godot_array_size godot_array_get low >>= mapM fromLowLevel
  toLowLevel high =
    mapM toLowLevel high >>= toLowLevelArray godot_array_new godot_array_append

-- We can't use fromLowLevelArray or toLowLevelArray here as Word8 is not an
-- instance of AsVariant.
instance GodotFFI GodotPoolByteArray ByteArray where
  fromLowLevel vs = do
    size <- fromIntegral <$> godot_pool_byte_array_size vs
    let maybeNext n v = if n == (size - 1) then Nothing else Just (v, n + 1)
    let variantAt n = maybeNext n <$> (godot_pool_byte_array_get vs n)
    Vec.unfoldrM variantAt 0
  toLowLevel vs = do
    array <- godot_pool_byte_array_new
    mapM_ (godot_pool_byte_array_append array) vs
    return array

instance GodotFFI GodotPoolIntArray IntArray where
  fromLowLevel = fromLowLevelArray godot_pool_int_array_size godot_pool_int_array_get
  toLowLevel = toLowLevelArray godot_pool_int_array_new godot_pool_int_array_append

instance GodotFFI GodotPoolRealArray RealArray where
  fromLowLevel = fromLowLevelArray godot_pool_real_array_size godot_pool_real_array_get
  toLowLevel = toLowLevelArray godot_pool_real_array_new godot_pool_real_array_append

instance GodotFFI GodotPoolStringArray StringArray where
  fromLowLevel = fromLowLevelArray godot_pool_string_array_size godot_pool_string_array_get
  toLowLevel = toLowLevelArray godot_pool_string_array_new godot_pool_string_array_append

instance GodotFFI GodotPoolVector2Array Vector2Array where
  fromLowLevel = fromLowLevelArray godot_pool_vector2_array_size godot_pool_vector2_array_get
  toLowLevel = toLowLevelArray godot_pool_vector2_array_new godot_pool_vector2_array_append

instance GodotFFI GodotPoolVector3Array Vector3Array where
  fromLowLevel = fromLowLevelArray godot_pool_vector3_array_size godot_pool_vector3_array_get
  toLowLevel = toLowLevelArray godot_pool_vector3_array_new godot_pool_vector3_array_append

instance GodotFFI GodotPoolColorArray ColorArray where
  fromLowLevel = fromLowLevelArray godot_pool_color_array_size godot_pool_color_array_get
  toLowLevel = toLowLevelArray godot_pool_color_array_new godot_pool_color_array_append

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
      vtKey <- toGodotVariant =<< toLowLevel k
      vtVal <- toLowLevel =<< toLowLevel v
      godot_dictionary_set dict vtKey vtVal
    return dict
