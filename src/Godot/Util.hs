{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Godot.Util
  ( godotPrint
  , getSingleton
  )
where

import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import           Data.Typeable                            ( Typeable )

import           Godot.Internal.Dispatch       as M
                                                          ( (:<) )
import qualified Godot.Gdnative.Internal.Api   as Api
import           Godot.Gdnative.Internal.Gdnative         ( GodotObject )
import           Godot.Gdnative.Types                     ( AsVariant
                                                          , Variant(..)
							  , toLowLevel
                                                          )
import           Godot.Nativescript

import           Foreign.C                                ( withCString )


godotPrint :: Text -> IO ()
godotPrint txt = do
  gstr <- toLowLevel txt
  Api.godot_print gstr

getSingleton
  :: forall a . (Typeable a, AsVariant a, GodotObject :< a) => IO (Maybe a)
getSingleton = withCString (T.unpack $ T.dropWhile (== '_') $ nameOf @a)
  $ \name -> Api.godot_global_get_singleton name >>= tryCast
