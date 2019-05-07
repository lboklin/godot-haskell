{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Lib
  ( exports
  )
where

import           Godot

import           Control.Monad                            ( when )
import qualified Data.Text                     as T

exports :: GdnativeHandle -> IO ()
exports desc = do
  registerClass $ RegClass desc $ classInit @Main
  registerClass $ RegClass desc $ classInit @SimpleNode


data Main = Main
  { _mBase :: GodotNode
  , _mTime :: TVar Float
  }

instance HasBaseClass Main where
  type BaseClass Main = GodotNode
  super = _mBase
instance NativeScript Main where
  classInit base = Main base <$> newTVarIO 0
  classMethods =
    [ func NoRPC "_ready" $ \self _ -> do
        godotPrint "Ready"

        Just rl <- getSingleton @Godot_ResourceLoader
        Just ns <- load rl "res://SimpleNode.gdns" "NativeScript" False >>= tryCast
        Just simpleNode <- new (ns :: GodotNativeScript) [] >>= asNativeScriptClass @SimpleNode

        set_name (super simpleNode) "Simple"
        add_child self (super simpleNode) False
        name <- get_name (super simpleNode)
        godotPrint $ T.unwords ["Added", name, "as child"]


    , func NoRPC "_process" $ \self [VariantReal delta] -> do
        (sec, sec') <- atomically $ do
            t <- readTVar (_mTime self)
            let t' = t + delta
            writeTVar (_mTime self) t'
            return (floor t, floor t') :: STM (Int, Int)

        when (sec < sec') $
          godotPrint $ T.pack $ "Seconds passed: " ++ Prelude.show sec'
    ]


newtype SimpleNode = SimpleNode GodotNode

instance HasBaseClass SimpleNode where
  type BaseClass SimpleNode = GodotNode
  super (SimpleNode base) = base
instance NativeScript SimpleNode where
  classInit base = return $ SimpleNode base
  classMethods =
    [ func NoRPC "_ready" $ \self _ -> do
        name <- get_name self
        godotPrint $ T.unwords ["Hi, I'm", name]
    ]



