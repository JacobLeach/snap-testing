{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Pool

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop foo <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    case param of
      Nothing -> writeBS "sad"
      Just a -> do
        liftIO $ print a
        liftIO $ threadDelay 9000000
        writeBS a

foo :: Snap ()
foo = do
  liftIO $ putStrLn "foo"
  liftIO $ threadDelay 9000000
  liftIO $ putStrLn "bar"
  writeBS "hello world"


