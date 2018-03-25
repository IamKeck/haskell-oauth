{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Network.HTTP.Simple
import System.Environment
import qualified Data.ByteString.Char8 as CB

main :: IO ()
main = do
  let url = "https://api.twitter.com/1.1/statuses/home_timeline.json"
  let param = [("count", "5"), ("exclude_replies", "false")]
  key <- TwitterKey <$> (getEnv "CK") <*> (getEnv "CS") <*> (getEnv "AT") <*> (getEnv "AS")
  createRequest False url param [] key >>= httpBS >>= print . getResponseBody




