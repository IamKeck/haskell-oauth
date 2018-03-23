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
  auth_base <- createAuthBaseDefault key
  let hmac_key = createHmacKey key 
  let signature = createSignature False url param auth_base hmac_key
  let auth_header = createAuthHeader auth_base signature
  let bs_param = map (\(f,s) -> (CB.pack f, Just . CB.pack $ s)) param
  request <- parseRequest url >>=
    return . (addRequestHeader "Authorization" $ CB.pack auth_header) .
    setRequestQueryString bs_param
  response <- httpBS request
  print $ getResponseBody response




