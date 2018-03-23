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
  consumer_secret <- getEnv "CS"
  consumer_key <- getEnv "CK"
  access_token <- getEnv "AT"
  access_secret <- getEnv "AS"
  auth_base <- createAuthBaseDefault consumer_key access_token
  let hmac_key = createHmacKey consumer_secret access_secret
  let signature = createSignature False url param auth_base hmac_key
  let auth_header = createAuthHeader auth_base signature
  let bs_param = map (\(f,s) -> (CB.pack f, Just . CB.pack $ s)) param
  request <- parseRequest url >>=
    return . (addRequestHeader "Authorization" $ CB.pack auth_header) .
    setRequestQueryString bs_param
  response <- httpBS request
  print $ getResponseBody response




