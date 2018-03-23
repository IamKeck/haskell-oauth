module Lib where
import Network.URI.Encode (encode)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (random, randomRIO)
import Data.List (sortOn, intercalate)
import Data.HMAC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as CB
import qualified Data.ByteString.Base64  as B64 (encode)

createHmacKey :: String -> String -> String
createHmacKey consumer_secret access_secret = (encode consumer_secret) ++ "&" ++ (encode access_secret)

createAuthBase :: String -> String -> String -> String -> [(String, String)]
createAuthBase consumer token nonce timestamp = [
  ("oauth_consumer_key", consumer),
  ("oauth_nonce", nonce),
  ("oauth_signature_method", "HMAC-SHA1"),
  ("oauth_timestamp", timestamp),
  ("oauth_token", token),
  ("oauth_version", "1.0")]


createAuthBaseDefault :: String -> String -> IO [(String, String)]
createAuthBaseDefault consumer token = do
  time <- show . floor . realToFrac <$> getPOSIXTime
  nonce <- show <$> randomRIO (0, 1.0 :: Float)
  return $ createAuthBase consumer token nonce time


createSignature :: Bool -> String -> [(String, String)] -> [(String, String)] -> String -> String
createSignature is_post url param auth_base hmac_key =
  let
    auth_param = auth_base ++ param
    encoded_param = [(encode k, encode v) | (k, v) <- auth_param ]
    sorted_param = sortOn fst encoded_param
    auth_str = intercalate "&" [k ++ "=" ++ v | (k, v) <- sorted_param] :: String
    method = if is_post then "POST" else "GET"
    auth_base_str = method ++ "&" ++ (encode url) ++ "&" ++ (encode auth_str) :: String
    str_to_octets = B.unpack . CB.pack
  in
    CB.unpack . B64.encode . B.pack $ hmac_sha1 (str_to_octets hmac_key) (str_to_octets auth_base_str)

createAuthHeader :: [(String, String)] -> String -> String
createAuthHeader auth_base signature =
  let
    auth_base_sorted = sortOn fst $ ("oauth_signature", signature) :  auth_base
  in
    "OAuth " ++ intercalate ", " [(encode k) ++ "=\"" ++ (encode v) ++ "\""  | (k, v) <- auth_base_sorted]

