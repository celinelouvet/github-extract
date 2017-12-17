{-# LANGUAGE OverloadedStrings #-}

module Github.Authorization
    ( getAuthorization
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (pack, unpack)
import System.Environment

import Github.Types (Authorization(..))

getAuthorization :: IO (Maybe Authorization)
getAuthorization =
  buildAuthorization <$> getEnv "GITHUB_USERNAME"
            <*> getEnv "GITHUB_PASSWORD"

buildAuthorization :: String -> String -> Maybe Authorization
buildAuthorization username password = maybeBasicAuth (encodeBase64 $ buildToken username password)

buildToken :: String -> String -> String
buildToken username password = username ++ ":" ++ password

encodeBase64 :: String -> ByteString
encodeBase64 = encode . pack

basicAuth :: ByteString -> Authorization
basicAuth t = Authorization $ "Basic " ++ (unpack t)

maybeBasicAuth :: ByteString -> Maybe Authorization
maybeBasicAuth = Just . basicAuth

