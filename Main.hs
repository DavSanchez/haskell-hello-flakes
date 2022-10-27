{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Process (readProcess)
import Data.List (sort)
import Network.HTTP.Simple
    ( defaultRequest,
      setRequestHeader,
      setRequestHost,
      setRequestPath,
      httpJSON,
      JSONException,
      getResponseBody,
      parseRequest,
      setRequestMethod )
import Network.HTTP.Client.Conduit (Response, setQueryString)
import Data.Aeson ( Value )
import Data.Aeson.Lens (values, key, _String, _Array, AsValue)
import Control.Lens ( (^..) )
import qualified Data.ByteString.Lazy.Char8 as S8
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Foldable (traverse_)
import Data.Yaml (decodeFileEither)
import System.OsPath

main :: IO ()
main = do
  res <- prometheusRequest
  traverse_ TIO.putStrLn (findValueList res)

prometheusRequest :: IO Value
prometheusRequest = do
  req' <- parseRequest "http://prometheus.url.com"
  let request
        = setRequestMethod "GET"
        $ setRequestPath "/api/v1/query"
        $ setQueryString [("query", Just "query_string")]
        req'
  res <- httpJSON request
  pure (getResponseBody res :: Value)

findValueList :: AsValue s => s -> [T.Text]
findValueList r = do
  let valueListLens
        = key "key1"
        . key "key2"
        . values
        . key "key3"
        . key "key4"
        . _String
  r^..valueListLens
 