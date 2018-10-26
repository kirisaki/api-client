{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Api.UrlSpec where

import           Network.Api.Internal
import           Network.Api.Url
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text
import           TestUtils

import           Data.Aeson
import qualified Data.ByteString                as SBS
import           Data.Char
import qualified Data.HashMap.Strict            as HM
import qualified Data.List                      as L
import           Data.String                    (IsString)
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Data.Word

spec :: Spec
spec = do
  prop "fromPort/toPort" $
    \(n :: Word16) ->
      (T.pack . show) n == (fromPort . right . toPort . T.pack . show) n
  prop "fromUrlPath/toUrlPath" $
    \path ->
      ( T.cons '/' .
        T.intercalate "/" .
        L.filter notRelative  .
        T.splitOn "/"
      ) path
      ==
      (fromUrlPath . toUrlPath) path
  prop " urlEncode/urlDecode" $
    \t -> t == (urlDecode . urlEncode) t
  prop "toJSON/fromJSON for Query" $
    \kvs -> kvs ==
     (fromQuery .
     success . fromJSON . toJSON . toQuery') kvs
  prop "toQuery/fromQuery" $
    \kvs -> kvs ==
    (fromQuery . toQuery') kvs

