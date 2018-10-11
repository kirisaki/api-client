{-# LANGUAGE OverloadedStrings #-}

module Network.Api.QuerySpec where

import           Network.Api.Query
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           TestUtils

import           Data.ByteString
import qualified Data.HashMap.Strict   as HM
import qualified Data.List             as L
import qualified Data.Text             as T
import           Data.Text.Encoding

spec :: Spec
spec = do
  describe "Query and JSON can convert mutal" specConvertQueryJSON
  describe "Convert UrlEncoded and Text each other" specUrlEncoded
  describe "Convert key-value pairs and Query each other" specConvertQueryList

specConvertQueryJSON :: Spec
specConvertQueryJSON = do
  it "normal case"
    pending
  it "include Nothing"
    pending

specUrlEncoded :: Spec
specUrlEncoded =
  prop "Encode text then decode, it should back the original" $
    \t -> t == (T.unpack . urlDecode . urlEncode . T.pack) t

specConvertQueryList :: Spec
specConvertQueryList =
  it ""
    pending
