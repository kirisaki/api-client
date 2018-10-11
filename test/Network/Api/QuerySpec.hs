{-# LANGUAGE OverloadedStrings #-}

module Network.Api.QuerySpec where

import           Network.Api.Query
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           TestUtils

import           Data.Aeson
import           Data.ByteString
import qualified Data.HashMap.Strict   as HM
import qualified Data.List             as L
import qualified Data.Text             as T
import           Data.Text.Encoding

spec :: Spec
spec =
  describe "Query function props" $ do

  prop "toJSON/fromJSON for Query" $
    \kvs -> (HM.fromList . L.sort) kvs ==
    (HM.fromList . L.sort . unpack' . fromQuery .
     success . fromJSON . toJSON . toQuery . pack' . L.sort) kvs

  prop " urlEncode/urlDecode" $
    \t -> t == (T.unpack . urlDecode . urlEncode . T.pack) t

  prop "toQuery/fromQuery" $
    \kvs -> (HM.fromList . L.sort) kvs ==
    (HM.fromList . L.sort . unpack' . fromQuery . toQuery . pack' . L.sort) kvs

  where
    apply f (k, v) = (f k, fmap f v)
    pack' = L.map (apply T.pack)
    unpack' = L.map (apply T.unpack)
