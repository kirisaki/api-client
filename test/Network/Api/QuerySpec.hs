{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Api.QuerySpec where

import           Network.Api.Query
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           TestUtils

import           Data.Aeson
import qualified Data.ByteString       as BSS
import qualified Data.HashMap.Strict   as HM
import qualified Data.List             as L
import qualified Data.Text             as T
import           Data.Text.Encoding

spec :: Spec
spec =
  describe "Query function props" $ do

  prop "toJSON/fromJSON for Query" $
    \kvs -> (HM.fromList . ascii) kvs ==
    (HM.fromList . L.sort . fromQuery .
     success . fromJSON . toJSON . toQuery . ascii) kvs

  prop " urlEncode/urlDecode" $
    \t -> t == (BSS.unpack . urlDecode . urlEncode . BSS.pack) t

  prop "toQuery/fromQuery" $
    \kvs -> (HM.fromList . ascii) kvs ==
    (HM.fromList . L.sort . fromQuery . toQuery . ascii) kvs

  where
    ascii = L.sort . L.map (
      \(k, v) -> ( (encodeUtf8 . T.pack . getASCIIString) k
                 , fmap (encodeUtf8 . T.pack . getASCIIString) v
                 ) :: (BSS.ByteString, Maybe BSS.ByteString)
      )



