{-# OPTIONS_HADDOCK not-home    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Network.Api.Query
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
module Network.Api.Query
  ( Query
  , toQuery
  , toQuery'
  , fromQuery
  , UrlEncoded
  , urlEncode
  , urlDecode
  ) where

import           Data.Aeson
import qualified Data.ByteString        as BSS
import           Data.Hashable
import           Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import           Data.Text.Encoding
import qualified Network.HTTP.Types.URI as U

-- | Collection of URL query parameters.
--   Behaviour when duplicated query keys at URL is not defined,
--   but it makes implements complecated, so treat keys as unique in this module.
type Query = HM.HashMap UrlEncoded (Maybe UrlEncoded)

-- | URIEncoded 'ByteString'
newtype UrlEncoded = UrlEncoded
  { unUrlEncoded :: BSS.ByteString -- ^ Unwrap encoded string.
  } deriving (Eq, Show, Ord)

instance Hashable UrlEncoded where
  hashWithSalt i = hashWithSalt i . unUrlEncoded

instance ToJSON UrlEncoded where
  toJSON = undefined

instance ToJSONKey UrlEncoded where
  toJSONKey = undefined

instance FromJSON UrlEncoded where
  parseJSON = undefined

instance FromJSONKey UrlEncoded where
  fromJSONKey = undefined

-- | Encode utf-8 text to URI encoded bytestrings.
--   It converts '='
urlEncode :: T.Text -> UrlEncoded
urlEncode = UrlEncoded . U.urlEncode True . encodeUtf8

-- | Decode URI encoded bytestrings to utf-8 text.
urlDecode :: UrlEncoded -> T.Text
urlDecode = decodeUtf8 . U.urlDecode True . unUrlEncoded

toQuery :: [(T.Text, Maybe T.Text)] -> Query
toQuery kvs = undefined

toQuery' :: [(T.Text, T.Text)] -> Query
toQuery' kvs = undefined

fromQuery :: Query -> [(T.Text, Maybe T.Text)]
fromQuery kvs = undefined
