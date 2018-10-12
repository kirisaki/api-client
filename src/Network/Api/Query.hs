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
  (
    -- * URL query parameter
    Query
  , toQuery
  , toQueryUtf8
  , toQuery'
  , toQueryUtf8'
  , toQueryWith
  , fromQuery
  , fromQueryUtf8
  , fromQueryWith

    -- * URL encoded string
  , UrlEncoded
  , urlEncode
  , urlEncodeUtf8
  , urlDecode
  , urlDecodeUtf8
  ) where

import           Data.Aeson
import           Data.Aeson.Encoding      (text)
import qualified Data.ByteString          as BSS
import           Data.Functor.Identity
import           Data.Hashable
import           Data.HashMap.Strict      as HM
import qualified Data.List                as L
import qualified Data.Text                as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import qualified Network.HTTP.Types.URI   as U

-- | Collection of URL query parameters.
--   Behaviour when duplicated query keys at URL is not defined,
--   but it makes implements complecated, so treat keys as unique in this module.
type Query = HM.HashMap UrlEncoded (Maybe UrlEncoded)


-- | Construct 'Query' with the supplied mappings.
toQuery :: [(BSS.ByteString, Maybe BSS.ByteString)] -> Query
toQuery = toQueryWith id

-- | Utf-8 version of 'toQuery'
toQueryUtf8 :: [(T.Text, Maybe T.Text)] -> Query
toQueryUtf8 = toQueryWith encodeUtf8

-- | Construct 'Query' without parameter-less field.
toQuery' :: [(BSS.ByteString, BSS.ByteString)] -> Query
toQuery' = toQueryWith' id

-- | Utf-8 version of 'toQuery\''
toQueryUtf8' :: [(T.Text, T.Text)] -> Query
toQueryUtf8' = toQueryWith' encodeUtf8

-- | To 'Query' with mapping functions.
toQueryWith :: (a -> BSS.ByteString) -> [(a, Maybe a)] -> Query
toQueryWith f = HM.fromList . L.map (\(k, v) -> (urlEncode $ f k, urlEncode . f <$> v))

-- | To 'Query' with mapping functions without parameter-less field.
toQueryWith' :: (a -> BSS.ByteString) -> [(a, a)] -> Query
toQueryWith' f = HM.fromList . L.map (\(k, v) -> (urlEncode $ f k, (Just . urlEncode . f) v))

-- | Return a list of 'ByteString' encoded fields.
fromQuery :: Query -> [(BSS.ByteString, Maybe BSS.ByteString)]
fromQuery = fromQueryWith id

-- | Utf-8 version 'fromQuery'.
fromQueryUtf8 :: Query -> [(T.Text, Maybe T.Text)]
fromQueryUtf8 = fromQueryWith (decodeUtf8With ignore)

-- | To 'Query' with mapping functions.
fromQueryWith :: (BSS.ByteString -> a)  -> Query -> [(a, Maybe a)]
fromQueryWith f = L.map (\(k, v) -> (f $ urlDecode k, f . urlDecode <$> v)) . HM.toList

-- | URIEncoded 'ByteString'
newtype UrlEncoded = UrlEncoded
  { unUrlEncoded :: BSS.ByteString -- ^ Unwrap encoded string.
  } deriving (Eq, Show, Ord)

instance Hashable UrlEncoded where
  hashWithSalt i = hashWithSalt i . unUrlEncoded

instance ToJSON UrlEncoded where
  toJSON = String . urlDecodeUtf8

instance ToJSONKey UrlEncoded where
  toJSONKey = ToJSONKeyText f g
    where
      f = urlDecodeUtf8
      g = text . f

instance FromJSON UrlEncoded where
  parseJSON = withText "UrlEncoded" (return . urlEncodeUtf8)

instance FromJSONKey UrlEncoded where
  fromJSONKey = FromJSONKeyTextParser (return . urlEncodeUtf8)

-- | ByteString text to URI encoded bytestring.
--   It converts '='
urlEncode :: BSS.ByteString -> UrlEncoded
urlEncode = UrlEncoded . U.urlEncode True

-- | Utf-8 version of 'urlEncode'.
urlEncodeUtf8 :: T.Text -> UrlEncoded
urlEncodeUtf8 = urlEncode . encodeUtf8

-- | Decode URI encoded bytestrings to ByteString.
urlDecode :: UrlEncoded -> BSS.ByteString
urlDecode = U.urlDecode True . unUrlEncoded

-- | Utf-8 version of 'urlDecode'.
urlDecodeUtf8 :: UrlEncoded -> T.Text
urlDecodeUtf8 = decodeUtf8With ignore . urlDecode

