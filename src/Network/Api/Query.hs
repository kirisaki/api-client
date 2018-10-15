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
  ) where

import           Network.Api.Url

import           Data.Aeson
import qualified Data.ByteString          as SBS
import           Data.HashMap.Strict      as HM
import qualified Data.List                as L
import qualified Data.Text                as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error

-- | Collection of URL query parameters.
--   Behaviour when duplicated query keys at URL is not defined,
--   but it makes implements complecated, so treat keys as unique in this module.
type Query = HM.HashMap UrlEncoded (Maybe UrlEncoded)

-- | Construct 'Query' with the supplied mappings.
toQuery :: [(SBS.ByteString, Maybe SBS.ByteString)] -> Query
toQuery = toQueryWith id

-- | Utf-8 version of 'toQuery'
toQueryUtf8 :: [(T.Text, Maybe T.Text)] -> Query
toQueryUtf8 = toQueryWith encodeUtf8

-- | Construct 'Query' without parameter-less field.
toQuery' :: [(SBS.ByteString, SBS.ByteString)] -> Query
toQuery' = toQueryWith' id

-- | Utf-8 version of 'toQuery\''
toQueryUtf8' :: [(T.Text, T.Text)] -> Query
toQueryUtf8' = toQueryWith' encodeUtf8

-- | To 'Query' with mapping functions.
toQueryWith :: (a -> SBS.ByteString) -> [(a, Maybe a)] -> Query
toQueryWith f = HM.fromList . L.map (\(k, v) -> (urlEncode $ f k, urlEncode . f <$> v))

-- | To 'Query' with mapping functions without parameter-less field.
toQueryWith' :: (a -> SBS.ByteString) -> [(a, a)] -> Query
toQueryWith' f = HM.fromList . L.map (\(k, v) -> (urlEncode $ f k, (Just . urlEncode . f) v))

-- | Return a list of 'ByteString' encoded fields.
fromQuery :: Query -> [(SBS.ByteString, Maybe SBS.ByteString)]
fromQuery = fromQueryWith id

-- | Utf-8 version 'fromQuery'.
fromQueryUtf8 :: Query -> [(T.Text, Maybe T.Text)]
fromQueryUtf8 = fromQueryWith (decodeUtf8With ignore)

-- | From 'Query' with mapping functions.
fromQueryWith :: (SBS.ByteString -> a)  -> Query -> [(a, Maybe a)]
fromQueryWith f = L.map (\(k, v) -> (f $ urlDecode k, f . urlDecode <$> v)) . HM.toList
