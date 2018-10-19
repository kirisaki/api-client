{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK not-home    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Network.Api.Url
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
module Network.Api.Url
  (
    Url
  , Port
  , fromPort
  , toPort
    -- * URL query parameter
  , Query
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

    -- * Utilities
  , inject
  ) where

import           Network.Api.Parser

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Encoding        (text)
import           Data.Attoparsec.ByteString as A
import qualified Data.ByteString            as SBS
import qualified Data.ByteString.Lazy       as LBS
import           Data.Char                  (ord)
import           Data.Hashable
import           Data.HashMap.Strict        as HM
import qualified Data.List                  as L
import           Data.Text                  as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import           Data.Word
import           Data.Word8
import qualified Network.HTTP.Types.URI     as U

-- | Convert Url to 'ByteString'
fromUrl :: Url -> SBS.ByteString
fromUrl (Url s a h p ps) = undefined

-- | Wrapped URL.
data Url = Url
  { scheme  :: Scheme
  , auth    :: Maybe Auth
  , host    :: Host
  , port    :: Maybe Port
  , urlPath :: UrlPath
  } deriving (Show, Ord, Eq)

-- | URL scheme.
data Scheme
  = Http
  | Https
  deriving (Show, Ord, Eq, Read)

fromScheme :: Scheme -> SBS.ByteString
fromScheme Http  = "http"
fromScheme Https = "https"

toScheme :: SBS.ByteString -> Maybe Scheme
toScheme "http"  = Just Http
toScheme "https" = Just Https
toScheme _       = Nothing

-- | A pair of user and password.
newtype Auth = Auth (UrlEncoded, UrlEncoded) deriving (Show, Ord, Eq)

-- | Wrapped port number. Port number is limited from 0 to 65535.
--   See <https://tools.ietf.org/html/rfc793#section-3.1 RFC793>.
newtype Port = Port
  { fromPort :: Int
  } deriving (Show, Ord, Eq)

toPort :: Int -> Either T.Text Port
toPort n =
  if 0 <= n && n <= 65535
  then (Right . Port) n
  else Left "Invalid port number"

-- | Wrapped hostname or IP address. IPv6 is valid too.
--   It can't deal with Punycode hostname.
newtype Host = Host
  { fromHost :: SBS.ByteString
  } deriving (Show, Ord, Eq)

toHost :: SBS.ByteString -> Either T.Text Host
toHost t =
  let
    p = hostname -- <|> ipv4 <|> ipv6
  in
    case feed (parse p t) "" of
      Done "" h -> Right h
      _         -> Left "Failed to parse host."

hostname :: Parser Host
hostname = Host <$>
  mconcat
  [ takeWhile1 isAlphaNum
  , option "" (A.takeWhile (\c ->
                            isAlphaNum c ||
                            c == fromIntegral (ord '-'))
            )
  , takeWhile1 isAlphaNum
  ]
-- | Wrapped path.
newtype UrlPath = UrlPath
  { unUrlPath :: SBS.ByteString
  } deriving (Show, Ord, Eq)

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

-- | URIEncoded 'ByteString'.
newtype UrlEncoded = UrlEncoded
  { unUrlEncoded :: SBS.ByteString -- ^ Unwrap encoded string.
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
--   It converts ' '(0x20) to '+'
urlEncode :: SBS.ByteString -> UrlEncoded
urlEncode = urlEncodeWith id

-- | Utf-8 version of 'urlEncode'.
urlEncodeUtf8 :: T.Text -> UrlEncoded
urlEncodeUtf8 = urlEncodeWith encodeUtf8

-- | Encode with a encoder.
urlEncodeWith :: (a -> SBS.ByteString) -> a -> UrlEncoded
urlEncodeWith enc = UrlEncoded . U.urlEncode True . enc

-- | Decode URI encoded 'Builder' to strict 'ByteString'.
urlDecode :: UrlEncoded -> SBS.ByteString
urlDecode = urlDecodeWith id

-- | Utf-8 version of 'urlDecode'.
urlDecodeUtf8 :: UrlEncoded -> T.Text
urlDecodeUtf8 = urlDecodeWith $ decodeUtf8With ignore

-- | Decode with a decoder.
urlDecodeWith :: (SBS.ByteString -> a) -> UrlEncoded -> a
urlDecodeWith dec (UrlEncoded t) = (dec . U.urlDecode True) t

-- | Inject parameters to a path represented with colon or braces.
inject :: Text -> [(Text, Text)] -> Either Text Text
inject path params = undefined
