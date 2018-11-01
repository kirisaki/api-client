{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
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
    -- * URL
    Url (..)
  , parseUrl
  , buildUrl
    -- * Scheme
  , Scheme
  , fromScheme
  , toScheme
    -- * Authority
  , Authority (..)
  , buildAuthority
  , parseAuthority
    -- ** Userinfo
  , Userinfo
  , fromUserinfo
  , toUserinfo
    -- ** Host
  , Host
  , buildHost
  , parseHost
    -- ** Port
  , Port
  , fromPort
  , toPort
    -- * Path
  , UrlPath
  , buildUrlPath
  , parseUrlPath
  , toUrlPath
  , fromUrlPath
    -- * URL query parameter
  , Query
  , buildQuery
  , parseQuery
  , fromQuery
  , toQuery
  , toQuery'
    -- * URL encoded string
  , UrlEncoded
  , urlEncode
  , urlEncodeBS
  , urlDecode
  , urlDecodeBS
  , urlP
  ) where

import           Network.Api.Internal

import           Control.Applicative      as AP
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encoding      (text)
import           Data.Attoparsec.Text     as AT
import qualified Data.ByteString          as SBS
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy     as LBS
import           Data.Char
import           Data.Functor
import           Data.Hashable
import           Data.HashMap.Strict      as HM
import qualified Data.List                as L
import           Data.String              (IsString)
import           Data.Text                as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import qualified Data.Vector              as V
import           Data.Word
import qualified Dhall                    as DH
import           Dhall.Core
import qualified Network.HTTP.Types.URI   as U

-- | Parse Url
parseUrl :: T.Text -> Either Text Url
parseUrl = parse' urlP "Failed parsing Url"

-- | Build URL as 'Text'
buildUrl :: Url -> T.Text
buildUrl (Url s a p q) =
  fromScheme s <> "://" <>
  buildAuthority a <> "/" <>
  if L.null (unUrlPath p)
  then ""
  else buildUrlPath p
  <>
  if L.null (unQuery q)
  then ""
  else "?" <> buildQuery q

-- | Build URL as 'ByteString'
buildUrlBS :: Url -> SBS.ByteString
buildUrlBS (Url s a p q) =
  LBS.toStrict . toLazyByteString $
  (string7 . T.unpack . fromScheme) s <>
  byteString "://" <>
  authorityBuilderBS a <>
  char7 '/' <>
  urlPathBuilderBS p <>
  if L.null (unQuery q)
  then mempty
  else char7 '?' <> queryBuilderBS q


urlP :: Parser Url
urlP = Url <$>
       (schemeP <* string "://") <*>
       authorityP <*>
       option (UrlPath []) (char '/' *> urlPathP) <*>
       option (Query []) (char '?' *> queryP)

-- | Wrapped URL.
data Url = Url
  { scheme    :: Scheme
  , authority :: Authority
  , urlPath   :: UrlPath
  , query     :: Query
  } deriving (Show, Eq)

-- | URL scheme.
data Scheme
  = Http
  | Https
  deriving (Show, Ord, Eq, Read)

fromScheme :: Scheme -> T.Text
fromScheme Http  = "http"
fromScheme Https = "https"

toScheme :: T.Text -> Either T.Text Scheme
toScheme = parse' schemeP "Failed parsing scheme"

schemeP :: Parser Scheme
schemeP =
  (string "https" $> Https) <|>
  (string "http" $> Http)

-- | A pair of user and password.
data Authority = Authority
  { userinfo :: Maybe Userinfo
  , host     :: Host
  , port     :: Maybe Port
  } deriving (Show, Eq)

buildAuthority :: Authority -> T.Text
buildAuthority auth =
  maybe "" ((`T.snoc` '@') . fromUserinfo) (userinfo auth) <>
  buildHost (host auth) <>
  maybe "" (T.cons ':' . fromPort) (port auth)

parseAuthority :: T.Text -> Either Text Authority
parseAuthority = parse' authorityP "Failed parsing Authority"

authorityBuilderBS :: Authority -> Builder
authorityBuilderBS (Authority u h p) =
  case u of
    Just u' -> unUrlEncoded (unUserinfo u') <> char7 '@'
    Nothing -> mempty
  <>
  unHost h <>
  case p of
    Just p' -> char7 ':' <> (string7 . show . unPort) p'
    Nothing -> mempty

authorityP :: Parser Authority
authorityP =
  Authority <$>
  optional (toUserinfo . T.pack <$> (many (satisfy (\c -> c /= '@' && c /= '/' )) <* char '@')) <*>
  hostP <*>
  optional (char ':' *> portP)

  -- | Wrapped userinfo
newtype Userinfo = Userinfo
  { unUserinfo :: UrlEncoded
  } deriving (Show, Eq)

fromUserinfo :: Userinfo -> T.Text
fromUserinfo = urlDecode . unUserinfo

toUserinfo :: T.Text -> Userinfo
toUserinfo = Userinfo . urlEncode

-- | Wrapped hostname.
--   It can't deal with IPv6 and Internationalized Domain Name yet.
newtype Host = Host { unHost :: Builder }

instance Show Host where
  show = T.unpack . decodeUtf8With ignore .
    LBS.toStrict . toLazyByteString . unHost

instance Eq Host where
  x == y = toLazyByteString (unHost x) ==
           toLazyByteString (unHost y)

buildHost :: Host -> T.Text
buildHost = decodeUtf8With ignore . LBS.toStrict . toLazyByteString . unHost

parseHost :: T.Text -> Either T.Text Host
parseHost = parse' hostP "Failed to parse host."

hostP :: Parser Host
hostP =
  let
    label = T.append <$> takeWhile1 isAlphaNum' <*>
      option ""
      ( AT.takeWhile (\c -> isAscii c && (isAlphaNum c || c == '-') ) <>
        takeWhile1 isAlphaNum'
      )
    isAlphaNum' c = isAscii c && isAlphaNum c
  in
    Host . encodeUtf8Builder . T.intercalate "." <$> label `sepBy` char '.'

-- | Wrapped port number. Port number in URL is defined in
--   <https://tools.ietf.org/html/rfc3986#section-3.2.3 RFC3986> as
--  @port = *DIGIT@, but port numbers of protocols used
--  by HTTP (<https://tools.ietf.org/html/rfc793 TCP>
--  , <https://tools.ietf.org/html/rfc768 UDP>
--  , <https://tools.ietf.org/html/rfc4960#section-3.1 SCTP>) are
--  limited from 0 to 65535.
newtype Port = Port { unPort :: Word16 } deriving (Show, Eq)

fromPort :: Port -> T.Text
fromPort = T.pack . show . unPort

toPort :: T.Text -> Either T.Text Port
toPort = parse' portP "Invalid port number"

portP :: Parser Port
portP = Port . fromIntegral <$> ((<= 65535) =|< decimal)

-- | Wrapped path.
newtype UrlPath = UrlPath
  { unUrlPath :: [UrlEncoded]
  } deriving (Show, Eq)

buildUrlPath :: UrlPath -> T.Text
buildUrlPath = T.intercalate "/" . L.map urlDecode . unUrlPath

urlPathBuilderBS :: UrlPath -> Builder
urlPathBuilderBS = mconcat . L.intersperse (char7 '/') .
  L.map unUrlEncoded . unUrlPath

parseUrlPath :: T.Text -> Either Text UrlPath
parseUrlPath =  parse' urlPathP "Failed parsing UrlPath."

toUrlPath :: [T.Text] -> UrlPath
toUrlPath = UrlPath . L.map urlEncode

fromUrlPath :: UrlPath -> [T.Text]
fromUrlPath = L.map urlDecode . unUrlPath

(</>) :: UrlPath -> UrlPath -> UrlPath
x </> y = UrlPath $ unUrlPath x ++ unUrlPath y

urlPathP :: Parser UrlPath
urlPathP = UrlPath . L.map (urlEncode . T.pack) . L.filter notRelative <$> (many (satisfy (/= '/')) `sepBy` "/")


-- | Encoded Query string.
--   <https://www.w3.org/MarkUp/HTMLPlus/htmlplus_42.html W3C> details
--   how query strings to be should deal with.
newtype Query = Query
  { unQuery :: [(UrlEncoded, Maybe UrlEncoded)]
  } deriving (Show, Eq)

instance ToJSON Query where
  toJSON = Array . V.fromList . L.map toJSON . unQuery

instance FromJSON Query where
  parseJSON = withArray "Query" (pure . toQuery' <=< traverse parseJSON . V.toList)

instance DH.Interpret Query where
  autoWith _ = toQuery' <$>
    DH.list (DH.pair DH.strictText (DH.maybe DH.strictText))

-- | 'Build' of query string.
buildQuery :: Query -> T.Text
buildQuery = T.intercalate "&" . L.map (
  \case
    (k, Just v) ->
      urlDecode k <> "=" <> urlDecode v
    (k, Nothing) ->
      urlDecode k
  ) . unQuery

queryBuilderBS :: Query -> Builder
queryBuilderBS =
  mconcat .
  L.intersperse (char7 '&') . L.map (
  \case
    (k, Just v) ->
      unUrlEncoded k <> char7 '=' <> unUrlEncoded v
    (k, Nothing) ->
      unUrlEncoded k
  ) . unQuery

-- | Parse to 'Query'
parseQuery :: T.Text -> Either Text Query
parseQuery t =
  case feed (parse queryP t) "" of
    Done "" q -> Right q
    _         -> Left "Failed parsing query"

queryP :: Parser Query
queryP =
  let
    field = (,) <$>
      takeTill (== '=') <*
      char '=' <*> (Just <$> takeTill (== '&'))
    paramLess = (, Nothing) <$> takeTill (== '&')
  in
    toQuery' <$>
    (field <|> paramLess) `sepBy` "&"

-- | Get list of key-value pair from 'Query'.
fromQuery :: Query -> [(T.Text, Maybe T.Text)]
fromQuery = L.map (\(k, v) -> (urlDecode k , urlDecode <$> v)) . unQuery

-- | List of key-value pair to 'Query'.
toQuery :: [(T.Text, T.Text)] -> Query
toQuery = Query . L.map (\(k, v) -> (urlEncode k, Just . urlEncode $ v))

-- | With parameterless field.
toQuery' :: [(T.Text, Maybe T.Text)] -> Query
toQuery' = Query . L.map (\(k, v) -> (urlEncode k, urlEncode <$> v))

-- | URL Encoded 'ByteString'.
newtype UrlEncoded = UrlEncoded
  { unUrlEncoded :: Builder
  }
instance Show UrlEncoded where
  show = T.unpack . decodeUtf8With ignore .
    LBS.toStrict . toLazyByteString . unUrlEncoded

instance Eq UrlEncoded where
  x == y = toLazyByteString (unUrlEncoded x) ==
           toLazyByteString (unUrlEncoded y)

instance Semigroup UrlEncoded where
  x <> y = UrlEncoded $ unUrlEncoded x <> unUrlEncoded y

instance Monoid UrlEncoded where
  mempty = UrlEncoded mempty
  mappend = (<>)
  mconcat = L.foldr mappend mempty

instance Hashable UrlEncoded where
  hashWithSalt i = hashWithSalt i . urlDecode

instance ToJSON UrlEncoded where
  toJSON = String . urlDecode

instance ToJSONKey UrlEncoded where
  toJSONKey = ToJSONKeyText f g
    where
      f = urlDecode
      g = text . f

instance FromJSON UrlEncoded where
  parseJSON = withText "UrlEncoded" (pure . urlEncode)

instance FromJSONKey UrlEncoded where
  fromJSONKey = FromJSONKeyTextParser (pure . urlEncode)

instance DH.Interpret UrlEncoded where
  autoWith _ = DH.Type {..}
    where
      extract (TextLit (Chunks [] t)) = pure (urlEncode t)
      extract  _                      = AP.empty
      expected = Text

-- | ByteString text to URI encoded bytestring.
--   It converts ' '(0x20) to '+'
urlEncodeBS :: SBS.ByteString -> UrlEncoded
urlEncodeBS = urlEncodeWith id

-- | Utf-8 version of 'urlEncode'.
urlEncode :: T.Text -> UrlEncoded
urlEncode = urlEncodeWith encodeUtf8

-- | Encode with a encoder.
urlEncodeWith :: (a -> SBS.ByteString) -> a -> UrlEncoded
urlEncodeWith enc = UrlEncoded . U.urlEncodeBuilder True . enc

-- | Decode URI encoded 'Builder' to strict 'ByteString'.
urlDecodeBS :: UrlEncoded -> SBS.ByteString
urlDecodeBS = urlDecodeWith id

-- | Utf-8 version of 'urlDecode'.
urlDecode :: UrlEncoded -> T.Text
urlDecode = urlDecodeWith $ decodeUtf8With ignore

-- | Decode with a decoder.
urlDecodeWith :: (SBS.ByteString -> a) -> UrlEncoded -> a
urlDecodeWith dec = dec . U.urlDecode True .
  LBS.toStrict . toLazyByteString . unUrlEncoded

