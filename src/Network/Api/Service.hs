{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK not-home    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Network.Api.Service
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
module Network.Api.Service
  (
    -- * Service
    Service(..)
  , Endpoint(..)
  , HttpVersion(..)
  , Method(..)
    -- * Path with parameters
  , inject
  , PathParams(..)
  , Piece(..)
  , buildPathParams
  , parsePathParams
  ) where

import           Network.Api.Header
import           Network.Api.Internal
import           Network.Api.Url

import           Control.Applicative  as AP
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Either
import           Data.List            as L
import qualified Data.Text            as T
import           Data.Word
import           GHC.Generics
import           Numeric.Natural

-- | API Definition
data Service = Service
  { baseUrl           :: Url
  , endpoints         :: [Endpoint]
  , httpVersion       :: HttpVersion
  , defaultHeader     :: Header
  , tokenHeaderName   :: Maybe FieldName
  , tokenHeaderPrefix :: Maybe T.Text
  , tokenQueryName    :: Maybe T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON Service
instance ToJSON Service

-- | Version of HTTP.
--   You don't think numbers of version take negative value, do you?
data HttpVersion = HttpVersion Natural Natural deriving (Eq, Ord, Show)

instance ToJSON HttpVersion where
  toJSON (HttpVersion major minor) =
    if major > 1
    then String . T.pack $ show major
    else String . T.pack $ show major <> "." <> show minor

instance FromJSON HttpVersion where
  parseJSON =
    withText "HttpVersion" $ \case
    "0.9" -> pure $ HttpVersion 0 9
    "1.0" -> pure $ HttpVersion 1 0
    "1.1" -> pure $ HttpVersion 1 1
    "2" -> pure $ HttpVersion 2 0
    "3" -> pure $ HttpVersion 3 0
    _         -> empty

httpVersionP :: Parser HttpVersion
httpVersionP = HttpVersion <$> decimal <* char '.' <*> decimal

-- | API Endpoint definition
data Endpoint = Endpoint
  { endpointMethod :: Method
  , endpointPath   :: PathParams
  } deriving (Eq, Show)

instance FromJSON Endpoint where
  parseJSON = withText "endpoint" $ \t ->
    case T.words t of
      [method, url] ->
        liftA2 Endpoint (parseJSON $ String  method) (parseJSON $ String url)
      _ ->
        empty
instance ToJSON Endpoint where
  toJSON (Endpoint method path) =
    let
      String m = toJSON method
      String p = toJSON path
    in
      String (m <> " " <> p)

-- | HTTP method
data Method
  = GET
  | POST
  | HEAD
  | PUT
  | DELETE
  | TRACE
  | CONNECT
  | OPTIONS
  | PATCH
  | Custom T.Text
  deriving (Show, Ord, Eq, Read)

instance FromJSON Method where
  parseJSON = withText "Method" $ \case
    "GET" -> pure GET
    "POST" -> pure POST
    "HEAD" -> pure HEAD
    "PUT" -> pure PUT
    "DELETE" -> pure DELETE
    "TRACE" -> pure TRACE
    "CONNECT" -> pure CONNECT
    "OPTIONS" -> pure OPTIONS
    "PATCH" -> pure PATCH
    t -> pure . Custom $ T.toUpper t

instance ToJSON Method where
  toJSON = \case
    Custom t -> String $ T.toUpper t
    m -> String . T.pack $ show m

-- | Inject parameters to a path represented with colon or braces.
inject :: PathParams -> [(T.Text, T.Text)] -> Either T.Text UrlPath
inject (PathParams params) args =
  let
    f param =
      case param of
        Raw r -> Right r
        Param p -> case L.lookup p args of
          Just a  -> Right a
          Nothing -> Left p
    (ls, rs) = partitionEithers $ L.map f params
  in
    if L.null ls
    then Right $ toUrlPath rs
    else Left $
         "Lacks following parameters: " `T.append` T.intercalate ", " ls

-- | URL with parameters.
newtype PathParams = PathParams
  { unPathParams :: [Piece] } deriving (Show, Eq, Ord)

data Piece = Raw T.Text | Param T.Text deriving (Show, Eq, Ord)

instance ToJSON PathParams where
  toJSON = String . buildPathParams

instance FromJSON PathParams where
  parseJSON = withText "PathParams" $
    \t -> case parsePathParams t of
      Right r -> return r
      Left l  -> fail $ T.unpack l

-- | Build path with colon parameter.
buildPathParams :: PathParams -> T.Text
buildPathParams =
  let
    f x = case x of
      Raw t   -> t
      Param t -> ':' `T.cons` t
  in
    T.intercalate "/" . L.map f . unPathParams

-- | Parse the path.
parsePathParams :: T.Text -> Either T.Text PathParams
parsePathParams =
  let
    paramString =
      takeWhile1
      ( \c ->
          c /= '/' && c /= '{' && c /= '}' && c/= ':'
      )
    bracedParam = Param <$> (char '{' *> paramString <* char '}')
    colonParam = Param <$> (char ':' *> paramString)
    rawPath = Raw <$> (notRelative =|< paramString)
    segment = colonParam <|> bracedParam <|> rawPath
    p = PathParams <$> ( many (char '/') *>
                         segment `sepBy` char '/' <* many (char '/')
                       )
  in
    parse' p "Failed parsing PathParams"
