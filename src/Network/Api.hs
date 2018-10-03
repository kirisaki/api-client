----------------------------------------------------------------------------
-- |
-- Module      :  Network.Api
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Api where

import Control.Applicative
import Data.Aeson
import Data.Attoparsec.Text as A
import Data.ByteString as BS
import Data.List as L
import Data.Maybe
import Data.Text as T
import GHC.Generics
import qualified Network.HTTP.Client as C

call :: Request -> Service -> IO ByteString
call req service = undefined

buildRequest :: Request -> Method -> Service -> C.Request
buildRequest req method service = undefined


lookupMethod :: Request -> Service -> Maybe Method
lookupMethod req service =
  undefined

injectUrlParams :: Text -> [(Text, Text)] -> Either Text Text
injectUrlParams path [] = undefined
  
bracedParam :: Parser Segment
bracedParam = Param <$> (char '{' *> A.takeTill (== '}') <* char '}')

colonParam :: Parser Segment
colonParam = Param <$> (char ':' *> A.takeTill (== '/'))

rawPath :: Parser Segment
rawPath = Raw <$> (takeTill (== '/'))

data Segment = Param Text | Raw Text

segment :: Parser Segment
segment = colonParam <|> bracedParam <|> rawPath


data Method = Method
  { httpMethod :: HttpMethod
  , apiEndpoint :: Text
  } deriving (Eq, Show, Ord, Read, Generic)
instance FromJSON Method
instance ToJSON Method

data Service = Service
  { baseUrl :: Text
  , tokenUrl :: Maybe Text
  , tokenGetter :: Maybe Method
  , tokenRefresher :: Maybe Method
  , methods :: [Method]
  , defaultHeader :: [(Text, Text)]
  , tokenHeaderName :: Maybe Text
  , tokenQueryName :: Maybe Text
  } deriving (Eq, Show, Ord, Read, Generic)
instance FromJSON Service
instance ToJSON Service

data HttpMethod
  = GET
  | POST
  | HEAD
  | PUT
  | DELETE
  | TRACE
  | CONNECT
  | OPTIONS
  | PATCH
  | Custom Text
  deriving (Show, Ord, Eq, Read, Generic)
instance FromJSON HttpMethod
instance ToJSON HttpMethod

data Request = Request
  { requestMethod :: HttpMethod
  , requestPath :: Text
  , pathParams :: [(Text, Text)]
  , queryParams :: [(Text, Text)]
  , headerParams :: [(Text, Text)]
  , requestBody :: ByteString
  } deriving (Eq, Show, Ord, Read, Generic)
