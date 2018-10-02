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

import Data.Aeson
import Data.ByteString as BS
import Data.List as L
import Data.Text as T
import GHC.Generics
import qualified Network.HTTP.Client as C

call :: Request -> Service -> IO ByteString
call req service = undefined

buildRequest :: Request -> Method -> Service -> C.Request
buildRequest req method service = undefined

lookupMethod :: Request -> Service -> Maybe Method
lookupMethod req service = undefined


injectUrlParams :: Text -> [(Text, Text)] -> Either Text Text
injectUrlParams path params =
  let
    replace (k, v) p =
      if p == ':' `T.cons` k ||
         p == '{' `T.cons` k `T.snoc` '}'
      then v else p
    replaceAll kv ps = L.map (replace kv) ps 
  in
    undefined

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
