{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_HADDOCK not-home    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Network.Api.Request
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
module Network.Api.Request
  (
    call
  , Request(..)
  , req
  , Response(..)
  , Token(..)
  , ClientException(..)
  , buildHttpRequest
  , lookupEndpoint

  -- * Re-export
  , C.newManager
  , C.defaultManagerSettings
  ) where

import           Network.Api.Header
import           Network.Api.Service
import           Network.Api.Url

import           Control.Applicative
import           Control.Exception.Safe     as E
import           Data.Attoparsec.Text       as A
import qualified Data.ByteString            as SBS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.CaseInsensitive       as CI
import           Data.Either.Combinators
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import           Data.List                  as L
import           Data.Maybe
import           Data.Text                  as T
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Typeable              (Typeable)
import           Data.Word
import           GHC.Generics
import qualified Network.HTTP.Client        as C
import           Network.HTTP.Types         (Status)
import           Network.HTTP.Types.URI     hiding (Query)
import qualified Network.HTTP.Types.Version as V


-- | Call WebAPI.
call :: C.Manager -> Request -> Service -> IO Response
call man req ser = do
  hreq <- case buildHttpRequest req ser of
            Right r -> return r
            Left l  -> fail $ T.unpack l
  res <- C.httpLbs hreq man
  return $ Response
    { resStatus = C.responseStatus res
    , resHeader = toHeader $ C.responseHeaders res
    , resBody = C.responseBody res
    }

-- | Request to call API.
data Request = Request
  { reqMethod         :: Method -- ^ HTTP request method.
  , reqPath           :: PathParams -- ^ Path of API endpoint.
  , reqParams         :: [(Text, Text)] -- ^ Parameters injected to the path.
  , reqQuery          :: Query -- ^ Query parameters.
  , reqHeader         :: Header -- ^ Header fields.
  , reqBody           :: SBS.ByteString -- ^ Request body.
  , reqToken          :: Maybe Token -- ^ Token to call API.
  , reqAltUrl         :: Maybe Url -- ^ Alternative base URL.
  , reqAltHttpVersion :: Maybe HttpVersion -- ^ Alternative HTTP version.
  }

-- | Simple 'Request'.
req :: Request
req = Request
  { reqMethod = GET
  , reqPath = PathParams []
  , reqParams = []
  , reqQuery = emptyQuery
  , reqHeader = emptyHeader
  , reqBody = ""
  , reqToken = Nothing
  , reqAltUrl = Nothing
  , reqAltHttpVersion = Nothing
  }

-- | Response to calling API
data Response = Response
  { resStatus :: Status
  , resHeader :: Either Text Header
  , resBody   :: LBS.ByteString
   } deriving (Eq, Show)

-- | Build a Network.HTTP.Client.Request from Request.
buildHttpRequest :: Request -> Service -> Either Text C.Request
buildHttpRequest req service = do
  let url' = fromMaybe (baseUrl service) (reqAltUrl req)
  let version' =
        let ver s = V.HttpVersion
                    (fromIntegral $ majorVersion s)
                    (fromIntegral $ minorVersion s)
        in
          maybe (ver $ httpVersion service) ver (reqAltHttpVersion req)
  scheme' <- if scheme url' == Http && version' >= V.HttpVersion 2 0
            then Left "HTTP/2.0 requires HTTPS"
            else Right $ scheme url'
  let port' = case (port $ authority url', scheme') of
        (Just p, _)      -> (fromIntegral . unPort) p
        (Nothing, Http)  -> 80
        (Nothing, Https) -> 443
  header' <-
    let
      mergedHeader = HM.union (reqHeader req) (defaultHeader service)
      token = case (reqToken req, tokenHeaderPrefix service) of
        (Just t, Nothing) -> Just $ tokenText t
        (Just t, Just p)  -> Just (p <> " " <> tokenText t)
        _                 -> Nothing
    in
      fromHeader <$> case (tokenHeaderName service, token) of
        (Just name, Just t) ->
          case fieldValue . encodeUtf8 $ t of
            Right t -> Right $ HM.insert name t mergedHeader
            Left l -> Left "Invalid token has invalid string as a header value."
        (_, _) ->
          Right mergedHeader
  let query' =
        let
          mergedQuery = reqQuery req <> query url'
        in
          buildQueryBS $ case (tokenQueryName service, reqToken req) of
            (Just k, Just v) ->
              mergedQuery <> toQuery [(k, tokenText v)]
            _ ->
              mergedQuery
  apiEndpoint' <- lookupEndpoint req service
  path' <- buildUrlPathBS  . (urlPath url' </>) <$> inject (endpointPath apiEndpoint') (reqParams req)
  return C.defaultRequest
    { C.host = (buildHostBS . host . authority) url'
    , C.port = port'
    , C.secure = scheme' == Https
    , C.requestHeaders  = header'
    , C.path            = path'
    , C.queryString     = query'
    , C.method          = encodeUtf8 . T.pack . show $ endpointMethod apiEndpoint'
    , C.requestBody     = C.RequestBodyBS $ reqBody req
    , C.requestVersion  = version'
    }


-- | Exceptions
data ClientException
  = EndpointNotDefined
  | FailedToInjectUrlParams Text
  | FailedToAttachToken
  deriving(Show, Typeable)
instance Exception ClientException

-- | Token for authorization
data Token = Token
  { tokenText :: Text
  , expire    :: Maybe UTCTime
  } deriving (Eq, Show)

-- | Look up a method which matchs an API definition.
lookupEndpoint :: Request -> Service -> Either Text Endpoint
lookupEndpoint req service =
  let
    matchParams :: PathParams -> PathParams -> Bool
    matchParams (PathParams reqParams) (PathParams serviceParams) =
      L.length reqParams == L.length serviceParams &&
      L.all
       (\case
           (Raw r, Raw s) -> r == s
           (Raw _, Param _) -> True
           (Param r, Raw _) -> False
           (Param r, Param s) -> r == s
       ) (L.zip reqParams serviceParams)
    matchedEndpoint = L.find
      (\e ->
          reqMethod req == endpointMethod e &&
          reqPath req `matchParams` endpointPath e
      ) (endpoints service)
  in
    case matchedEndpoint of
      Just e  -> Right e { endpointPath = reqPath req }
      Nothing -> Left "Endpoint not found."

