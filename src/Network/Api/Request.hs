{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
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
  , attachToken
  , Request(..)
  , Response(..)
  , Token(..)
  , ClientException(..)
  , buildHttpRequest
  , lookupMethod

  -- * Re-export
  , C.newManager
  , C.defaultManagerSettings
  ) where

import           Network.Api.Header
import           Network.Api.Service
import           Network.Api.Url

import           Control.Applicative
import           Control.Exception.Safe  as E
import           Data.Attoparsec.Text    as A
import qualified Data.ByteString         as SBS
import qualified Data.ByteString.Lazy    as LBS
import           Data.CaseInsensitive    (CI, mk, original)
import           Data.Either.Combinators
import           Data.Hashable
import qualified Data.HashMap.Strict     as HM
import           Data.List               as L
import           Data.Maybe
import           Data.Text               as T
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Typeable           (Typeable)
import           Data.Word
import           GHC.Generics
import qualified Network.HTTP.Client     as C
import           Network.HTTP.Types      (Status)
import           Network.HTTP.Types.URI  hiding (Query)


-- | Call WebAPI with getting or updating token automatically.
--   It's also possible to give a token explicitly.
call :: C.Manager -> Request -> Service -> IO Response
call man req ser = do
  hreq <- buildHttpRequest req ser
  res <- C.httpLbs hreq man
  return $ Response
    { resStatus = C.responseStatus res
    , resHeader = toHeader $ C.responseHeaders res
    , resBody = C.responseBody res
    , resToken = Nothing
    }

-- | If you need to define method to get or update token.
callWith :: C.Manager -> Request -> Service -> IO Response
callWith man req ser = undefined

-- | Request to call API.
data Request = Request
  { reqMethod :: HttpMethod -- ^ HTTP request method.
  , reqPath   :: PathParams -- ^ Path of API endpoint.
  , reqParams :: [(Text, Text)] -- ^ Parameters injected to the path.
  , reqQuery  :: Maybe Query -- ^ Query parameters.
  , reqHeader :: Header -- ^ Header fields.
  , reqBody   :: SBS.ByteString -- ^ Request body.
  , reqToken  :: Maybe Token -- ^ Token to call API.
  , reqAltUrl :: Maybe Url -- ^ Alternative base URL.
  }

-- | Response to calling API
data Response = Response
  { resStatus :: Status
  , resHeader :: Either Text Header
  , resBody   :: LBS.ByteString
  , resToken  :: Maybe Token
  } deriving (Eq, Show)

-- | Attach a token to a request.
--   This priors a token at the request.
attachToken :: MonadThrow m => Request -> Service -> Token -> m Request
attachToken tok req ser = undefined

-- | Build a Network.HTTP.Client.Request from Request.
buildHttpRequest :: MonadThrow m => Request -> Service -> m C.Request
buildHttpRequest req service = undefined

-- | Exceptions
data ClientException
  = MethodNotDefined
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
lookupMethod :: Request -> Service -> Maybe Method
lookupMethod req service =
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
    matchedMethod = L.find
      (\method ->
          reqMethod req == httpMethod method &&
          reqPath req `matchParams` apiEndpoint method
      ) (methods service)
  in
    case matchedMethod of
      Just m  -> Just m { apiEndpoint = reqPath req }
      Nothing -> Nothing
