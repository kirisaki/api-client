----------------------------------------------------------------------------
-- |
-- Module      :  Network.Api
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
module Network.Api
  (
    -- * Request
    call
  , Request(..)
  , req
  , buildHttpRequest
  , lookupMethod

    -- * Service
  , Service(..)
  , Method(..)
  , HttpMethod(..)

    -- * Header
  , Header
  , toHeader
  , toHeaderUtf8
  , fromHeader
  , fromHeaderUtf8
  , FieldName
  , fieldName
  , unFieldName
  , FieldValue
  , fieldValue
  , unFieldValue

    -- * Url
  , Url (..)
  , parseUrl
  , buildUrl
  , buildUrlBS
    -- ** Scheme
  , Scheme(..)
  , fromScheme
  , toScheme
    -- ** Authority
  , Authority (..)
  , parseAuthority
  , buildAuthority
    -- *** Userinfo
  , Userinfo
  , fromUserinfo
  , toUserinfo
    -- *** Host
  , Host
  , buildHost
  , buildHostBS
  , parseHost
    -- *** Port
  , Port
  , unPort
  , fromPort
  , toPort
    -- ** Path
  , UrlPath
  , buildUrlPath
  , buildUrlPathBS
  , parseUrlPath
  , toUrlPath
  , fromUrlPath
  , (</>)
    -- ** URL query parameter
  , Query
  , emptyQuery
  , buildQuery
  , buildQueryBS
  , parseQuery
  , fromQuery
  , toQuery
  , toQuery'
    -- ** URL encoded string
  , UrlEncoded
  , urlEncode
  , urlEncodeBS
  , urlDecode
  , urlDecodeBS
  ) where

import           Network.Api.Header
import           Network.Api.Request
import           Network.Api.Service
import           Network.Api.Url

