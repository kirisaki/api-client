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
  , buildHttpRequest
  , lookupMethod
  , injectUrlParams

    -- * Service
  , Service(..)
  , Method(..)
  , HttpMethod(..)

    -- * Query
  , Query
  , toQuery
  , toQueryUtf8
  , toQuery'
  , toQueryUtf8'
  , toQueryWith
  , fromQuery
  , fromQueryUtf8
  , fromQueryWith
  , UrlEncoded
  , urlEncode
  , urlEncodeUtf8
  , urlDecode
  , urlDecodeUtf8

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
  ) where

import           Network.Api.Header
import           Network.Api.Query
import           Network.Api.Request
import           Network.Api.Service

