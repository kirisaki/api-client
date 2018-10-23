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

    -- * Service
  , Service(..)
  , Method(..)
  , HttpMethod(..)

    -- * URL
  , inject

    -- * Query
  , Query
  , toQuery
  , toQueryBS
  , toQuery'
  , toQueryBS'
  , fromQuery
  , fromQueryBS
  , UrlEncoded
  , urlEncode
  , urlEncodeBS
  , urlDecode
  , urlDecodeBS

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
import           Network.Api.Request
import           Network.Api.Service
import           Network.Api.Url

