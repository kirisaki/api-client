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

    -- * Header
  , Header
  , toHeader
  , fromHeader
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

