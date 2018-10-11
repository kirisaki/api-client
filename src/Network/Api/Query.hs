{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK not-home    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Network.Api.Query
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
module Network.Api.Query
  (
    -- * Field of HTTP header
    Query
  , toQuery
  , fromQuery

    -- * Query key
  , QueryKey
  , queryKey
  , unQueryKey

    -- * Query value
  , QueryValue
  , queryValue
  , unQueryValue
  ) where

import qualified Data.Text as T

data Query
toQuery = undefined
fromQuery = undefined

data QueryKey = QueryKey { unQueryKey :: T.Text }
queryKey = undefined


data QueryValue = Queryalue { unQueryValue :: T.Text }
queryValue = undefined

