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
  ( Query
  ) where

import Data.HashMap.Strict as HM
import qualified Data.ByteString as BSS
import qualified Data.Text as T
import Network.HTTP.Types.URI hiding (Query)

-- | Collection of URL query parameters.
--   Behaviour when duplicated query keys at URL is not defined,
--   but this makes implements complecated, so treat keys as unique in this module.

type Query = HM.HashMap URIEncoded (Maybe URIEncoded)

type URIEncoded = BSS.ByteString

toQuery :: [(T.Text, Maybe T.Text)] -> Query
toQuery kvs = undefined

toQuery' :: [(T.Text, T.Text)] -> Query
toQuery' kvs = undefined

