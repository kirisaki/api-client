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
module Network.Api where

import Data.Text
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types


data ApiMethod = ApiMethod
