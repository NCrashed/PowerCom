-----------------------------------------------------------------------------
-- |
-- Module      :  Utility
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Often used utility functions.
--
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Utility (
      while
    , exitMsg
    , liftExceptions
    ) where

import Control.Distributed.Process
import Control.Monad 
import Control.Monad.Trans.Class  
import Control.Monad.Trans.Either
import Control.Exception (SomeException)

-- | Default handler for terminating protocol between layers.
-- If handler returns "False", layer listening function exits and
-- layer terminates.
exitMsg :: (ProcessId, String) -> Process Bool
exitMsg (_, msg) = case msg of
  "exit" -> return False
  _      -> return True

-- | Runs the action until it returns "False". Most layers use this
-- function to organize listening cycle.
while :: Process Bool -> Process ()
while f = do
    val <- f 
    when val $ while f

-- | Catches all synchronous exceptions all transforms them into EitherT String
-- | monad transformer. 
liftExceptions :: forall a . Process a -> EitherT String Process a
liftExceptions action = do
  res <- lift (try action :: Process (Either SomeException a))
  eitherT (left.show) right $ hoistEither res