-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Startup PowerCom module. The main purpose is to create Cloud Haskell
-- transport system and initialize first layer (Application layer). User
-- can pass two arguments: default serial port name and default user name.
-----------------------------------------------------------------------------
module Main (main) where

import Paths_PowerCom
import Application.Layer
import Utility

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.Chan
import System.Environment

-- | Main application function. Initializes application layer and waits
-- terminating command, also retrieves gui resource file and parses 
-- input arguments for channel layer.
main :: IO ()
main = do
  args <- getArgs

  t <- createTransport
  node <- newLocalNode t initRemoteTable
  gladeFile <- getDataFileName "views/gui.glade"

  runProcess node $ do 
    rootId <- getSelfPid
    initApplicationLayer gladeFile (convertArgs args) rootId 
    while $ receiveWait [match exitMsg]
    
  where 
    convertArgs args = case length args of
        2 -> Just (head args, args !! 1)
        _ -> Nothing