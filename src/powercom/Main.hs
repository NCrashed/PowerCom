-- Copyright 2013 Gushcha Anton 
-- This file is part of PowerCom.
--
--    PowerCom is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    PowerCom is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with PowerCom.  If not, see <http://www.gnu.org/licenses/>.
module Main (main) where

import ApplicationLevel
import ChannelLevel

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.Chan
import System.Exit

exitMsg :: (ProcessId, String) -> Process ()
exitMsg (id, msg) = case msg of
  "exit" -> liftIO exitSuccess
  _      -> return ()

main = do
  t <- createTransport
  node <- newLocalNode t initRemoteTable

  runProcess node $ do 
    rootId <- getSelfPid
    channelLevelId <- initChannelLevel
    appLevelId <- initApplicationLevel rootId channelLevelId
    forever $ receiveWait [match exitMsg]