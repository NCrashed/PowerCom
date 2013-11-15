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
module Channel.Connection (
      Connection
    , initConnection
    , closeConnection
    , openConnection
    , ifConnectedWithError
    , ifConnected 
    , ifNotConnected
    , connectHandler
    , disconnectHandler
    ) where

import Channel.Options
import Channel.Sending
import Channel.Miscs
import Channel.Frame 

import Data.IORef
import Control.Distributed.Process

type Connection = IORef Bool

initConnection :: Process Connection
initConnection = liftIO $ newIORef False

closeConnection :: Connection -> Process () 
closeConnection conn = liftIO $ writeIORef conn False

openConnection :: Connection -> Process ()
openConnection conn = liftIO $ writeIORef conn True

isConnected :: Connection -> Process Bool 
isConnected conn = liftIO $ readIORef conn 

ifConnectedWithError :: Connection -> ProcessId -> Process () -> Process ()
ifConnectedWithError connRef errorTransitId action = do 
    connection <- isConnected connRef
    thisId <- getSelfPid
    case connection of 
        True  -> action
        False -> send errorTransitId (thisId, "error", "Connection is not established!")

ifConnected :: Connection -> Process () -> Process ()
ifConnected = withConnectionDo True

ifNotConnected :: Connection -> Process () -> Process ()
ifNotConnected = withConnectionDo False

withConnectionDo :: Bool -> Connection -> Process () -> Process ()
withConnectionDo state connRef action = do 
    connection <- isConnected connRef
    if connection == state then action
    else return ()

connectHandler :: ProcessId -> Connection -> IORef ChannelOptions -> (ProcessId, String) -> Process Bool
connectHandler physLayerId conn optionsRef (senderId, _) = do
    thisId <- getSelfPid
    options <- liftIO $ readIORef optionsRef
    connection <- isConnected conn
    case connection of 
        False -> do 
            informSender senderId "Connecting..."
            send physLayerId (thisId, "reopen", options)
            connResult <- expect :: Process Bool
            case connResult of 
                True -> do 
                    sendRes <- sendFrameWithAck physLayerId $ LinkFrame $ userName options
                    case sendRes of 
                        True  -> openConnection conn >> return True
                        False -> do
                            informSenderError senderId "Remote host is not answering!"
                            return True
                False -> return True
        True -> return True

disconnectHandler :: ProcessId -> Connection -> IORef ChannelOptions -> (ProcessId, String) -> Process Bool
disconnectHandler physLayerId conn optionsRef (senderId, _) = do
    thisId <- getSelfPid
    options <- liftIO $ readIORef optionsRef
    connection <- isConnected conn
    case connection of 
        True -> do 
            informSender senderId "Disconnecting..."
            sendFrameWithAck physLayerId $ UnlinkFrame $ userName options
            closeConnection conn
            return True
        False -> return True