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
    , setRemoteUsername
    , remoteUserName
    , ifConnectedWithError
    , ifConnected 
    , ifNotConnected
    , connectHandler
    , disconnectHandler
    , sendFrameWithDisconnect
    ) where

import Channel.Options
import Channel.Sending
import Channel.Miscs
import Channel.Frame 

import Data.IORef
import Data.Functor
import Control.Monad
import Control.Applicative
import Control.Distributed.Process

-- | Connection is bool value with remote user name
type Connection = IORef (Bool, String)

initConnection :: Process Connection
initConnection = liftIO $ newIORef (False, "")

closeConnection :: Connection -> Process () 
closeConnection conn = liftIO $ do
    (_, uname) <- readIORef conn 
    writeIORef conn (False, uname)

-- User name is sended with link frame 
openConnection :: Connection -> Process ()
openConnection conn = liftIO $ do 
    (_, uname) <- readIORef conn 
    writeIORef conn (True, uname)

isConnected :: Connection -> Process Bool 
isConnected conn = liftIO $ fst <$> readIORef conn 

setRemoteUsername :: Connection -> String -> Process ()
setRemoteUsername conn uname = do 
    bool <- isConnected conn 
    liftIO $ writeIORef conn (bool, uname)

remoteUserName :: Connection -> Process String
remoteUserName conn = liftIO $ snd <$> readIORef conn 

ifConnectedWithError :: Connection -> ProcessId -> Process () -> Process ()
ifConnectedWithError connRef errorTransitId action = do 
    connection <- isConnected connRef
    thisId <- getSelfPid
    if connection then action
    else send errorTransitId (thisId, "error", "Connection is not established!") 

ifConnected :: Connection -> Process () -> Process ()
ifConnected = withConnectionDo True

ifNotConnected :: Connection -> Process () -> Process ()
ifNotConnected = withConnectionDo False

withConnectionDo :: Bool -> Connection -> Process () -> Process ()
withConnectionDo state connRef action = do 
    connection <- isConnected connRef
    when (connection == state) action

getIdConOpt optionsRef conn = (,,) <$> getSelfPid <*> getOptions optionsRef <*> isConnected conn

connectHandler :: ProcessId -> Connection -> InnerChannelOptions -> (ProcessId, String) -> Process Bool
connectHandler physLayerId conn optionsRef (senderId, _) = do
    (thisId, options, connection) <- getIdConOpt optionsRef conn
    if connection then return True else do 
        informSender senderId "Connecting..."
        send physLayerId (thisId, "reopen", options)
        connResult <- expect :: Process Bool
        if not connResult then return True else do 
            sendRes <- sendFrameWithAck physLayerId $ LinkFrame $ userName options
            if sendRes then openConnection conn >> return True
            else do 
                informSenderError senderId "Remote host is not answering!"
                return True


disconnectHandler :: ProcessId -> Connection -> InnerChannelOptions -> (ProcessId, String) -> Process Bool
disconnectHandler physLayerId conn optionsRef (senderId, _) = do
    (thisId, options, connection) <- getIdConOpt optionsRef conn
    when connection $ do
        informSender senderId "Disconnecting..."
        sendFrameWithDisconnect conn senderId physLayerId $ UnlinkFrame $ userName options
        closeConnection conn
    return True

sendFrameWithDisconnect :: Connection -> ProcessId -> ProcessId -> Frame -> Process ()
sendFrameWithDisconnect conn transitId targetId frame = 
    disconnectOnFail transitId conn $ sendFrameWithAck targetId frame

disconnectOnFail :: ProcessId -> Connection -> Process Bool -> Process ()
disconnectOnFail transitId conn action = do 
    res <- action 
    unless res $ ifConnected conn $ do 
        thisId <- getSelfPid
        uname <- remoteUserName conn
        sendDisconnectUser transitId uname
        informSenderError transitId "Remote host is not answering! Connection closed."
        closeConnection conn