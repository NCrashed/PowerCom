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
module Physical.Layer (
    initPhysicalLayer
    ) where

import Control.Distributed.Process
import qualified Data.ByteString                as BS
import Control.Exception (SomeException)
import Control.Monad (forever)

import Physical.Options
import Physical.Port
import Utility (while, exitMsg)

receiveFrameCycle :: ProcessId -> PortState -> Process () 
receiveFrameCycle channelId portState = do
    liftIO $ putStrLn "Recieving thread started..." 
    forever $ do
        frameResult <- receiveFrame portState 
        thisId <- getSelfPid
        case frameResult of 
            Right bs -> send channelId (thisId, "frame", bs)
            Left err -> send channelId (thisId, "error", "Error while receiving frame: " ++ err ++ "!")

sendFrameHandler :: PortState -> (ProcessId, String, BS.ByteString) -> Process Bool
sendFrameHandler portState (senderId, _, msg) = do 
    thisId <- getSelfPid
    result <- sendFrame portState msg
    case result of 
        Nothing -> return True
        Just err -> do
            send senderId (thisId, "error", err)
            return True

reopenPortHandler :: PortState -> (ProcessId, String, ChannelOptions) -> Process Bool 
reopenPortHandler portState (senderId, _, options) = do
    thisId <- getSelfPid
    res <- reopenPort portState options
    case res of
        Just err -> send senderId False >> send senderId (thisId, "error", err) >> return True
        Nothing  -> send senderId True  >> return True

closePortHandler :: PortState -> (ProcessId, String) -> Process Bool 
closePortHandler portState (_, _) = do 
    closePort portState
    return True

physicalLayerCycle :: ChannelOptions -> ProcessId -> Process ()
physicalLayerCycle options channelId = do
    thisId <- getSelfPid
    send channelId (thisId, "info", "Physical layer initialized...")

    initResult <- try (initPort options) :: Process (Either SomeException PortState)
    case initResult of 
        Right port -> do
            send channelId (thisId, "info", "Serial port opened...")

            spawnLocal $ receiveFrameCycle channelId port

            while $ receiveWait [
                  matchIf (\(_, com)    -> com == "exit")       exitMsg
                , matchIf (\(_, com, _) -> com == "send")       (sendFrameHandler port)
                , matchIf (\(_, com, _) -> com == "reopen")     (reopenPortHandler port)
                , matchIf (\(_, com) -> com == "close")         (closePortHandler port)]

            closePort port
        Left ex -> do
            send channelId (thisId, "error", "Exception while initing physical layer: " ++ show ex)
            (_, _, newOptions) <- expect :: Process (ProcessId, String, ChannelOptions)
            send channelId (thisId, "info", "Got new options, trying to init physical layer...")
            physicalLayerCycle newOptions channelId

initPhysicalLayer :: ChannelOptions -> ProcessId -> Process ProcessId
initPhysicalLayer options channelId = spawnLocal $ physicalLayerCycle options channelId