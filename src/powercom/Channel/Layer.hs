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
module Channel.Layer (
    initChannelLayer
    ) where

import Control.Distributed.Process
import Physical.Layer 
import Utility (while, exitMsg)
import Channel.Options
import qualified Data.ByteString as BS

connect :: (ProcessId, String) -> Process Bool
connect (senderId, _) = do
    thisId <- getSelfPid
    send senderId (thisId, "info", "Connecting...")
    return True

disconnect :: (ProcessId, String) -> Process Bool
disconnect (senderId, _) = do
    thisId <- getSelfPid
    send senderId (thisId, "info", "Disconnecting...")
    return True

sendMessage :: (ProcessId, String, String) -> Process Bool 
sendMessage (_, _, msg) = do 
    liftIO $ putStrLn $ "Sending: " ++ msg
    return True

changeOptions :: (ProcessId, String, ChannelOptions) -> Process Bool 
changeOptions (senderId, _, options) = do 
    thisId <- getSelfPid
    send senderId (thisId, "info", "Changing options...")
    return True

transitError :: ProcessId -> (ProcessId, String, String) -> Process Bool
transitError transitId (_, _, msg) = do 
    thisId <- getSelfPid
    send transitId (thisId, "error", msg)
    return True

receiveFrame :: ProcessId -> (ProcessId, String, BS.ByteString) -> Process Bool 
receiveFrame transitId (_, _, byteFrame) = do 
    thisId <- getSelfPid
    return True

initChannelLayer :: ProcessId -> Process ProcessId
initChannelLayer appLayer = do
    id <- spawnLocal $ do
        thisId <- getSelfPid
        physLevelId <- initPhysicalLayer defaultOptions thisId
        while $ receiveWait [
              matchIf (\(_, com)    -> com == "exit")       exitMsg
            , matchIf (\(_, com, _) -> com == "send")       sendMessage
            , matchIf (\(_, com)    -> com == "connect")    connect
            , matchIf (\(_, com)    -> com == "disconnect") disconnect
            , matchIf (\(_, com, _) -> com == "options")    changeOptions
            -- From physical layer
            , matchIf (\(_, com, _) -> com == "error")   $ transitError   appLayer
            , matchIf (\(_, com, _) -> com == "frame")   $ receiveFrame   appLayer]

        send physLevelId (thisId, "exit")
    return id
