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

import Channel.Options
import Channel.Frame 
import Physical.Layer 
import Utility (while, exitMsg)

import Control.Distributed.Process
import qualified Data.ByteString as BS
import Data.IORef 

connect :: ProcessId -> IORef ChannelOptions -> (ProcessId, String) -> Process Bool
connect physLayerId optionsRef (senderId, _) = do
    thisId <- getSelfPid
    options <- liftIO $ readIORef optionsRef
    send senderId (thisId, "info", "Opening port...")
    send physLayerId (thisId, "reopen", options)
    return True

disconnect :: (ProcessId, String) -> Process Bool
disconnect (senderId, _) = do
    thisId <- getSelfPid
    send senderId (thisId, "info", "Disconnecting...")
    return True

sendMessage :: ProcessId -> (ProcessId, String, String) -> Process Bool 
sendMessage physLayerId (_, _, msg) = do 
    thisId <- getSelfPid
    liftIO $ putStrLn $ "Sending: " ++ msg
    send physLayerId (thisId, "send", frameBuffer)
    return True
    where
        frameBuffer :: BS.ByteString
        frameBuffer = toByteString $ InformationFrame msg

changeOptions :: ProcessId -> (ProcessId, String, ChannelOptions) -> Process Bool 
changeOptions physLayerId (senderId, _, options) = do 
    thisId <- getSelfPid
    send senderId (thisId, "info", "Changing options...")
    liftIO $ putStrLn $ show options
    send physLayerId (thisId, "reopen", options)
    return True

transitError :: ProcessId -> (ProcessId, String, String) -> Process Bool
transitError transitId (_, _, msg) = do 
    thisId <- getSelfPid
    send transitId (thisId, "error", msg)
    return True

receiveFrame :: ProcessId -> (ProcessId, String, BS.ByteString) -> Process Bool 
receiveFrame transitId (_, _, byteFrame) = do 
    thisId <- getSelfPid
    case frameResult of 
        Left err -> do 
            send transitId (thisId, "error", "Failed to recieve frame: " ++ err)
            return True
        Right frame -> case frame of 
            InformationFrame msg -> send transitId (thisId, "info", "Recieved inf frame: "++msg) >> return True
            _ -> send transitId (thisId, "info", "Recieved another frame") >> return True
    return True
    where 
        (frameResult,_) = fromByteString byteFrame 

initChannelLayer :: ProcessId -> Process ProcessId
initChannelLayer appLayer = do
    id <- spawnLocal $ do
        thisId <- getSelfPid
        optionsRef <- liftIO $ newIORef defaultOptions
        physLayerId <- initPhysicalLayer defaultOptions thisId
        while $ receiveWait [
              matchIf (\(_, com)    -> com == "exit")       $ exitMsg
            , matchIf (\(_, com, _) -> com == "send")       $ sendMessage physLayerId
            , matchIf (\(_, com)    -> com == "connect")    $ connect physLayerId optionsRef
            , matchIf (\(_, com)    -> com == "disconnect") $ disconnect
            , matchIf (\(_, com, _) -> com == "options")    $ changeOptions physLayerId
            -- From physical layer
            , matchIf (\(_, com, _) -> com == "error")   $ transitError   appLayer
            , matchIf (\(_, com, _) -> com == "frame")   $ receiveFrame   appLayer]

        send physLayerId (thisId, "exit")
    return id
