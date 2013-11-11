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

sendMessage :: ProcessId -> IORef ChannelOptions -> (ProcessId, String, String) -> Process Bool 
sendMessage physLayerId optionsRef (_, _, msg) = do 
    thisId <- getSelfPid
    options <- liftIO $ readIORef optionsRef
    liftIO $ putStrLn $ "Sending: " ++ msg
    send physLayerId (thisId, "send", frameBuffer $ userName options)
    return True
    where
        frameBuffer :: String -> BS.ByteString
        frameBuffer uname = toByteString $ InformationFrame uname msg

changeOptions :: ProcessId -> IORef ChannelOptions -> (ProcessId, String, ChannelOptions) -> Process Bool 
changeOptions physLayerId optionsRef (senderId, _, options) = do 
    thisId <- getSelfPid
    send senderId (thisId, "info", "Changing options...")
    liftIO $ writeIORef optionsRef options
    send physLayerId (thisId, "send", frameBuffer)
    send physLayerId (thisId, "reopen", options)
    return True
    where
        frameBuffer :: BS.ByteString
        frameBuffer = toByteString $ OptionFrame $ getOptionPairs options 
            [ "portSpeed"
            , "portStopBits"
            , "portParityBits"
            , "portWordBits"]

transitError :: ProcessId -> (ProcessId, String, String) -> Process Bool
transitError transitId (_, _, msg) = do 
    thisId <- getSelfPid
    send transitId (thisId, "error", msg)
    return True

transitInfo :: ProcessId -> (ProcessId, String, String) -> Process Bool
transitInfo transitId (_, _, msg) = do 
    thisId <- getSelfPid
    send transitId (thisId, "info", msg)
    return True

receiveFrame :: ProcessId -> ProcessId -> IORef ChannelOptions -> (ProcessId, String, BS.ByteString) -> Process Bool 
receiveFrame physLayerId transitId optionsRef (_, _, byteFrame) = do 
    thisId <- getSelfPid
    options <- liftIO $ readIORef optionsRef
    case frameResult of 
        Left err -> do 
            send transitId (thisId, "error", "Failed to recieve frame: " ++ err)
            return True
        Right frame -> case frame of 
            InformationFrame name msg -> send transitId (thisId, "message", name, msg) >> return True
            OptionFrame props         -> do 
                let newOptions = updateOptionsFromPairs props options
                liftIO $ writeIORef optionsRef newOptions
                send physLayerId (thisId, "reopen", newOptions)
                send transitId   (thisId, "info", "Recieved new options from other side, changing...")
                send transitId   (thisId, "options", newOptions)
                return True
            _ -> send transitId (thisId, "info", "Recieved another frame") >> return True
    return True
    where 
        (frameResult,_) = fromByteString byteFrame 

initChannelLayer :: ProcessId -> ChannelOptions -> Process ProcessId
initChannelLayer appLayer options = do
    id <- spawnLocal $ do
        thisId <- getSelfPid
        optionsRef <- liftIO $ newIORef options
        physLayerId <- initPhysicalLayer options thisId
        while $ receiveWait [
              matchIf (\(_, com)    -> com == "exit")       $ exitMsg
            , matchIf (\(_, com, _) -> com == "send")       $ sendMessage   physLayerId optionsRef
            , matchIf (\(_, com)    -> com == "connect")    $ connect       physLayerId optionsRef
            , matchIf (\(_, com)    -> com == "disconnect") $ disconnect
            , matchIf (\(_, com, _) -> com == "options")    $ changeOptions physLayerId optionsRef
            -- From physical layer
            , matchIf (\(_, com, _) -> com == "error")   $ transitError   appLayer
            , matchIf (\(_, com, _) -> com == "info")    $ transitInfo    appLayer
            , matchIf (\(_, com, _) -> com == "frame")   $ receiveFrame   physLayerId appLayer optionsRef]

        send physLayerId (thisId, "exit")
    return id
