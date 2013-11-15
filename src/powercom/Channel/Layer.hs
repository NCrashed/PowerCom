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
import Channel.Buffer
import Channel.CyclicCode
import Physical.Layer 
import Utility (while, exitMsg)

import Control.Distributed.Process
import qualified Data.ByteString as BS
import Data.IORef 
import Data.Functor
import Data.List
import Data.Word
import Control.Applicative

connect :: ProcessId -> IORef Bool -> IORef ChannelOptions -> (ProcessId, String) -> Process Bool
connect physLayerId connRef optionsRef (senderId, _) = do
    thisId <- getSelfPid
    options <- liftIO $ readIORef optionsRef
    connection <- liftIO $ readIORef connRef
    case connection of 
        False -> do 
            send senderId (thisId, "info", "Connecting...")
            send physLayerId (thisId, "reopen", options)
            connResult <- expect :: Process Bool
            case connResult of 
                True -> do 
                    liftIO $ writeIORef connRef True
                    send physLayerId (thisId, "send", frameBuffer $ userName options)
                    return True
                False -> return True
        True -> return True
    where
        frameBuffer :: String -> BS.ByteString
        frameBuffer uname = toByteString $ LinkFrame uname

disconnect :: ProcessId -> IORef Bool -> IORef ChannelOptions -> (ProcessId, String) -> Process Bool
disconnect physLayerId connRef optionsRef (senderId, _) = do
    thisId <- getSelfPid
    options <- liftIO $ readIORef optionsRef
    connection <- liftIO $ readIORef connRef
    case connection of 
        True -> do 
            send senderId (thisId, "info", "Disconnecting...")
            send physLayerId (thisId, "send", frameBuffer $ userName options)
            liftIO $ writeIORef connRef False
            return True
        False -> return True
    where
        frameBuffer :: String -> BS.ByteString
        frameBuffer uname = toByteString $ UnlinkFrame uname

ifConnected :: Bool -> IORef Bool -> ProcessId -> Process Bool -> Process Bool
ifConnected mute connRef errorTransitId action = do 
    connection <- liftIO $ readIORef connRef
    thisId <- getSelfPid
    case connection of 
        True  -> action
        False -> if not mute then send errorTransitId (thisId, "error", "Connection is not established!") >> return True
            else return True

ifNotConnected :: IORef Bool -> Process Bool -> Process Bool
ifNotConnected connRef action = do 
    connection <- liftIO $ readIORef connRef
    thisId <- getSelfPid
    case connection of 
        False  -> action
        True   -> return True

sendMessage :: ProcessId -> IORef Bool -> IORef ChannelOptions -> (ProcessId, String, String) -> Process Bool 
sendMessage physLayerId connRef optionsRef (senderId, _, msg) = do 
    thisId <- getSelfPid
    options <- liftIO $ readIORef optionsRef
    ifConnected False connRef senderId $ do
        mapM_ (\b -> send physLayerId (thisId, "send", b)) $ frameBuffers $ userName options
        return True
    where
        frameBuffers :: String -> [BS.ByteString]
        frameBuffers uname = startFrame : (dataFrames msg)
            where  
                lengthInFrame :: Int 
                lengthInFrame = 200

                startFrame :: BS.ByteString
                startFrame = toByteString $ InformationFrame uname $ fromIntegral dataFramesCount

                dataFramesCount :: Int 
                dataFramesCount = ((length msg) `quot` lengthInFrame) + 1

                dataFrames :: String -> [BS.ByteString]
                dataFrames [] = []
                dataFrames s =  (toByteString $ DataPartFrame $ take lengthInFrame s) : (dataFrames $ drop lengthInFrame s)

changeOptions :: ProcessId -> IORef Bool -> IORef ChannelOptions -> (ProcessId, String, ChannelOptions, ChannelOptions) -> Process Bool 
changeOptions physLayerId connRef optionsRef (senderId, _, options, oldOptions) = do 
    thisId <- getSelfPid
    liftIO $ writeIORef optionsRef options
    send physLayerId (thisId, "reopen", options)
    ifConnected True connRef senderId $ do 
        send senderId (thisId, "info", "Changing options...")
        case userName oldOptions == userName options of 
            False  -> send physLayerId (thisId, "send", frameBufferWithRemoteNames) >> return True
            True   -> send physLayerId (thisId, "send", frameBuffer) >> return True
    where
        frameBuffer :: BS.ByteString
        frameBuffer = toByteString $ OptionFrame optionPairs

        frameBufferWithRemoteNames :: BS.ByteString
        frameBufferWithRemoteNames = toByteString $ OptionFrame $ optionPairs ++
            [("remoteNameNew", userName options)
            ,("remoteNameOld", userName oldOptions)] 
            
        optionPairs :: [(String, String)]
        optionPairs = getOptionPairs options 
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

receiveFrame :: ProcessId -> ProcessId -> MessageBuffer -> IORef Bool -> IORef ChannelOptions 
    -> (ProcessId, String, BS.ByteString) -> Process Bool 
receiveFrame physLayerId transitId messageBuffer connRef optionsRef (_, _, byteFrame) = do 
    thisId <- getSelfPid
    options <- liftIO $ readIORef optionsRef
    case frameResult of 
        Left err -> do 
            send transitId (thisId, "error", "Failed to recieve frame: " ++ err)
            return True
        Right frame -> case frame of 
            InformationFrame name n -> do
                liftIO $ clearBuffer messageBuffer name (word2int n)
                return True
            DataPartFrame s -> do
                liftIO $ addMessagePart messageBuffer s 
                filled <- liftIO $ isMessageReady messageBuffer
                case filled of 
                    True  -> do
                        (name, msg) <- liftIO $ collectMessage messageBuffer
                        send transitId (thisId, "message", name, msg)
                        return True
                    False -> return True
            OptionFrame props         -> do 
                case getRemoteNames props of 
                    Just (newName, oldName) -> do 
                        send transitId (thisId, "info", "Remote name changing, " ++ oldName ++ " to " ++ newName)
                        send transitId (thisId, "disconnect", oldName)
                        send transitId (thisId, "connect", newName)
                    Nothing -> return ()
                let newOptions = updateOptionsFromPairs props options
                liftIO $ writeIORef optionsRef newOptions
                send physLayerId (thisId, "reopen", newOptions)
                send transitId   (thisId, "info", "Recieved new options from other side, changing...")
                send transitId   (thisId, "options", newOptions)
                return True

            LinkFrame   name -> do 
                send transitId (thisId, "connect",    name)
                ifNotConnected connRef $ do 
                    send transitId   (thisId, "info", "Remote host connected!")
                    send physLayerId (thisId, "send", toByteString $ LinkFrame $ userName options)
                    liftIO $ writeIORef connRef True
                    return True

            UnlinkFrame name -> do 
                send transitId (thisId, "disconnect", name)
                ifConnected True connRef transitId $ do 
                    send transitId   (thisId, "info", "Remote host disconnected!")
                    send physLayerId (thisId, "send", toByteString $ UnlinkFrame $ userName options)
                    liftIO $ writeIORef connRef False
                    return True

            _ -> send transitId (thisId, "info", "Recieved another frame") >> return True
    return True
    where 
        (frameResult,_) = fromByteString byteFrame 
        getRemoteNames :: [(String, String)] -> Maybe (String, String)
        getRemoteNames props = (,) <$> getValue props "remoteNameNew" <*> getValue props "remoteNameOld"

        getValue :: [(String, String)] -> String -> Maybe String 
        getValue props key = snd <$> find (\(k, _) -> k == key) props

        word2int :: Word32 -> Int 
        word2int = fromInteger . toInteger

initChannelLayer :: ProcessId -> ChannelOptions -> Process ProcessId
initChannelLayer appLayer options = do
    id <- spawnLocal $ do
        thisId <- getSelfPid
        optionsRef <- liftIO $ newIORef options
        connRef    <- liftIO $ newIORef False
        messageBuffer <- liftIO $ initMessageBuffer
        physLayerId <- initPhysicalLayer options thisId
        while $ receiveWait [
              matchIf (\(_, com)       -> com == "exit")       $ exitMsg
            , matchIf (\(_, com, _)    -> com == "send")       $ sendMessage   physLayerId connRef optionsRef
            , matchIf (\(_, com)       -> com == "connect")    $ connect       physLayerId connRef optionsRef
            , matchIf (\(_, com)       -> com == "disconnect") $ disconnect    physLayerId connRef optionsRef
            , matchIf (\(_, com, _, _) -> com == "options")    $ changeOptions physLayerId connRef optionsRef
            -- From physical layer
            , matchIf (\(_, com, _)    -> com == "error")      $ transitError   appLayer
            , matchIf (\(_, com, _)    -> com == "info")       $ transitInfo    appLayer
            , matchIf (\(_, com, _)    -> com == "frame")      $ receiveFrame   physLayerId appLayer messageBuffer connRef optionsRef]

        send physLayerId (thisId, "exit")
    return id
