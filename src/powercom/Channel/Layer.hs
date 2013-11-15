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
import Channel.Connection
import Channel.Sending
import Channel.Miscs

import Physical.Layer 
import Utility (while, exitMsg)

import Control.Distributed.Process
import qualified Data.ByteString as BS
import Data.IORef 
import Data.Functor
import Data.List
import Data.Word
import Control.Applicative

sendMessageHandler :: ProcessId -> Connection -> IORef ChannelOptions -> (ProcessId, String, String) -> Process Bool 
sendMessageHandler physLayerId conn optionsRef (senderId, _, msg) = do 
    thisId <- getSelfPid
    options <- liftIO $ readIORef optionsRef
    ifConnectedWithError conn senderId $ 
        mapM_ (sendFrameWithAck physLayerId) $ frameBuffers $ userName options
    return True
    where
        frameBuffers :: String -> [Frame]
        frameBuffers uname = startFrame : (dataFrames msg)
            where  
                lengthInFrame :: Int 
                lengthInFrame = 200

                startFrame :: Frame
                startFrame = InformationFrame uname $ fromIntegral dataFramesCount

                dataFramesCount :: Int 
                dataFramesCount = ((length msg) `quot` lengthInFrame) + 1

                dataFrames :: String -> [Frame]
                dataFrames [] = []
                dataFrames s =  (DataPartFrame $ take lengthInFrame s) : (dataFrames $ drop lengthInFrame s)

changeOptionsHandler :: ProcessId -> Connection -> IORef ChannelOptions -> (ProcessId, String, ChannelOptions, ChannelOptions) -> Process Bool 
changeOptionsHandler physLayerId conn optionsRef (senderId, _, options, oldOptions) = do 
    thisId <- getSelfPid
    liftIO $ writeIORef optionsRef options
    send physLayerId (thisId, "reopen", options)
    ifConnected conn $ do 
        informSender senderId "Changing options..."
        case userName oldOptions == userName options of 
            False  -> send physLayerId (thisId, "send", frameBufferWithRemoteNames)
            True   -> send physLayerId (thisId, "send", frameBuffer)
    return True
    where
        frameBuffer :: BS.ByteString
        frameBuffer = codeFrame $ OptionFrame optionPairs

        frameBufferWithRemoteNames :: BS.ByteString
        frameBufferWithRemoteNames = codeFrame $ OptionFrame $ optionPairs ++
            [("remoteNameNew", userName options)
            ,("remoteNameOld", userName oldOptions)] 
            
        optionPairs :: [(String, String)]
        optionPairs = getOptionPairs options 
            [ "portSpeed"
            , "portStopBits"
            , "portParityBits"
            , "portWordBits"]

transitError :: ProcessId -> (ProcessId, String, String) -> Process Bool
transitError transitId (_, _, msg) = informSenderError transitId msg >> return True

transitInfo :: ProcessId -> (ProcessId, String, String) -> Process Bool
transitInfo transitId (_, _, msg) = informSender transitId msg >> return True

receiveFrameHandler :: ProcessId -> ProcessId -> MessageBuffer -> Connection -> IORef ChannelOptions 
    -> (ProcessId, String, BS.ByteString) -> Process Bool 
receiveFrameHandler physLayerId transitId messageBuffer conn optionsRef (_, _, byteFrame) = do 
    thisId <- getSelfPid
    case decodeFrame byteFrame of 
        Just frame -> do
            case frame of 
                AckFrame -> return ()
                RetFrame -> return ()
                _ -> do 
                    sendFrame physLayerId AckFrame
                    processFrame frame
        _ -> do 
            informSenderError transitId $ "Failed to recieve frame!"
            sendFrame physLayerId RetFrame
    return True
    where 
        getRemoteNames :: [(String, String)] -> Maybe (String, String)
        getRemoteNames props = (,) <$> getValue props "remoteNameNew" <*> getValue props "remoteNameOld"

        getValue :: [(String, String)] -> String -> Maybe String 
        getValue props key = snd <$> find (\(k, _) -> k == key) props

        word2int :: Word32 -> Int 
        word2int = fromInteger . toInteger

        processFrame :: Frame -> Process ()
        processFrame frame = do 
            options <- liftIO $ readIORef optionsRef
            case frame of 
                InformationFrame name n -> clearBuffer messageBuffer name (word2int n)
                DataPartFrame s -> do
                    addMessagePart messageBuffer s 
                    filled <- isMessageReady messageBuffer
                    if filled then do  
                        (name, msg) <- collectMessage messageBuffer
                        sendMessage transitId name msg 
                    else return ()
                OptionFrame props         -> do 
                    case getRemoteNames props of 
                        Just (newName, oldName) -> do 
                            informSender transitId $ "Remote name changing, " ++ oldName ++ " to " ++ newName
                            sendDisconnectUser transitId oldName
                            sendConnectUser transitId newName
                        Nothing -> return ()
                    let newOptions = updateOptionsFromPairs props options
                    liftIO $ writeIORef optionsRef newOptions
                    informSender transitId "Recieved new options from other side, changing..."
                    sendReopenPort physLayerId newOptions
                    sendUpdateOptions transitId newOptions

                LinkFrame   name -> do 
                    sendConnectUser transitId name
                    ifNotConnected conn $ do 
                        informSender transitId "Remote host connected!"
                        sendFrameWithAck physLayerId $ LinkFrame $ userName options
                        openConnection conn

                UnlinkFrame name -> do 
                    sendDisconnectUser transitId name
                    ifConnected conn $ do 
                        informSender transitId "Remote host disconnected!"
                        sendFrameWithAck physLayerId $ UnlinkFrame $ userName options
                        closeConnection conn 

                RetFrame -> return ()
                AckFrame -> informSender transitId "Problems!" 

initChannelLayer :: ProcessId -> ChannelOptions -> Process ProcessId
initChannelLayer appLayer options = do
    id <- spawnLocal $ do
        thisId <- getSelfPid
        optionsRef    <- liftIO $ newIORef options
        connection    <- initConnection
        messageBuffer <- initMessageBuffer
        physLayerId   <- initPhysicalLayer options thisId
        while $ receiveWait [
              matchIf (\(_, com)       -> com == "exit")       $ exitMsg
            , matchIf (\(_, com, _)    -> com == "send")       $ sendMessageHandler   physLayerId connection optionsRef
            , matchIf (\(_, com)       -> com == "connect")    $ connectHandler       physLayerId connection optionsRef
            , matchIf (\(_, com)       -> com == "disconnect") $ disconnectHandler    physLayerId connection optionsRef
            , matchIf (\(_, com, _, _) -> com == "options")    $ changeOptionsHandler physLayerId connection optionsRef
            -- From physical layer
            , matchIf (\(_, com, _)    -> com == "error")      $ transitError        appLayer
            , matchIf (\(_, com, _)    -> com == "info")       $ transitInfo         appLayer
            , matchIf (\(_, com, _)    -> com == "frame")      $ receiveFrameHandler physLayerId appLayer messageBuffer connection optionsRef]

        send physLayerId (thisId, "exit")
    return id