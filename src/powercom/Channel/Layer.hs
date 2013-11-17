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
import Channel.Connection
import Channel.Sending
import Channel.Miscs
import Channel.Processing 
import Channel.ConnectionChecker 

import Physical.Layer 
import Utility (while, exitMsg)

import Control.Distributed.Process

import qualified Data.ByteString as BS

sendMessageHandler :: ProcessId -> Connection -> InnerChannelOptions -> (ProcessId, String, String) -> Process Bool 
sendMessageHandler physLayerId conn optionsRef (senderId, _, msg) = do 
    options <- getOptions optionsRef
    ifConnectedWithError conn senderId $ 
        mapM_ (sendFrameWithDisconnect conn senderId physLayerId) $ frameBuffers $ userName options
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

-- TODO: Move to sending Frame instead of bytestring
transitFrameHandler :: ProcessId -> Connection -> (ProcessId, String, BS.ByteString) -> Process Bool 
transitFrameHandler physLayerId conn (senderId, _, framebs) = do
    let (res, _) = fromByteString framebs 
    case res of 
        Right frame -> ifConnected conn $ sendFrameWithDisconnect conn senderId physLayerId frame
        Left _ -> return ()
    return True

changeOptionsHandler :: ProcessId -> Connection -> InnerChannelOptions -> (ProcessId, String, ChannelOptions, ChannelOptions) -> Process Bool 
changeOptionsHandler physLayerId conn optionsRef (senderId, _, options, oldOptions) = do 
    thisId <- getSelfPid
    setOptions optionsRef options
    send physLayerId (thisId, "reopen", options)
    ifConnected conn $ do 
        informSender senderId "Changing options..."
        case userName oldOptions == userName options of 
            False  -> sendFrameWithDisconnect conn senderId physLayerId frameWithRemoteNames
            True   -> sendFrameWithDisconnect conn senderId physLayerId frame
    return True
    where
        frame :: Frame
        frame = OptionFrame optionPairs

        frameWithRemoteNames :: Frame
        frameWithRemoteNames = OptionFrame $ optionPairs ++
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

initChannelLayer :: ProcessId -> ChannelOptions -> Process ProcessId
initChannelLayer appLayer options = do
    id <- spawnLocal $ do
        thisId <- getSelfPid
        optionsRef    <- initInnerOptions options
        connection    <- initConnection
        messageBuffer <- initMessageBuffer
        physLayerId   <- initPhysicalLayer options thisId
        spawnConnectionChecker thisId appLayer connection
        while $ receiveWait [
            -- From connection checker
              matchIf (\(_, com, _)    -> com == "transit-frame") $ transitFrameHandler   physLayerId connection
            -- From application layer
            , matchIf (\(_, com)       -> com == "exit")       $ exitMsg
            , matchIf (\(_, com, _)    -> com == "send")       $ sendMessageHandler   physLayerId connection optionsRef
            , matchIf (\(_, com)       -> com == "connect")    $ connectHandler       physLayerId connection optionsRef
            , matchIf (\(_, com)       -> com == "disconnect") $ disconnectHandler    physLayerId connection optionsRef
            , matchIf (\(_, com, _, _) -> com == "options")    $ changeOptionsHandler physLayerId connection optionsRef
            -- From physical layer
            , matchIf (\(_, com, _)    -> com == "error")      $ transitError        appLayer
            , matchIf (\(_, com, _)    -> com == "info")       $ transitInfo         appLayer
            , matchIf (\(_, com, _)    -> com == "frame" || com == "frame-acked")      
                $ receiveFrameHandler physLayerId appLayer messageBuffer connection optionsRef]

        send physLayerId (thisId, "exit")
    return id