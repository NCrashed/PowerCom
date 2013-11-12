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
import qualified Data.ByteString.Lazy           as BL
import qualified System.Hardware.Serialport     as Serial
import Data.Binary.Strict.Get
import Data.Binary.Put
import Data.Word 
import Data.IORef
import Control.Exception (SomeException)
import Control.Monad (forever)
import Control.Concurrent (yield)

import Physical.Options
import Utility (while, exitMsg)

type PortState = IORef (Serial.SerialPort, Bool)

toStrict :: BL.ByteString -> BS.ByteString
toStrict = BS.concat . BL.toChunks

initPort :: ChannelOptions -> Process PortState
initPort channel = do
    port <- liftIO $ Serial.openSerial (portName channel) (channel2physicalOptions channel)
    liftIO $ newIORef (port, True)

closePort :: PortState -> Process ()
closePort portState = do
    (port, opened) <- liftIO $ readIORef portState
    if opened then liftIO $ Serial.closeSerial port
    else return ()

reopenPort :: PortState -> ChannelOptions -> Process (Maybe String)
reopenPort portState options = do
    (port, opened) <- liftIO $ readIORef portState 
    if(opened == True) then closePort portState else return ()

    res <- try (liftIO $ liftIO $ 
        Serial.openSerial (portName options) (channel2physicalOptions options)) 
            :: Process (Either SomeException Serial.SerialPort)
    case res of 
        Left ex -> return $ Just $ show ex 
        Right newPort -> liftIO $ writeIORef portState (newPort, True) >> return Nothing
    

receiveFrame :: PortState -> Process (Either String BS.ByteString)
receiveFrame portState = do 
    bsLengthRes <- receiveNonEmpty portState 4
    case bsLengthRes of 
        Left ex        -> return $ Left ex
        Right bsLength -> 
            case fst $ runGet (getWord32be) bsLength of
                Left _            -> return $ Left ("Parsing failed! " ++ show bsLength)
                Right frameLength -> receiveNonEmpty portState $ fromIntegral frameLength
    where
        receiveNonEmpty :: PortState -> Int -> Process (Either String BS.ByteString)
        receiveNonEmpty portState msgLength = do
            (port, opened) <- liftIO $ readIORef portState
            if opened == False then return $ Left "Port closed!"
            else do
                liftIO $ yield
                res <- try (liftIO $ Serial.recv port msgLength) :: Process (Either SomeException BS.ByteString)
                liftIO $ yield
                case res of 
                    Left ex         -> return $ Left (show ex)
                    Right msg       -> if BS.length msg == 0 
                        then receiveNonEmpty portState msgLength 
                        else if BS.length msg < msgLength 
                            then do 
                                resRec <- receiveNonEmpty portState (msgLength - (BS.length msg))
                                case resRec of
                                    Left ex -> return $ Left ex 
                                    Right rec -> return $ Right $ BS.concat [msg,rec]   
                            else return $ Right msg 


receiveFrameCycle :: ProcessId -> PortState -> Process () 
receiveFrameCycle channelId portState = do
    liftIO $ putStrLn "Recieving thread started..." 
    forever $ do
        frameResult <- receiveFrame portState 
        thisId <- getSelfPid
        case frameResult of 
            Right bs -> send channelId (thisId, "frame", bs)
            Left err -> send channelId (thisId, "error", "Error while receiving frame: " ++ err ++ "!")
                
sendFrame :: PortState -> BS.ByteString -> Process (Maybe String)
sendFrame portState msg = do
    (port, opened) <- liftIO $ readIORef portState
    if opened == False then return Nothing
    else do
        sendedLength <- liftIO $ Serial.send port bsLength
        if sendedLength == 4 then do 
            sendedMsg <- liftIO $ Serial.send port msg 
            if sendedMsg == frameLength 
                then return Nothing
                else return $ Just "Failed to send frame body!"
        else return $ Just "Failed to send frame length!"
        where
            bsLength :: BS.ByteString
            bsLength = toStrict $ runPut $ putWord32be $ fromIntegral frameLength
            frameLength = BS.length msg

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

physicalLayerCycle :: ChannelOptions -> ProcessId -> Process ()
physicalLayerCycle options channelId = do
    thisId <- getSelfPid
    send channelId (thisId, "info", "Physical layer initialized...")

    initResult <- try (initPort options) :: Process (Either SomeException PortState)
    case initResult of 
        Right port -> do
            send channelId (thisId, "info", "Serial port opened...")

            receiveId <- spawnLocal $ receiveFrameCycle channelId port

            while $ receiveWait [
                  matchIf (\(_, com)    -> com == "exit")       exitMsg
                , matchIf (\(_, com, _) -> com == "send")       (sendFrameHandler port)
                , matchIf (\(_, com, _) -> com == "reopen")     (reopenPortHandler port)]

            closePort port
        Left ex -> do
            send channelId (thisId, "error", "Exception while initing physical layer: " ++ show ex)
            (_, _, newOptions) <- expect :: Process (ProcessId, String, ChannelOptions)
            send channelId (thisId, "info", "Got new options, trying to init physical layer...")
            physicalLayerCycle newOptions channelId

initPhysicalLayer :: ChannelOptions -> ProcessId -> Process ProcessId
initPhysicalLayer options channelId = do
    id <- spawnLocal $ physicalLayerCycle options channelId
    return id