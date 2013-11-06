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
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import System.Hardware.Serialport     as Serial
import Data.Binary.Strict.Get
import Data.Binary.Put
import Data.Word 
import Data.IORef

import Physical.Options
import Utility (while, exitMsg)

toStrict :: BL.ByteString -> BS.ByteString
toStrict = BS.concat . BL.toChunks

initPort :: ChannelOptions -> Process (IORef SerialPort)
initPort channel = liftIO $ newIORef =<< openSerial (portName channel) (channel2physicalOptions channel)

closePort :: IORef SerialPort -> Process ()
closePort portRef = liftIO $ closeSerial =<< readIORef portRef

reopenPort :: IORef SerialPort -> ChannelOptions -> Process ()
reopenPort portRef options = do
    closePort portRef
    liftIO $ writeIORef portRef =<< openSerial (portName options) (channel2physicalOptions options)
    return ()

receiveFrame :: IORef SerialPort -> Process (Maybe BS.ByteString)
receiveFrame portRef = do 
    port <- liftIO $ readIORef portRef
    bsLength <- liftIO $ recv port 4 
    case fst $ runGet (getWord32be) bsLength of
        Left _ -> return Nothing
        Right frameLength -> do
            msg <- liftIO $ recv port $ fromIntegral frameLength
            return $ Just msg

sendFrame :: IORef SerialPort -> BS.ByteString -> Process (Maybe String)
sendFrame portRef msg = do
    port <- liftIO $ readIORef portRef
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

initPhysicalLayer :: ChannelOptions -> ProcessId -> Process ProcessId
initPhysicalLayer options channelId = do
    id <- spawnLocal $ do
        thisId <- getSelfPid
        portRef <- initPort options

        while $ receiveWait [match exitMsg]
        closePort portRef
    return id