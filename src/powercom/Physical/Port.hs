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
module Physical.Port (
      PortState
    , initPort
    , closePort
    , reopenPort
    , receiveFrame
    , serialSendSafe
    , sendFrame    
    ) where

import qualified System.Hardware.Serialport     as Serial
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import Data.Binary.Strict.Get 
import Data.Binary.Put 
import Data.Word 
import Data.IORef
import Control.Exception (SomeException)
import Control.Concurrent (yield)
import Control.Distributed.Process
import Control.Monad 

import Physical.Options

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
    when opened $ liftIO $ do 
        Serial.closeSerial port
        writeIORef portState (port, False)

reopenPort :: PortState -> ChannelOptions -> Process (Maybe String)
reopenPort portState options = do
    (port, opened) <- liftIO $ readIORef portState 
    when opened $ closePort portState

    res <- try (liftIO $ 
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
            case fst $ runGet getWord32be bsLength of
                Left _            -> return $ Left ("Parsing failed! " ++ show bsLength)
                Right frameLength -> receiveNonEmpty portState $ fromIntegral frameLength
    where
        receiveNonEmpty :: PortState -> Int -> Process (Either String BS.ByteString)
        receiveNonEmpty portState msgLength = do
            (port, opened) <- liftIO $ readIORef portState
            if not opened then return $ Left "Port closed!"
            else do
                liftIO yield
                res <- try (liftIO $ Serial.recv port msgLength) :: Process (Either SomeException BS.ByteString)
                liftIO yield
                case res of 
                    Left ex         -> return $ Left (show ex)
                    Right msg | BS.length msg == 0 -> receiveNonEmpty portState msgLength
                              | BS.length msg < msgLength -> do 
                                resRec <- receiveNonEmpty portState $ msgLength - BS.length msg
                                case resRec of
                                    Left ex -> return $ Left ex
                                    Right rec -> return $ Right $ BS.concat [msg, rec]
                              | otherwise -> return $ Right msg 

serialSendSafe :: PortState -> BS.ByteString -> Process (Maybe Int)
serialSendSafe portState msg = do 
    (port, _) <- liftIO $ readIORef portState
    res <- try (liftIO $ Serial.send port msg) :: Process (Either SomeException Int)
    case res of 
        Left err -> return Nothing
        Right l -> return $ Just l 

sendFrame :: PortState -> BS.ByteString -> Process (Maybe String)
sendFrame portState msg = do
    (_, opened) <- liftIO $ readIORef portState
    if not opened then return Nothing
    else do
        sendLengthRes <- serialSendSafe portState bsLength
        case sendLengthRes of 
            Just 4 -> do 
                sendedMsgRes <- serialSendSafe portState msg 
                case sendedMsgRes of
                    Just frameLength -> return Nothing
                    _                -> return $ Just "Failed to send frame body!"
            _ -> return $ Just "Failed to send frame length!"
        where
            bsLength :: BS.ByteString
            bsLength = toStrict $ runPut $ putWord32be $ fromIntegral frameLength
            frameLength = BS.length msg
