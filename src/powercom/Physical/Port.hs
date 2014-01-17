-----------------------------------------------------------------------------
-- |
-- Module      :  Physical.Port
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Handy wrappers around serial API for status and error control.
--
-----------------------------------------------------------------------------
{-# LANGUAGE MultiWayIf #-}
module Physical.Port (
      PortState
    , initPort
    , closePort
    , reopenPort
    , receiveFrame
    , serialSend
    , sendFrame    
    ) where

import qualified System.Hardware.Serialport as Serial (closeSerial, openSerial, recv, send, SerialPort)
import qualified Data.ByteString            as BS (ByteString, concat, length)
import qualified Data.ByteString.Lazy       as BL (ByteString, toChunks)
import           Data.Binary.Strict.Get               (getWord32be, runGet) 
import           Data.Binary.Put                      (putWord32be, runPut) 
import           Data.IORef                           (IORef, newIORef, readIORef, writeIORef)
import           Control.Concurrent                   (yield)
import           Control.Distributed.Process          (liftIO, Process)
import           Control.Monad                        (when) 
import           Control.Monad.Trans.Either           (EitherT, left, right)

import           Physical.Options                     (channel2physicalOptions, ChannelOptions, portName)
import           Utility                              (liftExceptions)
import           Control.Monad.Trans.Class            (lift)

-- | Encapsulates information about serial port including
-- info about open/close status.
newtype PortState = PortState (IORef (Serial.SerialPort, Bool))

readState :: PortState -> Process (Serial.SerialPort, Bool)
readState (PortState ref) = liftIO $ readIORef ref
  
writeState :: PortState -> (Serial.SerialPort, Bool) -> Process ()
writeState (PortState ref) val = liftIO $ writeIORef ref val

toStrict :: BL.ByteString -> BS.ByteString
toStrict = BS.concat . BL.toChunks

-- | Serial port initialization. Takes channel layer options
-- that is internally converted to physical options. All
-- exceptions are lifted to EitherT monad transformer.
initPort :: ChannelOptions -> EitherT String Process PortState
initPort channel = do
  res <- liftExceptions . liftIO $ Serial.openSerial (portName channel) (channel2physicalOptions channel)
  ref <- liftIO $ newIORef (res, True)
  right $ PortState ref

-- | Closes serial port. Can throw exception. All
-- exceptions are lifted to EitherT monad transformer.
closePort :: PortState -> EitherT String Process ()
closePort portState = do
    (port, opened) <- lift $ readState portState
    when opened $ do 
        liftExceptions . liftIO $ Serial.closeSerial port
        lift $ writeState portState (port, False)

-- | If the serial port is opened, then closes it. After that
-- tries to reopen serial port with particular options. All
-- exceptions are lifted to EitherT monad transformer.
reopenPort :: PortState -> ChannelOptions -> EitherT String Process ()
reopenPort portState options = do
    (_, opened) <- lift $ readState portState 
    when opened $ closePort portState

    newport <- liftExceptions . liftIO $ Serial.openSerial (portName options) (channel2physicalOptions options)
    lift $ writeState portState (newport, True)
    
-- | Synchronous way to get a frame from serial port. Calling thread is blocked
-- until frame is received or any error (parse or reading from port) is occured.
-- All exceptions are lifted to EitherT monad transformer. 
-- Function expects following physical frame format:
--   * 4 bytes for frame length
--   * Frame body with specified length 
receiveFrame :: PortState -> EitherT String Process BS.ByteString
receiveFrame portState = do 
    bsLength <- receiveNonEmpty portState 4
    case fst $ runGet getWord32be bsLength of
      Left _            -> left ("Parsing failed! " ++ show bsLength)
      Right frameLength -> receiveNonEmpty portState $ fromIntegral frameLength
    where
      receiveNonEmpty :: PortState -> Int -> EitherT String Process BS.ByteString
      receiveNonEmpty pstate msgLength = do
        (port, opened) <- lift $ readState pstate
        if not opened then left "Port closed!"
        else do
          liftIO yield
          msg <- liftExceptions . liftIO $ Serial.recv port msgLength
          liftIO yield
          if | BS.length msg == 0 -> receiveNonEmpty pstate msgLength
             | BS.length msg < msgLength -> do 
               rec <- receiveNonEmpty pstate $ msgLength - BS.length msg
               right $ BS.concat [msg, rec]
             | otherwise -> right msg 

-- | Tries to send bytes through specified serial port and returns a number of 
-- transmited bytes. All exceptions are lifted to EitherT monad transformer.
serialSend :: PortState -> BS.ByteString -> EitherT String Process Int
serialSend portState msg = do 
  (port, _) <- lift $ readState portState
  liftExceptions . liftIO $ Serial.send port msg

-- | Sends frame bytestring through specified serial port. First 4 bytes of 
-- frame length are sent and at last whole frame bytes are sent.
-- All exceptions are lifted to EitherT monad transformer.
sendFrame :: PortState -> BS.ByteString ->  EitherT String Process ()
sendFrame portState msg = do
  (_, opened) <- lift $ readState portState
  if not opened then left "Serial port is closed!"
  else do
    sendLength <- serialSend portState bsLength
    case sendLength of 
      4 -> do 
        sendedMsg <- serialSend portState msg
        let notSended = sendedMsg - frameLength in if notSended == 0 then right ()
        else left $ "Failed to send frame body! Bytes that wasn't sent: " ++ show notSended
      _ -> left "Failed to send frame length!"
    where
        bsLength :: BS.ByteString
        bsLength = toStrict $ runPut $ putWord32be $ fromIntegral frameLength
        frameLength = BS.length msg
