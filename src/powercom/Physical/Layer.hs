-----------------------------------------------------------------------------
-- |
-- Module      :  Physical.Detector
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- System physical layer. It has following main purposes: 
--
--  * Opening/Closing serial port.
--
--  * Sending/Receiving frames as byte array.
--
--  * Reopening serial port on demand from upper layers with new settings.
--
--  * Informing upper layers about errors and exceptions.
--
-- Physical layer is able to communicate only with Channel layer. Communication
-- protocol includes following points:
--  
--  * Incoming \"exit\" - layer should stop all operations and exit.
--
--  * Incoming \"send\" bytestring - frame to send to the other side.
--
--  * Incoming \"reopen\" options - command to reopen serial port with new settings
--
--  * Incoming \"close\" - layer should close serial port and don't send any frames until it is opened again.
--
--  * Outgoing \"info\" string - useful information for upper layers.
--
--  * Outgoing \"error\" string - /something went wrong/ information for upper layers.
--  
--  * Outgoing \"frame\" bytestring - layer received frame from other side.
-----------------------------------------------------------------------------
module Physical.Layer (
    initPhysicalLayer
    ) where

import           Control.Distributed.Process      (expect, getSelfPid, liftIO, matchIf, Process, ProcessId, receiveWait, send, spawnLocal)
import qualified Data.ByteString as BS (ByteString)
import           Control.Monad         (forever)

import           Physical.Options      (ChannelOptions)
import           Physical.Port         (closePort, initPort, PortState, receiveFrame, reopenPort, sendFrame)
import           Utility               (exitMsg, while)
import           Control.Monad.Trans.Either      (eitherT)

-- | Infinite cycle of frame receiving. Informs channel layer
-- about errors and automatically sends received frames.
receiveFrameCycle :: ProcessId -> PortState -> Process () 
receiveFrameCycle channelId portState = do
    liftIO $ putStrLn "Receiving thread started..." 
    forever $ do 
        thisId <- getSelfPid
        eitherT 
          (\err -> send channelId (thisId, "error", "Error while receiving frame: " ++ err ++ "!"))
          (\bs  -> send channelId (thisId, "frame", bs))
          $ receiveFrame portState

-- | Called when "send" message is received from channel layer. Tries to
-- send frame bytestring to the other side and informs the sender about errors. 
sendFrameHandler :: PortState -> (ProcessId, String, BS.ByteString) -> Process Bool
sendFrameHandler portState (senderId, _, msg) = do 
    thisId <- getSelfPid
    eitherT 
      (\err -> send senderId (thisId, "error", err) >> return True)
      (const $ return True)  
      $ sendFrame portState msg

-- | Called when "reopen" message is received from channel layer. Tries to
-- reopen the serial port and informs the sender about errors.
reopenPortHandler :: PortState -> (ProcessId, String, ChannelOptions) -> Process Bool 
reopenPortHandler portState (senderId, _, options) = do
    thisId <- getSelfPid
    eitherT
       (\err -> send senderId False >> send senderId (thisId, "error", err) >> return True)
       (const $ send senderId True  >> return True)
       $ reopenPort portState options

-- | Handy function to close serial port with informing upper layers about errors.
closePort' :: PortState -> ProcessId -> Process ()
closePort' portState senderId = do
  thisId <- getSelfPid
  eitherT
    (\err -> send senderId (thisId, "error", err))
    (const $ return ())
    $ closePort portState
  
-- | Called when "close" message is received from channel layer. Tries to
-- close the serial port and informs the sender about errors.
closePortHandler :: PortState -> (ProcessId, String) -> Process Bool 
closePortHandler portState (senderId, _) = closePort' portState senderId >> return True

-- | Main function of physical layer. If it cannot initialize serial port, it
-- will wait for new port options to try again.
--
-- The function spawns receiving thread that operates independently. Upper layer (the second
-- argument) should support Physical <-> Channel layers communication protocol.
physicalLayerCycle :: ChannelOptions -> ProcessId -> Process ()
physicalLayerCycle options channelId = do
    thisId <- getSelfPid
    send channelId (thisId, "info", "Physical layer initialized...")

    eitherT 
      (\ex -> do
        send channelId (thisId, "error", "Exception while initing physical layer: " ++ show ex)
        (_, _, newOptions) <- expect :: Process (ProcessId, String, ChannelOptions)
        send channelId (thisId, "info", "Got new options, trying to init physical layer...")
        physicalLayerCycle newOptions channelId
      )
      (\port -> do
        send channelId (thisId, "info", "Serial port opened...")

        spawnLocal $ receiveFrameCycle channelId port

        while $ receiveWait [
              matchIf (\(_, com)    -> com == "exit")       exitMsg
            , matchIf (\(_, com, _) -> com == "send")       (sendFrameHandler port)
            , matchIf (\(_, com, _) -> com == "reopen")     (reopenPortHandler port)
            , matchIf (\(_, com) -> com == "close")         (closePortHandler port)]
        closePort' port channelId
       )
       $ initPort options

-- | Entry point for physical layer. Takes initial port settings and upper layer id.
-- Upper layer is informed about all errors that raised in the layer. 
initPhysicalLayer :: ChannelOptions -> ProcessId -> Process ProcessId
initPhysicalLayer options channelId = spawnLocal $ physicalLayerCycle options channelId