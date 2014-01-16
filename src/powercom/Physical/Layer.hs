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

import           Control.Distributed.Process      (expect, getSelfPid, liftIO, matchIf, Process, ProcessId, receiveWait, send, spawnLocal)
import qualified Data.ByteString as BS (ByteString)
import           Control.Monad         (forever)

import           Physical.Options      (ChannelOptions)
import           Physical.Port         (closePort, initPort, PortState, receiveFrame, reopenPort, sendFrame)
import           Utility               (exitMsg, while)
import           Control.Monad.Trans.Either      (eitherT)

receiveFrameCycle :: ProcessId -> PortState -> Process () 
receiveFrameCycle channelId portState = do
    liftIO $ putStrLn "Receiving thread started..." 
    forever $ do 
        thisId <- getSelfPid
        eitherT 
          (\err -> send channelId (thisId, "error", "Error while receiving frame: " ++ err ++ "!"))
          (\bs  -> send channelId (thisId, "frame", bs))
          $ receiveFrame portState

sendFrameHandler :: PortState -> (ProcessId, String, BS.ByteString) -> Process Bool
sendFrameHandler portState (senderId, _, msg) = do 
    thisId <- getSelfPid
    eitherT 
      (\err -> send senderId (thisId, "error", err) >> return True)
      (const $ return True)  
      $ sendFrame portState msg

reopenPortHandler :: PortState -> (ProcessId, String, ChannelOptions) -> Process Bool 
reopenPortHandler portState (senderId, _, options) = do
    thisId <- getSelfPid
    eitherT
       (\err -> send senderId False >> send senderId (thisId, "error", err) >> return True)
       (const $ send senderId True  >> return True)
       $ reopenPort portState options

closePort' :: PortState -> ProcessId -> Process ()
closePort' portState senderId = do
  thisId <- getSelfPid
  eitherT
    (\err -> send senderId (thisId, "error", err))
    (const $ return ())
    $ closePort portState
  
closePortHandler :: PortState -> (ProcessId, String) -> Process Bool 
closePortHandler portState (senderId, _) = closePort' portState senderId >> return True

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

initPhysicalLayer :: ChannelOptions -> ProcessId -> Process ProcessId
initPhysicalLayer options channelId = spawnLocal $ physicalLayerCycle options channelId