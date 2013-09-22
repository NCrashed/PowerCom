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
module PhysicalLevel (
        initPhysicalLevel
    ) where

import qualified Data.ByteString as BS
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.Hardware.Serialport
import Control.Distributed.Process

import Utility

openPort :: (String, SerialPortSettings) -> Process SerialPort
openPort settings = liftIO $ openSerial (fst settings) (snd settings)

sendFrame :: SerialPort -> BS.ByteString -> Process SerialPort
sendFrame port str = do
    liftIO $ System.Hardware.Serialport.send port str
    return port

receiveCycle :: SerialPort -> ProcessId -> Process ()
receiveCycle port channelLevel = do
    newport <- receiveWait [match $ sendFrame port, match openPort]
    receiveCycle newport channelLevel

initPhysicalLevel :: ProcessId -> Process ProcessId
initPhysicalLevel channelLevel = do
    id <- spawnLocal $ forever $ do
        port <- openPort ("COM1", defaultSerialSettings { commSpeed = CS2400 })
        receiveCycle port channelLevel
    return id