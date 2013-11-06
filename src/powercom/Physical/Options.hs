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
module Physical.Options (
      ChannelOptions(..)
    , channel2physicalOptions
    ) where 

import Channel.Options
import System.Hardware.Serialport

channel2physicalOptions :: ChannelOptions -> SerialPortSettings
channel2physicalOptions channel = SerialPortSettings
    {
      commSpeed    = portSpeed      channel
    , bitsPerWord  = portWordBits   channel 
    , stopb        = portStopBits   channel 
    , parity       = portParityBits channel
    , flowControl  = NoFlowControl
    , timeout      = 1
    }