-----------------------------------------------------------------------------
-- |
-- Module      :  Physical.Options
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Describes options for physical layer.
--
-----------------------------------------------------------------------------
module Physical.Options (
      ChannelOptions(..)
    , channel2physicalOptions
    ) where 

import Channel.Options
import System.Hardware.Serialport

-- | Converts options from channel to physical format.
-- Physical options include also information about flow control and timeout for
-- particular serial port.
channel2physicalOptions :: ChannelOptions -> SerialPortSettings
channel2physicalOptions channel = SerialPortSettings
    {
      commSpeed    = portSpeed      channel
    , bitsPerWord  = portWordBits   channel 
    , stopb        = portStopBits   channel 
    , parity       = portParityBits channel
    , flowControl  = NoFlowControl
    , timeout      = 0 -- other values will throw you in the abyss of pain! Please, don't touch ;)
    }