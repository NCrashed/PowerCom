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
module Application.Types (
      GuiCallbacks(..)
    , ChannelOptions(..)
    , portSpeed2String
    , string2PortSpeed
    , stopBit2String
    , string2StopBit
    , parityBit2String
    , string2ParityBit
    ) where 

import System.Hardware.Serialport hiding (send)
import Data.Word 

data ChannelOptions = 
    ChannelOptions 
    {
      portName      :: String
    , userName      :: String
    , portSpeed     :: CommSpeed
    , portStopBits  :: StopBits  
    , portParityBits:: Parity
    , portWordBits  :: Word8
    }
    

data GuiCallbacks = GuiCallbacks {
      sendMessageCallback   :: String -> IO ()
    , connectCallback       :: IO ()
    , disconnectCallback    :: IO ()
    , optionChangedCallback :: ChannelOptions -> IO ()
}

portSpeed2String :: CommSpeed -> String 
portSpeed2String spd = case spd of
    CS110    -> "110"
    CS300    -> "300"
    CS600    -> "600"
    CS1200   -> "1200"
    CS2400   -> "2400"
    CS4800   -> "4800"
    CS9600   -> "9600"
    CS19200  -> "19200"
    CS38400  -> "38400"
    CS57600  -> "57600"
    CS115200 -> "115200"

string2PortSpeed :: String -> CommSpeed
string2PortSpeed str = case str of
    "110"    -> CS110
    "300"    -> CS300
    "600"    -> CS600
    "1200"   -> CS1200
    "2400"   -> CS2400
    "4800"   -> CS4800
    "9600"   -> CS9600
    "19200"  -> CS19200
    "38400"  -> CS38400
    "57600"  -> CS57600
    "115200" -> CS115200
    _        -> CS2400

stopBit2String :: StopBits -> String 
stopBit2String sp =  case sp of 
    One -> "One" 
    Two -> "Two"

string2StopBit :: String -> StopBits 
string2StopBit s = case s of 
    "One" -> One
    "Two" -> Two
    _     -> One

parityBit2String :: Parity -> String 
parityBit2String p = case p of
    Even     -> "Even"
    Odd      -> "Odd"
    NoParity -> "No parity"

string2ParityBit :: String -> Parity 
string2ParityBit s = case s of
    "Even"      -> Even 
    "Odd"       -> Odd
    "No parity" -> NoParity
    _           -> NoParity