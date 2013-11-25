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
{-# Language StandaloneDeriving, DeriveDataTypeable #-}
module Channel.Options (
      InnerChannelOptions
    , initInnerOptions
    , getOptions
    , setOptions
    , ChannelOptions(..)
    , defaultOptions
    , Binary(..)
    , portSpeed2String
    , string2PortSpeed
    , stopBit2String
    , string2StopBit
    , parityBit2String
    , string2ParityBit
    , updateOptionsFromPairs
    , getOptionPairs
    ) where 

import System.Hardware.Serialport

import Data.Binary (Binary(..))
import Data.Binary.Get

import Data.Word
import Data.Typeable
import Data.IORef
import Control.Distributed.Process

deriving instance Typeable CommSpeed
deriving instance Typeable StopBits
deriving instance Typeable Parity 
deriving instance Typeable FlowControl
deriving instance Typeable SerialPortSettings

deriving instance Show Parity 
deriving instance Show StopBits 

deriving instance Eq CommSpeed 
deriving instance Eq StopBits 
deriving instance Eq Parity 

type InnerChannelOptions = IORef ChannelOptions

initInnerOptions :: ChannelOptions -> Process InnerChannelOptions
initInnerOptions opt = liftIO $ newIORef opt 

getOptions :: InnerChannelOptions -> Process ChannelOptions
getOptions inner = liftIO $ readIORef inner 

setOptions :: InnerChannelOptions -> ChannelOptions -> Process ()
setOptions inner opt = liftIO $ writeIORef inner opt 

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
    deriving (Typeable, Show)
    
defaultOptions :: ChannelOptions
defaultOptions = ChannelOptions
        {
          portName       = "COM1"
        , userName       = "Username"
        , portSpeed      = CS2400
        , portStopBits   = Two
        , portParityBits = NoParity 
        , portWordBits   = 8
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

getOptionPairs :: ChannelOptions -> [String] -> [(String, String)]
getOptionPairs options = foldl getProperty []
    where
        getProperty :: [(String, String)] -> String -> [(String, String)]
        getProperty acc prop = case prop of 
            "portName"       -> (prop, portName options) : acc
            "userName"       -> (prop, userName options) : acc
            "portSpeed"      -> (prop, portSpeed2String $ portSpeed      options) : acc
            "portStopBits"   -> (prop, stopBit2String   $ portStopBits   options) : acc
            "portParityBits" -> (prop, parityBit2String $ portParityBits options) : acc
            "portWordBits"   -> (prop, show $ portWordBits options) : acc
            _                -> acc    

updateOptionsFromPairs :: [(String, String)] -> ChannelOptions -> ChannelOptions
updateOptionsFromPairs pairs options = foldl setProperty options pairs 
    where
        setProperty :: ChannelOptions -> (String, String) -> ChannelOptions
        setProperty opt (name, val) = case name of 
            "portName"       -> opt { portName = val }
            "userName"       -> opt { userName = val }
            "portSpeed"      -> opt { portSpeed = string2PortSpeed val }
            "portStopBits"   -> opt { portStopBits = string2StopBit val }
            "portParityBits" -> opt { portParityBits = string2ParityBit val }
            "portWordBits"   -> opt { portWordBits = read val }
            _                -> opt


instance Binary CommSpeed where
    put sp = case sp of 
        CS110    -> put (0 :: Word8)
        CS300    -> put (1 :: Word8)    
        CS600    -> put (2 :: Word8)    
        CS1200   -> put (3 :: Word8)    
        CS2400   -> put (4 :: Word8)   
        CS4800   -> put (5 :: Word8)   
        CS9600   -> put (6 :: Word8)   
        CS19200  -> put (7 :: Word8)  
        CS38400  -> put (8 :: Word8)  
        CS57600  -> put (9 :: Word8)  
        CS115200 -> put (10 :: Word8)

    get = do
        sp <- getWord8
        case sp of
            0  -> return CS110    
            1  -> return CS300    
            2  -> return CS600    
            3  -> return CS1200   
            4  -> return CS2400   
            5  -> return CS4800   
            6  -> return CS9600   
            7  -> return CS19200  
            8  -> return CS38400  
            9  -> return CS57600  
            10 -> return CS115200
            _  -> return CS2400  

instance Binary StopBits where
    put sb = case sb of 
        One -> put (0 :: Word8)
        Two -> put (1 :: Word8)

    get = do
        sb <- getWord8
        case sb of
            0 -> return One
            1 -> return Two
            _ -> return One

instance Binary Parity where
    put pr = case pr of 
        Even     -> put (0 :: Word8)
        Odd      -> put (1 :: Word8)
        NoParity -> put (2 :: Word8)

    get = do
        pr <- getWord8
        case pr of
            0 -> return Even 
            1 -> return Odd 
            2 -> return NoParity
            _ -> return NoParity

instance Binary FlowControl where
    put fc = case fc of
        Software      -> put (0 :: Word8)
        NoFlowControl -> put (1 :: Word8)

    get = do
        fc <- getWord8
        case fc of
            0 -> return Software
            1 -> return NoFlowControl
            _ -> return NoFlowControl

instance Binary SerialPortSettings where
    put (SerialPortSettings speed wordBits stopbits parityBits flowCnt t) = do
        put speed
        put wordBits
        put stopbits
        put parityBits
        put flowCnt
        put t

    get = do
        speed       <- get :: Get CommSpeed
        wordBits    <- get :: Get Word8
        stopbits    <- get :: Get StopBits
        parityBits  <- get :: Get Parity
        flowCnt     <- get :: Get FlowControl
        t           <- get :: Get Int
        return $ SerialPortSettings speed wordBits stopbits parityBits flowCnt t

instance Binary ChannelOptions where
    put o = do 
        put (portName o)
        put (userName o)
        put (portSpeed o)
        put (portStopBits o)
        put (portParityBits o)
        put (portWordBits o)

    get = do 
        uname <- get :: Get String 
        pname <- get :: Get String
        speed <- get :: Get CommSpeed 
        stbit <- get :: Get StopBits
        party <- get :: Get Parity 
        wordb <- get :: Get Word8 
        return ChannelOptions 
               {
                 portName = uname 
               , userName = pname 
               , portSpeed = speed 
               , portStopBits = stbit 
               , portParityBits = party
               , portWordBits = wordb 
               }