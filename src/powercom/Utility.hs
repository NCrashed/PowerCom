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
module Utility(
    Binary(..)
    ) where

import System.Hardware.Serialport

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8

import Data.Binary (Binary(..))
import Data.Binary.Get
import Data.Binary.Put

import Data.Word
import Data.Typeable

deriving instance Typeable CommSpeed
deriving instance Typeable StopBits
deriving instance Typeable Parity 
deriving instance Typeable FlowControl
deriving instance Typeable SerialPortSettings

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

instance Binary StopBits where
    put sb = case sb of 
        One -> put (0 :: Word8)
        Two -> put (1 :: Word8)

    get = do
        sb <- getWord8
        case sb of
            0 -> return One
            1 -> return Two

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

instance Binary FlowControl where
    put fc = case fc of
        Software      -> put (0 :: Word8)
        NoFlowControl -> put (1 :: Word8)

    get = do
        fc <- getWord8
        case fc of
            0 -> return Software
            1 -> return NoFlowControl

instance Binary SerialPortSettings where
    put (SerialPortSettings commSpeed bitsPerWord stopb parity flowControl timeout) = do
        put commSpeed
        put bitsPerWord
        put stopb
        put parity
        put flowControl
        put timeout

    get = do
        commSpeed   <- get :: Get CommSpeed
        bitsPerWord <- get :: Get Word8
        stopb       <- get :: Get StopBits
        parity      <- get :: Get Parity
        flowControl <- get :: Get FlowControl
        timeout     <- get :: Get Int
        return $ SerialPortSettings commSpeed bitsPerWord stopb parity flowControl timeout