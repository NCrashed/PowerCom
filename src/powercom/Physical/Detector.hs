-----------------------------------------------------------------------------
-- |
-- Module      :  Physical.Detector
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Facilities to search serial ports in Windows and GNU/Linux. 
--
-----------------------------------------------------------------------------
module Physical.Detector (
    getSerialPorts
    ) where

import qualified System.Hardware.Serialport as Serial (openSerial, SerialPort)
import           System.Info                          (os)
import           System.Directory                     (getDirectoryContents)
import           Control.Exception                    (SomeException, try)
import           Control.Monad                        (filterM)

import           Physical.Options                     (channel2physicalOptions)
import           Channel.Options                      (defaultOptions) 

-- | Returns list of available serial ports in the system.
-- Scans dev folder in GNU/Linux. As devices in linux are files, can find not only serial port.
-- Tries serial port name patterns like "COM4" to discover serial ports in Windows.
getSerialPorts :: IO [FilePath]
getSerialPorts = case os of 
    "linux" -> do 
        fileList <- getDirectoryContents "/dev"
        filterM isSerialPort $ map ("/dev/" ++) fileList
    "windows" ->
        filterM isSerialPort $ take 20 $ map (("COM"++) . show) [0 :: Int ..]
    _ -> return []

-- | Tries to open specified serial port name. If any error occurs, then
-- it is not serial port.
isSerialPort :: FilePath -> IO Bool
isSerialPort fileName = do 
    res <- try (Serial.openSerial fileName $ channel2physicalOptions defaultOptions)
        :: IO (Either SomeException Serial.SerialPort)
    return $ either (const False) (const True) res