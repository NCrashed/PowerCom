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
module Physical.Detector (
    getSerialPorts
    ) where

import qualified System.Hardware.Serialport     as Serial
import System.Info (os)
import System.Directory (getDirectoryContents)
import Control.Exception (SomeException, try)
import Control.Monad (filterM)

import Physical.Options
import Channel.Options 

getSerialPorts :: IO [FilePath]
getSerialPorts = case os of 
    "linux" -> do 
        fileList <- getDirectoryContents "/dev"
        filterM isSerialPort $ map (\s -> "/dev/" ++ s) fileList
    _ -> return [] -- todo serial port detection for other systems

isSerialPort :: FilePath -> IO Bool
isSerialPort fileName = do 
    res <- try (Serial.openSerial fileName $ channel2physicalOptions defaultOptions)
        :: IO (Either SomeException Serial.SerialPort)
    case res of 
        Right _ -> return True
        Left _  -> return False