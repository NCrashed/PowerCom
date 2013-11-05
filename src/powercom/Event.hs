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
module Event (
      Event
    , initEvent
    , tag
    , getTag
    , riseEvent
    , checkEvent
    ) where

import Data.IORef
import Control.Distributed.Process

data Event a = Event (IORef Bool) (IORef a)

initEvent :: a -> IO (Event a)
initEvent val = do
    flagRef <- newIORef False
    valRef <- newIORef val
    return $ Event flagRef valRef

getTag :: Event a -> IO a 
getTag (Event _ val) = readIORef val

tag :: Event a -> a -> IO (Event a)
tag (Event flag valRef) val = do
    writeIORef valRef val 
    return $! Event flag valRef

riseEvent :: Event a -> IO(Event a)
riseEvent (Event flagRef val) = do 
    writeIORef flagRef True
    return $! Event flagRef val

checkEvent :: Event a -> (a -> Process b) -> b -> Process b 
checkEvent (Event flagRef valRef) f failVal = do
    flag <- liftIO $ readIORef flagRef
    if flag then do
        val <- liftIO $ do 
            writeIORef flagRef False
            readIORef valRef
        f val 
    else return failVal
