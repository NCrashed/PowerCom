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
module Channel.ConnectionChecker (
      spawnConnectionChecker
    ) where

import Channel.Connection 
import Channel.Frame

import Control.Distributed.Process
import Control.Monad (forever)
import Control.Concurrent (threadDelay)

checkDelay :: Int
checkDelay = 5000000

spawnConnectionChecker :: ProcessId -> ProcessId -> Connection -> Process ProcessId
spawnConnectionChecker chanLayerId appLayerId conn = spawnLocal $ forever $ do
    ifConnected conn $ send chanLayerId (appLayerId, "transit-frame", toByteString Upcheck)
    liftIO $ threadDelay checkDelay