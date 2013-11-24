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
module Utility (
      while
    , exitMsg
    ) where

import Control.Distributed.Process
import Control.Monad 

exitMsg :: (ProcessId, String) -> Process Bool
exitMsg (_, msg) = case msg of
  "exit" -> return False
  _      -> return True

while :: Process Bool -> Process ()
while f = do
    val <- f 
    when val $ while f