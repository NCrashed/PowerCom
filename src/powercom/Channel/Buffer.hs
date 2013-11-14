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
module Channel.Buffer (
      MessageBuffer
    , initMessageBuffer
    , addMessagePart
    , collectMessage
    , clearBuffer
    , isMessageReady
    ) where

import Data.List 
import Data.IORef 

type MessageBuffer = IORef (String, [String], Int)

initMessageBuffer ::IO MessageBuffer
initMessageBuffer = newIORef ("", [], 0)

addMessagePart :: MessageBuffer -> String -> IO ()
addMessagePart buff s = do
	(name, raw, n) <- readIORef buff 
	case length raw < n of
		True -> writeIORef buff $ (name, raw ++ [s], n)
		False -> return ()

isMessageReady :: MessageBuffer -> IO Bool
isMessageReady buff = do
	(_, raw, n) <- readIORef buff 
	return $ length raw == n

collectMessage :: MessageBuffer -> IO (String, String)
collectMessage buff = do
	(name, raw, n) <- readIORef buff 
	return (name, concat raw)

clearBuffer :: MessageBuffer -> String -> Int -> IO ()
clearBuffer buff name n = writeIORef buff (name, [], n)
