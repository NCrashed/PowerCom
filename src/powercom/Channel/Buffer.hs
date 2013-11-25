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

import Data.IORef 
import Control.Distributed.Process
import Control.Monad 

type MessageBuffer = IORef (String, [String], Int)

initMessageBuffer :: Process MessageBuffer
initMessageBuffer = liftIO $ newIORef ("", [], 0)

addMessagePart :: MessageBuffer -> String -> Process ()
addMessagePart buff s = liftIO $ do
    (name, raw, n) <- readIORef buff 
    when (length raw < n) $ writeIORef buff (name, raw ++ [s], n)

isMessageReady :: MessageBuffer -> Process Bool
isMessageReady buff = liftIO $ do
    (_, raw, n) <- readIORef buff 
    return $ length raw == n

collectMessage :: MessageBuffer -> Process (String, String)
collectMessage buff = liftIO $ do
    (name, raw, _) <- readIORef buff 
    return (name, concat raw)

clearBuffer :: MessageBuffer -> String -> Int -> Process ()
clearBuffer buff name n = liftIO $ writeIORef buff (name, [], n)
