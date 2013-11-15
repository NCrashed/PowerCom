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
module Channel.Sending (
      sendFrameWithAck
    , sendFrame
    , codeFrame
    , decodeFrame
    ) where

import Channel.Frame 
import Channel.CyclicCode

import Control.Distributed.Process 
import Control.Monad

import qualified Data.ByteString as BS 

codeFrame :: Frame -> BS.ByteString 
codeFrame = codeCyclic . toByteString

decodeFrame :: BS.ByteString -> Maybe Frame 
decodeFrame bs = case decode bs of 
    Just (Right frame, _) -> Just frame
    _ -> Nothing    
    where decode = (liftM fromByteString) . decodeCyclic

sendFrame :: ProcessId -> Frame -> Process ()
sendFrame targetId frame = do
    thisId <- getSelfPid
    send targetId (thisId, "send", codeFrame frame)

sendFrameWithAck :: ProcessId -> Frame -> Process Bool
sendFrameWithAck targetId frame = do 
    thisId <- getSelfPid
    sendFrame targetId frame
    expectAck sendTries
    where 
        sendTries = 3    
        timeout = 1000000 -- 1 s

        expectAck :: Int -> Process Bool
        expectAck 0 = return False
        expectAck nTries = do 
            thisId <- getSelfPid
            expectRes <- expectTimeout timeout :: Process (Maybe (ProcessId, String, BS.ByteString))
            case expectRes of 
                Nothing -> return False
                Just (_, _, bs) -> case decodeFrame bs of 
                    Nothing -> return False
                    Just frame -> case frame of 
                        AckFrame -> return True
                        RetFrame -> do 
                           sendFrame targetId frame
                           expectAck $ nTries-1 
                        _ -> return False