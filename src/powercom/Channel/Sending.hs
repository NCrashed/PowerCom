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
import Data.Maybe

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
    sendFrame targetId frame
    expectAck sendTries
    where 
        sendTries = 3    
        timeout = 1000000 -- 1 s

        expectAck :: Int -> Process Bool
        expectAck 0 = return False
        expectAck nTries = do 
            res <- receiveTimeout timeout [ -- todo here
                matchIf innerAckRetMatcher innerAckRetHandler
              , matchIf (\(_, com, _) -> com == "frame") otherFrameHandler]
            case res of 
                Just False -> expectAck nTries
                Just True -> return True
                Nothing -> return False
            where
                innerAckRetMatcher :: (ProcessId, String, BS.ByteString) -> Bool 
                innerAckRetMatcher (_, com, bs) = if com /= "frame" then False
                    else case decodeFrame bs of
                        Just AckFrame -> True
                        Just RetFrame -> True
                        _ -> False

                innerAckRetHandler :: (ProcessId, String, BS.ByteString) -> Process Bool
                innerAckRetHandler (_, _, bs) = case fromJust $ decodeFrame bs of
                        AckFrame -> return True
                        RetFrame -> do 
                           sendFrame targetId frame
                           expectAck $ nTries-1 

                otherFrameHandler :: (ProcessId, String, BS.ByteString) -> Process Bool
                otherFrameHandler (_, _, bs) = do
                    thisId <- getSelfPid
                    case decodeFrame bs of 
                        Just frame -> do 
                            sendFrame targetId AckFrame
                            send thisId (thisId, "frame-acked", codeFrame $ frame)
                            return False
                        _ -> do
                            sendFrame targetId RetFrame
                            return False
