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
module Channel.Processing (
      receiveFrameHandler
    ) where

import Channel.Buffer
import Channel.CyclicCode
import Channel.Connection
import Channel.Frame 
import Channel.Options
import Channel.Miscs
import Channel.Sending

import Control.Distributed.Process 
import Control.Applicative

import qualified Data.ByteString as BS
import Data.Functor
import Data.Word 
import Data.List

receiveFrameHandler :: ProcessId -> ProcessId -> MessageBuffer -> Connection -> InnerChannelOptions 
    -> (ProcessId, String, BS.ByteString) -> Process Bool 
receiveFrameHandler physLayerId transitId messageBuffer conn optionsRef (_, com, byteFrame) = do 
    thisId <- getSelfPid
    options <- getOptions optionsRef
    case decodeFrame byteFrame of 
        Just frame -> do
            case frame of 
                AckFrame -> return ()
                RetFrame -> return ()
                _ -> do 
                    if com /= "frame-acked" -- prevent double sending for not fully processed frames
                        then sendFrame physLayerId AckFrame 
                        else return ()
                    processFrame frame options
        _ -> do 
            informSenderError transitId $ "Failed to recieve frame!"
            if com /= "frame-acked"
                then sendFrame physLayerId RetFrame
                else return ()
    return True
    where 
        getRemoteNames :: [(String, String)] -> Maybe (String, String)
        getRemoteNames props = (,) <$> getValue props "remoteNameNew" <*> getValue props "remoteNameOld"

        getValue :: [(String, String)] -> String -> Maybe String 
        getValue props key = snd <$> find (\(k, _) -> k == key) props

        word2int :: Word32 -> Int 
        word2int = fromInteger . toInteger

        processFrame (InformationFrame name n) _ = clearBuffer messageBuffer name (word2int n)
        
        processFrame (DataPartFrame s) _ = do 
            addMessagePart messageBuffer s 
            filled <- isMessageReady messageBuffer
            if filled then do  
                (name, msg) <- collectMessage messageBuffer
                sendMessage transitId name msg 
            else return ()

        processFrame (OptionFrame props) options = do 
            case getRemoteNames props of 
                Just (newName, oldName) -> do 
                    informSender transitId $ "Remote name changing, " ++ oldName ++ " to " ++ newName
                    sendDisconnectUser transitId oldName
                    sendConnectUser transitId newName
                Nothing -> return ()
            let newOptions = updateOptionsFromPairs props options
            setOptions optionsRef newOptions
            informSender transitId "Recieved new options from other side, changing..."
            sendReopenPort physLayerId newOptions
            sendUpdateOptions transitId newOptions


        processFrame (LinkFrame name) options = do 
            sendConnectUser transitId name
            setRemoteUsername conn name
            ifNotConnected conn $ do 
                informSender transitId "Remote host connected!"
                sendFrameWithAck physLayerId $ LinkFrame $ userName options
                openConnection conn  

        processFrame (UnlinkFrame name) options = do 
            sendDisconnectUser transitId name
            ifConnected conn $ do 
                informSender transitId "Remote host disconnected!"
                sendFrameWithAck physLayerId $ UnlinkFrame $ userName options
                closeConnection conn 

        processFrame (RetFrame) _ = informSender transitId "Main handler got ret frame, it is bad!"
        processFrame (AckFrame) _ = informSender transitId "Main handler got ack frame, it is bad!"
        processFrame (Upcheck) _ = return () --informSender transitId "Upcheck got"