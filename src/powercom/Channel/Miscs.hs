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
module Channel.Miscs (
      informSender
    , informSenderError
    , sendMessage
    , sendConnectUser
    , sendDisconnectUser
    , sendReopenPort
    , sendUpdateOptions
    ) where

import Channel.Options
import Control.Distributed.Process 
import Data.Typeable

informSender :: ProcessId -> String -> Process ()
informSender = sendTyped1 "info"

informSenderError :: ProcessId -> String -> Process ()
informSenderError = sendTyped1 "error"

sendMessage :: ProcessId -> String -> String -> Process ()
sendMessage = sendTyped2 "message"

sendConnectUser :: ProcessId -> String -> Process ()
sendConnectUser = sendTyped1 "connect"

sendDisconnectUser :: ProcessId -> String -> Process ()
sendDisconnectUser = sendTyped1 "disconnect"

sendReopenPort :: ProcessId -> ChannelOptions -> Process ()
sendReopenPort = sendTyped1 "reopen"

sendUpdateOptions :: ProcessId -> ChannelOptions -> Process ()
sendUpdateOptions = sendTyped1 "options"

sendTyped1 :: (Binary a, Typeable a) => String -> ProcessId -> a -> Process ()
sendTyped1 msgType targetId msg = do 
    thisId <- getSelfPid
    send targetId (thisId, msgType, msg)

sendTyped2 :: (Binary a, Typeable a, Binary b, Typeable b) => 
    String -> ProcessId -> a -> b -> Process ()
sendTyped2 msgType targetId msg1 msg2 = do 
    thisId <- getSelfPid
    send targetId (thisId, msgType, msg1, msg2)