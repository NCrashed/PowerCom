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
module Application.Layer (
    initApplicationLayer
    ) where

import Application.Gui 
import Application.Types 
import Channel.Layer
import Utility (while, exitMsg)
import Event

import Control.Distributed.Process
import Control.Monad (forever)
import Control.Concurrent
import Graphics.UI.Gtk

data AppEvents = 
    AppEvents
    {
      sendEvent          :: Event String
    , connectEvent       :: Event ()
    , disconnectEvent    :: Event ()
    , optionChangedEvent :: Event ChannelOptions
    }

initAppEvents :: IO AppEvents
initAppEvents = do 
    sendEvent'             <- initEvent ""
    connectEvent'          <- initEvent ()
    disconnectEvent'       <- initEvent ()
    optionChangedEvent'    <- initEvent defaultOptions

    return 
        AppEvents
        {
          sendEvent          = sendEvent'
        , connectEvent       = connectEvent'
        , disconnectEvent    = disconnectEvent'
        , optionChangedEvent = optionChangedEvent'
        }

callbacks :: AppEvents -> GuiCallbacks
callbacks events = 
    GuiCallbacks {
                   sendMessageCallback   = \msg -> do
                    putStrLn $ "Tried to send message: " ++ msg
                    newEvent <- tag (sendEvent events) msg
                    riseEvent newEvent
                    return ()

                 , connectCallback       = putStrLn "Connection"
                 , disconnectCallback    = putStrLn "Disconnection"
                 , optionChangedCallback = \opt -> putStrLn $  "Options changed!"
                 }

initApplicationLayer :: FilePath -> ProcessId -> Process ()
initApplicationLayer gladeFile rootId = do 
    spawnLocal $ do
      thisId <- getSelfPid
      channelId <- initChannelLayer thisId
      events <- liftIO initAppEvents

      spawnLocal $ do
          liftIO $ do
            timeoutAddFull (yield >> return True) priorityDefaultIdle 50
            runGui gladeFile $ callbacks events
          mapM_ ((flip send) (thisId, "exit")) [thisId, channelId, rootId]
      
      spawnLocal $ forever $ do 
        checkEvent (sendEvent events) (\s -> liftIO $ putStrLn $ "Event! " ++ s) ()
        liftIO $ yield

      while $ receiveWait [match exitMsg]
    return ()