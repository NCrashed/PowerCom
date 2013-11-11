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
import Channel.Options 
import Utility (while, exitMsg)
import Event

import Control.Distributed.Process
import Control.Monad (forever)
import Control.Concurrent (yield)
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
                    newEvent <- tag (sendEvent events) msg
                    riseEvent newEvent
                    return ()

                 , connectCallback       = do 
                    riseEvent $ connectEvent events
                    return ()

                 , disconnectCallback    = do
                    riseEvent $ disconnectEvent events
                    return ()

                 , optionChangedCallback = \opt -> do 
                    newEvent <- tag (optionChangedEvent events) opt
                    riseEvent newEvent
                    return ()

                 }

printUserMessage :: GuiApi -> (ProcessId, String, String, String) -> Process Bool
printUserMessage api (_, _, user, msg) = do 
    liftIO $ (printMessage api) user msg 
    return True

printInfoMessage :: GuiApi -> (ProcessId, String, String) -> Process Bool
printInfoMessage api (_, _, msg) = do 
    liftIO $ (printInfo api) msg 
    return True

printErrorMessage :: GuiApi -> (ProcessId, String, String) -> Process Bool
printErrorMessage api (_, _, msg) = do 
    liftIO $ (printError api) msg 
    return True

setupOptionsHandler :: GuiApi -> (ProcessId, String, ChannelOptions) -> Process Bool
setupOptionsHandler api (_, _, options) = do 
    liftIO $ (setupOptions api) options 
    return True 

initApplicationLayer :: FilePath -> Maybe (String, String) -> ProcessId -> Process ()
initApplicationLayer gladeFile args rootId = do 
    spawnLocal $ do

      events <- liftIO initAppEvents
      (mainWindow, options, api) <- liftIO $ initGui gladeFile args $ callbacks events
        
      thisId <- getSelfPid
      channelId <- initChannelLayer thisId options
      
      spawnLocal $ do
          liftIO $ runGui mainWindow
          mapM_ ((flip send) (thisId, "exit")) [thisId, channelId, rootId]
      
      spawnLocal $ forever $ do 
        checkEvent (sendEvent events) (\s -> send channelId (thisId, "send", s)) ()
        checkEvent (connectEvent events) (\() -> send channelId (thisId, "connect")) ()
        checkEvent (disconnectEvent events) (\() -> send channelId (thisId, "disconnect")) ()
        checkEvent (optionChangedEvent events) (\opt -> send channelId (thisId, "options", opt)) ()
        liftIO $ yield

      while $ receiveWait [
              matchIf (\(_, com)       -> com == "exit")      exitMsg
            , matchIf (\(_, com, _, _) -> com == "message") $ printUserMessage api
            , matchIf (\(_, com, _)    -> com == "info")    $ printInfoMessage api
            , matchIf (\(_, com, _)    -> com == "error")   $ printErrorMessage api
            , matchIf (\(_, com, _)    -> com == "options") $ setupOptionsHandler api]

    return ()