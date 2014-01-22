-----------------------------------------------------------------------------
-- |
-- Module      :  Application.Layer
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module defines application layer of the application. It is the top layer
-- in application structure. The layer uses following messages to communicate
-- with channel layer:
-- 
-- * incoming \"exit\" - is sent from gui thread (same level), initializes recursive 
-- shutdown protocol.
--
-- * incoming \"message\" - is sent from channel layer. Indicates that other user 
-- sent a message. Holds username and contents.
--
-- * incoming \"info\" - is sent from channel layer and informing about internal events.
--
-- * incoming \"error\" - is sent from channel layer and informing about important errors.
--
-- * incoming \"options\" - is sent from channel layer. Holds new options to set in gui.
--
-- * incoming \"connect\" - is sent from channel layer. Indicates about connecting of a new user,
-- holds username.
--
-- * incoming \"disconnect\" - is sent from channel layer. Indicates about disconnecting of a user,
-- holds username.
--
-- * outgoing \"exit\" -- is sent to channel layer when terminating protocol is triggered.
--
-- * outgoing \"send\" -- is sent to channel layer when the user finishes to chat a message. Holds only
-- message body.
--
-- * outgoing \"connect\" -- is sent to channel layer when the user presses connecting button.
--
-- * outgoing \"disconnect\" -- is sent to channel layer when the user presses disconnecting button.
--
-- * outgoing \"options\" -- is sent to channel layer when the user finishes changing serial port options 
-- (or user name changes).
-----------------------------------------------------------------------------
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

-- | Application events that are listened. The events are connecting
-- gui callbacks and process monad.
data AppEvents = 
    AppEvents
    {
      sendEvent          :: Event String
    , connectEvent       :: Event ()
    , disconnectEvent    :: Event ()
    , optionChangedEvent :: Event (ChannelOptions, ChannelOptions)
    }

-- | Application event initialization.
initAppEvents :: IO AppEvents
initAppEvents = do 
    sendEvent'             <- initEvent ""
    connectEvent'          <- initEvent ()
    disconnectEvent'       <- initEvent ()
    optionChangedEvent'    <- initEvent (defaultOptions, defaultOptions)

    return 
        AppEvents
        {
          sendEvent          = sendEvent'
        , connectEvent       = connectEvent'
        , disconnectEvent    = disconnectEvent'
        , optionChangedEvent = optionChangedEvent'
        }

-- | Transforms application event to callbacks that rises the events.
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

                 , optionChangedCallback = \opt oldopt -> do 
                    newEvent <- tag (optionChangedEvent events) (opt, oldopt)
                    riseEvent newEvent
                    return ()

                 }

-- | Handler for incoming user message.
printUserMessage :: GuiApi -> (ProcessId, String, String, String) -> Process Bool
printUserMessage api (_, _, user, msg) = do 
    liftIO $ printMessage api user msg 
    return True

-- | Handler for incoming system info message.
printInfoMessage :: GuiApi -> (ProcessId, String, String) -> Process Bool
printInfoMessage api (_, _, msg) = do 
    liftIO $ printInfo api msg 
    return True

-- | Handler for incoming system error message.
printErrorMessage :: GuiApi -> (ProcessId, String, String) -> Process Bool
printErrorMessage api (_, _, msg) = do 
    liftIO $ printError api msg 
    return True

-- | Handler for incoming serial port options changes.
setupOptionsHandler :: GuiApi -> (ProcessId, String, ChannelOptions) -> Process Bool
setupOptionsHandler api (_, _, options) = do 
    liftIO $ setupOptions api options 
    return True 

-- | Handler for incoming remote connecting event. 
userConnectHandler :: GuiApi -> (ProcessId, String, String) -> Process Bool
userConnectHandler api (_, _, name) = do 
    liftIO $ addUser api name
    return True 

-- | Handler for incoming remote disconnecting event.
userDisconnectHandler :: GuiApi -> (ProcessId, String, String) -> Process Bool
userDisconnectHandler api (_, _, name) = do 
    liftIO $ removeUser api name
    return True 

-- | Initializes application layer.    
initApplicationLayer :: FilePath -- ^ Glade file name to load gui from 
  -> Maybe (String, String) -- ^ Optional arguments: serial port name and defualt user name. 
  -> ProcessId -- ^ Parent layer id, for application layer root is is Main thread. 
  -> Process ()
initApplicationLayer gladeFile args rootId = do 
    spawnLocal $ do

      events <- liftIO initAppEvents
      (mainWindow, options, api) <- liftIO $ initGui gladeFile args $ callbacks events
        
      thisId <- getSelfPid
      channelId <- initChannelLayer thisId options
      
      spawnLocal $ do
          liftIO $ runGui mainWindow
          mapM_ (`send` (thisId, "exit")) [thisId, channelId, rootId]
      
      spawnLocal $ forever $ do 
        checkEvent (sendEvent events) (\s -> send channelId (thisId, "send", s)) ()
        checkEvent (connectEvent events) (\() -> send channelId (thisId, "connect")) ()
        checkEvent (disconnectEvent events) (\() -> send channelId (thisId, "disconnect")) ()
        checkEvent (optionChangedEvent events) (\(opt, oldopt) -> do
          liftIO $ removeUser api $ userName oldopt
          liftIO $ addUser api $ userName opt
          send channelId (thisId, "options", opt, oldopt)) ()
        liftIO yield

      while $ receiveWait [
              matchIf (\(_, com)       -> com == "exit")         exitMsg
            , matchIf (\(_, com, _, _) -> com == "message")    $ printUserMessage       api
            , matchIf (\(_, com, _)    -> com == "info")       $ printInfoMessage       api
            , matchIf (\(_, com, _)    -> com == "error")      $ printErrorMessage      api
            , matchIf (\(_, com, _)    -> com == "options")    $ setupOptionsHandler    api
            , matchIf (\(_, com, _)    -> com == "connect")    $ userConnectHandler     api
            , matchIf (\(_, com, _)    -> com == "disconnect") $ userDisconnectHandler  api]

    return ()