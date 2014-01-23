-----------------------------------------------------------------------------
-- |
-- Module      :  Application.Types
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module defines data structure for communication between application layer
-- and GTK gui. Layer passes callbacks to the gui to form event-based system,
-- callbacks are binded to corresponding GTK events. Also gui returns structure
-- with API functions to backward communication: putting messages in the chat box,
-- displaying connected users, e.t.c.
-----------------------------------------------------------------------------
module Application.Types (
      GuiCallbacks(..)
    , GuiApi(..)  
    ) where 

import Channel.Options

-- | Callbacks needed to be passed into GTK gui for operating. These functions
-- are called when corresponding GTK event occurs. Application layer passes
-- functions that forms event pipe between IO and Process monads.
data GuiCallbacks = GuiCallbacks {
      sendMessageCallback   :: String -> IO () -- ^ Called when user finishes typing message for remote companion
    , connectCallback       :: IO () -- ^ Called when user wants to connect to the remote program instance
    , disconnectCallback    :: IO () -- ^ Called when user wants to disconnect from the remote progam instance
    , optionChangedCallback :: ChannelOptions -> ChannelOptions -> IO () -- ^ Called when user finishes changing options in special dialog.
                                                                         -- Old options among new ones are passed to the callback. 
}

-- | Unlike callbacks API functions provide interface to affect the GUI from
-- layer side. This structure is filled and returned by "initGui" function.
data GuiApi = GuiApi {
      printMessage :: String -> String -> IO () -- ^ Puts message into chat box. First string is user name, second one is the message.
    , printInfo    :: String -> IO () -- ^ Puts system message into the chat box.
    , printError   :: String -> IO () -- ^ Puts system error message (more aggressive color) into the chat box.
    , setupOptions :: ChannelOptions -> IO () -- ^ Updates contents of option changing dialog.
    , getChatText  :: IO String -- ^ Retrieves all contents of the chat box.
    , addUser      :: String -> IO () -- ^ Adding user to special user list, first parameter is user name. Names are considered to be unique.
    , removeUser   :: String -> IO () -- ^ Removing user from special user list, first parameter is user name. Names are considered to be unique.
}