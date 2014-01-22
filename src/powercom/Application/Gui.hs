-----------------------------------------------------------------------------
-- |
-- Module      :  Application.Gui
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module defines all GUI procedures. There are defined all save/load/about
-- dialogs and main window logic. Also this module collects gui functions for
-- interconnecting gui and ppplication layer in one structure "GuiApi". 
-- Application layer passes "GuiCallbacks" to bind callbacks to corresponding
-- events.
-----------------------------------------------------------------------------
module Application.Gui (
      initGui
    , runGui
    ) where

import Graphics.UI.Gtk
import Application.OptionDialog
import Application.ChatView
import Application.UserList
import Application.Types
import Channel.Options

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Data.Functor
import Data.IORef
import Data.Foldable 

-- | Creates about dialog with information about authors, license and version.
createAboutDialog :: IO ()
createAboutDialog = do
    dialog <- aboutDialogNew 
    set dialog 
        [ aboutDialogName      := "About application"
        , aboutDialogVersion   := "1.1"
        , aboutDialogCopyright := "Copyright 2013 Гуща Антон, Нардид Анатолий, Оганян Левон"
        , aboutDialogComments  := "Application for messaging within serial port."
        , aboutDialogLicense   := Just license]
    dialog `on` response $ const $ widgetHideAll dialog 
    widgetShowAll dialog

-- | Function handling license for about dialog
license :: String
license = "PowerCom is free software: you can redistribute it and/or modify\n\
\it under the terms of the GNU General Public License as published by\n\
\the Free Software Foundation, either version 3 of the License, or\n\
\(at your option) any later version.\n\
\\n\
\PowerCom is distributed in the hope that it will be useful,\n\
\but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
\MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
\GNU General Public License for more details.\n\
\\n\
\You should have received a copy of the GNU General Public License\n\
\along with PowerCom.  If not, see <http://www.gnu.org/licenses/>."

-- | Callback on \"save\" button. If user already
-- have selected file name, the save as dialog
-- will not be shown. 
saveAction :: IORef (Maybe String) -- ^ Information about last dialogs call. 
  -> GuiApi -- ^ Api to access chat content.
  -> IO ()
saveAction lastSaveRef api = do 
    lastSave <- readIORef lastSaveRef 
    case lastSave of 
        Nothing -> saveAsAction lastSaveRef api   
        Just fileName -> saveChatToFile fileName api 

-- | Callaback on \"save as\" button. This function
-- caches file name in it's first parameter. 
saveAsAction :: IORef (Maybe String) -- ^ Here file name is written after dialog is closed. 
  -> GuiApi -- ^ Api to access chat content. 
  -> IO ()
saveAsAction lastSaveRef api = do 
    dialog <- newSaveDialog
    withFileChooserDo dialog $ \s -> do 
        writeIORef lastSaveRef $ Just s 
        saveChatToFile s api
    widgetDestroy dialog 

-- | Callback on \"open\" button. If first parameter
-- carries file name (cached by other dialogs), then
-- the file is opened without showing open dialog.
openAction :: IORef (Maybe String) -- ^ Information about last dialogs call. 
  -> TextView -- ^ Chat to fill with contents 
  -> IO ()
openAction lastSaveRef chatView = do 
    dialog <- newOpenDialog 
    withFileChooserDo dialog $ \s -> do
        writeIORef lastSaveRef $ Just s 
        loadChatFromFile chatView s
    widgetDestroy dialog 

-- | Wrapper for GTK filechooser.
withFileChooserDo :: FileChooserDialog -- ^ Operating filechooser 
  -> (String -> IO ()) -- ^ Action to perform after end of the dialog. 
                       -- Action argument is file name that was choosed. 
  -> IO () 
withFileChooserDo dialog action = do 
    dialResponse <- dialogRun dialog
    case dialResponse of 
        ResponseOk -> do 
            newFileNameOpt <- fileChooserGetFilename dialog
            forM_ newFileNameOpt action
        _ -> return ()

-- | Initialises \"save as\" dialog.
newSaveDialog :: IO FileChooserDialog
newSaveDialog = fileChooserDialogNew Nothing Nothing FileChooserActionSave [("Save", ResponseOk), ("Cancel", ResponseCancel)]

-- | Initialises \"open\" dialog.
newOpenDialog :: IO FileChooserDialog 
newOpenDialog = fileChooserDialogNew Nothing Nothing FileChooserActionOpen [("Open", ResponseOk), ("Cancel", ResponseCancel)]

-- | Saves chat contents to the file.
saveChatToFile :: FilePath -> GuiApi -> IO ()
saveChatToFile filename api = writeFile filename =<< getChatText api 

-- | Writes file contents to the chat.
loadChatFromFile :: TextView -> FilePath -> IO ()
loadChatFromFile textView fileName = textViewSetText textView =<< readFile fileName

-- | There gui is ctreated. You need to call \"runGui\" function 
-- to actually start gui operating.
initGui :: FilePath -- ^ Glad file name to load gui from 
  -> Maybe (String, String) -- ^ Optional arguments: serial port name and user name 
  -> GuiCallbacks -- ^ Callbacks to bind 
  -> IO (Window, ChannelOptions, GuiApi) -- ^ Returns main window, initial options and api to communicate with the gui
initGui gladeFile initArgs callbacks = do 
    initGUI
    builder <- builderNew
    builderAddFromFile builder gladeFile
    
    -- Binding main window
    mainWindow <- builderGetObject builder castToWindow "MainWindow"
    onDestroy mainWindow mainQuit

    -- Exit item
    exitItem <- builderGetObject builder castToMenuItem "ExitItem"
    exitItem `on` menuItemActivate $ mainQuit

    -- Show about dialog
    aboutItem <- builderGetObject builder castToMenuItem "AboutItem"
    aboutItem `on` menuItemActivate $ createAboutDialog

    -- OptionDialog 
    (optionsRef, setupOptions') <- setupOptionDialog builder callbacks initArgs
    options <- readIORef optionsRef

    -- TextView for messages
    chatTextView <- initChatTextView builder 

    -- Send buffer
    sendEntry <- builderGetObject builder castToEntry "SendEntry"

    let sendBtnAction = do 
            msg <- entryGetText sendEntry
            username <- userName <$> readIORef optionsRef
            putUserMessage chatTextView username msg
            sendMessageCallback callbacks msg 
            entrySetText sendEntry ""

    sendEntry `on` keyPressEvent $ tryEvent $ do 
        "Return" <- eventKeyName
        liftIO sendBtnAction

    -- Send Button
    sendButton <- builderGetObject builder castToButton "SendButton"
    sendButton `on` buttonActivated $ sendBtnAction
        
    -- Connect button
    connectButton <- builderGetObject builder castToToolButton "ConnectButton"
    onToolButtonClicked connectButton $ connectCallback callbacks
        
    -- Disconnect button
    disconnectButton <- builderGetObject builder castToToolButton "DisconnectButton"
    onToolButtonClicked disconnectButton $ disconnectCallback callbacks

    -- User list
    (_, addUser', removeUser') <- initUserList builder (userName options)

    let api = GuiApi {
              printMessage = putUserMessage     chatTextView
            , printInfo    = putInfoMessage     chatTextView
            , printError   = putErrorMessage    chatTextView
            , setupOptions = setupOptions'
            , getChatText  = textViewGetAllText chatTextView
            , addUser      = addUser'
            , removeUser   = removeUser'
            }

    -- save dialog
    fileNameRef <- newIORef (Nothing :: Maybe String)
    saveItem <- builderGetObject builder castToMenuItem "SaveItem"
    saveItem `on` menuItemActivate $ saveAction fileNameRef api 

    -- save as dialog
    saveAsItem <- builderGetObject builder castToMenuItem "SaveAsItem"
    saveAsItem `on` menuItemActivate $ saveAsAction fileNameRef api 

    -- open dialog
    openItem <- builderGetObject builder castToMenuItem "OpenItem"
    openItem `on` menuItemActivate $ openAction fileNameRef chatTextView

    return (mainWindow, options, api)
            
-- | Starts infinite loop with gui event handling and widget drawing. Blocks calling 
-- thread until user exit. Special yielding is produced to not block other threads.
runGui :: Window -> IO ()
runGui mainWindow = do 
    -- Yielding GTK thread
    timeoutAddFull (yield >> return True) priorityDefaultIdle 1
    widgetShowAll mainWindow
    mainGUI