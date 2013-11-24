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
module Application.Gui (
      initGui
    , runGui
    ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
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

createAboutDialog :: IO ()
createAboutDialog = do
    dialog <- aboutDialogNew 
    set dialog 
        [ aboutDialogName      := "About application"
        , aboutDialogVersion   := "1.1"
        , aboutDialogCopyright := "Copyright 2013 Гуща Антон, Нардид Анатолий, Оганян Левон"
        , aboutDialogComments  := "Application for messaging within serial port."
        , aboutDialogLicense   := Just license]
    dialog `on` response $ \id -> widgetHideAll dialog 
    widgetShowAll dialog

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

saveAction :: IORef (Maybe String) -> GuiApi -> IO ()
saveAction lastSaveRef api = do 
    lastSave <- readIORef lastSaveRef 
    case lastSave of 
        Nothing -> saveAsAction lastSaveRef api   
        Just fileName -> saveChatToFile fileName api 

saveAsAction :: IORef (Maybe String) -> GuiApi -> IO ()
saveAsAction lastSaveRef api = do 
    dialog <- newSaveDialog
    withFileChooserDo dialog $ \s -> do 
        writeIORef lastSaveRef $ Just s 
        saveChatToFile s api
    widgetDestroy dialog 

openAction :: IORef (Maybe String) -> TextView -> IO ()
openAction lastSaveRef chatView = do 
    dialog <- newOpenDialog 
    withFileChooserDo dialog $ \s -> do
        writeIORef lastSaveRef $ Just s 
        loadChatFromFile chatView s
    widgetDestroy dialog 

withFileChooserDo :: FileChooserDialog -> (String -> IO ()) -> IO () 
withFileChooserDo dialog action = do 
    response <- dialogRun dialog
    case response of 
        ResponseOk -> do 
            newFileNameOpt <- fileChooserGetFilename dialog
            forM_ newFileNameOpt action
        _ -> return ()

newSaveDialog :: IO FileChooserDialog
newSaveDialog = fileChooserDialogNew Nothing Nothing FileChooserActionSave [("Save", ResponseOk), ("Cancel", ResponseCancel)]

newOpenDialog :: IO FileChooserDialog 
newOpenDialog = fileChooserDialogNew Nothing Nothing FileChooserActionOpen [("Open", ResponseOk), ("Cancel", ResponseCancel)]

saveChatToFile :: FilePath -> GuiApi -> IO ()
saveChatToFile filename api = writeFile filename =<< getChatText api 

loadChatFromFile :: TextView -> FilePath -> IO ()
loadChatFromFile textView fileName = textViewSetText textView =<< readFile fileName

initGui :: FilePath -> Maybe (String, String) -> GuiCallbacks -> IO (Window, ChannelOptions, GuiApi)
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
    (userList, addUser', removeUser') <- initUserList builder (userName options)

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
            
runGui :: Window -> IO ()
runGui mainWindow = do 
    -- Yielding GTK thread
    timeoutAddFull (yield >> return True) priorityDefaultIdle 1
    widgetShowAll mainWindow
    mainGUI