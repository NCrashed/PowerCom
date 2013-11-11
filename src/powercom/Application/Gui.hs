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
import Application.Types
import Channel.Options

import Control.Concurrent
import Data.Functor
import Data.IORef

putUserMessage :: TextView -> String -> String -> IO ()
putUserMessage textView username msg = do 
    buffer <- textViewGetBuffer textView
    bufferAddStringWithTag buffer ("[" ++ username ++ "]: ") "UsernameColor"
    bufferAddStringWithTag buffer (msg++"\n") "MessageColor"

    textViewScrollToEnd textView

putInfoMessage :: TextView -> String -> IO ()
putInfoMessage textView msg = do 
    buffer <- textViewGetBuffer textView
    bufferAddStringWithTag buffer (msg++"\n") "InfoColor"

    textViewScrollToEnd textView

putErrorMessage :: TextView -> String -> IO ()
putErrorMessage textView msg = do 
    buffer <- textViewGetBuffer textView
    bufferAddStringWithTag buffer (msg++"\n") "ErrorColor"

    textViewScrollToEnd textView

textViewScrollToEnd :: TextView -> IO ()
textViewScrollToEnd textView = do 
    buffer <- textViewGetBuffer textView
    endIter <- textBufferGetEndIter buffer
    textViewScrollToIter textView endIter 0.0 Nothing 
    return ()

bufferAddStringWithTag :: TextBuffer -> String -> String -> IO ()
bufferAddStringWithTag buffer string tagName =  do 
    oldEnd <- textBufferGetEndIter buffer
    line <- textIterGetLine oldEnd
    offset <- textIterGetLineOffset oldEnd

    textBufferInsert buffer oldEnd string

    newEnd <- textBufferGetEndIter buffer 
    newBegin <- textBufferGetIterAtLineOffset buffer line offset 
    textBufferApplyTagByName buffer tagName newBegin newEnd

    return ()

textViewGetAllText :: TextView -> IO String 
textViewGetAllText textView = do 
    buffer <- textViewGetBuffer textView 
    beginIter <- textBufferGetStartIter buffer 
    endIter <- textBufferGetEndIter buffer 
    textBufferGetText buffer beginIter endIter True

initTextView :: Builder -> IO TextView
initTextView builder = do 
    textView <- builderGetObject builder castToTextView "MessageArea"
    buffer <- textViewGetBuffer textView
    tagTable <- textBufferGetTagTable buffer

    usernameColorTag <- textTagNew $ Just "UsernameColor"
    usernameColorTag `set` 
        [ textTagBackground := "White"
        , textTagForeground := "Dark Green"
        ]
    textTagTableAdd tagTable usernameColorTag

    messageColorTag <- textTagNew $ Just "MessageColor"
    messageColorTag `set` 
        [ textTagBackground := "White"
        , textTagForeground := "Dark Blue"
        ]
    textTagTableAdd tagTable messageColorTag

    errorColorTag <- textTagNew $ Just "ErrorColor"
    errorColorTag `set` 
        [ textTagBackground := "White"
        , textTagForeground := "Crimson"
        ]
    textTagTableAdd tagTable errorColorTag

    infoColorTag <- textTagNew $ Just "InfoColor"
    infoColorTag `set` 
        [ textTagBackground := "White"
        , textTagForeground := "Cadet Blue"
        ]
    textTagTableAdd tagTable infoColorTag

    return textView

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

    -- OptionDialog 
    (optionsRef, setupOptions') <- setupOptionDialog builder callbacks initArgs
    options <- readIORef optionsRef

    -- TextView for messages
    textView <- initTextView builder 

    -- Send buffer
    sendEntry <- builderGetObject builder castToEntry "SendEntry"
    
    -- Send Button
    sendButton <- builderGetObject builder castToButton "SendButton"
    sendButton `on` buttonActivated $ do
        msg <- entryGetText sendEntry
        username <- userName <$> readIORef optionsRef
        putUserMessage textView username msg
        sendMessageCallback callbacks msg 
        entrySetText sendEntry ""
        
    -- Connect button
    connectButton <- builderGetObject builder castToToolButton "ConnectButton"
    onToolButtonClicked connectButton $ connectCallback callbacks
        
    -- Disconnect button
    disconnectButton <- builderGetObject builder castToToolButton "DisconnectButton"
    onToolButtonClicked disconnectButton $ disconnectCallback callbacks


    return (mainWindow, options, GuiApi
        {
          printMessage = putUserMessage textView
        , printInfo    = putInfoMessage textView
        , printError   = putErrorMessage textView
        , setupOptions = setupOptions'
        , getChatText  = textViewGetAllText textView
        })

runGui :: Window -> IO ()
runGui mainWindow = do 
    -- Yielding GTK thread
    timeoutAddFull (yield >> return True) priorityDefaultIdle 50
    widgetShowAll mainWindow
    mainGUI
