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
      runGui
    , defaultOptions
    ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Application.OptionDialog
import Application.Types

runGui :: FilePath -> GuiCallbacks -> IO ()
runGui gladeFile callbacks = do 
    initGUI
    builder <- builderNew
    builderAddFromFile builder gladeFile
    
    -- Binding main window
    mainWindow <- builderGetObject builder castToWindow "MainWindow"
    onDestroy mainWindow mainQuit

    -- Exit item
    exitItem <- builderGetObject builder castToMenuItem "ExitItem"
    exitItem `on` menuItemActivate $ mainQuit

    -- Send buffer
    sendEntry <- builderGetObject builder castToEntry "SendEntry"
    
    -- Send Button
    sendButton <- builderGetObject builder castToButton "SendButton"
    sendButton `on` buttonActivated $ do 
        sendMessageCallback callbacks =<< entryGetText sendEntry
        entrySetText sendEntry ""

    -- Connect button
    connectButton <- builderGetObject builder castToToolButton "ConnectButton"
    onToolButtonClicked connectButton $ connectCallback callbacks
        
    -- Disconnect button
    disconnectButton <- builderGetObject builder castToToolButton "DisconnectButton"
    onToolButtonClicked disconnectButton $ disconnectCallback callbacks

    -- OptionDialog 
    optionDialog <- setupOptionDialog builder callbacks


    widgetShowAll mainWindow
    mainGUI
