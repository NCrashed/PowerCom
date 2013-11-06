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
module Application.OptionDialog (
      setupOptionDialog
    , defaultOptions
    ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Application.Types
import Channel.Options
import System.Hardware.Serialport hiding (send)

import Data.Word 
import Data.Functor
import Data.Data
import Data.List
import Data.IORef

createEnumCombo combo descr = do
    store <- listStoreNew descr
    cell <- cellRendererTextNew
    cellLayoutPackStart combo cell True
    cellLayoutSetAttributes combo cell store $ 
        \text -> [cellText := text]
    comboBoxSetModel combo $ Just store
    return combo

setupDefaultOptions :: Builder -> IO ChannelOptions
setupDefaultOptions builder = do
    portNameEntry  <- getEntry "PortNameEntry"
    userNameEntry  <- getEntry "UserNameEntry"
    speedCombo     <- getComboBox "SpeedCombo"
    stopBitCombo   <- getComboBox "StopBitCombo"
    parityBitCombo <- getComboBox "ParityBitCombo"
    wordBitCombo   <- getComboBox "WordBitCombo"

    entrySetText portNameEntry $ portName defaultOptions
    entrySetText userNameEntry $ userName defaultOptions
    comboBoxSetActive speedCombo     4
    comboBoxSetActive stopBitCombo   1
    comboBoxSetActive parityBitCombo 2
    comboBoxSetActive wordBitCombo   0

    return defaultOptions
    where
        getEntry     = builderGetObject builder castToEntry
        getComboBox  = builderGetObject builder castToComboBox

collectOptions :: Builder -> IO ChannelOptions
collectOptions builder = do 
    portNameEntry  <- getEntry "PortNameEntry"
    userNameEntry  <- getEntry "UserNameEntry"
    speedCombo     <- getComboBox "SpeedCombo"
    stopBitCombo   <- getComboBox "StopBitCombo"
    parityBitCombo <- getComboBox "ParityBitCombo"
    wordBitCombo   <- getComboBox "WordBitCombo"

    portNameVal    <- entryGetText portNameEntry
    userNameVal    <- entryGetText portNameEntry
    portSpeedVal   <- string2PortSpeed <$> getFromCombo speedCombo
    stopBitVal     <- string2StopBit   <$> getFromCombo stopBitCombo
    parityBitVal   <- string2ParityBit <$> getFromCombo parityBitCombo
    wordBitVal     <- getWordBit       <$> getFromCombo wordBitCombo

    return ChannelOptions
        {
          portName       = portNameVal
        , userName       = userNameVal
        , portSpeed      = portSpeedVal
        , portStopBits   = stopBitVal
        , portParityBits = parityBitVal 
        , portWordBits   = wordBitVal
        }
    where
        getFromCombo :: ComboBox -> IO String
        getFromCombo combo = do 
            iter' <- comboBoxGetActiveIter combo
            case iter' of
                Just iter -> do 
                    Just model <- comboBoxGetModel combo
                    treeModelGetValue model iter $ makeColumnIdString 0
                Nothing   -> return ""

        getEntry     = builderGetObject builder castToEntry
        getComboBox  = builderGetObject builder castToComboBox
        getWordBit s = case s of 
            "" -> 7
            _  -> (read s)::Word8

setupOptionDialog :: Builder -> GuiCallbacks -> IO (IORef ChannelOptions)
setupOptionDialog builder callbacks = do
    optionDialog <- builderGetObject builder castToDialog "OptionDialog" 
    optionDialog `set` [windowDeletable := False]

    -- Setup options
    initOptions <- setupDefaultOptions builder 
    options <- newIORef initOptions

    -- OptionDialog item
    optionItem <- builderGetObject builder castToMenuItem "OptionItem"
    optionItem `on` menuItemActivate $ widgetShowAll optionDialog

    -- OptionDialog tool button
    optionButton <- builderGetObject builder castToToolButton "OptionButton"
    onToolButtonClicked optionButton $ widgetShowAll optionDialog

    optionDialog `on` response $ \respId -> do
          case respId of 
            ResponseUser 1 -> do
                newOptions <- collectOptions builder
                writeIORef options newOptions
                optionChangedCallback callbacks newOptions
            ResponseUser 2 -> return ()
            _ -> return ()
          widgetHideAll optionDialog

    -- Speed combo
    speedCombo <- builderGetObject builder castToComboBox "SpeedCombo"
    createEnumCombo speedCombo $ map portSpeed2String
        [CS110
        ,CS300
        ,CS600
        ,CS1200
        ,CS2400
        ,CS4800
        ,CS9600
        ,CS19200
        ,CS38400
        ,CS57600
        ,CS115200]

    stopBitCombo <- builderGetObject builder castToComboBox "StopBitCombo"
    createEnumCombo stopBitCombo $ map stopBit2String [One,Two]

    parityBitCombo <- builderGetObject builder castToComboBox "ParityBitCombo"
    createEnumCombo parityBitCombo $ map parityBit2String [Even, Odd, NoParity]

    wordBitCombo <- builderGetObject builder castToComboBox "WordBitCombo"
    createEnumCombo wordBitCombo $ map show [7,8,9]

    -- OptionDialog
    return options