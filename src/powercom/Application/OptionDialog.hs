-----------------------------------------------------------------------------
-- |
-- Module      :  Application.OptionDialog
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module holds function to work with options dialog. It is handles automatic
-- options deconstructing to setup needed comboboxes when new options are arrived
-- from remote side.
-----------------------------------------------------------------------------
module Application.OptionDialog (
      setupOptionDialog
    , defaultOptions
    ) where

import Graphics.UI.Gtk
import Application.Types
import System.Hardware.Serialport hiding (send)

import Control.Monad.IO.Class (liftIO)
import Control.Applicative 
import Control.Monad

import Data.Word 
import Data.List
import Data.IORef
import Data.Maybe 


import Channel.Options
import Physical.Detector 

-- | Fills combo with list of showable values and return function to
-- matching that values with combo elements 
createEnumCombo :: (Eq a) => ComboBox         -- ^ Combo box to fill
    -> (a -> String)                          -- ^ Function to map elem into string, show for instance
    -> [a]                                    -- ^ List of values the combo be filled
    -> IO (a -> Maybe Int)                    -- ^ Matching function to search values in the combo
createEnumCombo combo f descr = do
    comboBoxSetModelText combo
    mapM_ (comboBoxAppendText combo . f) descr
    return $ \val -> elemIndex val descr

-- | Data type used to mapping options to option dialog combos indexes
data OptionMappings = OptionMappings
    {
      speedMapping    :: CommSpeed -> Maybe Int 
    , stopBitMapping  :: StopBits  -> Maybe Int 
    , parityMapping   :: Parity    -> Maybe Int
    , portWordMapping :: Word8     -> Maybe Int
    }

-- | If optional arguments are passed to program, then uses them to fill
-- default serial port name and user name.
defaultOptionsWithArgs :: Maybe (String, String) -> ChannelOptions
defaultOptionsWithArgs args = case args of 
    Nothing -> defaultOptions 
    Just (portname, username) -> defaultOptions { portName = portname, userName = username}

-- | Retrieves from glade builder all needed gui elements to operate over options fields.
getOptionElements :: Builder -> IO (ComboBox, Entry, ComboBox, ComboBox, ComboBox, ComboBox)
getOptionElements builder = (,,,,,) <$> 
    getComboBox "PortNameCombo" <*>
    getEntry "UserNameEntry" <*> 
    getComboBox "SpeedCombo" <*>
    getComboBox "StopBitCombo" <*>
    getComboBox "ParityBitCombo" <*>
    getComboBox "WordBitCombo"
    where
        getEntry     = builderGetObject builder castToEntry
        getComboBox  = builderGetObject builder castToComboBox
                
-- | Fills gui elements with options fields.          
setupGuiOptions :: Builder -- ^ Builder to get gui elements 
  -> OptionMappings -- ^ Mappings from option values to gui elements inner indexes 
  -> ChannelOptions -- ^ Options values to get data from 
  -> IO ()
setupGuiOptions builder mappings options = do
    (_,userNameEntry,speedCombo,stopBitCombo,parityBitCombo,wordBitCombo) <- getOptionElements builder

    --entrySetText portNameEntry $ portName options
    entrySetText userNameEntry $ userName options
    comboBoxSetActive speedCombo     $ fromMaybe 0 $ speedMapping mappings $ portSpeed options
    comboBoxSetActive stopBitCombo   $ fromMaybe 0 $ stopBitMapping mappings $ portStopBits options
    comboBoxSetActive parityBitCombo $ fromMaybe 0 $ parityMapping mappings $ portParityBits options
    comboBoxSetActive wordBitCombo   $ fromMaybe 0 $ portWordMapping mappings $ portWordBits options

-- | Collects options values from gui elements. 
collectOptions :: Builder -> IO ChannelOptions
collectOptions builder = do 
    (portNameCombo,userNameEntry,speedCombo,stopBitCombo,parityBitCombo,wordBitCombo) 
        <- getOptionElements builder

    portNameVal    <- getFromCombo portNameCombo
    userNameVal    <- entryGetText userNameEntry
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
            maybeText <- comboBoxGetActiveText combo
            case maybeText of
                Just str -> return str
                Nothing  -> return ""
        getWordBit s = case s of 
            "" -> 7
            _  -> read s :: Word8

-- | Creates option dialogs. Fills dialog with default options values and 
-- calculates appropriate mappings between options values and gui elements inner indexes.
setupOptionDialog :: Builder -- ^ GUI builder to get elements from 
  -> GuiCallbacks -- ^ Gui callbacks to use 
  -> Maybe (String, String) -- ^ Optional arguments containing default serial port name and user name
  -> IO (IORef ChannelOptions, ChannelOptions -> IO ()) -- ^ Returns reference to current options contained in gui and API function to set up new options
setupOptionDialog builder callbacks initArgs = do
    optionDialog <- builderGetObject builder castToDialog "OptionDialog" 
    optionDialog `set` [windowDeletable := False]

    -- Combos
    portNameCombo <- builderGetObject builder castToComboBox "PortNameCombo"
    createEnumCombo portNameCombo id =<< getSerialPorts

    speedCombo <- builderGetObject builder castToComboBox "SpeedCombo"
    speedMatch <- createEnumCombo speedCombo portSpeed2String
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
    stopBitMatch <- createEnumCombo stopBitCombo stopBit2String [One,Two]

    parityBitCombo <- builderGetObject builder castToComboBox "ParityBitCombo"
    parityBitMatch <- createEnumCombo parityBitCombo parityBit2String [Even, Odd, NoParity]

    wordBitCombo <- builderGetObject builder castToComboBox "WordBitCombo"
    wordBitMatch <- createEnumCombo wordBitCombo show [7,8]

    let mappings = OptionMappings
            {
              speedMapping    = speedMatch
            , stopBitMapping  = stopBitMatch
            , parityMapping   = parityBitMatch
            , portWordMapping = wordBitMatch
            } 
    -- Setup options
    let initOptions = defaultOptionsWithArgs initArgs
    options <- newIORef initOptions
    setupGuiOptions builder mappings initOptions 
    
    -- OptionDialog item
    optionItem <- builderGetObject builder castToMenuItem "OptionItem"
    optionItem `on` menuItemActivate $ widgetShowAll optionDialog

    -- OptionDialog tool button
    optionButton <- builderGetObject builder castToToolButton "OptionButton"
    onToolButtonClicked optionButton $ widgetShowAll optionDialog

    optionDialog `on` keyPressEvent $ tryEvent $ do 
        "Return" <- eventKeyName
        liftIO $ dialogResponse optionDialog $ ResponseUser 1 

    optionDialog `on` keyPressEvent $ tryEvent $ do 
        "Escape" <- eventKeyName 
        liftIO $ dialogResponse optionDialog $ ResponseUser 2 

    optionDialog `on` response $ \respId -> do
          case respId of 
            ResponseUser 1 -> do
                newOptions <- collectOptions builder
                oldOptions <- readIORef options 
                writeIORef options newOptions
                optionChangedCallback callbacks newOptions oldOptions
            ResponseUser 2 -> return ()
            _ -> return ()
          widgetHideAll optionDialog

    -- OptionDialog
    return (options, void . setupGuiOptions builder mappings)