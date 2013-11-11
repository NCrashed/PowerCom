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
module Application.Types (
      GuiCallbacks(..)
    , GuiApi(..)  
    ) where 

import Channel.Options

data GuiCallbacks = GuiCallbacks {
      sendMessageCallback   :: String -> IO ()
    , connectCallback       :: IO ()
    , disconnectCallback    :: IO ()
    , optionChangedCallback :: ChannelOptions -> IO ()
}

data GuiApi = GuiApi {
      printMessage :: String -> String -> IO ()
    , printInfo    :: String -> IO ()
    , printError   :: String -> IO ()
    , setupOptions :: ChannelOptions -> IO ()
    , getChatText  :: IO String
}