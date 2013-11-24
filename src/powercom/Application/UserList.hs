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
module Application.UserList (
      initUserList
    ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Data.List (elemIndex)
import Control.Monad 

defaultUserIcon :: String 
defaultUserIcon =  "comotron-user"

addUserToList :: ListStore (String, String) -> String -> IO ()
addUserToList store name = do
    list <- listStoreToList store 
    case elemIndex name $ map snd list of
        Just i  -> return ()
        Nothing -> void $ listStoreAppend store (defaultUserIcon, name)

removeUserFromList :: ListStore (String, String) -> String -> IO ()
removeUserFromList store name = do 
    list <- listStoreToList store 
    case elemIndex name $ map snd list of
        Just i  -> listStoreRemove store i 
        Nothing -> return ()


initUserList :: Builder -> String -> IO (TreeView, String -> IO (), String -> IO ())
initUserList builder username = do
    treeView <- builderGetObject builder castToTreeView "UserListView"
    store <- listStoreNew [(defaultUserIcon, username)]
    
    treeModelSetColumn store (makeColumnIdString 0) fst 
    treeModelSetColumn store (makeColumnIdString 1) snd 
    treeViewSetModel treeView store 

    return (treeView, addUserToList store, removeUserFromList store)