-----------------------------------------------------------------------------
-- |
-- Module      :  Application.UserList
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module contains function to operate with user list widget. User list
-- contains names of connected users, if two users have equal names, then
-- users are equal. 
-----------------------------------------------------------------------------
module Application.UserList (
      initUserList
    ) where

import Graphics.UI.Gtk
import Data.List (elemIndex)
import Control.Monad 

-- | Resource name for user icon
defaultUserIcon :: String 
defaultUserIcon =  "comotron-user"

-- | Adds new user to the list. Does nothing
-- if user is already in the list.
addUserToList :: ListStore (String, String) -- ^ List model (icon, name)
  -> String -- ^ User name to add
  -> IO ()
addUserToList store name = do
    list <- listStoreToList store 
    case elemIndex name $ map snd list of
        Just _  -> return ()
        Nothing -> void $ listStoreAppend store (defaultUserIcon, name)

-- | Removes user from the list. Does nothing if user is not in the list.
removeUserFromList :: ListStore (String, String) -- ^ List model (icon, name)
  -> String  -- ^ User name to remove
  -> IO ()
removeUserFromList store name = do 
    list <- listStoreToList store 
    case elemIndex name $ map snd list of
        Just i  -> listStoreRemove store i 
        Nothing -> return ()

-- | Creates user list widget. Returns api functions to operate with the list.
initUserList :: Builder -- ^ Builder to get widgets from
  -> String -- ^ Local user name 
  -> IO (String -> IO (), String -> IO ()) -- ^ Returns api functions for adding and removing users from the list
initUserList builder username = do
    treeView <- builderGetObject builder castToTreeView "UserListView"
    store <- listStoreNew [(defaultUserIcon, username)]
    
    treeModelSetColumn store (makeColumnIdString 0) fst 
    treeModelSetColumn store (makeColumnIdString 1) snd 
    treeViewSetModel treeView store 

    return (addUserToList store, removeUserFromList store)