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
module Main (main) where

import Channel.Layer
import Channel.Options
import Utility

import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent (threadDelay)
import Network.Transport.Chan
import System.Exit
import System.Environment

printUserMessage :: (ProcessId, String, String, String) -> Process Bool
printUserMessage (_, _, user, msg) = do 
  liftIO $ putStrLn $ "[" ++ user ++ "]:" ++ msg 
  return True

printInfoMessage :: (ProcessId, String, String) -> Process Bool
printInfoMessage (_, _, msg) = do 
  liftIO $ putStrLn $ "Info: " ++ msg 
  return True

printErrorMessage :: (ProcessId, String, String) -> Process Bool
printErrorMessage (_, _, msg) = do 
  liftIO $ putStrLn $ "Error: " ++ msg 
  return True

setupOptionsHandler :: (ProcessId, String, ChannelOptions) -> Process Bool
setupOptionsHandler (_, _, options) = return True

userConnectHandler :: (ProcessId, String, String) -> Process Bool
userConnectHandler (_, _, name) = do 
  liftIO $ putStrLn $ "User connected: " ++ name 
  return True

userDisconnectHandler :: (ProcessId, String, String) -> Process Bool
userDisconnectHandler (_, _, name) = do 
   liftIO $ putStrLn $ "User disconnected: " ++ name 
   return True

main = do
  args <- getArgs

  t <- createTransport
  node <- newLocalNode t initRemoteTable

  runProcess node $ do 
    rootId <- getSelfPid
    channelId <- initChannelLayer rootId $ startOptions $ convertArgs args 
    liftIO $ threadDelay 5000000
    send channelId (rootId, "connect")
    liftIO $ threadDelay 1000000

    when (length args == 3) $ do 
      dt <- liftIO $ testData args    
      send channelId (rootId, "send", dt)

    while $ receiveWait [
              matchIf (\(_, com)       -> com == "exit")         exitMsg
            , matchIf (\(_, com, _, _) -> com == "message")      printUserMessage       
            , matchIf (\(_, com, _)    -> com == "info")         printInfoMessage       
            , matchIf (\(_, com, _)    -> com == "error")        printErrorMessage      
            , matchIf (\(_, com, _)    -> com == "options")      setupOptionsHandler    
            , matchIf (\(_, com, _)    -> com == "connect")      userConnectHandler     
            , matchIf (\(_, com, _)    -> com == "disconnect")   userDisconnectHandler]

    
  where 
    testData :: [String] -> IO String
    testData args = readFile (args !! 2)

    convertArgs args = if length args >= 2 then
        Just (head args, args !! 1)
        else Nothing
    startOptions args = case args of 
      Just (port, uname) -> defaultOptions {portName = port, userName = uname}
      Nothing -> defaultOptions