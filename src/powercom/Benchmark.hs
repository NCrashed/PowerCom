-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Benchmarking module that runs in console without gui. There are stubs for
-- application layer functions.
--
-- The test tries to connect to the other side (other instance of the benchmark)
-- over 5 seconds after the beginning. The first two arguments are as usual:
-- port name and user name. The third one is name for the test file that would
-- be sent over the serial port. 
-----------------------------------------------------------------------------
module Main (main) where 

import Channel.Layer
import Channel.Options
import Utility

import Control.Monad (when)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent (threadDelay)
import Network.Transport.Chan
import System.Environment

-- | Handler for incoming user messages.
printUserMessage :: (ProcessId, String, String, String) -> Process Bool
printUserMessage (_, _, user, msg) = do 
  liftIO $ putStrLn $ "[" ++ user ++ "]:" ++ msg 
  return True

-- | Handler for local info messages.
printInfoMessage :: (ProcessId, String, String) -> Process Bool
printInfoMessage (_, _, msg) = do 
  liftIO $ putStrLn $ "Info: " ++ msg 
  return True

-- | Handler for local error messages.
printErrorMessage :: (ProcessId, String, String) -> Process Bool
printErrorMessage (_, _, msg) = do 
  liftIO $ putStrLn $ "Error: " ++ msg 
  return True

-- | Stub handler for changing options event.
setupOptionsHandler :: (ProcessId, String, ChannelOptions) -> Process Bool
setupOptionsHandler = const $ return True

-- | Stub handler for user connection event.
userConnectHandler :: (ProcessId, String, String) -> Process Bool
userConnectHandler (_, _, name) = do 
  liftIO $ putStrLn $ "User connected: " ++ name 
  return True

-- | Stub handler for user disconnecting event.
userDisconnectHandler :: (ProcessId, String, String) -> Process Bool
userDisconnectHandler (_, _, name) = do 
   liftIO $ putStrLn $ "User disconnected: " ++ name 
   return True

-- | Main benchmark function.
main :: IO ()
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