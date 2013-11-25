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

import Paths_PowerCom
import Application.Layer

import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.Chan
import System.Exit
import System.Environment

exitMsg :: (ProcessId, String) -> Process ()
exitMsg (_, msg) = case msg of
  "exit" -> liftIO exitSuccess
  _      -> return ()

main :: IO ()
main = do
  args <- getArgs

  t <- createTransport
  node <- newLocalNode t initRemoteTable
  gladeFile <- getDataFileName "views/gui.glade"

  runProcess node $ do 
    rootId <- getSelfPid
    initApplicationLayer gladeFile (convertArgs args) rootId 
    forever $ receiveWait [match exitMsg]
    
  where 
    convertArgs args = case length args of
        2 -> Just (head args, args !! 1)
        _ -> Nothing