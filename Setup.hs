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
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

import System.Directory 

viewPrefix = "views"
viewIcons = [ "comotron-connect.png"
            , "comotron-disconnect.png"
            , "comotron-user.png"
            ]

copyIcons :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyIcons args instFlags descr info = do 
    userDir <- getHomeDirectory
    mapM_ (copyIcon userDir) viewIcons
    where
        copyIcon :: FilePath -> FilePath -> IO ()
        copyIcon userDir icon = copyFile (viewPrefix ++ "/" ++ icon) (userDir ++ "/.icons/" ++ icon) 


main = defaultMainWithHooks simpleUserHooks
       { 
            postInst = copyIcons
       }