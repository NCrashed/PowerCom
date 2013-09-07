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

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Multiline.TextBuffer

menuBarDescr
    = [ ("_Файл", [ ("_Открыть историю",    Nothing)
                  , ("_Сохранить историю",  Nothing)
                  , ("_Выход",              Just mainQuit)
                  ]
        )
      , ("Помощь",  [ ("_О программе", Nothing)
                  ]
        )
      ]

commandBarDescr
    = [ ("_Настройки",    Nothing)
      , ("_Открыть порт", Nothing)
      , ("_Закрыть порт", Nothing)
      , ("_Подключиться", Nothing)
      , ("О_тключиться",  Nothing)
      ]
      
createMenuBar descr
    = do bar <- menuBarNew
         mapM_ (createMenu bar) descr
         return bar
    where
      createMenu bar (name,items)
          = do menu <- menuNew
               item <- menuItemNewWithLabelOrMnemonic name
               menuItemSetSubmenu item menu
               menuShellAppend bar item
               mapM_ (createMenuItem menu) items
      createMenuItem menu (name,action)
          = do item <- menuItemNewWithLabelOrMnemonic name
               menuShellAppend menu item
               case action of
                 Just act -> onActivateLeaf item act
                 Nothing  -> onActivateLeaf item (return ())
      menuItemNewWithLabelOrMnemonic name
          | elem '_' name = menuItemNewWithMnemonic name
          | otherwise     = menuItemNewWithLabel name

createCommandArea descr 
    = do box <- vBoxNew False 5
         mapM_ (createCommandButton box) descr 
         return box 
    where 
       createCommandButton box (caption, action)
          = do button <- createButtonSimpleOrMnemonic caption
               boxPackStart box button PackNatural 0
               case action of 
                  Just a -> onClicked button a
                  Nothing -> onClicked button (return ())
          where
            createButtonSimpleOrMnemonic name 
                | elem '_' name = buttonNewWithMnemonic name
                | otherwise = buttonNewWithLabel name


createMessageArea =
    do box <- vBoxNew False 5
       textView    <- textViewNew
       messageEdit <- entryNew
       boxPackStart box textView PackGrow 0
       boxPackStart box messageEdit PackNatural 0
       return box

createMainWindowArea = 
    do box <- hBoxNew False 5
       messageArea <- createMessageArea
       commandArea <- createCommandArea commandBarDescr
       boxPackStart box messageArea PackGrow 0
       boxPackStart box commandArea PackNatural 0
       return box

main =
    do initGUI
       window <- windowNew

       box <- vBoxNew False 5
       menuBar <- createMenuBar menuBarDescr
       mainArea <- createMainWindowArea
       boxPackStart box menuBar PackNatural 0
       boxPackStart box mainArea PackGrow 0

       set window [ windowTitle := "ComPower Chat"
                  , windowDefaultWidth  := 400
                  , windowDefaultHeight := 400
                  , containerChild := box
                  ]
       onDestroy window mainQuit
       widgetShowAll window
       mainGUI