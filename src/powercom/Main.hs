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
import Graphics.UI.Gtk.Windows.Dialog
import System.Hardware.Serialport
import Data.Functor
import Control.Applicative
import Control.Monad

menuBarDescr
    = [ ("_Файл", [ ("_Открыть историю",    Nothing)
                  , ("_Сохранить историю",  Nothing)
                  , ("_Выход",              Just mainQuit)
                  ]
        )
      , ("_Помощь",  [ ("_О программе", Nothing)
                  ]
        )
      ]

commandBarDescr optionDialog
    = [ ("_Настройки",    Just $ do widgetShowAll optionDialog 
                                    return ())
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
       hbox <- hBoxNew False 5
       sendBtn <- buttonNewWithLabel "Отправить"
       boxPackStart hbox messageEdit PackGrow 0
       boxPackStart hbox sendBtn PackNatural 0
       boxPackStart box hbox PackNatural 0
       return box

createMainWindowArea bar = 
    do box <- hBoxNew False 5
       messageArea <- createMessageArea
       commandArea <- createCommandArea bar
       boxPackStart box messageArea PackGrow 0
       boxPackStart box commandArea PackNatural 0
       return box

createMainWindow bar =
    do window <- windowNew

       box <- vBoxNew False 5
       menuBar <- createMenuBar menuBarDescr
       mainArea <- createMainWindowArea bar
       boxPackStart box menuBar PackNatural 0
       boxPackStart box mainArea PackGrow 0

       set window [ windowTitle := "ComPower Chat"
                  , windowDefaultWidth  := 400
                  , windowDefaultHeight := 400
                  , containerChild := box
                  ]
       onDestroy window mainQuit
       return window

createOptionDialog =
    do dialog <- dialogNew
       windowSetModal dialog True
       set dialog [windowTitle := "Настройки"]
       box <- dialogGetUpper dialog

       speedCombo <- createEnumCombo [("110",    CS110)
                                     ,("300",    CS300)
                                     ,("600",    CS600)
                                     ,("1200",   CS1200)
                                     ,("2400",   CS2400)
                                     ,("4800",   CS4800)
                                     ,("9600",   CS9600)
                                     ,("19200",  CS19200)
                                     ,("38400",  CS38400)
                                     ,("57600",  CS57600)
                                     ,("115200", CS115200)]

       stopBitsCombo <- createEnumCombo [("One", One)
                                        ,("Two", Two)]

       parityCombo <- createEnumCombo [("Even", Even)
                                      ,("Odd",  Odd)
                                      ,("None", NoParity)]

       dataBitsCombo <- createEnumCombo [("7", 7)
                                        ,("8", 8)
                                        ,("9", 9)]
       portEntry <- entryNew
       nickEntry <- entryNew

       packTable box [("Имя порта:", toWidget portEntry)
                     ,("Псевдоним:", toWidget nickEntry)
                     ,("Скорость:",  toWidget speedCombo)
                     ,("Стоп биты:", toWidget stopBitsCombo)
                     ,("Бит четности:", toWidget parityCombo)
                     ,("Биты данный", toWidget dataBitsCombo)]
       
       dialogAddButton dialog "OK" ResponseOk
       dialogAddButton dialog "Cancel" ResponseCancel
       dialog `on` response $ \respId -> do
          case respId of 
            ResponseOk -> putStrLn "Ok button pressed!"
            ResponseCancel -> putStrLn "Cancel button pressed!"
            ResponseDeleteEvent -> putStrLn "Close button pressed!"
          widgetHideAll dialog
       return dialog
    where
       packTable :: (BoxClass a, WidgetClass b) => a -> [(String, b)] -> IO ()
       packTable parent descr =
          do table <- tableNew (length descr) 2 True
             foldM_ (packTableRow table) 0 descr
             boxPackStart parent table PackGrow 0
          where 
            packTableRow table row (text, widget) =
              do label <- labelNew $ Just text 
                 tableAttachDefaults table label  0 1 row (row+1)
                 tableAttachDefaults table widget 1 2 row (row+1)
                 return $ row+1

       createEnumCombo descr =
          do combo <- comboBoxNew
             store <- listStoreNew descr
             cell <- cellRendererTextNew
             cellLayoutPackStart combo cell True
             cellLayoutSetAttributes combo cell store $ 
                \(text, value) -> [cellText := text]
             comboBoxSetModel combo $ Just store
             return combo

main =
    do initGUI
       optionDialog <- createOptionDialog
       mainWindow <- createMainWindow (commandBarDescr optionDialog)
       widgetShowAll mainWindow
       mainGUI