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
module Application.ChatView (
      initChatTextView
    , putUserMessage
    , putInfoMessage
    , putErrorMessage
    , textViewGetAllText
    ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

putUserMessage :: TextView -> String -> String -> IO ()
putUserMessage textView username msg = do 
    buffer <- textViewGetBuffer textView
    bufferAddStringWithTag buffer ("[" ++ username ++ "]: ") "UsernameColor"
    bufferAddStringWithTag buffer (msg++"\n") "MessageColor"

    textViewScrollToEnd textView

putInfoMessage :: TextView -> String -> IO ()
putInfoMessage textView msg = do 
    buffer <- textViewGetBuffer textView
    bufferAddStringWithTag buffer (msg++"\n") "InfoColor"

    textViewScrollToEnd textView

putErrorMessage :: TextView -> String -> IO ()
putErrorMessage textView msg = do 
    buffer <- textViewGetBuffer textView
    bufferAddStringWithTag buffer (msg++"\n") "ErrorColor"

    textViewScrollToEnd textView

textViewScrollToEnd :: TextView -> IO ()
textViewScrollToEnd textView = do 
    buffer <- textViewGetBuffer textView
    endIter <- textBufferGetEndIter buffer
    textViewScrollToIter textView endIter 0.0 Nothing 
    return ()

bufferAddStringWithTag :: TextBuffer -> String -> String -> IO ()
bufferAddStringWithTag buffer string tagName =  do 
    oldEnd <- textBufferGetEndIter buffer
    line <- textIterGetLine oldEnd
    offset <- textIterGetLineOffset oldEnd

    textBufferInsert buffer oldEnd string

    newEnd <- textBufferGetEndIter buffer 
    newBegin <- textBufferGetIterAtLineOffset buffer line offset 
    textBufferApplyTagByName buffer tagName newBegin newEnd

    return ()

textViewGetAllText :: TextView -> IO String 
textViewGetAllText textView = do 
    buffer <- textViewGetBuffer textView 
    beginIter <- textBufferGetStartIter buffer 
    endIter <- textBufferGetEndIter buffer 
    textBufferGetText buffer beginIter endIter True

initChatTextView :: Builder -> IO TextView
initChatTextView builder = do 
    textView <- builderGetObject builder castToTextView "MessageArea"
    buffer <- textViewGetBuffer textView
    tagTable <- textBufferGetTagTable buffer

    usernameColorTag <- textTagNew $ Just "UsernameColor"
    usernameColorTag `set` 
        [ textTagBackground := "White"
        , textTagForeground := "Dark Green"
        ]
    textTagTableAdd tagTable usernameColorTag

    messageColorTag <- textTagNew $ Just "MessageColor"
    messageColorTag `set` 
        [ textTagBackground := "White"
        , textTagForeground := "Dark Blue"
        ]
    textTagTableAdd tagTable messageColorTag

    errorColorTag <- textTagNew $ Just "ErrorColor"
    errorColorTag `set` 
        [ textTagBackground := "White"
        , textTagForeground := "Crimson"
        ]
    textTagTableAdd tagTable errorColorTag

    infoColorTag <- textTagNew $ Just "InfoColor"
    infoColorTag `set` 
        [ textTagBackground := "White"
        , textTagForeground := "Cadet Blue"
        ]
    textTagTableAdd tagTable infoColorTag

    return textView