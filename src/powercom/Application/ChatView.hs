-----------------------------------------------------------------------------
-- |
-- Module      :  Application.ChatView
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Widget for displaying user's messages. Includes colorful text.
-----------------------------------------------------------------------------
module Application.ChatView (
      initChatTextView
    , putUserMessage
    , putInfoMessage
    , putErrorMessage
    , textViewGetAllText
    , textViewSetText
    ) where

import Graphics.UI.Gtk
import Data.Time 
import Data.Functor
import System.Locale 

-- | Writes user message in the chat with auto-scrolling.
putUserMessage :: TextView -- ^ Chat to write to 
  -> String -- ^ User name to display in brackets
  -> String -- ^ Message to display 
  -> IO ()
putUserMessage textView username msg = do 
    timeStr <- formatTime defaultTimeLocale "%T" <$> getCurrentTime
    buffer <- textViewGetBuffer textView
    bufferAddStringWithTag buffer ("[" ++ timeStr ++ ": " ++ username ++ "]: ") "UsernameColor"
    bufferAddStringWithTag buffer (msg++"\n") "MessageColor"

    textViewScrollToEnd textView

-- | Writes system message in the chat with auto-scrolling.
-- Info messages displayed in blue color.
putInfoMessage :: TextView -- ^ Chat to write to 
  -> String -- ^ System message body 
  -> IO ()
putInfoMessage textView msg = do 
    buffer <- textViewGetBuffer textView
    bufferAddStringWithTag buffer (msg++"\n") "InfoColor"

    textViewScrollToEnd textView

-- | Writes system errors in the chat with auto-scrolling.
-- Error messages displayed in red color.
putErrorMessage :: TextView -- ^ Chat to write to 
  -> String -- ^ System message body. 
  -> IO ()
putErrorMessage textView msg = do 
    buffer <- textViewGetBuffer textView
    bufferAddStringWithTag buffer (msg++"\n") "ErrorColor"

    textViewScrollToEnd textView

-- | Scrolls down chat to the end.
textViewScrollToEnd :: TextView -> IO ()
textViewScrollToEnd textView = do 
    buffer <- textViewGetBuffer textView
    endIter <- textBufferGetEndIter buffer
    textViewScrollToIter textView endIter 0.0 Nothing 
    return ()

-- | Wrapper to add tagges string into textbuffer.
bufferAddStringWithTag :: TextBuffer -- ^ Text buffer to add to 
  -> String -- ^ String to add 
  -> String -- ^ Tag name. The tag should be created before the call. 
  -> IO ()
bufferAddStringWithTag buffer string tagName =  do 
    oldEnd <- textBufferGetEndIter buffer
    line <- textIterGetLine oldEnd
    offset <- textIterGetLineOffset oldEnd

    textBufferInsert buffer oldEnd string

    newEnd <- textBufferGetEndIter buffer 
    newBegin <- textBufferGetIterAtLineOffset buffer line offset 
    textBufferApplyTagByName buffer tagName newBegin newEnd

    return ()

-- | Returns contents of the chat.
textViewGetAllText :: TextView -> IO String 
textViewGetAllText textView = do 
    buffer <- textViewGetBuffer textView 
    beginIter <- textBufferGetStartIter buffer 
    endIter <- textBufferGetEndIter buffer 
    textBufferGetText buffer beginIter endIter True

-- | Clear contents of the chat. Tags are not deleted.
bufferDeleteAllText :: TextBuffer -> IO ()
bufferDeleteAllText buffer = do 
    beginIter <- textBufferGetStartIter buffer 
    endIter <- textBufferGetEndIter buffer 
    textBufferDelete buffer beginIter endIter 

-- | Rewrites contents of the chat.
textViewSetText :: TextView -> String -> IO ()
textViewSetText textView text = do 
    buffer <- textViewGetBuffer textView 
    bufferDeleteAllText buffer 
    bufferAddStringWithTag buffer text "HistoryColor"

-- | Creates chat. Requires text view in glade file with name "MessageArea".
-- This function creates buffer and tags for highlighting.
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

    historyColorTag <- textTagNew $ Just "HistoryColor"
    historyColorTag `set` 
        [ textTagBackground := "White"
        , textTagForeground := "Chocolate"
        ]
    textTagTableAdd tagTable historyColorTag

    return textView