-----------------------------------------------------------------------------
-- |
-- Module      :  Event
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Tagged events to link together gui callbacks and Process monad.
-- Gui callbacks are writers, Process monad is listener.
-----------------------------------------------------------------------------
module Event (
      Event
    , initEvent
    , tag
    , getTag
    , riseEvent
    , checkEvent
    ) where

import Data.IORef
import Control.Distributed.Process

-- | Tagged event with value of specified type. Internally based on IORef
-- not MVars, thats why it is acceptable to use only as one way info path.
data Event a = Event (IORef Bool) (IORef a)

-- | Wraps pure value in a event. It is not raised by default.
initEvent :: a -> IO (Event a)
initEvent val = do
    flagRef <- newIORef False
    valRef <- newIORef val
    return $ Event flagRef valRef

-- | Retrieves event stored value.
getTag :: Event a -> IO a 
getTag (Event _ val) = readIORef val

-- | Rewrites value in the event.
tag :: Event a -> a -> IO (Event a)
tag (Event flag valRef) val = do
    writeIORef valRef val 
    return $! Event flag valRef

-- | Raises event. On the other side "checkEvent" is
-- able to detect the change. 
riseEvent :: Event a -> IO(Event a)
riseEvent (Event flagRef val) = do 
    writeIORef flagRef True
    return $! Event flagRef val

-- | Safe way to react on events.
checkEvent :: 
  Event a -- ^ Event to check;
  -> (a -> Process b) -- ^ Transform function, it is called when event is raised; 
  -> b -- ^ Default value that is returned if the event isn't raised;
  -> Process b 
checkEvent (Event flagRef valRef) f failVal = do
    flag <- liftIO $ readIORef flagRef
    if flag then do
        val <- liftIO $ do 
            writeIORef flagRef False
            readIORef valRef
        f val 
    else return failVal
