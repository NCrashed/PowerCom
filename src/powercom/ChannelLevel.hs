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
module ChannelLevel (
    initChannelLevel,
    connect,
    disconnect,
    changeOptions,
    sendMessage,
    prop_toByteString
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8

import Data.Functor
import Data.Maybe
import Control.Monad
import Data.Word 

import Data.Binary.Strict.Get
import Data.Binary.Put

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process

import Test.QuickCheck

class (Eq a) => FrameClass a where
    toByteString :: a -> BS.ByteString
    fromByteString :: BS.ByteString -> (Either String a, BS.ByteString)

frameStartByte :: Word8
frameStartByte = 0xFF

frameEndByte :: Word8
frameEndByte = 0xFF

frameType :: Frame -> Word8
frameType frame = case frame of 
                    InformationFrame _ -> 0x00
                    LinkFrame          -> 0x01
                    UnlinkFrame        -> 0x02
                    AckFrame           -> 0x03
                    RetFrame           -> 0x04
                    OptionFrame      _ -> 0x05

data Frame = InformationFrame String
             | OptionFrame [(String, String)]
             | LinkFrame   
             | UnlinkFrame 
             | AckFrame    
             | RetFrame    
             deriving (Show, Eq)
 
instance Arbitrary Frame where
    arbitrary = oneof [ InformationFrame <$> (arbitrary :: Gen String)
                      , return LinkFrame
                      , return UnlinkFrame
                      , return AckFrame
                      , return RetFrame
                      , OptionFrame <$> (arbitrary :: Gen [(String, String)])]

    shrink (OptionFrame os) = [OptionFrame nos | nos <- shrink os]
    shrink _ = []


int2word :: Int -> Word8 
int2word = fromInteger . toInteger

word2int :: Word8 -> Int 
word2int = fromInteger . toInteger

instance FrameClass Frame where
    toByteString frame = BS.concat . BL.toChunks $ runPut $ case frame of 
                            InformationFrame s  -> putBounded $ putMarkedString s
                            LinkFrame           -> putShort 
                            UnlinkFrame         -> putShort
                            AckFrame            -> putShort
                            RetFrame            -> putShort
                            OptionFrame      os -> putBounded $ putListLength os >> putOptions os
                         where 
                            putBegin          = putWord8 frameStartByte >> (putWord8 $ frameType frame)
                            putEnd            = putWord8 frameEndByte
                            putShort          = putBegin >> putEnd
                            putListLength     = putWord8 . int2word . length
                            putMarkedString s = putListLength s >> putByteString (C8.pack s)
                            putBounded      m = putBegin >> m >> putEnd
                            putOptions        = mapM_ (\(key,value) -> putMarkedString key >> putMarkedString value)

    fromByteString str = runGet parseFrame str
                            where
                                parseFrame :: Get Frame
                                parseFrame = do
                                    start <- getWord8
                                    case (start == frameStartByte) of
                                        False -> fail "Starting byte invalid!"
                                        True  -> do
                                            frameType <- getWord8
                                            frame <- case frameType of
                                                0x00 -> return InformationFrame `ap` parseMarkedString
                                                0x01 -> return LinkFrame
                                                0x02 -> return UnlinkFrame
                                                0x03 -> return AckFrame
                                                0x04 -> return RetFrame
                                                0x05 -> return OptionFrame `ap` parseKeyValue
                                                _    -> fail "Unknown frame type!"
                                            end <- getWord8
                                            case (end == frameEndByte) of
                                                False -> fail "Ending byte invalid!"
                                                True  -> return frame
                                parseMarkedString = do
                                    len <- getWord8
                                    body <- getByteString $ word2int len 
                                    return $ C8.unpack body

                                parseInformationFrame = do
                                    body <- parseMarkedString 
                                    return $ InformationFrame body

parseKeyValue :: Get [(String, String)]
parseKeyValue =  do
    pairsCount <- getWord8
    mapM parsePair [1..pairsCount]
    where
        parsePair :: a -> Get (String, String)
        parsePair _ = do
            keyCount <- getWord8
            key <- getByteString $ word2int keyCount
            valueCount <- getWord8
            value <- getByteString $ word2int valueCount
            return (C8.unpack key, C8.unpack value)

formPair :: [String] -> Maybe (String, String)
formPair (x1:x2:[]) = Just (x1, x2)
formPair _          = Nothing

-- Interfacing with layer
connect :: IO ()
connect = return ()

disconnect :: IO ()
disconnect = return ()

changeOptions :: [(String, String)] -> IO ()
changeOptions ops = return ()

sendMessage :: String -> IO ()
sendMessage msg = return ()

initChannelLevel :: Process ProcessId
initChannelLevel = do
    id <- spawnLocal $ forever $ do
        liftIO $ threadDelay (1*1000000)
    return id

-- Testing 
prop_toByteString :: Frame -> Bool
prop_toByteString f = case fst $ fromByteString $ toByteString f of 
                        Left  _ -> False
                        Right v -> v == f