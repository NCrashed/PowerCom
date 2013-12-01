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
module Channel.Frame (
      Frame(..)
    , FrameClass(..)
    , prop_toByteString
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UTF

import Data.Functor
import Data.Word 

import Data.Binary.Strict.Get
import Data.Binary.Put

import Control.Monad
import Control.Applicative

import Test.QuickCheck

class (Eq a) => FrameClass a where
    toByteString :: a -> BS.ByteString
    fromByteString :: BS.ByteString -> (Either String a, BS.ByteString)

frameType :: Frame -> Word8
frameType frame = case frame of 
                    InformationFrame _ _ -> 0x00
                    DataPartFrame _      -> 0x01
                    LinkFrame        _   -> 0x02
                    UnlinkFrame      _   -> 0x03
                    AckFrame             -> 0x04
                    RetFrame             -> 0x05
                    OptionFrame      _   -> 0x06
                    Upcheck              -> 0x07

data Frame = InformationFrame String Word32
             | DataPartFrame String
             | OptionFrame [(String, String)]
             | LinkFrame   String 
             | UnlinkFrame String 
             | AckFrame    
             | RetFrame    
             | Upcheck
             deriving (Show, Eq)
 
instance Arbitrary Frame where
    arbitrary = oneof [ InformationFrame <$> (arbitrary :: Gen String) <*> (arbitrary :: Gen Word32)
                      , DataPartFrame <$> (arbitrary :: Gen String)
                      , LinkFrame   <$> (arbitrary :: Gen String)
                      , UnlinkFrame <$> (arbitrary :: Gen String)
                      , return AckFrame
                      , return RetFrame
                      , OptionFrame <$> (arbitrary :: Gen [(String, String)])
                      , return Upcheck]

    shrink (OptionFrame os) = [OptionFrame nos | nos <- shrink os]
    shrink _ = []

-- TODO: Move to binary class instead of custom
{-instance Binary Frame where
    put = put . toByteString
    get = do
        (res, _) <- liftM fromByteString
        case res of 
            Right frame -> return frame 
            Left err -> error err-}

int2word :: Int -> Word32 
int2word = fromInteger . toInteger

word2int :: Word32 -> Int 
word2int = fromInteger . toInteger

instance FrameClass Frame where
    toByteString frame = BS.concat . BL.toChunks $ runPut $ case frame of 
                            InformationFrame u n -> putBounded $ putMarkedString u >> putWord32be n
                            DataPartFrame s      -> putBounded $ putMarkedString s 
                            LinkFrame   u        -> putBounded $ putMarkedString u
                            UnlinkFrame u        -> putBounded $ putMarkedString u
                            AckFrame             -> putShort
                            RetFrame             -> putShort
                            OptionFrame      os  -> putBounded $ putListLength os >> putOptions os
                            Upcheck              -> putShort
                         where 
                            putBegin          = putWord8 (frameType frame)
                            putShort          = putBegin
                            putListLength     = putWord32be . int2word . length
                            putBSLength       = putWord32be . int2word . BS.length
                            putMarkedString s = let bs = UTF.fromString s in putBSLength bs >> putByteString bs
                            putBounded      m = putBegin >> m
                            putOptions        = mapM_ (\(key,value) -> putMarkedString key >> putMarkedString value)

    fromByteString = runGet parseFrame
                            where
                                parseFrame :: Get Frame
                                parseFrame = do
                                    frameTypeId <- getWord8
                                    case frameTypeId of
                                        0x00 -> return InformationFrame `ap` parseMarkedString `ap` getWord32be
                                        0x01 -> return DataPartFrame `ap` parseMarkedString
                                        0x02 -> return LinkFrame   `ap` parseMarkedString
                                        0x03 -> return UnlinkFrame `ap` parseMarkedString
                                        0x04 -> return AckFrame
                                        0x05 -> return RetFrame
                                        0x06 -> return OptionFrame `ap` parseKeyValue
                                        0x07 -> return Upcheck
                                        _    -> fail "Unknown frame type!"
                                parseMarkedString = do
                                    len <- getWord32be
                                    body <- getByteString $ word2int len 
                                    return $ UTF.toString body

parseKeyValue :: Get [(String, String)]
parseKeyValue =  do
    pairsCount <- getWord32be
    mapM parsePair [1..pairsCount]
    where
        parsePair :: a -> Get (String, String)
        parsePair _ = do
            keyCount <- getWord32be
            key <- getByteString $ word2int keyCount
            valueCount <- getWord32be
            value <- getByteString $ word2int valueCount
            return (UTF.toString key, UTF.toString value)

-- Testing 
prop_toByteString :: Frame -> Bool
prop_toByteString f = case fst $ fromByteString $ toByteString f of 
                        Left  _ -> False
                        Right v -> v == f