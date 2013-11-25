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
module Channel.CyclicCode (
      codeCyclic
    , decodeCyclic
    , prop_codeDecodeEq
    , prop_polyConverting
    , prop_Word8BitCount
    , prop_quotRemPoly
    , prop_simpleCoding
    , prop_fullCodingDecoding
    , prop_falseWord4Coding
    , prop_falseWord8Coding
    ) where

import qualified Data.ByteString as BS 
import qualified Data.ByteString.Char8 as CH
import Math.Polynomial
import Data.Bits
import Data.Sequence (foldrWithIndex, fromList)
import Data.Word 
import Control.Monad
import Test.QuickCheck hiding ( (.&.) ) 

type Word4 = Word8 -- only for semantic concise
type Word7 = Word8

data Bit = Bit Bool 
    deriving Eq 

instance Show Bit where
    show (Bit val) = if val then "1" else "0"

instance Num Bit where
    (+) (Bit a) (Bit b) = case (a, b) of
        (True, True) -> Bit False
        (_, True)    -> Bit True
        (True, _)    -> Bit True
        _            -> Bit False

    (-) = (+)
    (*) (Bit a) (Bit b) = case (a, b) of
        (False, _)   -> Bit False
        (_, False)   -> Bit False
        _            -> Bit True

    abs ba = ba 
    signum ba = ba
    fromInteger int = Bit $ int > 0

instance Fractional Bit where
    (/) ba _ = ba
    fromRational = undefined

word8ToPoly :: Word8 -> Poly Bit
word8ToPoly wd = poly LE $ map 
    (Bit . testBit wd) [0 .. bitSize wd - 1]

polyToWord8 :: Poly Bit -> Word8 
polyToWord8 = foldrWithIndex coeff2Bit 0 . fromList . polyCoeffs LE
    where
        coeff2Bit :: Int -> Bit -> Word8 -> Word8 
        coeff2Bit i (Bit b) acc = if b then acc `setBit` i else acc

codeCyclic :: BS.ByteString -> BS.ByteString
codeCyclic = BS.pack . concatMap (\(a,b) -> [a, b]) . map codeWord8 . BS.unpack 

codeWord8 :: Word8 -> (Word7, Word7)
codeWord8 wd = (codeWord4 highWord, codeWord4 lowWord)
    where highWord = (wd .&. 0xF0) `shiftR` 4
          lowWord  = wd .&. 0x0F

codeWord4 :: Word4 -> Word7 -- n = 7 k = 4 
codeWord4 wd = polyToWord8 finalPoly
    where
        polyGen     = poly BE [1,0,1,1]
        wordPoly    = word8ToPoly wd
        shiftedPoly = wordPoly `multPoly` poly BE [1, 0, 0, 0] -- (n - k) = 3
        reminder    = shiftedPoly `remPoly` polyGen
        finalPoly   = shiftedPoly `addPoly` reminder

decodeCyclic :: BS.ByteString -> Maybe BS.ByteString
decodeCyclic = mPack . mapM decodeWord8 . makePairs . BS.unpack
    where
        mPack = liftM BS.pack

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (_:[]) = []
makePairs (x1:x2:xs) = (x1, x2) : makePairs xs

decodeWord8 :: (Word7, Word7) -> Maybe Word8 
decodeWord8 (a, b) = mShiftL4 (decodeWord4 a) `mOr` decodeWord4 b
    where
        mShiftL4 = liftM $ flip shiftL 4
        mOr = liftM2 (.|.)

decodeWord4 :: Word7 -> Maybe Word4 
decodeWord4 wd = if syndrome == zero then Just finalWord else Nothing
    where
        polyGen     = poly BE [1,0,1,1]
        wordPoly    = word8ToPoly wd
        syndrome    = wordPoly `remPoly` polyGen
        finalWord   = (wd `shiftR` 3) .&. 0x0F

-- Testing
prop_codeDecodeEq :: Word8 -> Bool
prop_codeDecodeEq wd = case decodeWord8 $ codeWord8 wd of
    Nothing  -> False
    Just val -> wd == val

prop_polyConverting :: Word8 -> Bool 
prop_polyConverting wd = wd == polyToWord8 (word8ToPoly wd)

prop_Word8BitCount :: Word8 -> Bool
prop_Word8BitCount wd = bitSize wd == 8

prop_quotRemPoly :: Word8 -> Word8 -> Bool
prop_quotRemPoly a b = (b == 0) || (newa == pa)
    where newa   = addPoly (multPoly q pb) r
          (q, r) = quotRemPoly pa pb
          pa = word8ToPoly a 
          pb = word8ToPoly b

prop_simpleCoding :: Word8 -> Bool
prop_simpleCoding wd = case decodeWord4 $ codeWord4 cutedWd of
    Nothing -> False
    Just val -> val == cutedWd
    where cutedWd = wd .&. 0x0F

prop_fullCodingDecoding :: String -> Bool
prop_fullCodingDecoding s = case decodeCyclic $ codeCyclic bs of
    Nothing -> False
    Just val -> val == bs 
    where bs = CH.pack s

newtype BitError = BitError Int 
    deriving (Eq, Show)

instance Arbitrary BitError where
    arbitrary = oneof $ map (return . BitError) [0 .. 7]
    shrink _ = []

prop_falseWord4Coding :: Word8 -> BitError -> Bool
prop_falseWord4Coding wd (BitError i) = case decodeWord4 $ complementBit (codeWord4 cutedWd) i of 
    Nothing -> True
    Just _ -> False 
    where cutedWd = wd .&. 0x0F

prop_falseWord8Coding :: Word8 -> BitError -> BitError -> Bool
prop_falseWord8Coding wd (BitError i1) (BitError i2) = 
    case decodeWord8 (cwd1 `complementBit` i1, cwd2 `complementBit` i2) of
        Nothing -> True
        Just _ -> False
    where 
        (cwd1, cwd2) = codeWord8 wd