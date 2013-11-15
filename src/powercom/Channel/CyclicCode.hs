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
    ) where

import qualified Data.ByteString as BS 
import Math.Polynomial
import Data.List
import Data.Bits
import Data.Sequence (foldrWithIndex, fromList)
import Data.Word 
import Control.Monad

type Word4 = Word8 -- only for semantic concise
type Word3 = Word8
type Word7 = Word8

instance Fractional Int where
    (/) a b = if a == 0 then 0 else 1
    fromRational = undefined

word8ToPoly :: Word8 -> Poly Int
word8ToPoly wd = poly BE $ map (\i -> if testBit wd i then 1 else 0) [1 .. bitSize wd]

polyToWord8 :: Poly Int -> Word8 
polyToWord8 = foldrWithIndex coeff2Bit (fromIntegral 0) . fromList . polyCoeffs BE
    where
        coeff2Bit :: Int -> Int -> Word8 -> Word8 
        coeff2Bit i coeff acc = if coeff > 0 then acc `setBit` i else acc

codeCyclic :: BS.ByteString -> BS.ByteString
codeCyclic =  BS.pack . concatMap (\(a,b) -> [a, b]) . map codeWord8 . BS.unpack 

codeWord8 :: Word8 -> (Word7, Word7)
codeWord8 wd = (codeWord4 $ (wd .&. 0xF0) `shiftL` 4, codeWord4 $ wd .&. 0x0F)

codeWord4 :: Word4 -> Word7 -- n = 7 k = 4 
codeWord4 wd = polyToWord8 finalPoly
    where
        polyGen     = poly BE [1,0,1,1]
        wordPoly    = word8ToPoly wd
        shiftedPoly = wordPoly `multPoly` (poly BE [1,0,0,0]) -- (n - k) = 3
        reminder    = shiftedPoly `remPoly` polyGen
        finalPoly   = shiftedPoly `addPoly` reminder

decodeCyclic :: BS.ByteString -> Maybe BS.ByteString
decodeCyclic = mPack . sequence . map decodeWord8 . makePairs . BS.unpack
    where
        mPack = liftM BS.pack
        makePairs ls = zip ls $ drop 1 ls

decodeWord8 :: (Word7, Word7) -> Maybe Word8 
decodeWord8 (a, b) = (mShiftL4 $ decodeWord4 a) `mOr` (decodeWord4 b)
    where
        mShiftL4 = liftM $ (flip shiftL) 4
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