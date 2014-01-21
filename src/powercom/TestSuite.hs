-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Gushcha Anton 2013-2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Default test suit.
-----------------------------------------------------------------------------
module Main (main) where

import Channel.Frame
import Channel.CyclicCode

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- | Runs application unit tests.
main :: IO ()
main = defaultMain tests

-- | List of all application tests.
tests :: [TF.Test]
tests = [
        testGroup "QuickCheck Channel.Frame" [
                testProperty "toByteString"           prop_toByteString
            ],
        testGroup "QuickCheck Channel.CyclicCode" [
                 testProperty "Coding decoding same word" prop_codeDecodeEq
               , testProperty "Convertion word to/from poly" prop_polyConverting
               , testProperty "Bits in word8" prop_Word8BitCount
               , testProperty "Poly division correctness" prop_quotRemPoly
               , testProperty "Simple coding word4" prop_simpleCoding
               , testProperty "Full bytestring coding/decoding" prop_fullCodingDecoding
               , testProperty "Error coding word4" prop_falseWord4Coding
               , testProperty "Error coding word8" prop_falseWord8Coding
            ]
       ]

  