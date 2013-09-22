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

import ApplicationLevel
import ChannelLevel

import Test.Framework as TF (defaultMain, testGroup, Test)
--import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
        testGroup "QuickCheck ChannelLevel" [
                testProperty "toByteString"           prop_toByteString
                ]
        -- For future HUnit integration
        --testGroup "Point tests Data.Decimal" [
        --        testCase "pi to 3dp"     (dec 3 3142  @=? realFracToDecimal 3 piD)]
       ]

  