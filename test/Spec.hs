module Spec where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Map.Strict as M

import Lib

ballots :: [[(Question, RawVote)]] -> [Ballot]
ballots = map (ballot . M.fromList)

opts :: [(Question, [Option])] -> OptionSet
opts = OptionSet . M.fromList

tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList
  [ "trivial tally" ~: tally (ballots [[("1", [["A"]])]]) (opts [("1",["A","B"])])
    ~=? Tally (M.fromList [("1",PrefCount (M.fromList [(("A","B"),1),(("B","A"),0)]))])
  ]

main :: IO ()
main = defaultMain tests
