import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import Lib

ballots :: [[(Question, RawVote)]] -> [Ballot]
ballots = map (ballot . M.fromList)

example1 :: (Set Option, PrefCount)
example1 = (options, foldr (tallyVote options) (emptyCount options) votes)
  where
    options = (S.fromList ["A", "B", "C", "D"])
    votes = concatMap (\(n, vs) -> replicate n . vote . map (: []) $ vs)
                      [(8, ["A", "C", "D", "B"])
                      ,(2, ["B", "A", "D", "C"])
                      ,(4, ["C", "D", "B", "A"])
                      ,(4, ["D", "B", "A", "C"])
                      ,(3, ["D", "C", "B", "A"])
                      ]

tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList
  [ "trivial count" ~: prefCountSet (M.fromList [("1",(S.fromList ["A","B"], [vote [["A"]]]))])
    ~=? M.fromList [("1", (S.fromList ["A", "B"], PrefCount . M.fromList $ [(("A", "B"), 1), (("B", "A"), 0)]))]
  , "Example 1 consistency" ~: fst (judge margin (S.toList $ fst example1) (snd example1)) ~=? ["D"]
  ]

main :: IO ()
main = defaultMain tests
