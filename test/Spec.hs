import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI

import Lib

ballots :: [[(Question, RawVote)]] -> [Ballot]
ballots = map (ballot . M.fromList)

example :: Relation -> [Char] -> [(Int, [[Char]])] -> Assertion
example rel expected vs = assertEqual "vote winner mismatch" expected (map (T.head . optionName) . fst $ judge rel (S.toList options) count)
  where
    toOptions = map (Option . CI.mk . T.singleton)
    options = S.fromList . toOptions . concat . concatMap snd $ vs
    count = foldr (tallyVote options) (emptyCount options) $ concatMap (\(n, vs') -> replicate n (vote (map toOptions vs'))) vs

tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList
  [ "trivial count" ~: prefCountSet (M.fromList [("1",(S.fromList ["A","B"], [vote [["A"]]]))])
    ~=? M.fromList [("1", (S.fromList ["A", "B"], PrefCount . M.fromList $ [(("A", "B"), 1), (("B", "A"), 0)]))]
  , "Example 1" ~: example margin "D"
                           [(8, ["A", "C", "D", "B"])
                           ,(2, ["B", "A", "D", "C"])
                           ,(4, ["C", "D", "B", "A"])
                           ,(4, ["D", "B", "A", "C"])
                           ,(3, ["D", "C", "B", "A"])
                           ]
  , "Example 2" ~: example margin "BD"
                           [(3, ["A", "B", "C", "D"])
                           ,(2, ["C", "B", "D", "A"])
                           ,(2, ["D", "A", "B", "C"])
                           ,(2, ["D", "B", "C", "A"])
                           ]
  , "Example 3" ~: example margin "D"
                           [(12, ["A", "B", "C", "D"]), (6, ["A", "D", "B", "C"]), (9, ["B", "C", "D", "A"]), (15, ["C", "D", "A", "B"]),
                            (21, ["D", "B", "A", "C"])]
  , "Example 4" ~: example margin "AD"
                           [(6, ["A", "C", "D", "B"]), (1, ["B", "A", "D", "C"]), (3, ["C", "B", "D", "A"]), (3, ["D", "B", "A", "C"])
                           ,(2, ["D", "C", "B", "A"])]
  , "Example 5" ~: example margin "a"
                           [(3, ["a", "d", "e", "b", "c", "f"]), (3, ["b", "f", "e", "c", "d", "a"]), (4, ["c", "a", "b", "f", "d", "e"])
                           ,(1, ["d", "b", "c", "e", "f", "a"]), (4, ["d", "e", "f", "a", "b", "c"]), (2, ["e", "c", "b", "d", "f", "a"])
                           ,(2, ["f", "a", "c", "d", "b", "e"])]
  , "Example 7 (margin)" ~:
    example margin "a"
            [(6, ["a", "b", "c", "d"]), (8, ["ab", "cd"]), (8, ["ac", "bd"]), (18, ["ac", "d", "b"]), (8, ["acd", "b"]), (40, ["b", "acd"])
            ,(4, ["c", "b", "d", "a"]), (9, ["c", "d", "a", "b"]), (8, ["cd", "ab"]), (14, ["d", "a", "b", "c"]), (11, ["d", "b", "c", "a"])
            ,(4, ["d", "c", "a", "b"])]
  ]

main :: IO ()
main = defaultMain tests
