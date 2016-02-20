import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (findIndex)
import Text.Parsec (parse)

import VoteCount.Schulze
import VoteCount.Condorcet
import qualified VoteCount.Parse as Parse

example :: Relation -> [Char] -> [(Int, [[Char]])] -> Assertion
example rel expected vs =
  assertEqual "vote winner mismatch" expected (map (\(Option i) -> options !! fromIntegral i) $ judge rel optCount prefs)
  where
    options = S.toList . S.fromList . concat . concatMap snd $ vs
    optCount = fromIntegral (length options) :: Word
    prefs = count optCount . concatMap (\(n, b) -> replicate n (mkVote $ map (map (\x -> Option . fromIntegral . fromJust $ findIndex (== x) options)) b)) $ vs

e7data :: [(Int, [[Char]])]
e7data = [(6, ["a", "b", "c", "d"]), (8, ["ab", "cd"]), (8, ["ac", "bd"]), (18, ["ac", "d", "b"]), (8, ["acd", "b"]), (40, ["b", "acd"])
         ,(4, ["c", "b", "d", "a"]), (9, ["c", "d", "a", "b"]), (8, ["cd", "ab"]), (14, ["d", "a", "b", "c"]), (11, ["d", "b", "c", "a"])
         ,(4, ["d", "c", "a", "b"])]

tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList
  [ "Example 1" ~: example margin "D"
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
  , "Example 7 (margin)" ~: example margin "a" e7data
  , "Example 7 (ratio)" ~: example ratio "b" e7data
  , "Example 7 (winning votes)" ~: example winning "d" e7data
  , "well-formed vote parsing" ~:
      Right ("1", [["A", "Plan Tran"], ["B"]]) ~=? parse Parse.voteLine "" "1 A, Plan Tran; B"
  , "mangled vote parsing" ~:
      Right ("1", [["A", "Plan Tran"], ["B"]]) ~=? parse Parse.voteLine "" "1 .A   ,, , Plan Tran,  ; ; B;;;"
  , "question suffix stripping" ~:
      Right ("1", [["A", "Plan Tran"], ["B"]]) ~=? parse Parse.voteLine "" "1. A, Plan Tran; B"
  , "empty vote parsing" ~:
      Right ("1", []) ~=? parse Parse.voteLine "" "1"
  , "ballot" ~:
      Right ("Voter", M.fromList [("1", [["A"]]), ("2", []), ("3", [["B"]])]) ~=? parse Parse.ballot "" "Voter\n1 a\n2\n3 b\n\nasdf"
  , "vote changes" ~:
      Right (M.fromList [("Voter", M.fromList [("1", [["C"]]), ("2", [["B"]])])])
      ~=? parse Parse.ballotFile' "" "Voter\n1 a\n2 b\n\nVoter\n1 c"
  ]

main :: IO ()
main = defaultMain tests
