module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec
import System.IO
import System.Exit
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.CaseInsensitive as CI

import Lib
import Parse (input)

main :: IO ()
main = do
  x <- parse input "stdin" <$> T.getContents
  case x of
    Left err -> T.hPutStrLn stderr "parse error:" >> hPutStrLn stderr (show err) >> exitWith (ExitFailure 1)
    Right (vs, opts) -> do
      T.putStrLn $ header (T.concat [(T.pack . show . M.size $ vs), " voters"])
      T.putStrLn $ printVotes vs
      T.putStrLn ""
      T.putStrLn $ header "Results"
      T.putStrLn $ printResults $ judgeSet margin (prefCountSet $ aggregate opts (M.elems vs))

header :: Text -> Text
header title = T.unlines [T.replicate 16 "=", title, T.replicate 16 "="]

printVotes :: Map Voter Ballot -> Text
printVotes = T.intercalate "\n\n" . map (uncurry formatBallot) . M.toList
  where
    formatBallot (Voter v) b = T.concat [CI.original v, "\n", T.intercalate "\n" . map (uncurry formatVote) . M.toList . ballotVotes $ b]
    formatVote (Question q) v = T.concat [CI.original q, " ", T.intercalate "; " . map (T.intercalate ", " . map optionName) . votePrefs $ v]

printResults :: Map Question [Option] -> Text
printResults = T.intercalate "\n" . map (\(Question q, os) -> T.concat [CI.original q, " ", formatOpts os]) . M.toList
  where
    formatOpts [] = "no votes"
    formatOpts [x] = optionName x
    formatOpts xs = T.append "tie: " . T.intercalate ", " . map optionName $ xs
