module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec (parse)
import System.Environment
import System.IO
import System.Exit
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Array.IArray
import Data.Foldable

import VoteCount.Condorcet as Condorcet
import VoteCount.Schulze
import VoteCount.Ballot as Ballot
import VoteCount.Parse
import VoteCount.Format

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> main' "stdin" =<< T.getContents
    [file] -> main' file =<< T.readFile file
    _ -> T.hPutStrLn stderr "too many arguments (expected nothing or filename)" >> exitFailure

main' :: String -> Text -> IO ()
main' file contents =
  case parse input file contents of
    Left err -> T.hPutStrLn stderr "parse error:" >> hPutStrLn stderr (show err) >> exitFailure
    Right r -> T.putStr (printCount r)
