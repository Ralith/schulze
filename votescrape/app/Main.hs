module Main where

import Control.Lens

import qualified Data.CaseInsensitive as CI
import Data.Foldable
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import qualified Data.Vector as V
import System.Environment
import System.Exit
import System.IO

import Text.HTML.TagSoup
import Text.Parsec (parse)

import Network.HTTP.Types.Status

import Scrape
import Control.Monad

import VoteCount.Parse (anonBallot)
import VoteCount.Ballot
import VoteCount.Format
import VoteCount.Condorcet

usage :: String
usage = "<start url> [end url]"

main :: IO ()
main = do
  args <- getArgs
  let printUsage = putStrLn . (++ ": " ++ usage) =<< getProgName
  case args of
    [] -> printUsage
    [startURI] ->
      case parsePostURI startURI of
        Left err -> hPutStr stderr "not a valid post URL: " >> hPutStrLn stderr err >>  printUsage >> exitFailure
        Right start -> process =<< getVotes start Nothing
    [startURI, endURI] ->
      case (parsePostURI startURI, parsePostURI endURI) of
        (Left err, _) -> do
          hPutStr stderr "start URL not a valid post URL: "
          hPutStrLn stderr err
          printUsage
          exitFailure
        (Right _, Left err) -> do
          hPutStr stderr "end URL not a valid post URL: "
          hPutStrLn stderr err
          printUsage
          exitFailure
        (Right start, Right end) -> process =<< getVotes start (Just end)
    _ -> printUsage >> exitFailure

process :: Either Status ([(PostData Text)], [String]) -> IO ()
process (Right (vs, errs)) = do
  forM_ errs $ \err -> do
    hPutStrLn stderr $ "error scraping post: " ++ err
  unless (null errs) $ hPutStrLn stderr "continuing"
  let parsed = map (postBody %~ (\x -> (x, parse anonBallot "" x))) vs
  clean <- foldrM (\v clean -> do
    case v ^. postBody of
      (x, Left err) -> do
        hPutStr stderr "error parsing vote from "
        T.hPutStr stderr $ v ^. postAuthor
        T.hPutStrLn stderr ": "
        T.hPutStrLn stderr x
        hPutStrLn stderr (show err)
        pure clean
      (_, Right x) -> pure ((CI.mk $ v ^. postAuthor, x):clean)
      ) [] parsed
  let bs = mkBallotSet . M.fromList $ clean
  T.putStr $ printCount bs
process (Left err) = do
  hPutStr stderr $ "server returned HTTP " ++ show (statusCode err) ++ ": "
  T.hPutStrLn stderr $ (decodeUtf8With lenientDecode $ statusMessage err ^. from strict) ^. strict
  exitFailure
