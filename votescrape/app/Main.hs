module Main where

import Control.Lens

import qualified Data.CaseInsensitive as CI
import Data.Foldable
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import System.Environment
import System.Exit
import System.IO

import Text.Parsec (parse, spaces)

import Network.HTTP.Client (createCookieJar, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status

import Scrape
import Control.Monad

import VoteCount.Parse (anonBallot)
import VoteCount.Ballot
import VoteCount.Format

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
        Right start -> begin start Nothing
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
        (Right start, Right end) -> begin start (Just end)
    _ -> printUsage >> exitFailure

begin :: (ThreadPage, Post) -> Maybe (ThreadPage, Post) -> IO ()
begin start end = do
  mgr <- newManager tlsManagerSettings
  putStrLn "fetching posts..."
  r <- getVotes mgr (createCookieJar []) start end
  putStrLn "done"
  process r

process :: Either VoteErr ([(PostData Text)], [String]) -> IO ()
process (Right (vs, errs)) = do
  putStr "scraping votes..."
  hFlush stdout
  forM_ errs $ \err -> do
    hPutStrLn stderr $ "error scraping post: " ++ err
  unless (null errs) $ hPutStrLn stderr "continuing"
  let parsed = map (postBody %~ (\x -> (x, parse (spaces *> anonBallot) "" x))) vs
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
  let bs = mkBallotSet . M.fromListWith M.union $ clean
  putStrLn " done"
  T.putStr $ printCount bs
process (Left err) = do
  hPutStr stderr $ "error fetching votes: "
  T.hPutStrLn stderr $ errMsg err
  exitFailure
