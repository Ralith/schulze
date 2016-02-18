module Main where

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import System.Environment
import System.Exit
import System.IO

import Text.HTML.TagSoup

import Network.HTTP.Types.Status

import Scrape

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
        Right start -> process =<< getPosts start Nothing
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
        (Right start, Right end) -> process =<< getPosts start (Just end)
    _ -> printUsage >> exitFailure

process :: Either Status [(Post, PostData [Tag Text])] -> IO ()
process (Right posts) = do
  putStr $ show (length posts) ++ " posts, "
  let votes = map (getVote . (postBody %~ bolded) . snd) posts ^.. folded._Just
  putStrLn $ show (length votes) ++ " votes"
  T.putStrLn . T.concat $ map (\x ->
                                 T.concat [ x ^. postAuthor, "\n"
                                          , x ^. postBody, "\n\n"
                                          ]) votes
process (Left err) = do
  hPutStr stderr $ "server returned HTTP " ++ show (statusCode err) ++ ": "
  T.hPutStrLn stderr $ (decodeUtf8With lenientDecode $ statusMessage err ^. from strict) ^. strict
  exitFailure
