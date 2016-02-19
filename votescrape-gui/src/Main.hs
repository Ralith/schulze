module Main where

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Lens hiding (set)
import Control.Monad

import qualified Data.CaseInsensitive as CI
import Data.FileEmbed
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import Graphics.UI.Gtk

import Network.HTTP.Types.Status

import System.IO.Error

import Scrape

import Text.Parsec (parse)
import VoteCount.Ballot
import VoteCount.Condorcet
import VoteCount.Format

import VoteCount.Parse (ballotFile, anonBallot)

main :: IO ()
main = do
  initGUI
  rootBuilder <- builderNew
  builderAddFromString rootBuilder (decodeUtf8 $(embedFile "gui/root_window.glade"))

  window <- builderGetObject rootBuilder castToWindow ("root_window" :: Text)
  on window objectDestroy mainQuit

  fromEntry <- builderGetObject rootBuilder castToEntry ("from_entry" :: Text)
  toEntry <- builderGetObject rootBuilder castToEntry ("to_entry" :: Text)

  notebook <- builderGetObject rootBuilder castToNotebook ("notebook" :: Text)
  scrapePage <- builderGetObject rootBuilder castToGrid ("scrape_page" :: Text)
  filePage <- builderGetObject rootBuilder castToFileChooserButton ("file_page" :: Text)

  countButton <- builderGetObject rootBuilder castToButton ("count_button":: Text)
  on countButton buttonActivated $ do
    page <- fromJust <$> (notebookGetNthPage notebook =<< notebookGetCurrentPage notebook)
    fromJust $ lookup page
      [ (castToWidget scrapePage
        , join $ scrape window <$> entryGetText fromEntry <*> entryGetText toEntry)
      , (castToWidget filePage
        , parseFile window =<< fileChooserGetFilename filePage)
      ]

  widgetShowAll window
  mainGUI

parseFile :: Window -> Maybe FilePath -> IO ()
parseFile window Nothing = dialog MessageError "No file selected" window Nothing
parseFile _ (Just file) = spawn $ \rw -> do
  x <- tryIOError $ T.readFile file
  case parse ballotFile file <$> x of
    Left err -> pure $ do
      dialog MessageError "IO error" (rwWindow rw) (Just . T.pack $ displayException err)
      widgetDestroy (rwWindow rw)
    Right (Left err) -> pure $ do
      dialog MessageError "Parse error" (rwWindow rw) (Just . T.pack $ show err)
      widgetDestroy (rwWindow rw)
    Right (Right bs) -> do
      info <- evaluate (process bs)
      pure $ fillResults rw info

scrape :: Window -> Text -> Text -> IO ()
scrape w "" _ = dialog MessageError "Input error" w (Just "\"From post\" URI must be specified")
scrape w x "" =
  case parsePostURI (T.unpack x) of
    Left err -> dialog MessageError "Input error" w (Just . T.pack $ err)
    Right x' -> spawn (\rw -> do rs <- fmap process' <$> getVotes x' Nothing
                                 seq (force (rs ^?_Right._1)) $ pure (displayResults rw rs))
scrape w x y =
  case (parsePostURI (T.unpack x), parsePostURI (T.unpack y)) of
    (Left err, _) -> dialog MessageError "Input error" w (Just $ T.pack err)
    (_, Left err) -> dialog MessageError "Input error" w (Just $ T.pack err)
    (Right x', Right y') -> spawn (\rw -> do rs <- fmap process' <$> getVotes x' (Just y')
                                             seq (force (rs ^?_Right._1)) $ pure (displayResults rw rs))

process :: BallotSet -> Maybe (Text, Text, Text)
process bs = if questionCount == 0 then Nothing else Just (a,b,c)
  where
    a = printResults bs (V.zip optionCounts voteCounts)
    b = printCounts bs voteCounts
    c = printVotes bs
    questionCount = length (questions bs)
    optionCounts = V.generate questionCount (\i -> fromIntegral . length . options bs $ fromIntegral i)
    voteCounts = V.map (uncurry count) $ V.zip optionCounts (aggregate (fromIntegral questionCount) (V.toList . votes $ bs))

process' :: ([(PostData Text)], [String]) -> (Maybe (Text, Text, Text), [Text])
process' (rs, htmlParseErrs) =
  ( txt
  , map (T.append "error parsing post HTML: " . T.pack) htmlParseErrs ++ voteParseErrs
  )
  where
    txt = process $ mkBallotSet . M.fromList $ voteMap
    (voteMap, voteParseErrs) =
      foldr (\post accum@(vs, errs) ->
               case post ^. postBody of
                 (x, Left err) ->
                   (vs, T.concat [ "failed parsing vote from ", post ^. postAuthor, ":\n"
                                 , x, "\n"
                                 , T.pack (show err)
                                 ] : errs)
                 (_, Right x) -> ((CI.mk $ post ^. postAuthor, x):vs, errs)
            ) ([], []) $ map (postBody %~ (\x -> (x, parse anonBallot "" x))) rs

data ResultWindow = ResultWindow { rwWindow :: Window
                                 , rwSpinner :: Widget
                                 , rwWinnerB :: TextBuffer
                                 , rwCountsB :: TextBuffer
                                 , rwVoteB :: TextBuffer}

spawn :: (ResultWindow -> IO (IO ())) -> IO ()
spawn f = do
  rw <- mkResultWindow
  op <- async $ do
    g <- f rw
    postGUIAsync g
  on (rwWindow rw) unrealize $ void $ async (cancel op)
  pure ()

fillResults :: ResultWindow -> Maybe (Text, Text, Text) -> IO ()
fillResults rw Nothing = do
  widgetHide (rwSpinner rw)
  dialog MessageError "No votes found" (rwWindow rw) Nothing
  widgetDestroy (rwWindow rw)
fillResults rw (Just (results, counts, vs)) = do
  textBufferSetText (rwWinnerB rw) results
  textBufferSetText (rwCountsB rw) counts
  textBufferSetText (rwVoteB rw) vs

displayResults :: ResultWindow -> Either Status (Maybe (Text, Text, Text), [Text]) -> IO ()
displayResults rw (Right (txt, errs)) = do
  fillResults rw txt
  unless (null errs) $ dialog MessageWarning "Parser failures" (rwWindow rw) (Just $ T.unlines errs)
displayResults rw (Left err) = do
  widgetHide (rwSpinner rw)
  dialog MessageError (T.append "HTTP error " (T.pack (show (statusCode err)))) (rwWindow rw) (Just . T.pack $ show err)
  widgetDestroy (rwWindow rw)

dialog :: MessageType -> Text -> Window -> Maybe Text -> IO ()
dialog mty ty parent msg = do
  d <- messageDialogNew (Just parent) [DialogModal, DialogDestroyWithParent] mty ButtonsClose ("" :: Text)
  set d [messageDialogText := Just ty, messageDialogSecondaryText := msg]
  dialogRun d
  widgetDestroy d

mkResultWindow :: IO ResultWindow
mkResultWindow = do
  b <- builderNew
  builderAddFromString b (decodeUtf8 $(embedFile "gui/result_window.glade"))
  window <- builderGetObject b castToWindow ("result_window" :: Text)
  spinner <- builderGetObject b castToWidget ("spinner" :: Text)
  winnerb <- builderGetObject b castToTextBuffer ("winner_buffer" :: Text)
  countsb <- builderGetObject b castToTextBuffer ("counts_buffer" :: Text)
  voteb <- builderGetObject b castToTextBuffer ("vote_buffer" :: Text)
  widgetShowAll window
  pure $ ResultWindow window spinner winnerb countsb voteb
