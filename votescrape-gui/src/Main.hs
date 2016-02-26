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
import Data.IORef

import Graphics.UI.Gtk

import Network.HTTP.Types
import Network.HTTP.Client (CookieJar, createCookieJar, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import System.IO.Error

import Scrape

import Text.Parsec (parse, spaces)
import VoteCount.Ballot
import VoteCount.Condorcet
import VoteCount.Format

import VoteCount.Parse (ballotFile, anonBallot)

main :: IO ()
main = do
  cookies <- newIORef (createCookieJar [])
  manager <- newManager tlsManagerSettings

  initGUI

  rootBuilder <- builderNew
  builderAddFromString rootBuilder (decodeUtf8 $(embedFile "gui/root_window.glade"))

  window <- builderGetObject rootBuilder castToWindow ("root_window" :: Text)
  on window objectDestroy mainQuit

  fromEntry <- builderGetObject rootBuilder castToEntry ("from_entry" :: Text)
  toEntry <- builderGetObject rootBuilder castToEntry ("to_entry" :: Text)

  notebook <- builderGetObject rootBuilder castToNotebook ("notebook" :: Text)
  scrapePage <- builderGetObject rootBuilder castToBox ("scrape_page" :: Text)
  filePage <- builderGetObject rootBuilder castToFileChooserButton ("file_page" :: Text)

  countButton <- builderGetObject rootBuilder castToButton ("count_button":: Text)
  on countButton buttonActivated $ do
    page <- fromJust <$> (notebookGetNthPage notebook =<< notebookGetCurrentPage notebook)
    cs <- readIORef cookies
    fromJust $ lookup page
      [ (castToWidget scrapePage
        , join $ scrape cs window <$> entryGetText fromEntry <*> entryGetText toEntry)
      , (castToWidget filePage
        , parseFile window =<< fileChooserGetFilename filePage)
      ]

  loginButton <- builderGetObject rootBuilder castToButton ("login_button" :: Text)
  on loginButton buttonActivated $ do
    dialogBuilder <- builderNew
    builderAddFromString dialogBuilder (decodeUtf8 $(embedFile "gui/login_dialog.glade"))

    d <- builderGetObject dialogBuilder castToDialog ("login_dialog" :: Text)
    set d [ windowTransientFor := window ]

    unameEntry <- builderGetObject dialogBuilder castToEntry ("username_entry" :: Text)
    passEntry <- builderGetObject dialogBuilder castToEntry ("password_entry" :: Text)

    ok <- builderGetObject dialogBuilder castToButton ("ok" :: Text)
    on ok buttonActivated $ do
      set d [ widgetSensitive := False ]
      uname <- entryGetText unameEntry
      pass <- entryGetText passEntry
      void . async $ do
        res <- login manager uname pass
        postGUIAsync $ do
          case res of
            Left err -> dialog MessageError (T.append "HTTP error " (T.pack (show (statusCode err)))) (castToWindow d) (Just . T.pack $ show err)
            Right cs -> writeIORef cookies cs
          widgetDestroy d

    cancel <- builderGetObject dialogBuilder castToButton ("cancel" :: Text)
    on cancel buttonActivated $ widgetDestroy d

    widgetShowAll d

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

scrape :: CookieJar -> Window -> Text -> Text -> IO ()
scrape _  w "" _ = dialog MessageError "Input error" w (Just "\"From post\" URI must be specified")
scrape cs w x "" =
  case parsePostURI (T.unpack x) of
    Left err -> dialog MessageError "Input error" w (Just . T.pack $ err)
    Right x' -> spawn (\rw -> do rs <- fmap process' <$> getVotes cs x' Nothing
                                 seq (force (rs ^?_Right._1)) $ pure (displayResults rw rs))
scrape cs w x y =
  case (parsePostURI (T.unpack x), parsePostURI (T.unpack y)) of
    (Left err, _) -> dialog MessageError "Input error" w (Just $ T.pack err)
    (_, Left err) -> dialog MessageError "Input error" w (Just $ T.pack err)
    (Right x', Right y') -> spawn (\rw -> do rs <- fmap process' <$> getVotes cs x' (Just y')
                                             seq (force (rs ^?_Right._1)) $ pure (displayResults rw rs))

process :: BallotSet -> Either Text (Text, Text, Text)
process bs = if questionCount == 0 then Left "No votes found" else Right (a,b,c)
  where
    a = printResults bs (V.zip optionCounts voteCounts)
    b = printCounts bs voteCounts
    c = printVotes bs
    questionCount = length (questions bs)
    optionCounts = V.generate questionCount (\i -> fromIntegral . length . options bs $ fromIntegral i)
    voteCounts = V.map (uncurry count) $ V.zip optionCounts (aggregate (fromIntegral questionCount) (V.toList . votes $ bs))

process' :: ([(PostData Text)], [String]) -> (Either Text (Text, Text, Text), [Text])
process' (rs, htmlParseErrs) =
  ( txt
  , map (T.append "error parsing post HTML: " . T.pack) htmlParseErrs ++ voteParseErrs
  )
  where
    txt = process $ mkBallotSet . M.fromListWith M.union $ voteMap
    (voteMap, voteParseErrs) =
      foldr (\post accum@(vs, errs) ->
               case post ^. postBody of
                 (x, Left err) ->
                   (vs, T.concat [ "failed parsing vote from ", post ^. postAuthor, ":\n"
                                 , x, "\n"
                                 , T.pack (show err)
                                 ] : errs)
                 (_, Right x) -> ((CI.mk $ post ^. postAuthor, x):vs, errs)
            ) ([], []) $ map (postBody %~ (\x -> (x, parse (spaces *> anonBallot) "" x))) rs

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

fillResults :: ResultWindow -> Either Text (Text, Text, Text) -> IO ()
fillResults rw (Left msg) = do
  widgetHide (rwSpinner rw)
  dialog MessageError msg (rwWindow rw) Nothing
  widgetDestroy (rwWindow rw)
fillResults rw (Right (results, counts, vs)) = do
  widgetHide (rwSpinner rw)
  textBufferSetText (rwWinnerB rw) results
  textBufferSetText (rwCountsB rw) counts
  textBufferSetText (rwVoteB rw) vs

displayResults :: ResultWindow -> Either VoteErr (Either Text (Text, Text, Text), [Text]) -> IO ()
displayResults rw (Right (txt, errs)) = do
  fillResults rw txt
  unless (null errs) $ dialog MessageWarning "Parser failures" (rwWindow rw) (Just $ T.unlines errs)
displayResults rw (Left err) = do
  widgetHide (rwSpinner rw)
  dialog MessageError "Error fetching votes"  (rwWindow rw) (Just $ errMsg err)
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
