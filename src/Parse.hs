module Parse where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI

import Text.Parsec
import Text.Parsec.Text

import Lib (Voter(..), votePrefs, Question(..), Option(..), Ballot, ballotVotes)
import qualified Lib as Lib

hspaces :: Parser ()
hspaces = skipMany (oneOf " \t")

nonspace :: Parser Char
nonspace = noneOf " \t\r\n"

vote :: Parser (Question, [[Option]])
vote = do
  q <- many1 nonspace
  hspaces
  ps <- sepEndBy1 (sepEndBy1 (many1 (noneOf ",;\r\n") <?> "vote option (e.g. \"A\")") (many1 (char ',' <* hspaces))) (many1 (char ';' <* hspaces))
  pure $ (Question . CI.mk . T.pack $ q, map (map (Option . CI.mk . T.strip . T.pack)) ps)

ballot :: Parser (Voter, Ballot)
ballot = do
  name <- manyTill (noneOf "\r\n") endOfLine
  hspaces
  vs <- sepEndBy1 (vote <?> "vote (e.g. \"2 A,B;C;D,E\")") endOfLine
  pure $ (Voter . CI.mk . T.strip . T.pack $ name, Lib.ballot $ M.fromList vs)

options :: [Ballot] -> Map Question (Set Option)
options =
  foldr (M.mergeWithKey (\_ v s -> Just $ S.union (S.fromList (concat (votePrefs v))) s) (M.map (S.fromList . concat . votePrefs)) id)
        M.empty
        . map ballotVotes

input :: Parser (Map Voter Ballot, Map Question (Set Option))
input = do
  spaces
  bs <- sepEndBy1 (ballot <?> "ballot (voter name followed by one vote per line)") (many1 endOfLine)
  pure (M.fromList bs, options (map snd bs))
