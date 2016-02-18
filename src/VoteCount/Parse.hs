module VoteCount.Parse (voteLine, ballot, input) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI

import Text.Parsec
import Text.Parsec.Text

import VoteCount.Ballot

hspaces :: Parser ()
hspaces = skipMany (oneOf " \t")

nonspace :: Parser Char
nonspace = noneOf " \t\r\n"

-- question, preferences
voteLine :: Parser (CI Text, [[CI Text]])
voteLine = (do
  q <- many1 nonspace
  hspaces
  ps <- sepEndBy1 (sepEndBy1 (many1 (noneOf ",;\r\n") <?> "vote option (e.g. \"A\")") (many1 (char ',' <* hspaces))) (many1 (char ';' <* hspaces))
  pure $ (CI.mk . T.pack $ q, map (map (CI.mk . T.strip . T.pack)) ps)
  ) <?> "vote (e.g. \"2 A,B;C;D,E\")"

-- voter, question to preferences
ballot :: Parser (CI Text, Map (CI Text) [[CI Text]])
ballot = (do
  name <- manyTill (noneOf "\r\n") endOfLine
  hspaces
  vs <- sepEndBy1 voteLine endOfLine
  pure $ (CI.mk . T.strip . T.pack $ name, M.fromList vs)
  ) <?> "ballot (voter name followed by one vote per line)"

input :: Parser BallotSet
input = do
  spaces
  bs <- M.fromList <$> sepEndBy1 ballot (many1 endOfLine)
  pure $ mkBallotSet bs
