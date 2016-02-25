module VoteCount.Parse (voteLine, anonBallot, ballot, ballotFile, ballotFile') where

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

nonQuestionNameChars :: [Char]
nonQuestionNameChars = " \t\r.:)"

-- question, preferences
voteLine :: Parser (CI Text, [[CI Text]])
voteLine = (do
  optional (char '(')
  q <- many1 (noneOf ('\n':nonQuestionNameChars) <?> "question name")
  skipMany (oneOf nonQuestionNameChars)
  ps <- maybe [] id <$> (optionMaybe $ sepEndBy1 (sepEndBy1 (many1 (noneOf ",;\r\n") <?> "vote option (e.g. \"A\")")
                                                            (many1 (char ',' <* hspaces)))
                                                 (many1 (char ';' <* hspaces)))
  pure $ (CI.mk . T.pack $ q, map (map (CI.mk . T.strip . T.pack)) ps)
  ) <?> "vote (e.g. \"2 A,B;C;D,E\")"

anonBallot :: Parser (Map (CI Text) [[CI Text]])
anonBallot = (do
  vs <- sepEndBy1 voteLine endOfLine
  pure $ M.fromList vs
  ) <?> "ballot (one vote per line)"

-- voter, question to preferences
ballot :: Parser (CI Text, Map (CI Text) [[CI Text]])
ballot = (do
  name <- manyTill (noneOf "\r\n" <?> "voter name") endOfLine
  spaces
  vs <- anonBallot
  pure $ (CI.mk . T.strip . T.pack $ name, vs)
  ) <?> "ballot (voter name followed by one vote per line)"

ballotFile' :: Parser (Map (CI Text) (Map (CI Text) [[CI Text]]))
ballotFile' = do
  spaces
  M.fromListWith M.union . filter (not . M.null . snd) <$> sepEndBy1 ballot (many1 endOfLine)

ballotFile :: Parser BallotSet
ballotFile = mkBallotSet <$> ballotFile'
