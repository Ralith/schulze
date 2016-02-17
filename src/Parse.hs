module Parse ( input

             , Result
             , vote
             , votes
             , voters
             , voterName
             , questions
             , questionName
             , options
             , optionName
             , Parse.dropOption
             ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Maybe (fromJust)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI

import Text.Parsec
import Text.Parsec.Text

import Ballot as Ballot
import Condorcet

hspaces :: Parser ()
hspaces = skipMany (oneOf " \t")

nonspace :: Parser Char
nonspace = noneOf " \t\r\n"

-- question, preferences
voteLine :: Parser (CI Text, [[CI Text]])
voteLine = do
  q <- many1 nonspace
  hspaces
  ps <- sepEndBy1 (sepEndBy1 (many1 (noneOf ",;\r\n") <?> "vote option (e.g. \"A\")") (many1 (char ',' <* hspaces))) (many1 (char ';' <* hspaces))
  pure $ (CI.mk . T.pack $ q, map (map (CI.mk . T.strip . T.pack)) ps)

-- voter, question to preferences
ballot :: Parser (CI Text, Map (CI Text) [[CI Text]])
ballot = do
  name <- manyTill (noneOf "\r\n") endOfLine
  hspaces
  vs <- sepEndBy1 (voteLine <?> "vote (e.g. \"2 A,B;C;D,E\")") endOfLine
  pure $ (CI.mk . T.strip . T.pack $ name, M.fromList vs)

-- options :: [Map (CI Text) [[CI Text]]] -> Map (CI Text) (Set (CI Text))
-- options =
--   foldr (M.mergeWithKey (\_ v s -> Just $ S.union (S.fromList (concat v)) s) (M.map (S.fromList . concat)) id)
--         M.empty

data Result = Result { voterNames :: Vector Text -- voter index
                     , questionNames :: Vector Text -- question index
                     , optionNames :: Vector (Vector Text) -- question x option index
                     , votes :: Vector Ballot -- voter index
                     }

voters :: Result -> [Voter]
voters r = map Voter [0 .. fromIntegral $ V.length (voterNames r) - 1]

voterName :: Result -> Voter -> Text
voterName r (Voter i) = voterNames r ! fromIntegral i

questions :: Result -> [Question]
questions r = map Question [0 .. fromIntegral $ V.length (questionNames r) - 1]

questionName :: Result -> Question -> Text
questionName r i = questionNames r ! fromIntegral i

options :: Result -> Question -> [Option]
options r i = map Option [0 .. fromIntegral $ V.length (optionNames r ! fromIntegral i) - 1]

optionName :: Result -> Question -> Option -> Text
optionName r i (Option j) = (optionNames r ! fromIntegral i) ! fromIntegral j

vote :: Result -> Voter -> Ballot
vote r i = votes r ! fromIntegral i

dropOption :: Question -> Option -> Result -> Result
dropOption q o r =
  r { votes = V.map (\(Ballot b) -> Ballot . M.mapWithKey (\k vs@(Vote vs') ->
                                                             if k == q
                                                                then Vote $ map (filter (/= o)) vs'
                                                                else vs)
                                                          $ b)
                    (votes r)
    , optionNames = V.imap (\i v -> if i == fromIntegral q
                                       then (V.++) (V.take (fromIntegral o) v) (V.drop (fromIntegral o + 1) v)
                                       else v)
                           (optionNames r)
    }

mkBallot :: Map (CI Text) Question -> Vector (Map (CI Text) Option)  -> Map (CI Text) [[(CI Text)]] -> Ballot
mkBallot qs os = Ballot.ballot . M.fromList
                 . map (\(q, vs) -> let (Question q') = fromJust (M.lookup q qs)
                                    in (Question q', map (map (fromJust . flip M.lookup (os ! (fromIntegral q')))) vs))
                 . M.toList

input :: Parser Result
input = do
  spaces
  bs <- M.fromList <$> sepEndBy1 (Parse.ballot <?> "ballot (voter name followed by one vote per line)") (many1 endOfLine)
  let qnames = S.elems . S.fromList . concatMap M.keys . M.elems $ bs
      qnamesToQs = M.fromList (zip qnames (map Question [0..]))
      onames = V.fromList $ map (V.fromList . S.elems) . M.elems
               . M.foldl' (\ns b -> M.unionWith S.union ns (M.map (S.fromList . concat) b)) M.empty $ bs
      onamesToOs = V.map (\o2n -> M.fromList $ zip (V.toList o2n) (map Option [0..])) onames
  pure $ Result { voterNames = V.fromList . map CI.original . M.keys $ bs
                , questionNames = V.fromList . map CI.original $ qnames
                , optionNames = V.map (V.map CI.original) onames
                , votes = V.fromList . map (mkBallot qnamesToQs onamesToOs) . M.elems $ bs
                }
