module VoteCount.Ballot
       ( Question(..)
       , Voter(..)

       , Ballot(..)
       , ballotVotes

       , BallotSet
       , mkBallotSet
       , voters
       , votes
       , voterName
       , questions
       , questionName
       , options
       , optionName
       , vote
       , dropOption

       , aggregate
       ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Foldable
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import Data.Maybe (fromJust)

import VoteCount.Condorcet hiding (dropOption)

newtype Question = Question Word
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

newtype Voter = Voter Word
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

newtype Ballot = Ballot (Map Question Vote)
  deriving (Eq, Show)

ballotVotes :: Ballot -> Map Question Vote
ballotVotes (Ballot vs) = vs

aggregate :: Word -> [Ballot] -> Vector [Vote]
aggregate nQuestions =
  foldl' (\v (Ballot b) -> V.unsafeUpd v $ map (\(q, vo) -> (fromIntegral q, vo : (v ! fromIntegral q))) . M.toList $ b)
         (V.replicate (fromIntegral nQuestions) [])


data BallotSet = BallotSet { bsVoterNames :: Vector Text -- voter index
                           , bsQuestionNames :: Vector Text -- question index
                           , bsOptionNames :: Vector (Vector Text) -- question x option index
                           , bsVotes :: Vector Ballot -- voter index
                           }

voters :: BallotSet -> [Voter]
voters r = map Voter [0 .. fromIntegral $ V.length (bsVoterNames r) - 1]

votes :: BallotSet -> Vector Ballot
votes = bsVotes

voterName :: BallotSet -> Voter -> Text
voterName r (Voter i) = bsVoterNames r ! fromIntegral i

questions :: BallotSet -> [Question]
questions r = map Question [0 .. fromIntegral $ V.length (bsQuestionNames r) - 1]

questionName :: BallotSet -> Question -> Text
questionName r i = bsQuestionNames r ! fromIntegral i

options :: BallotSet -> Question -> [Option]
options r i = map Option [0 .. fromIntegral $ V.length (bsOptionNames r ! fromIntegral i) - 1]

optionName :: BallotSet -> Question -> Option -> Text
optionName r i (Option j) = (bsOptionNames r ! fromIntegral i) ! fromIntegral j

vote :: BallotSet -> Voter -> Ballot
vote r i = bsVotes r ! fromIntegral i

dropOption :: Question -> Option -> BallotSet -> BallotSet
dropOption q o r =
  r { bsVotes = V.map (\(Ballot b) -> Ballot . M.mapWithKey (\k vs@(Vote vs') ->
                                                               if k == q
                                                                  then Vote $ map (filter (/= o)) vs'
                                                                  else vs)
                                                            $ b)
                      (bsVotes r)
    , bsOptionNames = V.imap (\i v -> if i == fromIntegral q
                                         then (V.++) (V.take (fromIntegral o) v) (V.drop (fromIntegral o + 1) v)
                                         else v)
                             (bsOptionNames r)
    }

type TextVoter = CI Text
type TextQuestion = CI Text
type TextOption = CI Text
type TextVote = [[TextOption]]

mkBallot :: Map TextQuestion Question -> Vector (Map TextOption Option)  -> Map TextQuestion TextVote -> Ballot
mkBallot qs os = Ballot . M.map mkVote . M.fromList
                 . map (\(q, vs) -> let q' = fromJust (M.lookup q qs)
                                    in (q', map (map (fromJust . flip M.lookup (os ! fromIntegral q'))) vs))
                 . M.toList

mkBallotSet :: Map TextVoter (Map TextQuestion TextVote) -> BallotSet
mkBallotSet bs =
  let qnames = S.elems . S.fromList . concatMap M.keys . M.elems $ bs
      qnamesToQs = M.fromList (zip qnames (map Question [0..]))
      onames = V.fromList $ map (V.fromList . S.elems) . M.elems
               . M.foldl' (\ns b -> M.unionWith S.union ns (M.map (S.fromList . concat) b)) M.empty $ bs
      onamesToOs = V.map (\o2n -> M.fromList $ zip (V.toList o2n) (map Option [0..])) onames
  in BallotSet { bsVoterNames = V.fromList . map CI.original . M.keys $ bs
               , bsQuestionNames = V.fromList . map CI.original $ qnames
               , bsOptionNames = V.map (V.map CI.original) onames
               , bsVotes = V.fromList . map (mkBallot qnamesToQs onamesToOs) . M.elems $ bs
               }
