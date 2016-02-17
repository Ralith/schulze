module Ballot
       ( Question(..)
       , Voter(..)

       , Ballot(..)
       , RawBallot
       , ballot
       , ballotVotes

       , aggregate
       ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Foldable

import Condorcet

newtype Question = Question Word
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

newtype Voter = Voter Word
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

newtype Ballot = Ballot (Map Question Vote)
  deriving (Eq, Show)

ballotVotes :: Ballot -> Map Question Vote
ballotVotes (Ballot vs) = vs

type RawBallot = Map Question RawVote

-- Sanitizes votes
ballot :: RawBallot -> Ballot
ballot b = Ballot $ M.map mkVote b

aggregate :: Word -> [Ballot] -> Vector [Vote]
aggregate nQuestions =
  foldl' (\v (Ballot b) -> V.unsafeUpd v $ map (\(q, vote) -> (fromIntegral q, vote : (v ! fromIntegral q))) . M.toList $ b)
         (V.replicate (fromIntegral nQuestions) [])
