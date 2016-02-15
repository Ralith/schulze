module Lib where

import Data.String (IsString)
import Data.Word (Word)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Applicative

newtype Question = Question Text
  deriving (Eq, Ord, Show, IsString)

newtype Voter = Voter Text
  deriving (Eq, Ord, Show, IsString)

newtype Option = Option Text
  deriving (Eq, Ord, Show, IsString)

-- List of groups of options in order of preference. Groups are non-empty, options never repeat.
newtype Vote = Vote [[Option]]
  deriving (Eq, Show)

newtype Ballot = Ballot (Map Question Vote)
  deriving (Eq, Show)

type RawVote = [[Option]]
type RawBallot = Map Question RawVote

-- number of voters favoring fst over snd
newtype PrefCount = PrefCount (Map (Option, Option) Word)
  deriving (Eq, Show)

newtype Tally = Tally (Map Question PrefCount)
  deriving (Eq, Show)

newtype OptionSet = OptionSet (Map Question [Option])
  deriving (Show)

parse :: Text -> (Map Voter RawBallot, OptionSet)
parse xs = undefined

-- Enumerate binary preference relations entailed by a vote
preferences :: [Option] -> Vote -> [(Option, Option)]
preferences opts vote = preferences' opts S.empty vote

preferences' :: [Option] -> Set Option -> Vote -> [(Option, Option)]
preferences' unseen seen (Vote []) = [(s, u) | s <- S.toList seen, u <- unseen]
preferences' unseen seen (Vote (x:xs)) = [(y, z) | y <- x, z <- concat xs]
                                         ++ preferences' (filter (flip S.notMember xSet) unseen) (S.union seen xSet) (Vote xs)
  where
    xSet = S.fromList x

-- Deletes lower-ranked duplicate options
vote :: RawVote -> Vote
vote v = Vote $ vote' S.empty v

vote' :: Set Option -> RawVote -> RawVote
vote' _ [] = []
vote' seen (x:xs) = (filter (flip S.notMember seen) x : (vote' (S.union (S.fromList x) seen) xs))

-- Sanitizes votes
ballot :: RawBallot -> Ballot
ballot b = Ballot $ M.map vote b

-- Construct a tally for no votes
emptyTally :: OptionSet -> Tally
emptyTally (OptionSet optset) = Tally $ M.map (\opts -> PrefCount $ M.fromList [((x, y), 0) | x <- opts, y <- opts, x /= y]) optset

-- Increment a a preference count based on a single vote
tallyVote :: [Option] -> Vote -> PrefCount -> PrefCount
tallyVote opts v (PrefCount c) = PrefCount $ foldr (M.adjust succ) c (preferences opts v)

-- Increment a tally based on all votes in a ballot
tallyBallot :: OptionSet -> Ballot -> Tally -> Tally
tallyBallot (OptionSet opts) (Ballot b) (Tally t) =
  Tally $ M.mergeWithKey (\_ opts f -> Just $ f opts) (const M.empty) (const M.empty) opts $ M.mergeWithKey (\_ vote count -> Just (\opts -> tallyVote opts vote count)) (const M.empty) (M.map const) b t

tally :: [Ballot] -> OptionSet -> Tally
tally ballots optset = foldr (tallyBallot optset) (emptyTally optset) ballots
