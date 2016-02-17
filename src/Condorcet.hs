module Condorcet
       ( Option(..)
       , optionID

       , Vote
       , RawVote
       , mkVote
       , voteRanks

       , Count(..)
       , count
       ) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Array.Unboxed

newtype Option = Option Word
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Ix)

optionID :: Option -> Word
optionID (Option i) = i

-- List of groups of options in order of preference. Groups are non-empty, options never repeat.
newtype Vote = Vote [[Option]]
  deriving (Eq, Show)

type RawVote = [[Option]]

voteRanks :: Vote -> [[Option]]
voteRanks (Vote ps) = ps

-- Enumerate binary preference relations entailed by a vote
votePrefs :: Word -> Vote -> [(Option, Option)]
votePrefs optCount v = go (S.fromList (map Option [0..optCount-1])) S.empty v
  where
    go unseen seen (Vote []) = [(s, u) | s <- S.toList seen, u <- S.toList unseen]
    go unseen seen (Vote (x:xs)) = [(y, z) | y <- x, z <- concat xs]
                                   ++ let xSet = S.fromList x
                                      in go (S.difference unseen xSet) (S.union seen xSet) (Vote xs)

-- Deletes lower-ranked duplicate options
mkVote :: RawVote -> Vote
mkVote v = Vote $ vote' S.empty v

vote' :: Set Option -> RawVote -> RawVote
vote' _ [] = []
vote' seen (x:xs) = (filter (flip S.notMember seen) x : (vote' (S.union (S.fromList x) seen) xs))

-- number of voters who strictly prefer fst to snd
newtype Count = Count (UArray (Option, Option) Word)
  deriving (Eq, Show)

count :: Word -> [Vote] -> Count
count optCount vs = Count $ accumArray (\x () -> succ x) 0 ((Option 0, Option 0), (Option (optCount-1), Option (optCount-1)))
                                       (map (\i -> (i, ())) . concatMap (votePrefs optCount) $ vs)
