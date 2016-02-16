module Lib
  ( RawBallot
  , Ballot
  , ballotVotes
  , ballot

  , margin

  , Voter(..)
  , Vote
  , vote
  , votePrefs
  , Question(..)
  , Option(..)
  , optionName
  , RawVote
  , PrefCount(..)
  , emptyCount
  , tallyVote
  , prefCountSet
  , Relation
  , judge
  , judgeSet
  , aggregate
  ) where

import Data.Maybe (fromJust)
import Data.String (IsString)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Array.MArray
import Data.Array.ST
import Data.Foldable
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Control.Monad
import Control.Monad.ST

newtype Question = Question (CI Text)
  deriving (Eq, Ord, Show, IsString)

newtype Voter = Voter (CI Text)
  deriving (Eq, Ord, Show, IsString)

newtype Option = Option (CI Text)
  deriving (Eq, Ord, Show, IsString)

optionName :: Option -> Text
optionName (Option n) = CI.original n

-- List of groups of options in order of preference. Groups are non-empty, options never repeat.
newtype Vote = Vote [[Option]]
  deriving (Eq, Show)

votePrefs :: Vote -> [[Option]]
votePrefs (Vote ps) = ps

newtype Ballot = Ballot (Map Question Vote)
  deriving (Eq, Show)

ballotVotes :: Ballot -> Map Question Vote
ballotVotes (Ballot vs) = vs

type RawVote = [[Option]]
type RawBallot = Map Question RawVote

-- number of voters who strictly prefer fst to snd
newtype PrefCount = PrefCount (Map (Option, Option) Word)
  deriving (Eq, Show)

type QMap a = Map Question (Set Option, a)

type PrefCountSet = QMap PrefCount

type VoteSet = QMap [Vote]

-- Enumerate binary preference relations entailed by a vote
preferences :: Set Option -> Vote -> [(Option, Option)]
preferences opts v = preferences' opts S.empty v

preferences' :: Set Option -> Set Option -> Vote -> [(Option, Option)]
preferences' unseen seen (Vote []) = [(s, u) | s <- S.toList seen, u <- S.toList unseen]
preferences' unseen seen (Vote (x:xs)) = [(y, z) | y <- x, z <- concat xs]
                                         ++ preferences' (S.difference unseen xSet) (S.union seen xSet) (Vote xs)
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

emptyCount :: Set Option -> PrefCount
emptyCount opts = PrefCount $ M.fromList [((x, y), 0) | x <- opts', y <- opts', x /= y]
  where
    opts' = S.toList opts

-- Increment a a preference count based on a single vote
tallyVote :: Set Option -> Vote -> PrefCount -> PrefCount
tallyVote opts v (PrefCount c) = PrefCount $ foldr (M.adjust succ) c (preferences opts v)

prefCountSet :: VoteSet -> PrefCountSet
prefCountSet = M.map (\(opts, vs) -> (opts, foldr (tallyVote opts) (emptyCount opts) vs))

aggregate :: Map Question (Set Option) -> [Ballot] -> VoteSet
aggregate options votes =
  foldl' (\m (Ballot b) -> M.mergeWithKey (\_ (os, vs) v -> Just (os, v:vs)) id (const M.empty) m b) (M.map (\x -> (x, [])) options) votes

type Relation = ((Word, Word) -> (Word, Word) -> Bool)

margin :: Relation
margin (ef, fe) (gh, hg) = (toInteger ef - toInteger fe) > (toInteger gh - toInteger hg)

minD :: (a -> a -> Bool) -> a -> a -> a
minD d x y = if not (x `d` y) then x else y

judge :: Relation -> [Option] -> PrefCount -> ([Option], [((Option, Option), (Word, Word))])
judge relation options (PrefCount n) = runST $ do
  let size = fromIntegral (length options) :: Word
  p <- newListArray ((1, 1), (size, size))
                    [if i == j then (-1,-1) else (fromJust $ M.lookup (i, j) n, fromJust $ M.lookup (j, i) n) | i <- options, j <- options]
                    :: ST s (STArray s (Word, Word) (Word, Word))
  preds <- newListArray ((1, 1), (size, size))
                        [if i == j then -1 else i | i <- [1..size], j <- [1..size]]
                        :: ST s (STArray s (Word, Word) Word)
  forM_ [1..size] $ \i ->
    forM_ [1..size] $ \j -> when (i /= j) $
      forM_ [1..size] $ \k -> when (i /= k && j /= k) $ do
        pdjk <- readArray p (j,k)
        pdji <- readArray p (j,i)
        pdik <- readArray p (i,k)
        let pmin = minD relation pdji pdik
        when (not (pdjk `relation` pmin) && pdjk /= pmin) $ do
          writeArray p (j,k) pmin
          writeArray preds (j,k) =<< readArray preds (i,k)
  winner <- newArray (1, size) True
                     :: ST s (STArray s Word Bool)
  forM_ [1..size] $ \i ->
    forM_ [1..size] $ \j -> when (i /= j) $ do
      pdji <- readArray p (j, i)
      pdij <- readArray p (i, j)
      when (pdji `relation` pdij) $ writeArray winner i False
  flags <- getElems winner
  p' <- getAssocs p
  pure $ (map snd . filter fst $ zip flags options, filter (\((i,j),_) -> i /= j) $ zip [(i, j)| i <- options, j <- options] (map snd p'))

judgeSet :: Relation -> PrefCountSet -> Map Question [Option]
judgeSet relation = M.map (\(opts, pc) -> fst $ judge relation (S.toList opts) pc)
