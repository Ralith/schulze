module Schulze where

import Data.Array.MArray
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.Array.IArray

import Condorcet

type Relation = ((Word, Word) -> (Word, Word) -> Bool)

margin :: Relation
margin (ef, fe) (gh, hg) = (toInteger ef - toInteger fe) > (toInteger gh - toInteger hg)

ratio :: Relation
ratio (ef, fe) (gh, hg) =
  or [ ef > fe && gh <= hg
     , ef >= fe && gh < hg
     , ef * hg > fe * gh
     , ef > gh && fe <= hg
     , ef >= gh && fe < hg
     ]

winning :: Relation
winning (ef, fe) (gh, hg) =
  or [ ef > fe && gh <= hg
     , ef >= fe && gh < hg
     , ef > fe && gh > hg && ef > gh
     , ef > fe && gh > hg && ef == gh && fe < hg
     , ef < fe && gh < hg && fe < hg
     , ef < fe && gh < hg && fe == hg && ef > gh
     ]

minD :: (a -> a -> Bool) -> a -> a -> a
minD d x y = if not (x `d` y) then x else y

judge :: Relation -> Word -> Count -> [Option]
judge relation optCnt (Count n) = runST $ do
  let lastOpt = Option (optCnt-1)
      optlist = [Option 0..lastOpt]
  p <- newArray_ ((Option 0, Option 0), (lastOpt, lastOpt))
                 :: ST s (STArray s (Option, Option) (Word, Word))
  preds <- newArray_ ((Option 0, Option 0), (lastOpt, lastOpt))
                     :: ST s (STArray s (Option, Option) Option)
  forM_ optlist $ \i ->
    forM_ optlist $ \j ->
      when (i /= j) $ do
        writeArray p (i,j) (n ! (i,j), n ! (j,i))
        writeArray preds (i,j) i
  forM_ optlist $ \i ->
    forM_ optlist $ \j -> when (i /= j) $
      forM_ optlist $ \k -> when (i /= k && j /= k) $ do
        pdjk <- readArray p (j,k)
        pdji <- readArray p (j,i)
        pdik <- readArray p (i,k)
        let pmin = minD relation pdji pdik
        when (not (pdjk `relation` pmin) && pdjk /= pmin) $ do
          writeArray p (j,k) pmin
          writeArray preds (j,k) =<< readArray preds (i,k)
  winner <- newArray (Option 0, lastOpt) True
                     :: ST s (STArray s Option Bool)
  forM_ optlist $ \i ->
    forM_ optlist $ \j -> when (i /= j) $ do
      pdji <- readArray p (j, i)
      pdij <- readArray p (i, j)
      when (pdji `relation` pdij) $ writeArray winner i False
  flags <- getElems winner
  pure $ map snd . filter fst $ zip flags optlist
