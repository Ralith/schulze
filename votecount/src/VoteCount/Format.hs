module VoteCount.Format where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Array.IArray
import Data.Foldable

import VoteCount.Ballot as Ballot
import VoteCount.Condorcet as Condorcet
import VoteCount.Schulze

header :: Text -> Text
header title = T.unlines [bar, title, bar]
  where
  bar = T.replicate 32 "="

printCount :: BallotSet -> Text
printCount r = T.concat
  [ header (T.concat [ (T.pack . show . length $ voters r), " voters, "
                     , (T.pack . show $ qcount), " question", if qcount /= 1 then "s" else ""
                     ]), "\n"
  , printVotes r, "\n\n"
  , header "Pairwise preference counts", "\n"
  , printCounts r counts, "\n"
  , "\n"
  , header "Results", "\n"
  , printResults r (V.zip optCounts counts), "\n"
  ]
  where
    qcount = length (questions r)
    optCounts = V.generate qcount (\i -> fromIntegral . length . options r $ fromIntegral i)
    counts = V.map (uncurry count) $ V.zip optCounts (aggregate (fromIntegral qcount) (V.toList . votes $ r))

printVotes :: BallotSet -> Text
printVotes r = T.intercalate "\n\n" . map (uncurry formatBallot) $ zip vs (map (vote r) vs)
  where
    vs = voters r
    formatBallot v b = T.concat [voterName r v, "\n", T.intercalate "\n" . map (uncurry formatVote) . M.toList . ballotVotes $ b]
    formatVote q v = T.concat [questionName r q, " ", T.intercalate "; " . map (T.intercalate ", " . map (optionName r q)) . voteRanks $ v]

formatTable :: (Integral i, Ix i) => [Text] -> [Text] -> Array (i, i) Text -> Text
formatTable rowLabels colLabels arr =
  T.concat [ T.intercalate "\n" . map headerRow $ [1..vmargin], "\n"
           , T.replicate (hmargin + 1) "-", "++"
           , T.intercalate "+" . replicate (fromIntegral $ 1 + (colMax - colMin)) $ T.replicate (cellMargin+2) "-"
           , "\n"
           , T.intercalate "\n" . map (uncurry formatRow) $ zip [0..] [rowMin..rowMax]
           ]
  where
    ((rowMin, colMin), (rowMax, colMax)) = bounds arr
    hmargin = maximum (map T.length rowLabels)
    vmargin = maximum (map T.length colLabels)
    cellMargin = foldl' (\x c -> max x (T.length c)) 0 arr
    headerRow i =
      T.concat [ T.replicate (hmargin + 3 + cellMargin) " "
               , T.intercalate (T.replicate (cellMargin + 2) " ") $
                 map (\x -> if T.length x > (vmargin - i)
                               then T.singleton $ T.index x (T.length x - (vmargin - i) - 1)
                               else " ")
                     colLabels
               ]
    formatRow rowi row =
      let label = rowLabels !! rowi
      in T.concat [ T.replicate (hmargin - T.length label) " ", label, " || "
                  , T.intercalate " | " (map (\col -> let cell = arr ! (row, col)
                                                      in T.append (T.replicate (cellMargin - T.length cell) " ") cell)
                                              [colMin..colMax])
                  ]

printCounts :: BallotSet -> Vector Count -> Text
printCounts r = T.intercalate "\n\n" . map (\(q, c) -> T.concat [questionName r q, "\n", formatCount q c]) . zip (questions r) . V.toList
  where
    formatCount :: Question -> Count -> Text
    formatCount q (Count c) =
      let labels = (map (optionName r q) (options r q))
      in formatTable labels labels (listArray (bounds c) (map (T.pack . show) (elems c)))

printResults :: BallotSet -> Vector (Word, Count) -> Text
printResults r =
  T.intercalate "\n" . map (\(q, oc) -> T.concat [questionName r q, " ", uncurry (formatResult r q) oc]) . zip (questions r) . V.toList

formatResult :: BallotSet -> Question -> Word -> Count -> Text
formatResult r q opts c =
  T.intercalate "; " . map (T.intercalate ", " . map (optionName r q))
  $ rank [Option 0 .. Option (opts-1)] (judge winning opts c)
