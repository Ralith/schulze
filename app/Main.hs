module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec (parse)
import System.Environment
import System.IO
import System.Exit
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Array.IArray
import Data.Foldable

import Condorcet
import Schulze
import Ballot
import Parse

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> main' "stdin" =<< T.getContents
    [file] -> main' file =<< T.readFile file
    _ -> T.hPutStrLn stderr "too many arguments (expected nothing or filename)" >> exitWith (ExitFailure 1)

main' :: String -> Text -> IO ()
main' file contents =
  case parse input file contents of
    Left err -> T.hPutStrLn stderr "parse error:" >> hPutStrLn stderr (show err) >> exitWith (ExitFailure 1)
    Right r -> do
      let qcount = length (questions r)
          optCounts = V.generate qcount (\i -> fromIntegral . length . options r $ fromIntegral i)
      T.putStrLn $ header (T.concat [(T.pack . show . length $ voters r), " voters, "
                                    , (T.pack . show $ qcount), " question", if qcount /= 1 then "s" else ""])
      T.putStrLn $ printVotes r
      T.putStrLn ""
      T.putStrLn $ header "Pairwise preference counts"
      let counts = V.map (uncurry count) $ V.zip optCounts (aggregate (fromIntegral qcount) (V.toList . votes $ r))
      T.putStrLn $ printCounts r counts
      T.putStrLn ""
      T.putStrLn $ header "Results"
      T.putStrLn $ printResults r (V.zip optCounts counts)

header :: Text -> Text
header title = T.unlines [bar, title, bar]
  where
  bar = T.replicate 32 "="

printVotes :: Parse.Result -> Text
printVotes r = T.intercalate "\n\n" . map (uncurry formatBallot) $ zip vs (map (Parse.vote r) vs)
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

printCounts :: Parse.Result -> Vector Count -> Text
printCounts r = T.intercalate "\n\n" . map (\(q, c) -> T.concat [questionName r q, "\n", formatCount q c]) . zip (questions r) . V.toList
  where
    formatCount :: Question -> Count -> Text
    formatCount q (Count c) =
      let labels = (map (optionName r q) (options r q))
      in formatTable labels labels (listArray (bounds c) (map (T.pack . show) (elems c)))

printResults :: Parse.Result -> Vector (Word, Count) -> Text
printResults r =
  T.intercalate "\n" . map (\(q, oc) -> T.concat [questionName r q, " ", uncurry (formatResult r q) oc]) . zip (questions r) . V.toList

formatResult :: Result -> Question -> Word -> Count -> Text
formatResult r q opts c =
  case judge winning opts c of
    [] -> "no votes"
    xs@(_:rest) -> T.concat [ T.intercalate ", " (map (optionName r q) xs)
                            , let winningSize = fromIntegral $ length xs in
                              if opts > winningSize
                                 then T.append "; " $
                                      formatResult (foldr (Parse.dropOption q) r xs) q
                                                   (opts - fromIntegral winningSize)
                                                   (foldr Condorcet.dropOption c xs)
                                 else ""
                            ]
