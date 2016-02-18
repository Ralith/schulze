module Main where

import Control.Lens
import Control.Monad
import Data.Foldable
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as T
import Data.Text.Lazy.Encoding (decodeUtf8With)
import qualified Data.Text.Read as T
import Data.Word (Word64)
import Debug.Trace

import Network.HTTP.Client
import Network.URI

import System.Environment
import System.Exit
import System.IO

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

instance Plated (TagTree a) where
  plate _ t@(TagLeaf _) = pure t
  plate f (TagBranch n attrs xs) = TagBranch n attrs <$> traverse f xs

newtype Thread = Thread { _thread :: Word64 }
  deriving (Eq, Show)
makePrisms ''Thread

newtype Post = Post { _post :: Word64 }
  deriving (Eq, Show)
makePrisms ''Post

data PostData = PostData { _postAuthor :: Text, _postBody :: [TagTree Text] }
makeLenses ''PostData

data ThreadPage = ThreadPage { _tpThread :: Thread, _tpPerPage :: Word, _tpPage :: Word }
makeLenses ''ThreadPage

instance Show ThreadPage where
  show = tpUrl

main :: IO ()
main = do
  args <- getArgs
  let printUsage = putStrLn . (++ ": " ++ usage) =<< getProgName
  case args of
    [] -> printUsage
    [startURI] ->
      case parsePostURI startURI of
        Left err -> hPutStr stderr "not a valid post URL: " >> hPutStrLn stderr err >>  printUsage >> exitFailure
        Right (tp, p) -> do
          posts <- getPosts tp p Nothing
          T.putStrLn . T.concat $ map (\(post, x) ->
                                        T.concat [ x ^. postAuthor
                                                 , " (post ", T.pack $ show (post ^. _Post), ")\n"
                                                 -- , T.pack . show $ x ^. postBody
                                                 -- , "\n\n"
                                                 ]) posts
    _ -> printUsage >> exitFailure

usage :: String
usage = "<start url> [end url]"

getPosts :: ThreadPage -> Post -> Maybe Post -> IO [(Post, PostData)]
getPosts startPage startPost endPost = do
  mgr <- newManager defaultManagerSettings
  -- tail . dropWhile ((/= startPost) . fst) <$> 
  ps <- getPosts' mgr startPage endPost
  putStrLn $ show (length ps) ++ " posts"
  pure ps

hasClass :: Text -> [Attribute Text] -> Bool
hasClass c t = isJust (join $ find (==c) . T.split (==' ') <$> lookup "class" t)

-- 455143180 20773
getPosts' :: Manager -> ThreadPage -> Maybe Post -> IO [(Post, PostData)]
getPosts' mgr currentPage endPost = do
  page <- getPage mgr currentPage
  putStrLn $ show (length (filter (\case {TagBranch _ attrs _ -> hasClass "post" attrs; _ -> False}) page)) ++ ""
  let isPost :: (Text, [Attribute Text], a) -> Bool
      isPost (n, attrs, _) = n == "table" && hasClass "post" attrs && lookup "id" attrs /= Just "post"
      getId (_, attrs, body) = do
        str <- maybe (Left "post missing id attribute") Right (lookup "id" attrs)
        idx <- maybe (Left "post missing data-idx attribute") Right (lookup "data-idx" attrs)
        pid <- Post <$> decimal (T.unpack . T.drop 4 $ (trace (show idx) str))
        pure (pid, body)
      branches :: [(Text, [Attribute Text], [TagTree Text])]
      branches = [(a, b, c) | TagBranch a b c <- concatMap universe page]
      posts = map (_Right._2 %~ parsePost) . map getId . filter isPost $ branches
  foldrM (\x xs ->
            case x of
              Left err -> hPutStrLn stderr ("malformed post id: " ++ err) >> pure xs
              Right (Post pid, Left err) -> hPutStrLn stderr ("malformed user info for post " ++ show pid ++ ": " ++ err) >> pure xs
              Right (pid, Right post) -> pure $ (pid, post) : xs
         ) [] posts

parsePost :: [TagTree Text] -> Either String PostData
parsePost tree = PostData <$> author <*> body
  where
    author = maybe (Left "couldn't find author") Right $ listToMaybe
               [t | (TagBranch _ attrs [TagLeaf (TagText t)]) <- concatMap universe tree, hasClass "author" attrs]
    body = maybe (Left "couldn't find body") Right $ listToMaybe
             [b | (TagBranch _ attrs b) <- concatMap universe tree, hasClass "postbody" attrs]

matches :: [a -> Bool] -> a -> Bool
matches fs x = all ($ x) fs

getPage :: Manager -> ThreadPage -> IO [TagTree Text]
getPage mgr tp = do
  response <- flip httpLbs mgr =<< parseUrl (tpUrl tp)
  pure $ (map (fmap (^. strict)) . parseTree . decodeUtf8With lenientDecode . responseBody) response

-- http://forums.somethingawful.com/showthread.php?threadid=3657951&userid=0&perpage=40&pagenumber=520#post455115330
parsePostURI :: String -> Either String (ThreadPage, Post)
parsePostURI u =
  case parseURI u of
    Nothing -> Left "malformed URI"
    Just u' ->
      let opts = queryOpts (uriQuery u') in
      case ( decimal <$> M.lookup "threadid" opts
           , decimal <$> M.lookup "perpage" opts
           , decimal <$> M.lookup "pagenumber" opts
           , _2 %~ decimal $ splitAt 5 (uriFragment u')
           ) of
        (Just (Right tid), Just (Right pp), Just (Right page), ("#post", Right pid)) ->
          Right (ThreadPage (Thread tid) pp page, Post pid)
        _ -> Left "missing or malformed parameters"

decimal :: Integral a => String -> Either String a
decimal t = case T.decimal (T.pack t) of
              Left err -> Left $ err ++ ": \"" ++ t ++ "\""
              Right (val, "") -> Right val
              Right (_, ts) -> Left $ "contains garbage: " ++ T.unpack ts

queryOpts :: String -> Map String String
queryOpts = M.fromList . map ((_2 %~ drop 1) . break (== '=')) . splitOn "&" . drop 1

tpUrl :: ThreadPage -> String
tpUrl tp = concat [ "http://forums.somethingawful.com/showthread.php?threadid="
                  , show (tp ^. tpThread . _Thread)
                  , "&userid=0"
                  , "&perpage=", show (tp ^. tpPerPage)
                  , "&pagenumber=", show (tp ^. tpPage)
                  ]
