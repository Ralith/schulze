{-# LANGUAGE TemplateHaskell #-}
module Scrape where

import Control.Lens
import Control.Monad
import Control.Concurrent.Async
import Data.Foldable
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import qualified Data.Text.Read as T
import Data.Word (Word64)
import Data.Either

import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.URI

import Text.HTML.TagSoup

newtype Thread = Thread { _thread :: Word64 }
  deriving (Eq, Show)
makePrisms ''Thread

newtype Post = Post { _post :: Word64 }
  deriving (Eq, Show, Ord)
makePrisms ''Post

data PostData a = PostData { _postAuthor :: Text, _postBody :: a }
makeLenses ''PostData

data ThreadPage = ThreadPage { _tpThread :: Thread, _tpPerPage :: Word, _tpPage :: Word }
  deriving Eq
makeLenses ''ThreadPage

instance Show ThreadPage where
  show = tpUrl

bolded :: [Tag Text] -> Text
bolded = T.strip . go 0
  where
    go :: Word -> [Tag Text] -> Text
    go n (TagOpen "b" _ : xs) = go (succ n) xs
    go n (TagClose "b" : xs) = go (pred n) xs
    go 0 (_:xs) = go 0 xs
    go n (TagText t : xs) = T.append t (go n xs)
    go n (_ : xs) = go n xs
    go _  [] = ""

getVote :: PostData Text -> Maybe (PostData Text)
getVote post =
  case voteLines of
    [] -> Nothing
    _ -> Just $ set postBody (T.intercalate "\n" (drop 1 voteLines)) post
  where
    flag = T.toCaseFold "##vote"
    voteLines :: [Text]
    voteLines = takeWhile (not . T.null) . dropWhile (not . (== flag) . T.toCaseFold) . map T.strip . T.lines $ post ^. postBody

getVotes :: (ThreadPage, Post) -> Maybe (ThreadPage, Post) -> IO (Either Status ([PostData Text], [String]))
getVotes start end =
  (_Right._1 %~ (\x -> (traverse %~ (getVote . (postBody %~ bolded) . snd)) x ^.. folded._Just)) <$> getPosts start end

getPosts :: (ThreadPage, Post) -> Maybe (ThreadPage, Post) -> IO (Either Status ([(Post, PostData [Tag Text])], [String]))
getPosts start end = do
  mgr <- newManager defaultManagerSettings
  allPosts <-
    case end of
      Nothing -> getPosts' mgr (start ^. _1)
      Just e -> do
        results <- mapConcurrently (getPagePosts mgr) $ map (\x -> tpPage .~ x $ start ^. _1)
                                                            [start ^. _1.tpPage .. e ^. _1.tpPage]
        pure $ foldr (\r xs ->
                       case (r, xs) of
                         (_, Left _) -> xs
                         (x@(Left _), _) -> x
                         (Right x, Right xs') -> Right (x ++ xs')
                     ) (Right []) results
  let ps = fmap (filter (\(p, _) -> p > (start ^. _2) && maybe True ((p <=) . (^._2)) end) . rights) allPosts
  pure $ case (ps, fmap lefts allPosts) of
           (Left err, _) -> Left err
           (Right ls, Right errs) -> Right (ls, errs)
           _ -> error "impossible: getPosts"

tagAttribs :: Tag a -> [Attribute a]
tagAttribs (TagOpen _ xs) = xs
tagAttribs _ = []

hasClass :: Text -> Tag Text -> Bool
hasClass c (TagOpen _ attribs) = isJust (join $ find (==c) . T.split (==' ') <$> lookup "class" attribs)
hasClass _ _ = False

hasClass' :: Text -> [Attribute Text] -> Bool
hasClass' c t = isJust (join $ find (==c) . T.split (==' ') <$> lookup "class" t)

isPost :: Tag Text -> Bool
isPost tag = hasClass "post" tag && fromAttrib "id" tag /= "post"

getPosts' :: Manager -> ThreadPage -> IO (Either Status [Either String (Post, PostData [Tag Text])])
getPosts' mgr currentPage = do
  page <- getPage mgr currentPage
  case page of
    Left err -> pure (Left err)
    Right tags -> do
      let posts = map parsePost . partitions isPost $ tags
      if fromIntegral (length posts) < (currentPage ^. tpPerPage)
         then pure (Right posts)
         else (fmap (posts ++)) <$> getPosts' mgr (currentPage & tpPage %~ succ)

getPagePosts :: Manager -> ThreadPage -> IO (Either Status [Either String (Post, PostData [Tag Text])])
getPagePosts mgr page = do
  x <- fmap (map parsePost . partitions isPost) <$> getPage mgr page
  pure x

parsePost :: [Tag Text] -> Either String (Post, PostData [Tag Text])
parsePost [] = error "parsePost: impossible" -- partitions guarantees nonempty
parsePost (x:xs) = do
  pidStr <- maybe (Left "post missing id attribute") Right $ lookup "id" (tagAttribs x)
  pid <- decimal (T.unpack . T.drop 4 $ pidStr)
  (author, rest) <-
    case dropWhile (not . hasClass "author") xs of
      (_:TagText author:rest) -> Right (author, rest)
      _ -> Left "post missing author"
  case dropWhile (/= TagComment " google_ad_section_start ") rest of
    (_:body) -> Right (Post pid, PostData author (takeWhile (/= TagComment " google_ad_section_end ") body))
    _ -> Left $ "post " ++ show pid ++ " missing body"

matches :: [a -> Bool] -> a -> Bool
matches fs x = all ($ x) fs

getPage :: Manager -> ThreadPage -> IO (Either Status [Tag Text])
getPage mgr tp = do
  req <- parseUrl (tpUrl tp)
  let req' = req { checkStatus = (\_ _ _ -> Nothing)}
  response <- flip httpLbs mgr req'
  pure $ if statusIsSuccessful (responseStatus response)
            then Right $ (map (fmap (^. strict)) . canonicalizeTags . parseTags . decodeUtf8With lenientDecode . responseBody)
                           response
            else Left (responseStatus response)

-- http://forums.somethingawful.com/showthread.php?threadid=3657951&userid=0&perpage=40&pagenumber=520#post455115330
parsePostURI :: String -> Either String (ThreadPage, Post)
parsePostURI u =
  case parseURI u of
    Nothing -> Left "malformed URI"
    Just u' ->
      let opts = queryOpts (uriQuery u') in
      case ( decimal <$> M.lookup "threadid" opts
           , maybe (Right 40) decimal $ M.lookup "perpage" opts
           , decimal <$> M.lookup "pagenumber" opts
           , _2 %~ decimal $ splitAt 5 (uriFragment u')
           ) of
        (Just (Right tid), Right pp, Just (Right page), ("#post", Right pid)) ->
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
