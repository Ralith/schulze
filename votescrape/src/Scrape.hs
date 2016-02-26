{-# LANGUAGE TemplateHaskell #-}
module Scrape where

import Control.Lens
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isSpace)
import Data.Foldable
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import qualified Data.Text.Read as T
import Data.Word (Word64)
import Data.Either
import Data.List (isPrefixOf, tails)

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import Network.URI

import Text.HTML.TagSoup

newtype Thread = Thread Word64
  deriving (Eq, Show)
makePrisms ''Thread

newtype Post = Post Word64
  deriving (Eq, Show, Ord)
makePrisms ''Post

data PostData a = PostData { _postAuthor :: Text, _postBody :: a }
  deriving Show
makeLenses ''PostData

data ThreadPage = ThreadPage { _tpThread :: Thread, _tpPerPage :: Word, _tpPage :: Word }
  deriving Eq
makeLenses ''ThreadPage

instance Show ThreadPage where
  show = tpUrl

filterQuotes :: [Tag Text] -> [Tag Text]
filterQuotes = go 0
  where
    go :: Word -> [Tag Text] -> [Tag Text]
    go n (TagOpen "blockquote" _ : xs) = go (succ n) xs
    go n (TagClose "blockquote" : xs) = go (pred n) xs
    go 0 (x : xs) = x : go 0 xs
    go n (_ : xs) = go n xs
    go _ [] = []

bolded :: [Tag Text] -> Text
bolded = T.strip . go 0
  where
    go :: Word -> [Tag Text] -> Text
    go n (TagOpen "b" _ : xs) = go (succ n) xs
    go n (TagClose "b" : xs) = go (pred n) xs
    go 0 (TagText t : xs) = T.append (T.filter isSpace t) (go 0 xs)
    go n (TagText t : xs) = T.append t (go n xs)
    go n (_ : xs) = go n xs
    go _  [] = ""

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

-- Gets the last paragraph of bold text starting with ##vote
getVote :: PostData Text -> Maybe (PostData Text)
getVote post = (\x -> set postBody x post) <$> safeLast votes
  where
    flag = T.toCaseFold "##vote"
    votes :: [Text]
    votes = map (T.intercalate "\n" . drop 1)
            . filter (isPrefixOf [flag] . map T.toCaseFold) . tails . map T.strip . T.lines $ post ^. postBody

login :: Manager -> Text -> Text -> IO (Either Status CookieJar)
login mgr uname passwd = do
  req <- urlEncodedBody [("action", "login"), ("username", encodeUtf8 uname), ("password", encodeUtf8 passwd)]
         <$> parseUrl "https://forums.somethingawful.com/account.php"
  resp <- httpNoBody (req { checkStatus = (\_ _ _ -> Nothing)}) mgr
  pure $ if statusIsSuccessful (responseStatus resp)
            then Right (responseCookieJar resp)
            else Left (responseStatus resp)

data VoteErr = HTTPErr Status | NeedsLogin
  deriving (Show)

errMsg :: VoteErr -> Text
errMsg (HTTPErr status) = T.concat ["HTTP ", T.pack $ show (statusCode status), ": ", decodeUtf8With lenientDecode (statusMessage status ^. from strict) ^. strict]
errMsg NeedsLogin = "paywall detected: you must log in"

getVotes :: CookieJar -> (ThreadPage, Post) -> Maybe (ThreadPage, Post) -> IO (Either VoteErr ([PostData Text], [String]))
getVotes cookies start end =
  (_Right._1 %~ (\x -> (traverse %~ (getVote . (postBody %~ bolded . filterQuotes) . snd)) x ^.. folded._Just))
  <$> getPosts cookies start end

getPosts :: CookieJar -> (ThreadPage, Post) -> Maybe (ThreadPage, Post) -> IO (Either VoteErr ([(Post, PostData [Tag Text])], [String]))
getPosts cookies start end = do
  mgr <- newManager tlsManagerSettings
  allPosts <-
    case end of
      Nothing -> getPosts' mgr cookies (start ^. _1)
      Just e -> do
        sem <- newQSem 2 -- SA DoS protection kicks in if this is too large
        results <- mapConcurrently (\x -> bracket (waitQSem sem) (const (signalQSem sem)) (const (getPagePosts mgr cookies x))) $
          map (\x -> tpPage .~ x $ start ^. _1)
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

getPosts' :: Manager -> CookieJar -> ThreadPage -> IO (Either VoteErr [Either String (Post, PostData [Tag Text])])
getPosts' mgr cookies currentPage = do
  page <- getPage mgr cookies currentPage
  case page of
    Left err -> pure (Left err)
    Right tags -> do
      let posts = map parsePost . partitions isPost $ tags
      if fromIntegral (length posts) < (currentPage ^. tpPerPage)
         then pure (Right posts)
         else (fmap (posts ++)) <$> getPosts' mgr cookies (currentPage & tpPage %~ succ)

getPagePosts :: Manager -> CookieJar -> ThreadPage -> IO (Either VoteErr [Either String (Post, PostData [Tag Text])])
getPagePosts mgr cookies page = do
  x <- fmap (map parsePost . partitions isPost) <$> getPage mgr cookies page
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

responseTags :: Response BSL.ByteString -> [Tag Text]
responseTags = (map (fmap (^. strict)) . canonicalizeTags . parseTags . decodeUtf8With lenientDecode . responseBody)

getPage :: Manager -> CookieJar -> ThreadPage -> IO (Either VoteErr [Tag Text])
getPage mgr cookies tp = do
  req <- parseUrl (tpUrl tp)
  let req' = req { checkStatus = (\_ _ _ -> Nothing), cookieJar = Just cookies }
  putStrLn $ "req thread " ++ show (tp ^. tpThread._Thread) ++ " page " ++ show (tp ^. tpPage)
  response <- flip httpLbs mgr req'
  putStrLn $ "got thread " ++ show (tp ^. tpThread._Thread) ++ " page " ++ show (tp ^. tpPage)
  pure $ if statusIsSuccessful (responseStatus response)
            then if BS.isInfixOf "Sorry, you must be a registered forums member to view this page" (responseBody response ^. strict)
                    then Left NeedsLogin
                    else Right (responseTags response)
            else Left (HTTPErr $ responseStatus response)

-- http://forums.somethingawful.com/showthread.php?threadid=3657951&userid=0&perpage=40&pagenumber=520#post455115330
parsePostURI :: String -> Either String (ThreadPage, Post)
parsePostURI u =
  case parseURI u of
    Nothing -> Left "malformed URI"
    Just u' ->
      case uriRegName <$> uriAuthority u' of
        Just "forums.somethingawful.com" ->
          let opts = queryOpts (uriQuery u') in
          case ( decimal <$> M.lookup "threadid" opts
               , maybe (Right 40) decimal $ M.lookup "perpage" opts
               , maybe (Right 1) decimal $ M.lookup "pagenumber" opts
               , _2 %~ decimal $ splitAt 5 (uriFragment u')
               ) of
            (Just (Right tid), Right pp, Right page, ("#post", Right pid)) ->
              Right (ThreadPage (Thread tid) pp page, Post pid)
            _ -> Left "missing or malformed URI parameters"
        Just _ -> Left $ "unsupported host"
        _ -> Left "malformed URI"

decimal :: Integral a => String -> Either String a
decimal t = case T.decimal (T.pack t) of
              Left err -> Left $ err ++ ": \"" ++ t ++ "\""
              Right (val, "") -> Right val
              Right (_, ts) -> Left $ "contains garbage: " ++ T.unpack ts

queryOpts :: String -> Map String String
queryOpts = M.fromList . map ((_2 %~ drop 1) . break (== '=')) . splitOn "&" . drop 1

tpUrl :: ThreadPage -> String
tpUrl tp = concat [ "https://forums.somethingawful.com/showthread.php?threadid="
                  , show (tp ^. tpThread . _Thread)
                  , "&userid=0"
                  , "&perpage=", show (tp ^. tpPerPage)
                  , "&pagenumber=", show (tp ^. tpPage)
                  ]
