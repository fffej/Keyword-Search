module KeywordSearch where

import Porter (stem)
import StopWords (isStopWord)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.Traversable as D
import Data.Text.Encoding as E
import Data.List (foldr)
import Control.Monad
import Data.Char (isLetter)

import Database.Redis.Redis
import Database.Redis.ByteStringClass

type TermWeights = M.Map T.Text Int

host :: String
host = "localhost"

port :: String
port = "6379"

data Document = Document {
  filePath :: FilePath ,
  text :: T.Text
}

instance BS T.Text where
  toBS = encodeUtf8
  fromBS = decodeUtf8
  
-- A simple query language
data Query = Contains T.Text
           | And Query Query
           | Or Query Query
           | Xor Query Query      
           deriving Show
                    
getKey :: Query -> T.Text
getKey = T.pack . show
             
             
    
-- Index the given document
index :: Redis -> FilePath -> IO ()
index redis path = do
  file <- T.readFile path
  addDoc redis path (getTerms file)
  
storeEntry :: Redis -> T.Text -> Int -> FilePath -> IO (Reply T.Text)
storeEntry r k v ref = zincrBy r k (fromIntegral v) (T.pack ref) 
  
addDoc :: Redis -> FilePath -> TermWeights -> IO ()
addDoc r ref termWeights = do
  mapM_ (\(k,v) -> storeEntry r k v ref) (M.toList termWeights)
  return ()

removeStopWords :: [T.Text] -> [T.Text]
removeStopWords = filter (not . isStopWord)

clean :: T.Text -> T.Text
clean = T.filter isLetter

getTerms :: T.Text -> TermWeights
getTerms ws = foldr (\word count -> M.insertWith' (+) word 1 count) M.empty filteredWords 
  where
    filteredWords = map stem $ removeStopWords (map clean ((T.words . T.toLower) ws))

getTerm :: T.Text -> T.Text
getTerm = stem . clean . T.toLower

query :: Redis -> Query -> IO T.Text
query r q@(And lhs rhs) = do
  lhs <- query r lhs
  rhs <- query r rhs
  let key = getKey q
  x <- zinterStore r key [lhs,rhs] [] SUM
  return key
query r q@(Or lhs rhs) = do
  lhs <- query r lhs
  rhs <- query r rhs
  let key = getKey q
  x <- zunionStore r key [lhs,rhs] [] SUM
  return key
query r (Contains text) = do
  let searchTerm = getTerm text
  return searchTerm
  
getQueryResponse :: Redis -> T.Text -> IO (Reply T.Text)
getQueryResponse r key = zrange r key (0,99999999) True

main :: IO ()             
main = do
  redis <- connect host port
  index redis "/home/jeff/Desktop/1.txt"
  return ()
    