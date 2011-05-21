module KeywordSearch where

import Porter (stem)
import StopWords (isStopWord)
import Indexer (getTerm)
import FortuneIndexer (indexFortunes)


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

data Query = Contains T.Text
           | And Query Query
           | Or Query Query
           | Xor Query Query      
           deriving (Show,Eq,Read)
                    
getKey :: Query -> T.Text                    
getKey = T.pack . show
                    
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
  indexFortunes redis
  return ()
    