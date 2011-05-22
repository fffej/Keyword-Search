module Search where

import Indexer (getTerm)

import qualified Data.Text as T
import Database.Redis.Redis
import Database.Redis.ByteStringClass
import Data.Maybe (fromJust)
import Data.List.Split
import Control.Monad (liftM)

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
  
getQueryResponse :: Redis -> T.Text -> IO [(Int,T.Text)]
getQueryResponse r key = do
  resp <- zrange r key (0,99999999) True 
  x <- fromRMultiBulk' resp
  let scores = map (read . T.unpack . head . tail) (splitEvery 2 x)
      values = map head (splitEvery 2 x)
  v <- mapM (\x -> get r x >>= fromRBulk') values
  return $ zip scores v
  
  

