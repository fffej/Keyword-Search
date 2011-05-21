module Search where

import Indexer (getTerm)

import qualified Data.Text as T
import Database.Redis.Redis
import Database.Redis.ByteStringClass

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
  return (getScoresAndKey resp)
  
getScoresAndKey :: BS a => Reply a -> [(Int,T.Text)]  
getScoresAndKey x = undefined
