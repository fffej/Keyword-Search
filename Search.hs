module Search where

import Indexer (getTerm)

import qualified Data.Text as T
import Database.Redis.Redis
import Database.Redis.ByteStringClass
import Data.Maybe (fromJust)

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
  
getQueryResponse :: Redis -> T.Text -> IO (Reply T.Text) -- [(Int,T.Text)]
getQueryResponse r key = do
  resp <- zrange r key (0,99999999) True 
  return resp -- (getScoresAndKeys resp)
  
getScoresAndKeys :: Reply T.Text -> [(Int,T.Text)]  
getScoresAndKeys (RMulti (Just x)) = undefined 
  where
    strings = (map fromJust x)
getScoresAndKeys (RMulti Nothing) = []
getScoresAndKeys _ = []
  
                     
everySecondAt :: Bool -> [a] -> [a]
everySecondAt _ [] = []
everySecondAt True  (x : xs) = x : every_second_at False xs
everySecondAt False (x : xs) =     every_second_at True xs            
