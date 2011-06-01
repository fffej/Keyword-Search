module Indexer (
  storeTermEntry,
  isBlank,
  notBlank,
  addTerms,
  getTerms,
  getTerm) where

import Porter (stem)
import StopWords

import qualified Data.Map as M
import qualified Data.Text as T
import Database.Redis.Redis(Redis,Reply,zincrBy)
import Database.Redis.ByteStringClass (BS,toBS,fromBS)
import Data.Text.Encoding as E
import Data.Char (isLetter,isSpace)

instance BS T.Text where
  toBS = encodeUtf8
  fromBS = decodeUtf8

type WordWeights = M.Map T.Text Int

isBlank :: T.Text -> Bool    
isBlank = T.all isSpace    

notBlank :: T.Text -> Bool
notBlank = not . isBlank

storeTermEntry :: Redis -> T.Text -> Int -> FilePath -> IO (Reply T.Text)
storeTermEntry r k v ref = zincrBy r k (fromIntegral v) (T.pack ref) 
  
addTerms :: Redis -> String -> WordWeights -> IO ()
addTerms r ref wordWeights = mapM_ (\(k,v) -> storeTermEntry r k v ref) (M.toList wordWeights)

removeStopWords :: [T.Text] -> [T.Text]
removeStopWords = filter (not . isStopWord)

clean :: T.Text -> T.Text
clean = T.filter isLetter

getTerms :: T.Text -> WordWeights
getTerms ws = foldr (\word count -> M.insertWith' (+) word 1 count) M.empty filteredWords 
  where
    filteredWords = filter notBlank stemmedWords  
    stemmedWords = map stem $ removeStopWords (map clean ((T.words . T.toLower) ws))

getTerm :: T.Text -> T.Text
getTerm = stem . clean . T.toLower
