module Indexer (
  TermWeights,
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
import Database.Redis.Redis
import Database.Redis.ByteStringClass
import Data.Text.Encoding as E
import Data.Char (isLetter,isSpace)

instance BS T.Text where
  toBS = encodeUtf8
  fromBS = decodeUtf8

type TermWeights = M.Map T.Text Int

isBlank :: T.Text -> Bool    
isBlank = T.all isSpace    

notBlank :: T.Text -> Bool
notBlank = not . isBlank

storeTermEntry :: Redis -> T.Text -> Int -> FilePath -> IO (Reply T.Text)
storeTermEntry r k v ref = zincrBy r k (fromIntegral v) (T.pack ref) 
  
addTerms :: Redis -> String -> TermWeights -> IO ()
addTerms r ref termWeights = do
  mapM_ (\(k,v) -> storeTermEntry r k v ref) (M.toList termWeights)
  return ()

removeStopWords :: [T.Text] -> [T.Text]
removeStopWords = filter (not . isStopWord)

clean :: T.Text -> T.Text
clean = T.filter isLetter

getTerms :: T.Text -> TermWeights
getTerms ws = foldr (\word count -> M.insertWith' (+) word 1 count) M.empty filteredWords 
  where
    filteredWords = filter notBlank stemmedWords  
    stemmedWords = map stem $ removeStopWords (map clean ((T.words . T.toLower) ws))

getTerm :: T.Text -> T.Text
getTerm = stem . clean . T.toLower
