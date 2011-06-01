{-# LANGUAGE OverloadedStrings #-}
module FortuneIndexer (
  getTerm
                      ) where

import Control.Monad (forM_)
import Data.List.Split (splitOn)
import Data.Char (isLetter, isSpace)

import Data.Text.Encoding as E
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Database.Redis.Redis (Redis,Reply,connect,set,zincrBy)
import Database.Redis.ByteStringClass (BS,toBS,fromBS)

import Porter (stem)
import StopWords

splitOnPercent :: T.Text -> [T.Text]
splitOnPercent = T.splitOn "%"

splitOnBlankLines :: T.Text -> [T.Text]
splitOnBlankLines t = (init . map T.concat) (splitOn [" "] (T.lines t))
  
fortunes :: [(FilePath,T.Text -> [T.Text])]
fortunes = [ ("./fortune/CatV.fortune", splitOnBlankLines)
           , ("./fortune/FreeBsd.fortune", splitOnPercent)
           , ("./fortune/KernelNewbies.fortune", splitOnPercent)]

indexFortune :: Redis -> (FilePath,T.Text -> [T.Text]) -> IO ()
indexFortune redis (path,sep) = do
  fortunesText <- T.readFile path
  let fortunes' = sep fortunesText
  let termCounts = map getTerms fortunes'
  forM_ (zip fortunes' [1..]) (\(fortune,n) -> set redis (path ++ show n) fortune)
  forM_ (zip termCounts [1..]) (\(terms,n) -> addTerms redis (path ++ show n) terms)
    
indexFortunes :: Redis -> IO () 
indexFortunes r = forM_ fortunes (indexFortune r)

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

main :: IO ()             
main = connect "localhost" "6379" >>= indexFortunes
