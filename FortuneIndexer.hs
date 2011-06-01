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
import StopWords (isStopWord)

instance BS T.Text where
  toBS = encodeUtf8
  fromBS = decodeUtf8

type WordWeights = M.Map T.Text Int

splitOnPercent :: T.Text -> [T.Text]
splitOnPercent = T.splitOn "%"

splitOnBlankLines :: T.Text -> [T.Text]
splitOnBlankLines t = (init . map T.concat) (splitOn [" "] (T.lines t))
  
fortunePaths :: [(FilePath,T.Text -> [T.Text])]
fortunePaths = [ ("./fortune/CatV.fortune", splitOnBlankLines)
           , ("./fortune/FreeBsd.fortune", splitOnPercent)
           , ("./fortune/KernelNewbies.fortune", splitOnPercent)]

indexFortune :: Redis -> (FilePath,T.Text -> [T.Text]) -> IO ()
indexFortune redis (path,sep) = do
  fortunesText <- T.readFile path
  let fortunes = sep fortunesText
  let termCounts = map getTerms fortunes
  forM_ (zip fortunes [1..]) (\(fortune,n) -> set redis (path ++ show n) fortune)
  forM_ (zip termCounts [1..]) (\(terms,n) -> addTerms redis (path ++ show n) terms)
    
indexFortunes :: Redis -> IO () 
indexFortunes r = forM_ fortunePaths (indexFortune r)

storeTermEntry :: Redis -> T.Text -> Int -> FilePath -> IO (Reply T.Text)
storeTermEntry r k v ref = zincrBy r k (fromIntegral v) (T.pack ref) 
  
addTerms :: Redis -> String -> WordWeights -> IO ()
addTerms r ref wordWeights = mapM_ (\(k,v) -> storeTermEntry r k v ref) (M.toList wordWeights)

getTerms :: T.Text -> WordWeights
getTerms ws = foldr (\word counts -> M.insertWith' (+) word 1 counts) M.empty stemmedWords 
  where
    stemmedWords = map getTerm $ filter (not . isStopWord) $ (T.words . T.toLower) ws

getTerm :: T.Text -> T.Text
getTerm = stem . T.filter isLetter . T.toLower

main :: IO ()             
main = connect "localhost" "6379" >>= indexFortunes
