{-# LANGUAGE OverloadedStrings #-}
module FortuneIndexer (main) where

import Indexer (addTerms,getTerms,isBlank,notBlank)
import Control.Monad (forM_)
import Data.List (groupBy)
import Data.Char (isSpace)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Redis.Redis (Redis,connect,set)

splitOnPercent :: T.Text -> [T.Text]
splitOnPercent = T.splitOn (T.singleton '%')

splitOnBlankLines :: T.Text -> [T.Text]
splitOnBlankLines t = map T.unlines filtered
  where 
    fileLines :: [T.Text]
    fileLines = T.lines t
    
    groups :: [[T.Text]]
    groups = groupBy (\x y -> notBlank x && notBlank y) fileLines
    
    filtered :: [[T.Text]]
    filtered = filter (not . isEmpty) groups

isEmpty :: [T.Text] -> Bool
isEmpty [] = True
isEmpty (x:_) = isBlank x
    
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

main :: IO ()             
main = do
  redis <- connect "localhost" "6379"
  indexFortunes redis
  return ()
