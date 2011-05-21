{-# LANGUAGE OverloadedStrings #-}
module FortuneIndexer (main) where

import Indexer (addTerms,getTerms)
import Control.Monad (forM_)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Redis.Redis

fortunes :: [FilePath]
fortunes = [ "./fortune/CatV.fortune"
           , "./fortune/FreeBsd.fortune"
           , "./fortune/Offensive.fortune"
           , "./fortune/KernelNewbies.fortune"]

indexFortune :: Redis -> FilePath -> IO ()
indexFortune redis path = do
  fortunesText <- T.readFile path
  let fortunes = (T.splitOn "%" fortunesText)
  let termCounts = map getTerms fortunes
  forM_ (zip fortunes [1..]) (\(fortune,n) -> set redis (path ++ show n) fortune)
  forM_ (zip termCounts [1..]) (\(terms,n) -> addTerms redis (path ++ show n) terms)
    
indexFortunes :: Redis -> IO () 
indexFortunes r = forM_ fortunes (indexFortune r)

-- TODO Should I bother using command line arguments?
main :: IO ()             
main = do
  redis <- connect "localhost" "6379"
  indexFortunes redis
  return ()
