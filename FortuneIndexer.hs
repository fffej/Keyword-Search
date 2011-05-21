{-# LANGUAGE OverloadedStrings #-}

module FortuneIndexer (indexFortunes) where

import Indexer (addDoc,getTerms)
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
  forM_ (zip fortunes [1..]) (\(fortune,n) -> do
                                 set redis (path ++ show n) fortune)
  forM_ (zip termCounts [1..]) (\(terms,n) -> do
                                   let ref = (path ++ show n)
                                   addDoc redis ref terms)    
    
indexFortunes :: Redis -> IO () 
indexFortunes r = forM_ fortunes (\x -> indexFortune r x)