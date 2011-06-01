{-# LANGUAGE OverloadedStrings #-}
module FortuneIndexer (main) where

import Indexer (addTerms,getTerms)
import Control.Monad (forM_)
import Data.List.Split (splitOn)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Redis.Redis (Redis,connect,set)

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

main :: IO ()             
main = connect "localhost" "6379" >>= indexFortunes
