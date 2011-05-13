module KeywordSearch where

import Porter (stem)
import StopWords (isStopWord)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.List (foldr)
import Control.Monad

data Document = Document {
  filePath :: FilePath ,
  text :: T.Text
}

-- Index the given document
index :: FilePath -> IO ()
index = undefined

removeStopWords :: [T.Text] -> [T.Text]
removeStopWords = filter (not . isStopWord)

-- Given a lump of text, filter out stop words
getTerms :: T.Text -> M.Map T.Text Int
getTerms ws = foldr (\word count -> M.insertWith' (+) word 1 count) M.empty filteredWords 
  where
    filteredWords = removeStopWords ((T.words . T.toLower) ws)

query :: Query -> IO [Document]
query = undefined

-- A simple query language
data Query = Contains T.Text
           | And Query Query
           | Or Query Query
           | Xor Query Query
      