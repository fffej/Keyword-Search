module KeywordSearch where

import Porter (stem)

import Data.Text as T

data Document = Document {
  text :: T.Text
}

-- Index the given document
index :: Document -> IO ()
index = undefined

-- Given a document, return the indexable words and a frequency occurrence
indexable :: Document -> [(T.Text,Int)]
indexable = undefined

-- Implement the Porter stemming algorithm
stem :: T.Text -> T.Text
stem = undefined

-- Simple lookup via a map
filterStopWords :: [T.Text] -> [T.Text]
filterStopWords = undefined

query :: Query -> IO [Document]
query = undefined

-- A simple query language
data Query = Contains T.Text
           | And Query Query
           | Or Query Query
           | Xor Query Query
      