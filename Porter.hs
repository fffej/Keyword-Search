module Porter (stem) where
-- All information nabbed from http://tartarus.org/~martin/PorterStemmer/
-- http://tartarus.org/~martin/PorterStemmer/haskell.txt
import qualified Data.Text as T

import Prelude 
import Control.Arrow ((***))
import Data.List (isSuffixOf)

foo :: T.Text -> Bool
foo word = not x
  where
    x :: Bool
    x = T.any p (T.pack "lsz")
    p :: Char -> Bool
    p = ((`T.isSuffixOf` word) . T.singleton)
    

-- Note 'y' is handled separately 
vowels :: [Char]
vowels = "aeiou"

-- Note 'y' is handled separately
consonants :: [Char]
consonants = "bcdfghjklmnpqrstvwxz"

isConsonant :: T.Text -> Int -> Bool
isConsonant str i
  | c `elem` consonants = False
  | c == 'y' = i == 0 || p  `elem` vowels
  | otherwise = True
    where
      c = T.index str i
      p = T.index str (pred i)
      
containsVowel :: T.Text -> Bool
containsVowel = undefined -- T.any 
      
step2 :: T.Text -> T.Text
step2 x = foldr (\(src,rep) word -> replaceLast src rep word) x step2Stems

replaceLast :: T.Text  -- ^ Text to search for
            -> T.Text  -- ^ Replacement
            -> T.Text  -- ^ input
            -> T.Text
replaceLast s r i | isLastEmpty = T.append (T.concat (init xs)) r
                  | otherwise = i
  where
    xs = T.splitOn s i
    isLastEmpty = T.null (last xs)
    
step3Stems :: [(T.Text,T.Text)]    
step3Stems = map (T.pack *** T.pack)
             [ ("icate", "ic")
             , ("ative", ""  )
             , ("alize", "al")
             , ("iciti", "ic")
             , ("ical" , "ic")
             , ("ful"  , ""  )
             , ("ness" , ""  ) ]

step2Stems :: [(T.Text,T.Text)]
step2Stems = map (T.pack *** T.pack)
             [ ("ational", "ate" )
             , ("tional",  "tion")
             , ("enci",    "ence")
             , ("anci",    "ance")
             , ("izer",    "ize" )
             , ("bli",     "ble" )
             , ("alli",    "al"  )
             , ("entli",   "ent" )
             , ("eli",     "e"   )
             , ("ousli",   "ous" )
             , ("ization", "ize" )
             , ("ation",   "ate" )
             , ("ator",    "ate" )
             , ("alism",   "al"  )
             , ("iveness", "ive" )
             , ("fulness", "ful" )
             , ("ousness", "ous" )
             , ("aliti",   "al"  )
             , ("iviti",   "ive" )
             , ("biliti",  "ble" )
             , ("logi",    "log" ) ]

stem :: T.Text -> T.Text
stem word | T.length word < 3 = word
          | otherwise = stem' word
                        
stem' :: T.Text -> T.Text                         
stem' = undefined