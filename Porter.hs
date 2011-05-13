module Porter where

import Control.Monad
import Control.Arrow ((***))
import Data.Maybe
import Data.List

import Data.Text.IO as IO

import qualified Data.Text as T


isConsonant :: T.Text -> Int -> Bool
isConsonant str i
    | c `elem` "aeiou"  = False
    | c == 'y'          = i == 0 || isVowel str (i - 1)
    | otherwise         = True
    where
        c = T.index str i

isVowel :: T.Text -> Int -> Bool
isVowel = (not .) . isConsonant

byIndex :: (T.Text -> [Int] -> t) -> T.Text -> t
byIndex fun str = fun str [0..T.length str - 1]

measure :: T.Text -> Int
measure = length . filter not . init . (True:) . map head . group . byIndex (map . isConsonant)

containsVowel :: T.Text -> Bool
containsVowel = byIndex (any . isVowel)

endsWithDouble :: T.Text -> Bool
endsWithDouble = startsWithDouble . T.reverse
    where
        startsWithDouble l | T.length l < 2 = False
                           | otherwise    = x == y && x `notElem` "aeiou"
                             where
                               x = T.head l
                               y = T.head $ T.tail l

cvc :: T.Text -> Bool
cvc word | T.length word < 3 = False
         | otherwise       = isConsonant word lastIndex       &&
                             isVowel     word (lastIndex - 1) &&
                             isConsonant word (lastIndex - 2) &&
                             T.last word `notElem` "wxy"
    where lastIndex = T.length word - 1

statefulReplace :: (T.Text -> Bool) -> T.Text -> T.Text -> T.Text -> Maybe (Either T.Text T.Text)
statefulReplace predicate str end replacement
    | end `T.isSuffixOf` str  = Just replaced
    | otherwise               = Nothing
    where
        part  = T.take (T.length str - T.length end) str
        replaced | predicate part = Right (part `T.append` replacement)
                 | otherwise      = Left str

replaceEnd :: (T.Text -> Bool) -> T.Text -> T.Text -> T.Text -> Maybe T.Text
replaceEnd predicate str end replacement = do
            result <- statefulReplace predicate str end replacement
            return (either id id result)


findStem :: (T.Text -> Bool) -> T.Text -> [(T.Text,T.Text)] -> Maybe T.Text
findStem f word pairs = msum $ map (uncurry (replaceEnd f word)) pairs

measureGT :: Int -> T.Text -> Bool
measureGT = flip ((>) . measure)

endings :: [(T.Text,T.Text)]
endings = map (T.pack *** T.pack) [("sses", "ss"), ("ies",  "i"), ("ss", "ss"), ("s", "")]

step1a :: T.Text -> T.Text
step1a word = fromMaybe word result
    where result = findStem (const True) word endings

beforeStep1b :: T.Text -> Either T.Text T.Text
beforeStep1b word = fromMaybe (Left word) result
    where
       cond23 x = do { v <- x; either (const Nothing) (return . Right) v }
       cond1  x = do { v <- x; return (Left v) }
       result =
           cond1  (replaceEnd (measureGT 0)  word (T.pack "eed") (T.pack "ee")) `mplus`
           cond23 (statefulReplace containsVowel word (T.pack "ed") T.empty ) `mplus`
           cond23 (statefulReplace containsVowel word (T.pack "ing") T.empty )

izeEndings :: [(T.Text,T.Text)]
izeEndings = map (T.pack *** T.pack) [("at", "ate"), ("bl", "ble"), ("iz", "ize")]

afterStep1b :: T.Text -> T.Text
afterStep1b word = fromMaybe word result
    where
        double        = endsWithDouble word && not (T.any ((`T.isSuffixOf` word) . T.singleton) (T.pack "lsz"))
        mEq1AndCvc    = measure word == 1 && cvc word
        iif cond val  = if cond then Just val else Nothing
        result        = findStem (const True) word izeEndings
                        `mplus` iif double (T.init word)
                        `mplus` iif mEq1AndCvc (T.snoc word 'e')

step1b :: T.Text -> T.Text
step1b = either id afterStep1b . beforeStep1b

step1c :: T.Text -> T.Text
step1c word = fromMaybe word result
    where result = replaceEnd containsVowel word (T.singleton 'y') (T.singleton 'i')

step1 :: T.Text -> T.Text
step1 = step1c . step1b . step1a

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


step2 :: T.Text -> T.Text
step2 word = fromMaybe word result
    where
       result = findStem (measureGT 0) word step2Stems

step3 :: T.Text -> T.Text
step3 word = fromMaybe word result
    where
       result = findStem (measureGT 0) word step3Stems

step4gt1 :: [T.Text]
step4gt1 = map T.pack ["al", "ance", "ence", "er", "ic", "able", "ible", "ant", "ement", "ment", "ent"]

step4gt2 :: [T.Text]
step4gt2 = map T.pack  ["ou", "ism", "ate", "iti", "ous", "ive", "ize"]


step4 :: T.Text -> T.Text
step4 word = fromMaybe word result
    where
        gt1andST str = measureGT 1 str && T.any ((`T.isSuffixOf` str) . T.singleton) (T.pack "st")
        findGT1      = findStem (measureGT 1) word . map (flip (,) T.empty)
        result       = findGT1 step4gt1 `mplus`
                       findStem gt1andST word [(T.pack "ion",T.empty)] `mplus`
                       findGT1 step4gt2

step5a :: T.Text -> T.Text
step5a word = fromMaybe word result
    where
        test str = measureGT 1 str || ((measure str == 1) && not (cvc str))
        result   = replaceEnd test word (T.singleton 'e') T.empty

step5b :: T.Text -> T.Text
step5b word = fromMaybe word result
    where
       cond s = T.last s == 'l' && measureGT 1 s
       result = replaceEnd cond word (T.singleton 'l') T.empty

step5 :: T.Text -> T.Text
step5 = step5b . step5a

allSteps :: T.Text -> T.Text
allSteps = step5 . step4 . step3 . step2 . step1

stem :: T.Text -> T.Text
stem s | T.length s < 3 = s
       | otherwise    = allSteps s

main :: IO ()
main = do
    content <- IO.readFile "/usr/share/dict/british-english"
    IO.writeFile "/home/jeff/Desktop/output.txt" $ T.unlines $ map stem $ T.lines content