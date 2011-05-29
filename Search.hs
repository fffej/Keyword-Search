module Search (
    getQueryResponse
  , parseQuery
  , query 
  ) where

import Indexer (getTerm)

import qualified Data.Text as T
import Database.Redis.Redis
import Data.List.Split (splitEvery)

import Data.Either (either)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator 
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token  
import Text.ParserCombinators.Parsec.Language 
import Text.ParserCombinators.Parsec.Error

data Query = Contains T.Text
           | And Query Query
           | Or Query Query
           deriving (Show,Eq,Read)
                    
-- http://www.haskell.org/haskellwiki/Parsing_expressions_and_statements
-- provides most of the boiler plate in easy to understand form
queryLang :: LanguageDef st                    
queryLang = emptyDef { identStart = letter
                     , identLetter = alphaNum
                     , reservedNames = ["and", "or"]
               }                  

TokenParser { parens = m_parens
            , identifier = m_identifier
            , reserved = m_reserved
            , whiteSpace = m_whiteSpace } = makeTokenParser queryLang
                                            
queryParser :: Parser Query                 
queryParser = buildExpressionParser table term <?> "query"

table :: OperatorTable Char () Query
table = [[binary "and" And AssocLeft, binary "or" Or AssocLeft]]
        
binary :: String -> (a -> a -> a) -> Assoc -> Operator Char () a    
binary name fun = Infix (do{ m_reserved name; return fun }) 
        
term = m_parens queryParser
       <|> fmap mkImplicitOr (many1 m_identifier)
       <?> "query term"

mkImplicitOr :: [String] -> Query
mkImplicitOr xs = foldl1 Or $ map (Contains . T.pack) xs

parseQuery :: String -> Either String Query
parseQuery q = either processError (Right . id) x
  where
    x = parse queryParser "http" q
    
processError :: ParseError -> Either String Query    
processError p = Left (concatMap messageString $ errorMessages p)
    
isLeft :: Either a b -> Bool
isLeft (Left x) = True
isLeft _ = False

getKey :: Query -> T.Text      
getKey = T.pack . show

query :: Redis -> Query -> IO T.Text
query r q@(And lhs rhs) = do
  lhs' <- query r lhs
  rhs' <- query r rhs
  let key = getKey q
  _ <- zinterStore r key [lhs',rhs'] [] SUM
  return key

query r q@(Or lhs rhs) = do
  lhs' <- query r lhs
  rhs' <- query r rhs
  let key = getKey q
  _ <- zunionStore r key [lhs',rhs'] [] SUM
  return key

query _ (Contains text) = return (getTerm text)
  
getQueryResponse :: Redis -> T.Text -> IO [T.Text]
getQueryResponse r key = do
  resp <- zrange r key (0,99999999) True 
  x <- fromRMultiBulk' resp
  let v = map head (splitEvery 2 x) :: [T.Text]
  mapM (\x -> get r x >>= fromRBulk') v
  
  
  
