module Search where

import Indexer (getTerm)

import qualified Data.Text as T
import Database.Redis.Redis
import Data.List.Split (splitEvery)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator 
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token  
import Text.ParserCombinators.Parsec.Language 

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
binary name fun assoc = Infix (do{ m_reserved name; return fun }) assoc      
        
term = m_parens queryParser
       <|> fmap mkContains m_identifier
       <?> "query term"

mkContains :: String -> Query
mkContains = Contains . T.pack

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
  
  
  
