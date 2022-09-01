module ParserJSON.JParser where
import Data.Char

import ParserJSON.JSON ( JSON(JString, JNull, JBoolean) )

import Text.Parsec hiding
  ( parse
  , choice
  , (<|>)
  , char
  , satisfy
  , eof
  , parserFail
  , sepBy
  , sepBy1
  )
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

--simpler parse implementation
parse :: Parser a -> String -> Either ParseError a 
parse prsr = P.parse prsr ""

char :: Char -> Parser Char
char = P.char

satisfy :: (Char -> Bool) -> Parser Char
satisfy = P.satisfy

eof :: Parser ()
eof = P.eof

parserFail :: String -> Parser a
parserFail = P.parserFail

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (P.try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . fmap P.try

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy body sep = P.sepBy1 body (P.try sep)

augName :: String
augName = "{ \"name\" : \"Augustus\"}" 

jStartParser :: Parser Char
jStartParser = char '{'

--able to parse a JNull
jNullParse :: Parser JSON
jNullParse = (pure JNull <* string "Null")
    <|> (pure JNull <* string "null")
    <|> (pure JNull <* string "NULL")

-- able to parse a JBool
jBoolParse :: Parser JSON
jBoolParse = (pure (JBoolean True) <* string "\"true\"")
    <|> (pure (JBoolean False) <* string "\"false\"")

jStringParser :: Parser JSON
jStringParser = do
    input <- satisfy isAlpha
    pure $ JString input
    

-- NEED A PARSER FOR EACH JSON DATA TYPE
 -- = JNull
  -- | JBoolean Bool
  -- | JInt Int
  -- | JFloat Double
  -- | JString String
  -- | JArray [JSON]
  -- | JObject [(String, JSON)]