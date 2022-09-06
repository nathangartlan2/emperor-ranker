module ParserJSON.JParser where
import Data.Char

import ParserJSON.JSON 

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

watermelonEsc :: String
watermelonEsc = "{\"genus\":\"Citrullus\",\"name\":\"Watermelon\",\"id\":25,\"family\":\"Cucurbitaceae\",\"order\":\"Cucurbitales\",\"nutritions\":{\"carbohydrates\":[1,2,3,4],\"protein\":0.6,\"fat\":0.2,\"calories\":30,\"sugar\":6}}"

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

--simpler sepBy1 implementation
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

augName :: String
augName = "\"name\":\"Augustus\"" 

jStartParser :: Parser Char
jStartParser = char '{'

--able to parse a JNull
jNullParser :: Parser JSON
jNullParser = (pure JNull <* string "Null")
    <|> (pure JNull <* string "null")
    <|> (pure JNull <* string "NULL")

-- able to parse a JBool
jBoolParser :: Parser JSON
jBoolParser = (pure (JBoolean True) <* string "\"true\"")
    <|> (pure (JBoolean False) <* string "\"false\"")

jStringEscapedParser :: Parser JSON
jStringEscapedParser = do
    _ <- (P.string "\"")
    input <- (P.many $ P.noneOf ['\"']) 
    _ <- (P.string "\"")
    pure $ JString input

jFullStringParser :: Parser JSON
jFullStringParser = jStringEscapedParser

jFloatParser :: Parser JSON
jFloatParser = do
   high_digits <- P.manyTill P.digit (P.char '.')
   low_digits <- P.many P.digit
   let highs = read high_digits :: Double
   let low = (read low_digits :: Double) / (10^(length low_digits))
   pure $ JFloat $ highs + low

jIntParser :: Parser JSON
jIntParser = do
   digString <- P.many P.digit
   let digits = read digString :: Int
   pure $ JInt $ digits 

   

-- building up the combined parser
jPrimitiveParse :: Parser JSON
jPrimitiveParse =  jNullParser 
    <|> jBoolParser
    <|> jObjectParser
    <|> jArryParser
    <|> jFullStringParser
    <|> jFloatParser
    <|> jIntParser


jParse :: Parser JSON
jParse = jPrimitiveParse
  

--parser to parse a command conjuction
jsonParseCombined :: Parser [JSON]
jsonParseCombined = do 
    jsons <- (sepBy1 (jParse) (P.string ","))
    _ <- P.eof
    pure jsons

parseString :: Parser String
parseString = do
    _ <- (P.string "\"")
    input <- (P.many $ P.noneOf ['\"']) 
    _ <- (P.string "\"")
    pure input


keyValueSeperator :: P.Parsec String () ()
keyValueSeperator = do
    P.spaces
    P.char ':'
    P.spaces

jPairParser :: Parser (String, JSON)
jPairParser = do
  _ <- P.spaces
  nm <- parseString
  _ <- keyValueSeperator
  dt <- jPrimitiveParse
  _ <- P.spaces
  pure (nm, dt)
 
pairSeparator :: P.Parsec String () ()
pairSeparator = do
    P.spaces
    P.char ','
    P.spaces

jObjectParser :: Parser JSON
jObjectParser = do
   _ <- char '{'
   parsedObject <-  sepBy jPairParser (pairSeparator) 
   _ <- char '}'
   pure $ JObject parsedObject

jArryParser :: Parser JSON
jArryParser = do 
   _ <- char '['
   parsedObject <-  sepBy jPrimitiveParse (pairSeparator) 
   _ <- char ']'
   pure $ JArray parsedObject

parseInput :: String -> Maybe JSON
parseInput str = case (parsed) of
                  Right x -> Just x
                  Left _ -> Nothing
    where parsed = (do
            jsons <- parse jPrimitiveParse str
            pure (jsons))
    



readJsonFile :: String -> IO String
readJsonFile nm = do
   content <- readFile $ "./data/" ++  nm ++ ".json"
   pure content 

-- parseJsonFromFile :: String -> Maybe JSON 
-- parseJsonFromFile nm = do
--         text <- readJsonFile nm
--         return $ JNull


