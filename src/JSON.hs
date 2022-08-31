module JSON 
where

import Data.List 

import Text.Parsec hiding (parse, runParser, (<|>), sepBy1, choice)
import qualified Text.Parsec as P 
import Text.Parsec.String (Parser)
 
-- Exercise 1: watermelon
data JSON
  = JNull
  | JBoolean Bool
  | JInt Int
  | JFloat Double
  | JString String
  | JArray [JSON]
  | JObject [(String, JSON)]
  deriving (Show, Eq)



--test
jstringTuple :: (String, String) -> (String, JSON)
jstringTuple inp =  fmap (\x -> JString x) inp

jdoubleTuple :: (String, Double) -> (String, JSON)
jdoubleTuple inp =  fmap (\x -> JFloat x) inp



stringList1 = [("genus","Citrullus"),("name", "Watermelon")]
doubleList = [("id", 25)]
stringList2 = [("family", "Cucurbitaceae"),("order", "Cucurbitales")]
testNutrientList = [("carbohydrates", 8), ("protein",0.6), ("fat", 0.2), ("calories",30),  ("sugar",6)]

makeNutrient :: (String, Double) -> (String, JSON)
makeNutrient (name,value) = (name, JFloat value)


makeNutrientList :: [(String, Double)] -> JSON
makeNutrientList lst =  JObject rawList
      where rawList = map (\item -> makeNutrient item) lst


testList10 = fmap (\x -> jstringTuple x) stringList1

--NATHAN'S ORIGINAL
-- watermelon :: JSON
-- watermelon = JObject $ first ++ second ++ third   ++ fourth
--     where first = fmap (\x -> jstringTuple x) stringList1
--           second = fmap (\x -> jdoubleTuple x) doubleList
--           third = fmap (\x -> jstringTuple x) stringList2
--           fourth = [("Nutrients" , makeNutrientList testNutrientList)]

watermelon :: JSON
watermelon = JObject [
	   ("genus", JString "Citrullus"),
	   ("name", JString "Watermelon"),
	   ("id", JInt 25),
	   ("family", JString "Cucurbitaceae"),
	   ("order", JString "Cucurbitales"),
	   ("nutritions", JObject [
	     ("carbohydrates", JInt 8),
	     ("protein", JFloat 0.6),
	     ("fat", JFloat 0.2),
	     ("calories", JInt 30),
	     ("sugar", JInt 6)
	   ])
	 ]


nutrientList :: JSON
nutrientList = JObject [
	   ("nutritions", JObject [
	     ("carbohydrates", JInt 8),
	     ("protein", JFloat 0.6),
	     ("fat", JFloat 0.2),
	     ("calories", JInt 30),
	     ("sugar", JInt 6)
	   ])
	 ]

carbs :: (String, JSON)
carbs = ("carbohydrates", JArray [JInt 1, JInt 2, JInt 3])

  -- = JNull
  -- | JBoolean Bool
  -- | JInt Int
  -- | JFloat Double
  -- | JString String
  -- | JArray [JSON]
  -- | JObject [(String, JSON)]

quote :: String -> String
quote str = "\"" ++ str ++ "\"" 

-- Exercise 2: serialize
serialize :: JSON -> String
serialize (JString x) = show x
serialize (JBoolean x) = if x then "true" else "false"
serialize (JInt x) = show x
serialize (JFloat x) = show x
serialize (JNull) = "null"
serialize (JObject lst) = serialListHelper lst
serialize (JArray arr) = "[" ++ (commaify $ map serialize arr) ++ "]"

--take list of string and turn into a comma seperated string
commaify :: [String] -> String
commaify lst =
      foldl (\acc item -> if acc == "" then (item) else (item) ++ "," ++ acc) "" (reverse lst)

serialTupleHelper :: (String, JSON) -> String
serialTupleHelper (key, val) = "{" ++ (quote key) ++ ":" ++ (serialize val) ++ "}"

serialListHelper :: [(String, JSON)] -> String
serialListHelper lst = commaify listed
        where listed =  map (\x -> serialTupleHelper x) lst
      


-- Exercise 3: get
data NextStep
  = O String
  | A Int
  deriving Show

--   λ> get [O "family"] watermelon
-- Just (JString "Cucurbitaceae")
-- λ> get [O "nutritions", O "sugar"] watermelon
-- Just (JInt 6)


--I know the syntax is bad, but I'm truong t 
get :: [NextStep] -> JSON -> Maybe JSON
get = undefined  --_ -> JObject lst -> map (isMatch) lst


isMatch :: String -> (String, JSON) -> Maybe JSON
isMatch name (key, value) 
    | (name == key ) = Just value
    | otherwise = Nothing

--trying to find the match in the JSon object
findOnObject ::  String -> JSON -> Maybe JSON
findOnObject str (JObject lst) = case filtered of
         [] -> Nothing
         otherwise -> head filtered
      where checkList =  [(isMatch str)] <*> lst
            filtered = filter (\x -> x /= Nothing) checkList

--just straight didn't get to this
-- Exercise 4: jsonToFruit
data Fruit = Fruit { name :: String
                   , calories :: Int
                   , fat :: Double
                   }
             deriving (Show, Eq)

jsonToFruit :: JSON -> Maybe Fruit
jsonToFruit = undefined


-- Extra Credit: prettySerialize
prettySerialize :: JSON -> String
prettySerialize = undefined

deserializePure :: String -> JSON
deserializePure = undefined

watermelonString :: String
watermelonString = "{\"genus\":\"Citrullus\"},{\"name\":\"Watermelon\"},{\"id\":25},{\"family\":\"Cucurbitaceae\"},{\"order\":\"Cucurbitales\"},{\"nutritions\":{\"carbohydrates\":8},{\"protein\":0.6},{\"fat\":0.2},{\"calories\":30},{\"sugar\":6}}"




readJsonFile :: IO ()
readJsonFile = do
   content <- readFile "./data/watermelon.json"
   putStrLn content 