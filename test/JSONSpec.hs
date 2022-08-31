module JSONSpec where

import Data.List (delete)
import JSON
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "watermelon" $ do
    let unObject (JObject lst) = Just lst
        unObject _ = Nothing
        theObject = unObject watermelon
        containsCucu lst = JString "Cucurbitaceae" `elem` lst
    it "length is correct" $ do
      length <$> theObject `shouldBe` Just 6
    it "first entry is genus: Citrullus" $ do
      head <$> theObject
        `shouldBe` Just ("genus", JString "Citrullus")
    it "Cucurbitaceae is in there" $ do
      fmap (containsCucu . map snd) theObject
        `shouldBe` Just True

  describe "serialize" $ do
    it "serializes null" $ do
      serialize JNull `shouldBe` "null"

    it "serializes ints" $ do
      serialize (JInt 12) `shouldBe` "12"

    it "serializes bools" $ do
      serialize (JBoolean True) `shouldBe` "true"

    it "serializes floats" $ do
      serialize (JFloat 4.58329) `shouldBe` "4.58329"

    it "serializes strings" $ do
      serialize (JString "happy happy joy joy")
        `shouldBe` "\"happy happy joy joy\""

    it "serializes arrays of ints" $ do
      serialize (JArray [JInt 1, JInt 2, JInt 3])
        `shouldBe` "[1,2,3]"

    it "serializes arrays of bools" $ do
      serialize (JArray [JBoolean False, JBoolean True])
        `shouldBe` "[false,true]"

    it "serializes objects" $ do
      serialize (JObject [("key", JString "value")])
        `shouldBe` "{\"key\":\"value\"}"

    it "serializes nested structures" $ do
      let example =
            JObject
              [ ( "dudebro",
                  JArray
                    [ JInt 4,
                      JFloat 8.75,
                      JNull
                    ]
                )
              ]
      serialize example `shouldBe` "{\"dudebro\":[4,8.75,null]}"

    it "empty structures" $ do
      serialize (JObject [("", JArray [])]) `shouldBe` "{\"\":[]}"

  describe "get" $ do
    it "gets a top-level object field" $ do
      get [O "family"] watermelon
        `shouldBe` Just (JString "Cucurbitaceae")

    it "gets a nested object field" $ do
      get [O "nutritions", O "sugar"] watermelon
        `shouldBe` Just (JInt 6)

    it "gets a field nested in objects and arrays" $ do
      let example = JObject [("smoky", JArray [JObject [("the", JObject [("bear", JBoolean False)])]])]

      get [O "smoky", A 0, O "the", O "bear"] example
        `shouldBe` Just (JBoolean False)

    it "doesn't find a field with an index that is too large" $ do
      get [O "nutritions", A 56] watermelon `shouldBe` Nothing

    it "gets a field in an array" $ do
      get [A 1] (JArray [JInt 1, JInt 2])
        `shouldBe` Just (JInt 2)

    it "doesn't find a field with an index on an object" $ do
      get [A 0] watermelon `shouldBe` Nothing

    it "doesn't allow negative indicies" $ do
      get [A (-3)] (JArray [JInt 4]) `shouldBe` Nothing

    it "doesn't allow negative indicies" $ do
      get [A 4] (JArray [JInt 4]) `shouldBe` Nothing

    it "doesn't allow indices that are too large" $ do
      let insideList = [JInt 4, JInt 5, JInt 6]
      let tooBig = fromIntegral . length $ insideList

      get [A tooBig] (JArray insideList) `shouldBe` Nothing
      get [A (tooBig - 1)] (JArray insideList)
        `shouldBe` Just (JInt 6)

  describe "jsonToFruit" $ do
    it "parses a Fruit into a JSON blob" $ do
      jsonToFruit watermelon
        `shouldBe` Just (Fruit "Watermelon" 30 0.2)
    it "refuses to parse JSON in an unexpected structure" $ do
      jsonToFruit (JInt 19) `shouldBe` Nothing

    it "parses JSON with a changed structure" $ do
      let messWithJSON (JObject lst) =
            JObject $
              ("name", JString "Watermelon By Another Name") :
              delete ("name", JString "Watermelon") lst

      jsonToFruit (messWithJSON watermelon)
        `shouldBe` Just (Fruit "Watermelon By Another Name" 30 0.2)

  describe "prettySerialize" $ do
    it "pretty serializes watermelon" $ do
      prettySerialize watermelon
        `shouldBe` "{\n    \"genus\": \"Citrullus\",\n    \"name\":\
                   \ \"Watermelon\",\n    \"id\": 25,\n    \"family\":\
                   \ \"Cucurbitaceae\",\n    \"order\": \"Cucurbitales\"\
                   \,\n    \"nutritions\": {\n        \"carbohydrates\":\
                   \ 8,\n        \"protein\": 0.6,\n        \"fat\": 0.2,\
                   \\n        \"calories\": 30,\n        \"sugar\": 6\n\
                   \    }\n}"

    it "pretty serializes nested arrays in objects" $ do
      let wut =
            JArray
              [ JObject
                  [ ( "wut",
                      JArray
                        [ JInt 1,
                          JInt 2,
                          JInt 3
                        ]
                    )
                  ],
                JInt 4
              ]
      prettySerialize wut
        `shouldBe` "[\n    {\n        \"wut\": [\n            1,\n\
                   \            2,\n            3\n        ]\n    },\
                   \\n    4\n]"

    it "pretty serializes empty arrays" $ do
      prettySerialize (JArray [])
        `shouldBe` "[]"

    it "pretty serializes arrays in objects" $ do
      prettySerialize (JObject [("", JArray [])])
        `shouldBe` "{\n    \"\": []\n}"

    it "empty objects" $ do
      prettySerialize (JObject [])
        `shouldBe` "{\n}"
