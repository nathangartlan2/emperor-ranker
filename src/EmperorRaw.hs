module EmperorRaw where

import Year
import Data.List

import Data.Aeson
import Data.Aeson.Types (withObject)
import Data.Text (pack)
import Control.Applicative
import Control.Monad



data EmperorRaw 
    = EmperorRaw {
                birth_cty :: String,
                name :: String,
                index:: Integer,
                name_full:: String
    }


-- instance FromJSON EmperorRaw where
--  parseJSON (Object v) =
--     EmperorRaw <$> v .: "birth_cty"
--            <*> v .: "name"
--            <*> v .: "index"
--            <*> v .: "name_full"
--  parseJSON _ = mzero

-- instance ToJSON EmperorRaw where
--  toJSON (EmperorRaw firstName lastName age likesPizza) =
--     object [ "birth_cty"  .= birth_cty
--            , "name"   .= name
--            , "index"        .= index
--            , "name_full" .= name_full
--              ]

augustus :: String
augustus = "{\"name\": \"augustus\", \"reignStart\" : 27, \"reignEnd\" : 14, \"bio\" : \"The first Roman Emperor\", \"killedBy\" : \"natural causes\"}"

augustus1 :: String
augustus1 = "{\"name\": \"augustus\", \"reignStart\" : 27, \"reignEnd\" : 14, \"bio\" : \"First Roman\"}"