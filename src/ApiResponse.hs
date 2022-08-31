module ApiResponse where

import Data.Aeson (FromJSON, parseJSON, (.:))
import Data.Aeson.Types (withObject)
import Data.Text (pack)

data ApiResponse b = ApiResponse
    { success :: Bool,
      body :: [b]
    } deriving (Show, Eq)

