module ApiResponse where

import Data.Aeson (FromJSON, parseJSON, (.:))
import Data.Aeson.Types (withObject)
import Data.Text (pack)
import Network.Wreq

apiUrl :: String
apiUrl = "https://documentation-resources.opendatasoft.com/api/records/1.0/search/?dataset=roman-emperors" 

data ApiResponse b = ApiResponse
    { success :: Bool,
      body :: [b]
    } deriving (Show, Eq)

callEndpoint :: String -> Response
callEndpoint url = do
  rsp <- get url
  pure rsp