module Year where

data Era 
    =  Ad 
    | Bc
    deriving (Show, Eq)

data Year = Year {
        value :: Int,
        era :: Era
    }
    deriving (Show, Eq)
