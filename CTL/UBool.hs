module CTL.UBool (UBool (T, F, U), parser) where

import Text.ParserCombinators.Parsec
import Text.JSON

data UBool = T | F | U deriving (Eq, Ord, Show)

parser :: Parser UBool
parser = spaces >> (choice [(char 'T') >> return T,
                            (char 'F') >> return F,
                            (char 'U') >> return U])

instance JSON UBool
  where showJSON = showJSON . show
        readJSON = error "not implemented"
