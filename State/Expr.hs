module State.Expr (VExpr (Variable, Lambda), CExpr (Call, Branch, Assign, Define), parseCExpr, parseVExpr) where 

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.List
import Text.JSON

data CExpr =   Call VExpr [VExpr]
             | Branch VExpr CExpr CExpr
             | Assign String VExpr CExpr
             | Define String VExpr CExpr
             deriving (Eq, Show)

data VExpr =   Variable String 
             | Lambda [String] CExpr
             deriving (Eq, Show)

instance JSON CExpr
  where showJSON (Call ve ves) = makeObj [("procedure", showJSON ve), ("arguments", showJSON ves)]
        showJSON (Branch ve ce1 ce2) = makeObj [("predicate", showJSON ve), ("consequent", showJSON ce1), ("alternative", showJSON ce2)]
        showJSON (Assign s ve ce) = makeObj [("define", showJSON s), ("value", showJSON ve), ("body", showJSON ce)]
        showJSON (Define s ve ce) = makeObj [("assign", showJSON s), ("value", showJSON ve), ("body", showJSON ce)]
        readJSON = error "not implemented"

instance JSON VExpr
  where showJSON (Variable s)   = showJSON s
        showJSON (Lambda ss ce) = makeObj [("parameters", showJSON ss), ("body", showJSON ce)]
        readJSON = error "not implemented"

parseVExpr :: String -> Either ParseError VExpr
parseVExpr = Text.ParserCombinators.Parsec.parse vexpr ""

parseCExpr :: String -> Either ParseError CExpr
parseCExpr = Text.ParserCombinators.Parsec.parse cexpr ""

cexpr = choice [try branch, try assign, try define, try call]

vexpr = (try lambda) <|> (liftM Variable var)

eat = many (space <|> tab <|> newline)

var = eat >> (choice [try vchar, try vstring, try vany])
  where vchar = do char '\''
                   c <- anyChar
                   char '\''
                   return $ '\'':c:'\'':[]
        vstring = do char '"'
                     s <- many $ noneOf ['"']
                     char '"'
                     return $ "\"" ++ s ++ "\""
        vany = (many1 $ noneOf [' ', '(', ')', '\n', '\t'])

call = do eat
          char '('
          ve <- vexpr
          ves <- many vexpr
          eat
          char ')'
          return $ Call ve ves

branch = do eat
            char '('
            eat
            string "if"
            ve <- vexpr
            ce1 <- cexpr
            ce2 <- cexpr
            eat
            char ')'
            return $ Branch ve ce1 ce2

assign = do eat
            char '('
            eat
            string "set!"
            s <- var
            ve <- vexpr
            ce <- cexpr
            eat
            char ')'
            return $ Assign s ve ce

define = do eat
            char '('
            eat
            string "define"
            s <- var
            ve <- vexpr
            ce <- cexpr
            eat
            char ')'
            return $ Define s ve ce

lambda = do eat
            char '('
            eat
            string "lambda"
            eat
            char '('
            ss <- many $ var
            eat
            char ')'
            ce <- cexpr
            eat
            char ')'
            return $ Lambda ss ce



