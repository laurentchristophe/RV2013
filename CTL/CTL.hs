module CTL.CTL (CTL (Cons, Var, Nega, Disj, Conj, EN, AN, EU, AU, ER, AR), CTL.CTL.parse) where

import Text.ParserCombinators.Parsec
import CTL.UBool
import Control.Monad
import Text.JSON

data CTL =   Cons UBool   -- constant
           | Var String   -- propositional variable
           | Nega CTL     -- boolean negation
           | Disj CTL CTL -- boolean disjunction
           | Conj CTL CTL -- boolean conjunction
           | EN CTL       -- exist next
           | AN CTL       -- forall next
           | EU CTL CTL   -- exist until
           | AU CTL CTL   -- forall until
           | ER CTL CTL   -- exist release
           | AR CTL CTL   -- forall release
           deriving (Eq, Ord)

instance Show CTL
  where show (Cons b) = show b
        show (Var s) = "'" ++ s ++ "'"
        show (Nega c) = "!! " ++ (show c)
        show (Disj c1 c2) = "(" ++ (show c1) ++ " and " ++ (show c2) ++ ")"
        show (Conj c1 c2) = "(" ++ (show c1) ++ " or " ++ (show c2) ++ ")"
        show (EN c) = "EN " ++ (show c)
        show (AN c) = "AN " ++ (show c)
        show (EU c1 c2) = "(" ++ (show c1) ++ " EU " ++ (show c2) ++ ")"
        show (AU c1 c2) = "(" ++ (show c1) ++ " AU " ++ (show c2) ++ ")"
        show (ER c1 c2) = "(" ++ (show c1) ++ " ER " ++ (show c2) ++ ")"
        show (AR c1 c2) = "(" ++ (show c1) ++ " AR " ++ (show c2) ++ ")"

instance JSON CTL
  where showJSON = JSString . toJSString . show
        readJSON = error "not implemented"

-------------
-- Parsing --
-------------

parse :: String -> Either ParseError CTL
parse = Text.ParserCombinators.Parsec.parse ctl ""

ctl = choice [try $ eat >> (liftM Cons CTL.UBool.parser),
              try $ var,
              try $ una "!!" Nega,
              try $ bin "||" Disj,
              try $ bin "&&" Conj,
              try $ una "EN" EN,
              try $ una "AN" AN,
              try $ bin "EU" EU,
              try $ bin "AU" AU,
              try $ bin "ER" ER,
              try $ bin "AR" AR]
        
eat = many $ space <|> tab <|> newline

var = do eat
         char '\''
         s <- many (noneOf ['\''])
         char '\''
         return $ Var s

una s f = do eat
             string s
             liftM f ctl

bin s f = do eat
             char '('
             c1 <- ctl
             eat
             string s
             c2 <- ctl
             eat
             char ')'
             return $ f c1 c2
