{-# LANGUAGE MultiParamTypeClasses #-}

module Config.Type (Type (Static, Dynamic), Static (TBool, TChar, TFloat, TString, SList, Null), Dynamic (DList)) where

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Maybe
import State.State
import State.Env
import State.Value
import State.Expr
import Text.JSON

data Type =   Static Static
            | Dynamic Dynamic
            deriving Show

data Static =   TBool Bool
              | TChar
              | TFloat
              | TString
              | SList Static
              | Null
              deriving (Eq, Show)

data Dynamic = DList [Ref] deriving Show

instance JSON Static
  where showJSON (TBool b) = showJSON b
        showJSON TFloat    = showJSON "float"
        showJSON TChar     = showJSON "char"
        showJSON TString   = showJSON "string"
        showJSON (SList s) = makeObj [("static list", showJSON s)]
        showJSON Null      = showJSON "null"
        readJSON = error "not implemented"

instance JSON Type
  where showJSON (Static s) = showJSON s
        showJSON (Dynamic (DList vs)) = makeObj [("dynamic list", JSArray $ map showJSON vs)]
        readJSON = error "not implemented"

instance Refered Type
  where  refs (Static _) = []
         refs (Dynamic (DList rs)) = rs
         refMap (Static s1, Static s2) = if s1 == s2 then Just [] else Nothing
         refMap (Dynamic _, Dynamic _) = Nothing
         refMap _                      = Nothing

instance Dom Type []
  where -- Misc
        cpu "end" [v] r e = return $ End v
        cpu "eval" [Pure (Static TString), k] r e = []
        -- IO
        cpu "input"        [                       k] r e = return $ App k [Pure $ Static TString] r e
        cpu "input-float"  [                       k] r e = return $ App k [Pure $ Static TFloat]  r e
        cpu "output"       [Pure (Static TString), k] r e = return $ App k [Pure $ Static Null] r e
        cpu "output-float" [Pure (Static TFloat),  k] r e = return $ App k [Pure $ Static Null] r e
        -- Float operations
        cpu "+"    [Pure (Static TFloat), Pure (Static TFloat), k] r e = return $ App k [Pure $ Static TFloat] r e
        cpu "-"    [Pure (Static TFloat), Pure (Static TFloat), k] r e = return $ App k [Pure $ Static TFloat] r e
        cpu "*"    [Pure (Static TFloat), Pure (Static TFloat), k] r e = return $ App k [Pure $ Static TFloat] r e
        cpu "/"    [Pure (Static TFloat), Pure (Static TFloat), k] r e = return $ App k [Pure $ Static TFloat] r e
        -- Bool operation
        cpu "not"  [Pure (Static (TBool b)),                            k] r e = return $ App k [Pure $ Static $ TBool $ not b]    r e
        cpu "and"  [Pure (Static (TBool b1)), Pure (Static (TBool b2)), k] r e = return $ App k [Pure $ Static $ TBool $ b1 && b2] r e
        cpu "or"   [Pure (Static (TBool b1)), Pure (Static (TBool b2)), k] r e = return $ App k [Pure $ Static $ TBool $ b1 || b2] r e
        -- Number comparison
        cpu "="    [Pure (Static TFloat), Pure (Static TFloat), k] r e = [App k [Pure $ Static $ TBool True] r e, App k [Pure $ Static $ TBool False] r e]
        cpu "<"    [Pure (Static TFloat), Pure (Static TFloat), k] r e = [App k [Pure $ Static $ TBool True] r e, App k [Pure $ Static $ TBool False] r e]
        cpu "<="   [Pure (Static TFloat), Pure (Static TFloat), k] r e = [App k [Pure $ Static $ TBool True] r e, App k [Pure $ Static $ TBool False] r e]
        cpu ">"    [Pure (Static TFloat), Pure (Static TFloat), k] r e = [App k [Pure $ Static $ TBool True] r e, App k [Pure $ Static $ TBool False] r e]
        cpu ">="   [Pure (Static TFloat), Pure (Static TFloat), k] r e = [App k [Pure $ Static $ TBool True] r e, App k [Pure $ Static $ TBool False] r e]
        -- Cons
        cpu "cons" [Pure (Static s), Pure (Static Null)      , k] r e = return $ App k [Pure $ Static $ SList s] r e
        cpu "cons" [Pure (Static s), Pure (Static (SList s')), k] r e = if s == s'
                                                                        then return $ App k [Pure $ Static $ SList s] r e
                                                                        else return $ App k [Pure $ Dynamic $ DList []] r e
        cpu "cons" [v              , Pure (Static (SList s')) , k] r e = return $ App k [Pure $ Dynamic $ DList $ refs v] r e
        cpu "cons" [v              , Pure (Static Null)       , k] r e = return $ App k [Pure $ Dynamic $ DList $ refs v] r e
        cpu "cons" [v              , Pure (Dynamic (DList rs)), k] r e = return $ App k [Pure $ Dynamic $ DList $ rs ++ (refs v)] r e
        -- Head
        cpu "head"  [Pure (Static (SList s)), k] r e = return $ App k [Pure $ Static s] r e
        cpu "head"  [Pure (Dynamic (DList _)), k] r e = []
        -- Tail
        cpu "tail" [Pure (Static (SList s)), k] r e = return $ App k [Pure $ Static $ SList s] r e
        cpu "tail" [Pure (Dynamic (DList rs)), k] r e = return $ App k [Pure $ Dynamic $ DList rs] r e
        -- Remaining case
        cpu s _ _ _ = return $ Fail $ "Bad primitive call " ++ s
        -- Library
        lib "null"  = Pure $ Static Null
        lib "true"  = Pure $ Static $ TBool True
        lib "false" = Pure $ Static $ TBool False
        lib s
          | elem s prims = Primitive s
          | otherwise = case (reads s, reads s, reads s)
                        of ([(f,"")],_,_) -> const (Pure $ Static TFloat)  (f :: Float)
                           (_,[(c,"")],_) -> const (Pure $ Static TChar)   (c :: Char)
                           (_,_,[(s,"")]) -> const (Pure $ Static TString) (s :: String)
                           _              -> Pure $ Static Null
        -- Branching
        branch (Pure (Static (TBool b))) = b
        branch _                         = True

prims :: [String]
prims = ["end", "eval", "input", "output", "+", "-", "*", "/", "=", "<", "<=", ">", ">=", "not", "and", "or", "cons", "car", "cdr"]
