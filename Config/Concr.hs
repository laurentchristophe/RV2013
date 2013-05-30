{-# LANGUAGE MultiParamTypeClasses #-}

module Config.Concr (Concr (CFloat, CChar, CString, CBool, CList)) where

import Text.ParserCombinators.Parsec
import Control.Monad
import State.State
import State.Env
import State.Value
import State.Expr
import Text.JSON

data Concr =   CFloat Float
             | CChar Char
             | CString String
             | CBool Bool
             | CList [Value Concr]
             deriving Show

instance JSON Concr
  where showJSON (CFloat f)  = showJSON f
        showJSON (CChar c)   = showJSON c
        showJSON (CString s) = showJSON s
        showJSON (CBool b)   = showJSON b
        showJSON (CList vs)  = showJSON vs
        readJSON = error "not implemented"

instance Refered Concr
  where  refs (CList vs) = concat $ map refs vs
         refs _      = []
         refMap (CFloat f1, CFloat f2) = if f1 == f2 then Just [] else Nothing
         refMap (CChar c1 , CChar c2)  = if c1 == c2 then Just [] else Nothing
         refMap (CBool b1 , CBool b2)  = if b1 == b2 then Just [] else Nothing
         refMap (CList vs1, CList vs2) = liftM concat (mapM refMap (zip vs1 vs2))
         refMap _                      = Nothing

instance Dom Concr IO
  where -- Misc
        cpu "end"    [v] r e = return $ End v
        cpu "eval"  [(Pure (CString s)), k] r e = return $ either (\er -> Fail $ "Eval parse error: " ++ (show er))
                                                                  (\ex -> App k [State.State.reduce r e ex] r e)
                                                                  (State.Expr.parseVExpr s)
        -- IO
        cpu "input"        [k]                   r e = getLine >>= (\s -> return $ App k [Pure $ CString s] r e)
        cpu "input-float"  [k]                   r e = getLine >>= (\s -> return $ App k [Pure $ CFloat $ read s] r e)
        cpu "output"       [Pure (CString s), k] r e = (putStrLn s)        >> (return $ App k [Pure $ CList []] r e)
        cpu "output-float" [Pure (CFloat f),  k] r e = (putStrLn $ show f) >> (return $ App k [Pure $ CList []] r e)
        -- Numbers
        cpu "+"    [Pure (CFloat f1), Pure (CFloat f2), k] r e = return $ App k [Pure $ CFloat $ f1 + f2] r e
        cpu "-"    [Pure (CFloat f1), Pure (CFloat f2), k] r e = return $ App k [Pure $ CFloat $ f1 - f2] r e
        cpu "*"    [Pure (CFloat f1), Pure (CFloat f2), k] r e = return $ App k [Pure $ CFloat $ f1 * f2] r e
        cpu "/"    [Pure (CFloat f1), Pure (CFloat f2), k] r e = return $ App k [Pure $ CFloat $ f1 / f2] r e
        -- Number comparison
        cpu "="    [Pure (CFloat f1), Pure (CFloat f2), k] r e = return $ App k [Pure $ CBool $ f1 == f2] r e
        cpu "<"    [Pure (CFloat f1), Pure (CFloat f2), k] r e = return $ App k [Pure $ CBool $ f1 <  f2] r e
        cpu "<="   [Pure (CFloat f1), Pure (CFloat f2), k] r e = return $ App k [Pure $ CBool $ f1 <= f2] r e
        cpu ">"    [Pure (CFloat f1), Pure (CFloat f2), k] r e = return $ App k [Pure $ CBool $ f1 >  f2] r e
        cpu ">="   [Pure (CFloat f1), Pure (CFloat f2), k] r e = return $ App k [Pure $ CBool $ f1 >= f2] r e
        -- Boolean
        cpu "not"  [Pure (CBool b)                    , k] r e = return $ App k [Pure $ CBool $ not b]    r e
        cpu "and"  [Pure (CBool b1), Pure (CBool b2)  , k] r e = return $ App k [Pure $ CBool $ b1 && b2] r e
        cpu "or"   [Pure (CBool b1), Pure (CBool b2)  , k] r e = return $ App k [Pure $ CBool $ b1 || b2] r e
        -- Lists
        cpu "cons" [v, Pure (CList vs)                , k] r e = return $ App k [Pure $ CList $ v:vs]     r e
        cpu "car"  [Pure (CList (v:_))                , k] r e = return $ App k [v]                       r e
        cpu "cdr"  [Pure (CList (_:vs))               , k] r e = return $ App k [Pure $ CList vs]         r e
        -- Remaining case
        cpu s _ _ _ = return $ Fail $ "Bad primitive call " ++ s
        -- Library
        lib "null"  = Pure $ CList []
        lib "true"  = Pure $ CBool True
        lib "false" = Pure $ CBool False
        lib s
          | elem s prims = Primitive s
          | otherwise = case (reads s, reads s, reads s)
                        of ([(f,"")],_,_) -> Pure $ CFloat  f
                           (_,[(c,"")],_) -> Pure $ CChar   c
                           (_,_,[(s,"")]) -> Pure $ CString s
                           _              -> Pure $ CList []
        -- Branching
        branch (Pure (CBool b)) = b
        branch _                = True

prims :: [String]
prims = ["end", "eval", "input", "output", "+", "-", "*", "/", "=", "<", "<=", ">", ">=", "not", "and", "or", "cons", "car", "cdr"]
