--{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}

module Config.Config (abstr, label) where

import Config.Concr
import Config.Type
import State.Value
import State.State
import State.Env
import qualified Text.ParserCombinators.Parsec as Parsec
{-
class (Dom da [], Dom dc IO) => Config da dc
  where abstr :: dc -> da
        label :: State da -> String -> Bool
-}
abstr :: Concr -> Type
abstr (CFloat _)  = Static TFloat
abstr (CChar _)   = Static TChar
abstr (CString _) = Static TString
abstr (CBool b)   = Static $ TBool b
abstr (CList [])  = Static Null
abstr (CList vs)  = if all isPure vs
                    then let ds = map abstr (map (\(Pure d) -> d) vs)
                         in if all isStatic ds
                            then let ss = map (\(Static s) -> s) ds
                                 in if all (==(head ss)) ss
                                    then head ds
                                    else Dynamic $ DList $ concat $ map refs vs
                            else Dynamic $ DList $ concat $ map refs vs
                    else Dynamic $ DList $ concat $ map refs vs

label :: State Type -> String -> Bool
-- Type error
label (Fail _) "safe" = False
label _        "safe" = True
label (App (Primitive "input-float") _ _ _) "?Float" = True
label _ "?Float" = False
label (App (Primitive "output-float") _ _ _) "!Float" = True
label _ "!Float" = False
-- Is x a float
label (Eval _ r e) "p" = p r e
label (App  _ _ r e) "p" = p r e

p :: Ref -> Env (Value Type) -> Bool
p r e = maybe True
              (\x -> case x
	                 of (Pure (Static TFloat)) -> True
	                    _ -> False)
              (fetch "x" r e)

isPure :: Value d -> Bool
isPure (Pure _) = True
isPure _ = False

isStatic :: Type -> Bool
isStatic (Static _) = True
isStatic _ = False

