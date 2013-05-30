{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module State.State (State (App, Eval, Fail, End), Dom (cpu, lib, branch), start, step, reduce, galois) where

import State.Env
import State.Expr
import State.Value
import Text.JSON

import System.IO.Unsafe

data State d =   Eval CExpr Ref (Env (Value d))
               | App  (Value d) [Value d] Ref (Env (Value d))
               | Fail String
               | End  (Value d)
               deriving Show

class (Refered d, Monad m) => Dom d m | d -> m where
  cpu    :: String -> [Value d] -> Ref -> Env (Value d) -> m (State d)
  lib    :: String -> Value d
  branch :: Value d -> Bool

instance (JSON d) => JSON (State d)
  where showJSON (Eval ce r e)     = makeObj [("eval", showJSON ce), ("ref", showJSON r), ("env", showJSON e)]
        showJSON (App  v  vs  r e) = makeObj [("app",  showJSON (v:vs)), ("ref", showJSON r), ("env", showJSON e)]
        showJSON (Fail s)          = makeObj [("fail", showJSON s)]
        showJSON (End v)           = makeObj [("end", showJSON v)]
        readJSON = error "not implemented"

instance Refered d => Eq (State d)
  where (Eval ce1 r1 e1)   == (Eval ce2 r2 e2)   = if ce1 == ce2 then isomorphic [(r1,r2)] (e1,e2) else False
        (App v1 vs1 r1 e1) == (App v2 vs2 r2 e2) = maybe False ((flip isomorphic (e1,e2)).concat) (mapM refMap (zip (v1:vs1) (v2:vs2)))
        (Fail s1)          == (Fail s2)          = s1 == s2
        (End v1)           == (End v2)           = maybe False null (refMap (v1,v2))
        _                  == _                  = False

start :: CExpr -> State d
start ce = Eval ce root empty

reduce :: (Show d, Dom d m) => Ref -> Env (Value d) -> VExpr -> Value d
reduce r e (Variable s)   = maybe (lib s) id (seq (unsafePerformIO $ putStrLn $ show $ fetch s r e)
                                                  fetch s r e)
reduce r _ (Lambda ss ce) = Closure r ss ce

step :: (Show d, Dom d m) => State d -> m (State d)
step (Eval (Call ve ves) r e) = return $ App (reduce r e ve) (map (reduce r e) ves) r e
step (Eval (Define s ve ce) r e) = return $ Eval ce r (define s (reduce r e ve) r e)
step (Eval (Assign s ve ce) r e) = return $ maybe (Eval ce r e) (Eval ce r) (assign s (reduce r e ve) r e)
step (Eval (Branch ve ce1 ce2) r e) = if branch $ reduce r e ve
                                      then return $ Eval ce1 r e
                                      else return $ Eval ce2 r e
step (App  (Pure _) _ _ _) = return $ Fail "Pure value used as a procedure."
step (App  (Primitive s) vs r e) = cpu s vs r e
step (App  (Closure r ss ce) vs _ e) = if (length vs) /= (length ss)
                                       then return $ Fail "Argument number mismatch."
                                       else let r' = push r e
                                                e' = extend ss vs r' e
                                            in return $ Eval ce r' e'
step s@(Fail _) = return s
step s@(End  _) = return s

galois :: (d1 -> d2) -> State d1 -> State d2
galois f (Eval ce r e)  = Eval ce r (mapValues (mapDom f) e)
galois f (App v vs r e) = App (mapDom f v) (map (mapDom f) vs) r (mapValues (mapDom f) e)
galois f (Fail s)       = Fail s
galois f (End v)        = End $ mapDom f v

