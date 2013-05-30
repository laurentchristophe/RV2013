module State.Value (Value (Closure, Primitive, Pure), mapDom) where

import Text.JSON
import State.Expr
import State.Env

data Value d =   Closure Ref [String] CExpr
               | Primitive String
               | Pure d
               deriving Show

instance Refered d => Refered (Value d) where
  refs (Closure r _ _) = [r]
  refs (Primitive _)   = []
  refs (Pure d)        = refs d
  refMap (Closure r1 ss1 ce1, Closure r2 ss2 ce2) = if (ss1 == ss2) && (ce1 == ce2) then Just [(r1,r2)] else Nothing
  refMap (Primitive s1      , Primitive s2)       = if s1 == s2 then Just [] else Nothing
  refMap (Pure d1           , Pure d2)            = refMap (d1, d2)
  refMap (_                 , _)                  = Nothing

instance (JSON d) => JSON (Value d)
  where showJSON (Closure a ss ce) = makeObj [("address", showJSON a), ("lambda", showJSON $ Lambda ss ce)]
        showJSON (Primitive s)     = makeObj [("primitive", showJSON s)]
        showJSON (Pure d)          = makeObj [("value", showJSON d)]
        readJSON = error "not implemented"

mapDom :: (d1 -> d2) -> Value d1 -> Value d2
mapDom f (Closure r ss ce) = Closure r ss ce
mapDom f (Primitive s)     = Primitive s
mapDom f (Pure d)          = Pure $ f d
