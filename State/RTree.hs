module State.RTree (Ref, RTree, Refered (refs, refMap), make, root, isomorphic, reachable, getBranch, setBranch, mapValues, push, pop) where

import Text.JSON
import Data.List
import Data.Maybe
import Control.Monad

data Ref = Ref [Int] deriving Show

data RTree v = RTree { index  :: Int,
                       value  :: v,
                       childs :: [RTree v] }
                       deriving Show

-- RTree value needs to implement this for ismorphic and reachable
class Refered v
  where refs :: v -> [Ref]
        refMap :: (v,v) -> Maybe [(Ref, Ref)]

make :: v -> RTree v
make v = RTree 0 v []

root :: Ref
root = Ref [0]

--------------
-- Internal --
--------------

-- Return the values denoted by the reference (stop on bad index).
getChain :: [Int] -> RTree v -> [v]
getChain (i:is) (RTree i' v' ts')
  | i /= i' = []
  | otherwise = v':(concat $ map (getChain is) ts')
getChain _ _ = []

-- Force a RTree to contain a chain (value setting + expansion).
setChain :: [Int] -> [v] -> RTree v -> RTree v
setChain (i:is) (v:vs) t@(RTree i' v' ts')
  | i /= i' = t
  | (null is) || (null vs) = RTree i v ts'
  | all (\t -> (index t) /= (head is)) ts' = RTree i v (ts'++ (foldr (\(i,v) ts -> [RTree i v ts]) [] (zip is vs)))
  | otherwise = RTree i v (map (setChain is vs) ts')
setChain _ _ t = t

-- Return the RTree at the last valid reference.
fetchTree :: [Int] -> RTree v -> RTree v
fetchTree (_:i:is) t@(RTree _ v' ts') = maybe t (fetchTree (i:is)) (find ((==i).index) ts')
fetchTree (_:[])   t                  = t
fetchTree []       t                  = t

----------------------------
-- Structural equivalence --
----------------------------

-- Tells if there exists a child-permutation that respect the constraints.
-- The algorithm builds the permutation with an new (RTree Int).
isomorphic :: Refered v => [(Ref, Ref)] -> (RTree v, RTree v) -> Bool
isomorphic cs (t1@(RTree i1 _ _), t2@(RTree i2 _ _)) = loop (map extract cs) (RTree i1 i2 [])
  where loop [] c = True
        loop ((is1, is2):cs) t = let is2' = getChain is1 t
                                     bij  = drop (length is2') (zip (getChain is1 t1) (getChain is2 t2))
                                     mcs  = liftM ((map extract).concat) (mapM refMap bij)
                                 in if ((length is1) == (length is2)) && (isPrefixOf is2' is2) && (isJust mcs)
                                    then loop (cs ++ (fromJust mcs)) (setChain is1 is2 t)
                                    else False
        extract (Ref is1, Ref is2) = (is1, is2)

---------------------------
-- Reachability analysis --
---------------------------

-- Discards all the nodes that cannot be reached from given references.
reachable :: Refered v => [Ref] -> RTree v -> RTree v
reachable rs t@(RTree i v _) = loop rs (RTree i v [])
  where loop []     t' = t'
        loop (r:rs) t' = let vs  = getBranch r t
                             vs' = getBranch r t'
                         in loop (rs ++ (concat $ map refs (drop (length $ vs') vs))) (setBranch r vs t')

-------------------------
-- Reference Interface --
-------------------------

-- Find a free reference at the specified location.
push :: Ref -> RTree v -> Ref
push (Ref is) t = Ref $ is ++ [free 0 (map index (childs $ fetchTree is t))]
   where free i is = if elem i is then free (i+1) is else i

-- Go to the parent tree
pop :: Ref -> Ref
pop (Ref is) = Ref (init is)

----------------------
-- Branch Interface --
----------------------

-- Unbox the reference.
getBranch :: Ref -> RTree v -> [v]
getBranch (Ref is) = getChain is

-- Unbox the reference.
setBranch :: Ref -> [v] -> RTree v -> RTree v
setBranch (Ref is) = setChain is

-------------------
-- Value mapping --
-------------------

mapValues :: (v1 -> v2) -> RTree v1 -> RTree v2
mapValues f (RTree i v ts) = RTree i (f v) (map (mapValues f) ts)

----------
-- JSON --
----------

instance JSON Ref
  where showJSON (Ref is) = JSArray (map showJSON is)
        readJSON = error "not implemented"

instance (JSON v) => JSON (RTree v)
  where showJSON (RTree i v ts) = JSObject $ toJSObject [("index", showJSON i), ("value", showJSON v), ("childs", JSArray $ map showJSON ts)]
        readJSON = error "not implemented"
