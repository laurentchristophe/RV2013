{-# LANGUAGE FlexibleInstances #-}

module State.Env (Env, Ref, Refered (refs, refMap), empty, root, fetch, assign, extend, define, reachable, isomorphic, push, pop, State.Env.mapValues) where

import Control.Monad
import Text.JSON
import Data.List
import qualified Data.Map as Map

import State.RTree

type Env v = RTree (Map.Map String v)

instance (Refered v) => Refered (Map.Map String v)
  where refs m = concat $ map refs (Map.elems m)
        refMap (m1,m2)
          | (Map.keys m1) == (Map.keys m2) = liftM concat (mapM refMap (zip (Map.elems m1) (Map.elems m2)))
          | otherwise = Nothing

empty :: Env v
empty = make Map.empty

fetch :: String -> Ref -> Env v -> Maybe v
fetch s r e = (find (Map.member s) (reverse $ getBranch r e)) >>= Map.lookup s

assign :: String -> v -> Ref -> Env v -> Maybe (Env v)
assign s v r e = let (ms1,ms2) = break (Map.member s) (reverse $ getBranch r e)
                 in if null ms2
                    then Nothing
                    else Just $ setBranch r ((reverse $ tail ms2) ++ ((Map.insert s v (head ms2)):(reverse ms1))) e

define :: String -> v -> Ref -> Env v -> Env v
define s v r e = let ms = getBranch r e
                 in setBranch r ((init ms) ++ [Map.insert s v (last ms)]) e

extend :: [String] -> [v] -> Ref -> Env v -> Env v
extend ss vs r e = setBranch (push r e) ((getBranch r e) ++ [Map.fromList $ zip ss vs]) e

mapValues :: (v1 -> v2) -> Env v1 -> Env v2
mapValues f = State.RTree.mapValues (Map.map f)
