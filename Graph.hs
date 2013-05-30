{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TupleSections, UndecidableInstances #-}

module Graph (Graph, Node(childs), Knot(untie,free), node, empty, findMax, Graph.insert, size, jump, leafs, foreigns, reachable, traps, expand, showGEXF, result, solve) where

import qualified Data.Map as Map
import Data.List
import Data.Maybe

type Graph n i = Map.Map i n

node :: (Ord i) => i -> Graph n i -> Maybe n
node = Map.lookup

insert :: (Ord i) => i -> n -> Graph n i -> Graph n i
insert = Map.insert

empty :: Graph n i
empty = Map.empty

findMax :: Graph n i -> (i,n)
findMax = Map.findMax

size :: Graph n i -> Int
size = Map.size

----------
-- Node --
----------

class (Ord i) => Node n i | n -> i
  where childs :: n -> [i]

jump :: (Node n i) => i -> Graph n i -> [i]
jump i g = maybe [] childs (Map.lookup i g)

leafs :: (Node n i) => Graph n i -> [i]
leafs g = Map.foldrWithKey (\i n is -> if null $ childs n then i : is else is) [] g

reachable :: (Node n i) => i -> Graph n i -> Graph n i
reachable i g = loop [i] (Map.empty) g
  where loop [] g1 _  = g1
        loop is g1 g2 = let (g,g'') = Map.partitionWithKey (\i _ -> elem i is) g2
                        in loop (Map.foldr (\n is -> (childs n) ++ is) [] g) (Map.union g1 g) g''

showGEXF :: (Show i, Show n, Node n i) => Graph n i -> String
showGEXF g =    "<gexf><graph mode=\"static\" defaultedgetype=\"directed\"><nodes>"
             ++ (concat $ map showVertex ((Map.keys g)))
             ++ "</nodes><edges>"
             ++ (concat $ map showEdge (Map.foldrWithKey (\i n es -> (map (i,) (childs n)) ++ es) [] g))
             ++ "</edges></graph></gexf>"
  where showVertex i     = "<node id=\"" ++ (show i) ++ "\" label=\"" ++ (show i) ++ "\" />"
        showEdge (i1,i2) = "<edge id=\"" ++ (show i1) ++ "->" ++ (show i2) ++ "\" source=\"" ++ (show i1) ++ "\" target=\"" ++ (show i2) ++ "\" />"

-- Construction

foreigns :: (Node n i) => Graph n i -> [i]
foreigns g = (nub $ Map.foldr (\n is -> (childs n) ++ is) [] g) \\ (Map.keys g)

expand :: (Node n i) => (i -> Maybe n) -> Graph n i -> Graph n i
expand f g = loop (foreigns g) g
  where loop [] g = g
        loop (i:is) g = if Map.member i g
                        then loop is g
                        else maybe (loop is g) (\n -> loop ((childs n) ++ is) (Map.insert i n g)) (f i)

-- Traps

-- Trap: 
-- 1) Strongly connected
-- 2) Not empty
-- 3) No reference to foreigner vertexes
-- 4) No references to outside vertexes
-- 4) No single leaf vertex
traps :: (Node n i) => i -> Graph n i -> [[i]]
traps i g = let m' = Map.foldrWithKey (\i c m -> Map.alter (Just . (maybe [i] (i:))) c m) Map.empty (tarjan i g)
            in filter (\is -> ((length is) > 0) && (not $ null $ jump (head is) g) && (closed is g)) (Map.elems m')

tarjan :: (Node n i) => i -> Graph n i -> Map.Map i Int
tarjan i g = loop i Map.empty
  where loop i m
          | isJust $ Map.lookup i m = m
          | otherwise = let is  = jump i g -- foreigns vertex are threated as leafs
                            c   = Map.size m
                            m'  = foldr loop (Map.insert i c m) is
                            c'  = minimum (c : (map (m' Map.!) is))
                        in (Map.insert i c' m')

closed :: (Node n i) => [i] -> Graph n i -> Bool
closed is g = maybe False
                    ((all (flip elem is)) . concat)
                    (mapM (\i -> (Map.lookup i g) >>= (Just . childs)) is)

----------
-- Knot --
----------

class (Node n i) => Knot n i v | n -> i v
  where untie :: Map.Map i v -> n -> n
        free :: n -> Maybe v

result :: (Knot n i v) => i -> Graph n i -> Maybe v
result i g = (Map.lookup i g) >>= free

solve :: (Knot n i v) => Map.Map i v -> Graph n i -> Graph n i
solve m g = loop g Map.empty m
  where loop g1 g2 m = let (g',g) = Map.partition (isNothing . free) (Map.map (untie m) g1)
                       in if Map.null g
                          then Map.union g2 g'
                          else loop g' (Map.union g2 g) (Map.map (fromJust . free) g)

---------------
-- Inception --
---------------

-- A rooted graph can be seen as a Node/Knot


