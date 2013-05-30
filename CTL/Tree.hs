{-# LANGUAGE TupleSections, MultiParamTypeClasses, FlexibleInstances #-}

module CTL.Tree (Tree, Vertex(Root, Inner), Kripke (jump, label), make, result, crawl, leafs, check, showGEXF) where

import CTL.CTL
import CTL.UNode
import CTL.UBool

import qualified Graph as Graph
import qualified Data.Map as Map

import Data.Maybe
import System.IO.Unsafe

class (Eq s) => Kripke k s
  where jump :: s -> k -> [s]
        label :: s -> String -> k -> Bool

step :: (Ord s, Kripke k s) => CTL -> s -> k -> [(Vertex s, UNode (Vertex s))]
step (Cons b)     s _ = [(Root, cons b)]
step (Var v)      s k = let b = if label s v k then T else F
                        in [(Root, cons b)]
step (Nega c)     s _ = [(Root,                              nega (Inner s c))]
step (Disj c1 c2) s _ = [(Root,                              disj [Inner s c1, Inner s c2])]
step (Conj c1 c2) s _ = [(Root,                              conj [Inner s c1, Inner s c2])]
step (EN c)       s k = [(Root,                              disj $ map (flip Inner c) (jump s k))]
step (AN c)       s k = [(Root,                              conj $ map (flip Inner c) (jump s k))]
step (EU c1 c2)   s k = [(Root,                              disj [Inner s c2, Inner s (Conj c1 (EN $ EU c1 c2))]),
                         (Inner s (Conj c1 (EN $ EU c1 c2)), conj [Inner s c1, Inner s (EN $ EU c1 c2)]),
                         (Inner s (EN $ EU c1 c2),           disj $ map (flip Inner (EU c1 c2)) (jump s k))]
step (AU c1 c2)   s k = [(Root,                              disj [Inner s c2, Inner s (Conj c1 (AN $ AU c1 c2))]),
                         (Inner s (Conj c1 (AN $ AU c1 c2)), conj $ (Inner s c1):(map (flip Inner (AU c1 c2)) (jump s k)))]
step (ER c1 c2)   s k = [(Root,                              conj [Inner s c2, Inner s (Disj c1 (EN $ ER c1 c2))]),
                         (Inner s (Disj c1 (EN $ ER c1 c2)), disj $ (Inner s c1):(map (flip Inner (ER c1 c2)) (jump s k)))]
step (AR c1 c2)   s k = [(Root,                              conj [Inner s c2, Inner s (Disj c1 (AN $ AR c1 c2))]),
                         (Inner s (Disj c1 (AN $ AR c1 c2)), disj [Inner s c1, Inner s (AN $ AR c1 c2)]),
                         (Inner s (AN $ AR c1 c2)          , conj $ map (flip Inner (AR c1 c2)) (jump s k))]


data Vertex s = Root | Inner s CTL deriving (Eq, Ord)
newtype Knot s = Knot (Graph.Graph (UNode (Vertex s)) (Vertex s)) deriving Show
newtype Tree s = Tree (Graph.Graph (Knot s) (Vertex s)) deriving Show

instance (Show s) => Show (Vertex s)
  where show Root = "root"
        show (Inner s c) = show (s,c)

instance (Ord s) => Graph.Node (Knot s) (Vertex s)
  where childs (Knot g) = Graph.foreigns g

instance (Ord s) => Graph.Knot (Knot s) (Vertex s) UBool
  where untie m (Knot g) = Knot $ Graph.reachable Root (Graph.solve m g)
        free (Knot g) = Graph.result Root g

make :: (Ord s) => s -> CTL -> Tree s
make s c = Tree $ Graph.insert Root (Knot $ Graph.insert Root (iden $ Inner s c) Graph.empty) Graph.empty

result :: (Ord s) => Tree s -> Maybe UBool
result (Tree g) = Graph.result Root g

crawl :: (Ord s, Show s) => (Kripke k s) => k -> Tree s -> Tree s
crawl k (Tree g) = Tree $ Graph.expand (\i@(Inner s c) -> if null $ jump s k
                                                          then Nothing
                                                          else Just $ Knot $ foldr (\(i,n) -> Graph.insert i n) Graph.empty (step c s k))
                                       g

leafs :: (Ord s) => Tree s -> [Vertex s]
leafs (Tree g) = Graph.leafs g

check :: (Show s, Ord s) => Map.Map (Vertex s) UBool -> Tree s -> Tree s
check m (Tree g) = loop $ Graph.solve m g
  where loop g = case Graph.traps Root g
                 of []  -> Tree g
                    iss -> loop $ Graph.solve (foldr (\i@(Inner _ c) m -> Map.insert i (f c) m) Map.empty (concat iss)) g 
        f (EU _ _) = F
        f (AU _ _) = F
        f (ER _ _) = T
        f (AR _ _) = T
        f _        = error "cycle on non cyclic CTL formula"

---------------------------
-- Crappy XML generation --
---------------------------

type FlatTree s = Map.Map (Vertex s, Vertex s) (Maybe (String, [(Vertex s, Vertex s)]))


showGEXF :: (Ord s, Show s) => Tree s -> String
showGEXF = complete . flatten
                 
complete :: (Ord s, Show s) => FlatTree s -> String 
complete m = let (sn,se) = Map.foldrWithKey acc ("","") m
             in    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                ++ "<gexf xmlns=\"http://www.gexf.net/1.2draft\" version=\"1.2\" xmlns:viz=\"http://www.gexf.net/1.2draft/viz\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd\">"
                ++ "<graph defaultedgetype=\"directed\" mode=\"static\">"
                ++ "<attributes class=\"node\" mode=\"static\"><attribute id=\"group\" title=\"Group\" type=\"string\"></attribute></attributes>"
                ++ "<nodes>" ++ sn ++ "</nodes><edges>" ++ se ++ "</edges></graph></gexf>"
  where acc x@(vg, vn) Nothing (sn,se) = ("<node id=\"" ++ (show $ Map.findIndex x m) ++ "\" label=\"BORDER: " ++ (showNode (vg,vn)) ++ "\"></node>" ++ sn, se)
        acc x@(vg, vn) (Just (so, xs)) (sn,se) = let s = (show $ Map.findIndex x m)
                                                     sn' = "<node id=\"" ++ s ++ "\" label=\"" ++ so ++ ": " ++ (showNode (vg,vn)) ++ "\"><attvalues><attvalue for=\"group\" value=\"" ++ (show vg) ++ "\"/></attvalues></node>\n" ++ sn
                                                     se' = foldr (\x se -> "<edge source=\"" ++ s ++ "\" target=\"" ++ (show $ Map.findIndex x m) ++ "\"></edge>\n" ++ se) se xs
                                                     in (sn', se')
        showNode (vg,Root) = show vg
        showNode (_,vn) = show vn 

flatten :: (Ord s) => Tree s -> FlatTree s
flatten (Tree gt) = Map.foldrWithKey accKnot Map.empty gt

accKnot :: (Ord s) => Vertex s -> Knot s -> FlatTree s -> FlatTree s
accKnot vk (Knot gk) m = Map.foldrWithKey accUNode m gk
  where accUNode vn n m = let xs = map (\vc -> if isNothing $ Map.lookup vc gk then (vc, Root) else (vk, vc)) (Graph.childs n)
                              m' = foldr (\x m -> if Map.member x m then m else Map.insert x Nothing m) m xs
                          in Map.insert (vk, vn) (Just (showOperator n, xs)) m'

