{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module CTL.UNode(UNode, showOperator, iden, nega, conj, disj, cons) where

import Data.List
import CTL.UBool

import qualified Graph as Graph
import qualified Data.Map as Map

data UOp = Iden | Nega | Conj | UConj | Disj | UDisj | Const UBool deriving Show
data UNode a = UNode [a] UOp deriving Show

instance (Ord a) => Graph.Node (UNode a) a
  where childs (UNode as _) = as

instance (Ord a) => Graph.Knot (UNode a) a UBool
  where untie m t = Map.foldrWithKey solve t m
        free = result

result :: (UNode a) -> Maybe UBool
result (UNode [] uop) = Just $ case uop
                                 of Const b -> b
                                    Conj    -> T
                                    UConj   -> U
                                    Disj    -> F
                                    UDisj   -> U
result (UNode _ _) = Nothing

showOperator :: UNode a -> String
showOperator (UNode _ Iden)      = "IDENTITY"
showOperator (UNode _ Nega)      = "NEGATION"
showOperator (UNode _ Conj)      = "CONJUNCTION"
showOperator (UNode _ UConj)     = "UNDEFINED - CONJUNCTION"
showOperator (UNode _ Disj)      = "DISJUNCTION"
showOperator (UNode _ UDisj)     = "UNDEFINED - DISJUNCTION"
showOperator (UNode _ (Const b)) = "CONSTANT - " ++ (show b)

solve :: (Eq a) => a -> UBool -> UNode a -> UNode a
solve a v n@(UNode as uop) = if elem a as then perform v uop (filter (/=a) as) else n
          where -- Iden
                perform b Iden _ = UNode [] (Const b) 
                -- Nega
                perform T Nega _ = UNode [] (Const F)
                perform F Nega _ = UNode [] (Const T)
                perform U Nega _ = UNode [] (Const U)
                -- Conj
                perform T Conj as = UNode as Conj
                perform F Conj _  = UNode [] (Const F)
                perform U Conj as = UNode as UConj
                -- UConj
                perform T UConj as = UNode as UConj
                perform F UConj _  = UNode [] (Const F)
                perform U UConj as = UNode [] UConj
                -- Disj
                perform T Disj _  = UNode [] (Const T)
                perform F Disj as = UNode as Disj
                perform U Disj as = UNode as UDisj
                -- UDisj
                perform T UDisj _  = UNode [] (Const T)
                perform F UDisj as = UNode as UDisj
                perform U UDisj as = UNode as UDisj

iden :: a -> UNode a
iden a = UNode [a] Iden

nega :: a -> UNode a
nega a = UNode [a] Nega

conj :: [a] -> UNode a
conj as = UNode as Conj

disj :: [a] -> UNode a
disj as = UNode as Disj

cons :: UBool -> UNode a
cons b = UNode [] (Const b)
