{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}

-- Config
import Config.Config
import Config.Type
import Config.Concr
-- CTL
import CTL.CTL
import CTL.Tree
import CTL.UBool
-- State
import State.Expr
import State.State
-- Top level
import Graph
-- Haskell
import Text.JSON
import Data.List
import Data.Array
import System.Environment
import Data.Either
import Data.Maybe
import qualified Data.Map as Map

type Path d = [(Int, State d)]

main :: IO ()
main = do [p1,p2] <- getArgs
          s1 <- readFile p1
          s2 <- readFile p2
          let prog = either (\e -> error $ show e) id (State.Expr.parseCExpr s1)
              ctl  = either (\e -> error $ show e) id (CTL.CTL.parse    s2)
              c = (start prog) :: State Concr
          b <- run 1 0 c (make 0 ctl)
          putStrLn $ "\n\n==== RESULT: " ++ (show b) ++ " ====\n\n" 

run :: Int -> Int -> State Concr -> CTL.Tree.Tree Int -> IO UBool
run _ _ (Fail _) _ = return U
run _ _ (End  _) _ = return U
run o i c        t = do c' <- step c
                        let a = galois abstr c
                            a' = galois abstr c'
                            g = Main.crawl 100 o a'
                            k =  Graph.insert i (a,[o]) g
                            bij = foldr (\x m -> Map.insert x U m) Map.empty (filter (\(Inner i' _) -> (i' < o) && (i' /= i)) (CTL.Tree.leafs t))
                            t' = CTL.Tree.crawl k t
                            t'' = CTL.Tree.check bij t'
                        writeFile ("labo/output/" ++ (show o) ++ "state.gexf") (Graph.showGEXF g)
                        writeFile ("labo/output/" ++ (show o) ++ "ctl-expand.gexf") (CTL.Tree.showGEXF t')
                        writeFile ("labo/output/" ++ (show o) ++ "ctl-solve.gexf") (CTL.Tree.showGEXF t'')
                        writeFile ("labo/output/" ++ (show o) ++ "state.json") (showJSValue (showJSON k) "")
                        maybe (do p@((i'',c''):_) <- execute k [(o,c')]
                                  writeFile ("labo/output/" ++ (show o) ++ "path.json") (showJSValue (showJSON p) "")
                                  run (o + (Graph.size g)) i'' c'' t'')
                              return
                              (CTL.Tree.result t'')

execute :: (Graph (State Type, [Int]) Int) -> Path Concr -> IO (Path Concr)
execute k (b@(i,End  _):bs) = return $ b:bs
execute k (b@(i,Fail _):bs) = return $ b:bs
execute k (b@(i,c):bs) = case Graph.jump i k
                         of [] -> return $ b:bs
                            is -> do c' <- step c
                                     let a' = galois abstr c'
                                         i' = maybe (error "inconsistent abstraction") id (find (\i -> (fst $ fromJust $ Graph.node i k) == a') is)
                                     execute k ((i',c'):(i,c):bs)

-------------------------------
-- Graph as Kripke structure --
-------------------------------

instance (Ord i) => Kripke (Graph (State Type, [i]) i) i
  where jump = Graph.jump
        label i v g = Config.Config.label (fst $ fromJust $ node i g) v

instance (Ord i) => Node (s, [i]) i
  where childs = snd

crawl :: (Enum i, Ord i) => Int -> i -> State Type -> Graph (State Type, [i]) i
crawl t o s = loop (succ o) Graph.empty [(s,o)] [(s,o)]
  where loop o g [] bij = g
        loop o g ((s,i):bs) bij
          | isJust $ node i g = loop o g bs bij
          | (Graph.size g) > t = loop o (Graph.insert i (s,[]) g) bs bij
          | otherwise = let ss = nub $ step s
                            es = map (\s -> maybe (Left s) Right (lookup s bij)) ss
                            (o',bs') = foldr (\s (o,bs) -> (succ o, (s,o) : bs)) (o,[]) (lefts es)
                            g' = Graph.insert i (s, (rights es) ++ (map snd bs')) g
                        in loop o' g' (bs' ++ bs) (bs' ++ bij)

-------------
-- Testing --
-------------

yo = do s1 <- readFile "fac-iter.txt"
        let prog = either (\e -> error $ show e) id (State.Expr.parseCExpr s1)
            c0 = (start prog) :: State Concr
        loop c0
  where loop c@(Fail _) = putStrLn $ showJSValue (showJSON $ c) ""
        loop c@(End _)  = putStrLn $ showJSValue (showJSON $ c) ""
        loop c          = do putStrLn $ showJSValue (showJSON $ c) ""
                             c' <- step c
                             loop c'
