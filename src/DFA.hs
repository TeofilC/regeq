{-# LANGUAGE RecordWildCards #-}
module DFA where
import Control.Monad
import qualified Data.PQueue.Min as PQ
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Data.List (sort)
import NFA
import Regex

data DFA s a = DFA { states :: [s]
                   , delta  :: M.Map (s, a) s -- s -> a -> s
                   , start  :: s
                   , accept :: [s]
                   }


determinise :: (Alphabet a, Ord s, Ord a) => NFA s a -> DFA (S.Set s) a
determinise NFA {..} = DFA { states = states'
                            , delta  = M.fromList [((s,a), delta' s a) | s <- states', a <- alphabet]
                            , start = S.singleton start
                            , accept = filter (\s -> not . S.null $ s `S.intersection` S.fromList accept) states'
                            }
  where
    delta' sz a = S.fromList (S.toList sz >>= \s -> M.findWithDefault [] (s,a) delta)
    states' = closure (\s -> map (\a -> delta' s a) alphabet) (S.singleton $ S.singleton start)
    powerset [] = [[]]
    powerset (x:xs) = let rs = powerset xs in map (x:) rs ++ rs

toDFA :: (Alphabet a, Eq a, Ord a) => Regex a -> DFA (S.Set Int) a
toDFA = determinise . deEpsilon . toNFAe

symmetricDiff :: (Alphabet a, Eq s1, Eq s2, Monoid s1, Monoid s2, Ord s1, Ord s2, Ord a) => DFA s1 a -> DFA s2 a -> DFA (s1,s2) a
symmetricDiff DFA {states = s1, delta = d1, start = q1, accept = a1}
              DFA {states = s2, delta = d2, start = q2, accept = a2} =
  DFA { states = liftM2 (,) s1 s2
      , delta  = M.fromList [(((x1,x2),a), (M.findWithDefault mempty (x1, a) d1, M.findWithDefault mempty (x2, a) d2)) | x1 <- s1, x2 <- s2, a <- alphabet]
      , start  = (q1,q2)
      , accept = [(x,y) | x <- s1, y <- s2, not (x `elem` a1 && y `elem` a2), (x `elem` a1 || y `elem` a2)]
      }

shortestWord :: (Alphabet a, Ord a, Ord s, Monoid s) => DFA s a -> Maybe [a]
shortestWord DFA {..} = shortestPath (PQ.singleton (0,start,[])) S.empty (\s a -> M.findWithDefault mempty (s,a) delta) accept

shortestPath :: (Alphabet a, Ord s, Ord a) => PQ.MinQueue (Int, s, [a]) -> S.Set s -> (s -> a -> s) -> [s] -> Maybe [a]
shortestPath pq vis d target = case PQ.minView pq of
                                 Nothing -> Nothing
                                 Just ((l, s, w), pq') ->
                                   if s `elem` target then
                                     Just $ reverse w
                                   else
                                     shortestPath (pq' `PQ.union` PQ.fromList unvisitedNeighbours) (S.insert s vis) d target
                                   where
                                     unvisitedNeighbours = filter (not . flip S.member vis . second) potential
                                     second (_,x,_) = x
                                     potential = map (\a -> (l+1, d s a, a:w)) alphabet
