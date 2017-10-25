{-# LANGUAGE RecordWildCards #-}
module DFA where
import Control.Monad
import qualified Data.PQueue.Min as PQ
import qualified Data.Set as S
import qualified Data.List as L
import NFA
import Regex

data DFA s a = DFA { states :: [s]
                   , delta  :: s -> a -> s
                   , start  :: s
                   , accept :: [s]
                   }

class Alphabet a where
  alphabet :: [a]

instance Alphabet Char where
  alphabet = enumFromTo (toEnum 0) (toEnum 256)

determinise :: (Alphabet a, Ord s) => NFA s a -> DFA [s] a
determinise NFA {..} = DFA { states = states'
                            , delta  = delta'
                            , start = [start]
                            , accept = filter (\s -> not . S.null $ S.fromList s `S.intersection` S.fromList accept) states'
                            }
  where
    delta' sz a = sz >>= \s -> delta s a
    states' = closure (\s -> map (\a -> delta' s a) alphabet) (S.singleton [start])
    powerset [] = [[]]
    powerset (x:xs) = let rs = powerset xs in map (x:) rs ++ rs

toDFA :: (Alphabet a, Eq a) => Regex a -> DFA [Int] a
toDFA = determinise . deEpsilon . toNFAe

symmetricDiff :: (Eq s1, Eq s2) => DFA s1 a -> DFA s2 a -> DFA (s1,s2) a
symmetricDiff DFA {states = s1, delta = d1, start = q1, accept = a1}
              DFA {states = s2, delta = d2, start = q2, accept = a2} =
  DFA { states = liftM2 (,) s1 s2
      , delta  = \(x1,x2) a -> (d1 x1 a, d2 x2 a)
      , start  = (q1,q2)
      , accept = [(x,y) | x <- s1, y <- s2, not (x `elem` a1 && y `elem` a2), (x `elem` a1 || y `elem` a2)]
      }

shortestWord :: (Alphabet a, Ord a, Ord s) => DFA s a -> Maybe [a]
shortestWord DFA {..} = shortestPath (PQ.singleton (0,start,[])) S.empty delta accept

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
