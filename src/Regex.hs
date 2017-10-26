{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Regex where
import Control.Monad.State
import NFA
import qualified Data.Map as M

data Regex a = Empty
             | Sym a
             | Union  (Regex a) (Regex a)
             | Concat (Regex a) (Regex a)
             | Star   (Regex a)
             deriving Show

newtype Gen a = Gen {unGen :: State Int a}
  deriving (Functor, Applicative, Monad, MonadState Int)

union :: (Ord k) => M.Map k [a] -> M.Map k [a] -> M.Map k [a]
union = M.unionWith (++)

next :: Gen Int
next = state (\s -> (s,s+1))

toNFAe :: (Eq a, Ord a) => Regex a -> NFAe Int a
toNFAe = flip evalState 0 . unGen . toNFAe'

toNFAe' :: (Eq a, Ord a) => Regex a -> Gen (NFAe Int a)
toNFAe' Empty = next >>= \start -> next >>= \end ->
  return $ NFAe {statese = [start, end]
                , deltae = M.fromList [((start, Nothing), [end])]
                , starte = start
                , accepte= [end]}
toNFAe' (Sym a) = next >>= \start -> next >>= \end ->
  let deltae s (Just x) = if s == start && x == a then [end] else []
      deltae _    _        = []
  in
  return $ NFAe { statese = [start, end]
                , deltae  = M.fromList [((start, Just a), [end])]
                , starte  = start
                , accepte = [end]
                }
toNFAe' (Union l r) = do
  l' <- toNFAe' l
  r' <- toNFAe' r
  start <- next
  {-let
    delta s Nothing = (if s == start then [starte l', starte r'] else []) ++ deltae l' s Nothing ++ deltae r' s Nothing
    delta s     a       = deltae l' s a ++ deltae r' s a -}
  return $ NFAe { statese = [start] ++ statese l' ++ statese r'
                , deltae  = M.fromList [((start, Nothing), [starte l', starte r'])]
                            `union` deltae l' `union` deltae r'
                , starte  = start
                , accepte = accepte l' ++ accepte r'
                }
toNFAe' (Concat l r) = do
  l' <- toNFAe' l
  r' <- toNFAe' r
  {-let
    delta s Nothing = (if s `elem` accepte l' then [starte r'] else []) ++ deltae l' s Nothing ++ deltae r' s Nothing
    delta s     a       = deltae l' s a ++ deltae r' s a -}
  return $ NFAe { statese = statese l' ++ statese r'
                , deltae  = M.fromList [((s, Nothing), [starte r']) | s <- accepte l']
                            `union` deltae l' `union` deltae r'
                , starte = starte l'
                , accepte = accepte r'}
toNFAe' (Star r) = do
  r' <- toNFAe' r
  {-let
    delta s Nothing = (if s `elem` accepte r' then [starte r'] else []) ++ deltae r' s Nothing
    delta s     a   = deltae r' s a -}
  return $ NFAe { statese = statese r' 
                , deltae  = M.fromList [((s, Nothing), [starte r']) | s <- accepte r'] `union` M.fromList [((starte r', Nothing), accepte r')] `union` deltae r'
                , starte  = starte r'
                , accepte = accepte r'}
