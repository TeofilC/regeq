{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Regex where
import Control.Monad.State
import NFA

data Regex a = Empty
             | Sym a
             | Union  (Regex a) (Regex a)
             | Concat (Regex a) (Regex a)
             | Star   (Regex a)
             deriving Show

newtype Gen a = Gen {unGen :: State Int a}
  deriving (Functor, Applicative, Monad, MonadState Int)

next :: Gen Int
next = state (\s -> (s,s+1))

toNFAe :: (Eq a) => Regex a -> NFAe Int a
toNFAe = flip evalState 0 . unGen . toNFAe'

toNFAe' :: (Eq a) => Regex a -> Gen (NFAe Int a)
toNFAe' Empty = do
  start <- next
  end   <- next
  let deltae s Nothing = if s == start then [end] else []
      deltae _    _        = []
  return $ NFAe { statese = [start, end]
                , deltae  = deltae
                , starte  = start
                , accepte = [end]
                }
toNFAe' (Sym a) = do
  start <- next
  end   <- next
  let deltae s (Just x) = if s == start && x == a then [end] else []
      deltae _    _        = []
  return $ NFAe { statese = [start, end]
                , deltae  = deltae
                , starte  = start
                , accepte = [end]
                }
toNFAe' (Union l r) = do
  l' <- toNFAe' l
  r' <- toNFAe' r
  start <- next
  let
    delta s Nothing = (if s == start then [starte l', starte r'] else []) ++ deltae l' s Nothing ++ deltae r' s Nothing
    delta s     a       = deltae l' s a ++ deltae r' s a
  return $ NFAe { statese = [start] ++ statese l' ++ statese r'
                , deltae  = delta
                , starte  = start
                , accepte = accepte l' ++ accepte r'
                }
toNFAe' (Concat l r) = do
  l' <- toNFAe' l
  r' <- toNFAe' r
  let
    delta s Nothing = (if s `elem` accepte l' then [starte r'] else []) ++ deltae l' s Nothing ++ deltae r' s Nothing
    delta s     a       = deltae l' s a ++ deltae r' s a
  return $ NFAe { statese = statese l' ++ statese r'
                , deltae  = delta
                , starte = starte l'
                , accepte = accepte r'}
toNFAe' (Star r) = do
  r' <- toNFAe' r
  let
    delta s Nothing = (if s `elem` accepte r' then [starte r'] else []) ++ deltae r' s Nothing
    delta s     a   = deltae r' s a
  return $ NFAe { statese = statese r'
                , deltae  = delta
                , starte  = starte r'
                , accepte = [starte r']}
