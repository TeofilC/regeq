{-# LANGUAGE RecordWildCards #-}
module NFA where
import qualified Data.Set as S
import qualified Data.Map as M

class Alphabet a where
  alphabet :: [a]

instance Alphabet Char where
  alphabet = enumFromTo '!' (toEnum 126)

data NFA s a = NFA { states :: [s]
                   , delta  :: M.Map (s, a) [s] -- s -> a -> [s]
                   , start  :: s
                   , accept :: [s]
                   }

data NFAe s a = NFAe { statese :: [s]
                     , deltae  :: M.Map (s, Maybe a) [s] -- s -> Maybe a -> [s]
                     , starte  :: s
                     , accepte :: [s]
                     }

deEpsilon :: (Eq s, Ord s, Alphabet a, Ord a) => NFAe s a -> NFA s a
deEpsilon NFAe {..} = NFA { states = statese
                          , delta  = M.fromList [((s,a), delta' s a) | s <- statese, a <- alphabet]
                          , start  = starte
                          , accept = accepte'
                          }
  where
    accepte'   = filter (\s -> not . S.null $ (S.fromList accepte) `S.intersection` (S.fromList $ closure' s)) statese
    delta' s a = closure' s >>= \x -> deltaef x (Just a)
    deltaef  s a   = M.findWithDefault [] (s, a) deltae
    closure' s = closure (flip deltaef Nothing) (S.singleton s)

closure :: (Eq s, Ord s) => (s -> [s]) -> S.Set s -> [s]
closure f set = if set == set' then S.toList set else closure f set'
      where
        set' = set `S.union` (S.fromList $ concatMap f $ S.toList set)
