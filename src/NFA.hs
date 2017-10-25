{-# LANGUAGE RecordWildCards #-}
module NFA where
import qualified Data.Set as S

data NFA s a = NFA { states :: [s]
                   , delta  :: s -> a -> [s]
                   , start  :: s
                   , accept :: [s]
                   }

data NFAe s a = NFAe { statese :: [s]
                     , deltae  :: s -> Maybe a -> [s]
                     , starte  :: s
                     , accepte :: [s]
                     }

deEpsilon :: (Eq s, Ord s) => NFAe s a -> NFA s a
deEpsilon NFAe {..} = NFA { states = statese
                          , delta  = delta'
                          , start  = starte
                          , accept = accepte'
                          }
  where
    accepte'   = filter (\s -> not . S.null $ (S.fromList accepte) `S.intersection` (S.fromList $ closure' s)) statese
    delta' s a = closure' s >>= \x -> deltae x (Just a)
    closure' s = closure (flip deltae Nothing) (S.singleton s)

closure :: (Eq s, Ord s) => (s -> [s]) -> S.Set s -> [s]
closure f set = if set == set' then S.toList set else closure f set'
      where
        set' = set `S.union` (S.fromList $ concatMap f $ S.toList set)
