module Util where
import Data.Either
import DFA
import Parser

fromRight = either undefined id

eq s1 s2  = shortestWord $ symmetricDiff (dfa s1) (dfa s2)
  where
    dfa = toDFA . fromRight . parseRegex
