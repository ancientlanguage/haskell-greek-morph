module Grammar.Greek.Morph.Paradigm.Ending where

import Grammar.Common
import Grammar.Greek.Morph.Types
import Grammar.Greek.Morph.Paradigm.Types
import Grammar.Greek.Script.Types

allSplitsList :: [a] -> [[a] :* [a]]
allSplitsList = go []
  where
  go xs [] = [xs :^ []]
  go xs ys@(y : ys') = xs :^ ys : go (xs ++ [y]) ys'

allSplits :: [CoreWord] -> [[CoreWord]]
allSplits [] = []
allSplits (x : xs) = []

extractEndings :: [Maybe ParadigmForm] -> Either String [Maybe ParadigmEnding]
extractEndings _ = Right []
