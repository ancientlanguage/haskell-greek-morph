module Grammar.Greek.Morph.Paradigm.Ending where

import Grammar.Common
import Grammar.Greek.Morph.Types
import Grammar.Greek.Morph.Paradigm.Types
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word

allSplitsList :: [a] -> [[a] :* [a]]
allSplitsList = go []
  where
  go xs [] = [xs :^ []]
  go xs ys@(y : ys') = xs :^ ys : go (xs ++ [y]) ys'

-- cda, fg
-- [][] [cda][fg]
-- [][c] [da][fg]
-- [][cd] [a][fg]
-- [cda][] [][fg]
-- [cda][f] [][g]
-- [cda][fg] [][]
allSplitsSyllable
  :: Syllable :* [ConsonantRho]
  -> [([Syllable] :* [ConsonantRho]) :* ([Syllable] :* [ConsonantRho])]
allSplitsSyllable (s@(Syllable cs v), fc)
  = fmap (\(ls, rs) -> ([] :^ ls) :^ ([Syllable rs v] :^ fc)) (allSplitsList cs)
  ++ fmap (\(ls, rs) -> ([s] :^ ls) :^ ([] :^ rs)) (allSplitsList fc)

allSplits :: [CoreWord] -> [[CoreWord]]
allSplits [] = []
allSplits (CoreWord asp ss fc : xs) = []

extractEndings :: [Maybe ParadigmForm] -> Either String [Maybe ParadigmEnding]
extractEndings _ = Right []
