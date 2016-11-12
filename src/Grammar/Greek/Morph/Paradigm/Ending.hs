module Grammar.Greek.Morph.Paradigm.Ending where

import Control.Lens (over, view, _2)
import qualified Data.List as List
import Data.Maybe
import Grammar.Common
import Grammar.Greek.Morph.Phoneme.Round
import Grammar.Greek.Morph.Types
import Grammar.Greek.Morph.Paradigm.Types

allSplitsList :: [a] -> [[a] :* [a]]
allSplitsList = go []
  where
  go xs [] = [xs :^ []]
  go xs ys@(y : ys') = xs :^ ys : go (xs ++ [y]) ys'

longestPrefix :: Eq a => [[a]] -> [a]
longestPrefix [] = []
longestPrefix (x : xs) = go (reverse . fmap fst . allSplitsList $ x) xs
  where
  go [] _ = []
  go (p : ps) ys =
    if and (fmap (List.isPrefixOf p) ys)
    then p
    else go ps ys

nounParadigmForms :: Gender -> NounParadigm -> [Morph :* ParadigmExemplar]
nounParadigmForms g (NounParadigm sgNom sgGen sgDat sgAcc sgVoc duNAV duGD plNomV plGen plDat plAcc) = catMaybes . fmap (_2 id) $
  [ substantiveMorph Nominative Singular g :^ sgNom
  , substantiveMorph Genitive Singular g :^ sgGen
  , substantiveMorph Dative Singular g :^ sgDat
  , substantiveMorph Accusative Singular g :^ sgAcc
  , substantiveMorph Vocative Singular g :^ sgVoc
  ]
  ++
  fmap (\c -> substantiveMorph c Dual g :^ duNAV) [Nominative, Accusative, Vocative]
  ++
  fmap (\c -> substantiveMorph c Dual g :^ duGD) [Genitive, Dative]
  ++
  fmap (\c -> substantiveMorph c Plural g :^ plNomV) [Nominative, Vocative]
  ++
  [ substantiveMorph Genitive Plural g :^ plGen
  , substantiveMorph Dative Plural g :^ plDat
  , substantiveMorph Accusative Plural g :^ plAcc
  ]

getParadigmForms :: FormKind -> [Morph :* ParadigmExemplar] -> [ParadigmForm]
getParadigmForms fk es =
  let
    toPhonemes = coreWordToPhonemes . accentedWordToCoreWord . paradigmExemplarWord
    pps = over (traverse . _2 . _2) toPhonemes . fmap (\(x,y) -> (x, (y, y))) $ es
    ps = fmap (view (_2 . _2)) pps
    prefixLen = length . longestPrefix $ ps
    pps' = over (traverse . _2 . _2) (drop prefixLen) pps
    getAccent = accentedWordAccent . paradigmExemplarWord
  in fmap (\(m, (ex, en)) -> ParadigmForm fk ex m (getAccent ex) en) pps'
