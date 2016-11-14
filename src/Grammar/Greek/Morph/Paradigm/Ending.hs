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

nounParadigmForms :: NounParadigm -> [Morph :* ParadigmExemplar]
nounParadigmForms (NounParadigm sgNom sgGen sgDat sgAcc sgVoc duNAV duGD plNomV plGen plDat plAcc) = catMaybes . fmap (_2 id) $
  [ setMorph Nominative Singular :^ sgNom
  , setMorph Genitive Singular :^ sgGen
  , setMorph Dative Singular :^ sgDat
  , setMorph Accusative Singular :^ sgAcc
  , setMorph Vocative Singular :^ sgVoc
  ]
  ++
  fmap (\c -> setMorph c Dual :^ duNAV) [Nominative, Accusative, Vocative]
  ++
  fmap (\c -> setMorph c Dual :^ duGD) [Genitive, Dative]
  ++
  fmap (\c -> setMorph c Plural :^ plNomV) [Nominative, Vocative]
  ++
  [ setMorph Genitive Plural :^ plGen
  , setMorph Dative Plural :^ plDat
  , setMorph Accusative Plural :^ plAcc
  ]
  where
  setMorph c n = emptyMorph { morphCase = Just c, morphNumber = Just n }

verbParadigmForms :: VerbParadigm -> [Morph :* ParadigmExemplar]
verbParadigmForms (VerbParadigm sg1 sg2 sg3 dual2 dual3 pl1 pl2 pl3) = catMaybes . fmap (_2 id) $
  [ setMorph Singular Person1 :^ sg1
  , setMorph Singular Person2 :^ sg2
  , setMorph Singular Person3 :^ sg3

  , setMorph Dual Person2 :^ dual2
  , setMorph Dual Person3 :^ dual3

  , setMorph Plural Person1 :^ pl1
  , setMorph Plural Person2 :^ pl2
  , setMorph Plural Person3 :^ pl3
  ]
  where
  setMorph n p = emptyMorph { morphNumber = Just n, morphPerson = Just p }

data ParadigmError = NoCommonPrefix
  deriving (Show)

getParadigmEndings :: [Morph :* ParadigmExemplar] -> Either ParadigmError [ParadigmEnding]
getParadigmEndings es =
  let
    toPhonemes = coreWordToPhonemes . accentedWordToCoreWord . paradigmExemplarWord
    pps = over (traverse . _2 . _2) toPhonemes . fmap (\(x,y) -> (x, (y, y))) $ es
    ps = fmap (view (_2 . _2)) pps
    prefixLen = length . longestPrefix $ ps
    pps' = over (traverse . _2 . _2) (drop prefixLen) pps
  in if prefixLen == 0
    then Left NoCommonPrefix
    else Right . fmap (\(m, (ex, en)) -> ParadigmEnding ex m en) $ pps'
