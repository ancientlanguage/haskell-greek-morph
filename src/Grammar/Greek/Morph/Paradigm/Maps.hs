module Grammar.Greek.Morph.Paradigm.Maps where

import Grammar.Common
import Grammar.Greek.Morph.Types
import Grammar.Greek.Morph.Paradigm.Types
import Grammar.Greek.Morph.Smyth.Nouns.Declension1
import Grammar.Greek.Morph.Smyth.Nouns.Declension2

endingFormGroups :: [[Phoneme] :* [ParadigmForm]]
endingFormGroups
  = groupPairs
  $ fmap (\f -> (paradigmEndingEnding . paradigmFormEnding) f :^ f)
  $ declension1Forms ++ declension2Forms
