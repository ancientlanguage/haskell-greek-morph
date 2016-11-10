module Grammar.Greek.Morph.AccentHandling.Round where

import qualified Data.Set as Set
import Grammar.Common
import Grammar.Greek.Morph.Forms.Enclitic
import Grammar.Greek.Morph.Forms.Proclitic
import Grammar.Greek.Morph.Types

accentHandling :: RoundId CoreWord (CoreWord :* AccentHandling)
accentHandling = RoundId to from
  where
  to x =
    if Set.member x encliticSet
    then x :^ Enclitic
    else if Set.member x procliticSet
    then x :^ Proclitic
    else x :^ NormalAccentHandling

  encliticSet = Set.fromList . fmap accentedWordToCoreWord $ encliticForms
  procliticSet = Set.fromList procliticForms

  from = fst
