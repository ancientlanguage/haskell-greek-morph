module Grammar.Greek.Morph.AccentHandling.Stage where

import Prelude hiding (Word)
import Control.Lens (over, _1, _2)
import Data.Either.Validation
import Data.Void
import Grammar.Common
import qualified Grammar.Greek.Morph.AccentHandling.Round as Round
import Grammar.Greek.Morph.AccentHandling.Types
import Grammar.Greek.Morph.ShouldElide.Types
import Grammar.Greek.Morph.Types

accentHandling :: forall ctx . RoundContext ctx Void Void
  WordShouldElide
  WordAccentHandling
accentHandling = Round to from
  where
  toExtract :: WordShouldElide -> WordShouldElide :* CoreWord
  toExtract w@(WordShouldElide cw _ _ _ _ _ _) = w :^ cw

  toApply :: WordShouldElide :* CoreWord :* AccentHandling -> WordAccentHandling
  toApply (WordShouldElide _ a b c d e f, (cw, ah)) = WordAccentHandling cw a ah b c d e f

  to
    = over (_Failure . traverse . _2 . _1) fst
    . over (_Success . traverse . _2) toApply
    . (traverseWithItemContext . _2 $ liftRoundIdTo Round.accentHandling)
    . over (traverse . _2) toExtract

  fromExtract :: WordAccentHandling -> WordAccentHandling :* CoreWord :* AccentHandling
  fromExtract w@(WordAccentHandling cw _ ah _ _ _ _ _) = w :^ cw :^ ah

  fromApply :: WordAccentHandling :* CoreWord -> WordShouldElide
  fromApply (WordAccentHandling _ a _ b c d e f, cw) = WordShouldElide cw a b c d e f

  from
    = over (_Failure . traverse . _2 . _1) fst
    . over (_Success . traverse . _2) fromApply
    . (traverseWithItemContext . _2 $ liftRoundIdFrom Round.accentHandling)
    . over (traverse . _2) fromExtract
