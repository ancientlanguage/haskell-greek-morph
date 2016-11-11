module Grammar.Greek.Morph.Clitic.Stage where

import Prelude hiding (Word)
import Control.Lens (over, _1, _2)
import Data.Either.Validation
import Data.Void
import Grammar.Common
import qualified Grammar.Greek.Morph.Clitic.Round as Round
import Grammar.Greek.Morph.Clitic.Types
import Grammar.Greek.Morph.ShouldElide.Types
import Grammar.Greek.Morph.Types

clitic :: forall ctx . RoundContext ctx Void Void
  WordShouldElide
  WordClitic
clitic = Round to from
  where
  toExtract :: WordShouldElide -> WordShouldElide :* CoreWord
  toExtract w@(WordShouldElide cw _ _ _ _ _ _) = w :^ cw

  toApply :: WordShouldElide :* CoreWord :* Clitic -> WordClitic
  toApply (WordShouldElide _ a b c d e f, (cw, ah)) = WordClitic cw a ah b c d e f

  to
    = over (_Failure . traverse . _2 . _1) fst
    . over (_Success . traverse . _2) toApply
    . (traverseWithItemContext . _2 $ liftRoundIdTo Round.clitic)
    . over (traverse . _2) toExtract

  fromExtract :: WordClitic -> WordClitic :* CoreWord :* Clitic
  fromExtract w@(WordClitic cw _ ah _ _ _ _ _) = w :^ cw :^ ah

  fromApply :: WordClitic :* CoreWord -> WordShouldElide
  fromApply (WordClitic _ a _ b c d e f, cw) = WordShouldElide cw a b c d e f

  from
    = over (_Failure . traverse . _2 . _1) fst
    . over (_Success . traverse . _2) fromApply
    . (traverseWithItemContext . _2 $ liftRoundIdFrom Round.clitic)
    . over (traverse . _2) fromExtract
