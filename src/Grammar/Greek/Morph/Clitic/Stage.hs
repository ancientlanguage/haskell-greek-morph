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
  toExtract w@(WordShouldElide cw _ _ _ _ _ _ _) = w :^ cw

  toApply :: WordShouldElide :* CoreWord :* Enclitic :* Proclitic -> WordClitic
  toApply (WordShouldElide _ a b c d e f g, (cw, (en, pro))) = WordClitic cw a en pro b c d e f g

  to
    = over (_Failure . traverse . _2 . _1) fst
    . over (_Success . traverse . _2) toApply
    . (traverseWithItemContext . _2 $ liftRoundIdTo Round.clitic)
    . over (traverse . _2) toExtract

  fromExtract :: WordClitic -> WordClitic :* CoreWord :* Enclitic :* Proclitic
  fromExtract w@(WordClitic cw _ en pro _ _ _ _ _ _) = w :^ cw :^ en :^ pro

  fromApply :: WordClitic :* CoreWord -> WordShouldElide
  fromApply (WordClitic _ a _ _ b c d e f g, cw) = WordShouldElide cw a b c d e f g

  from
    = over (_Failure . traverse . _2 . _1) fst
    . over (_Success . traverse . _2) fromApply
    . (traverseWithItemContext . _2 $ liftRoundIdFrom Round.clitic)
    . over (traverse . _2) fromExtract
