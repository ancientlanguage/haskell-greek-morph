{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Grammar.Greek.Morph.Stage where

import Prelude hiding (Word)
import Data.Void
import Grammar.Common
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word
import qualified Grammar.Greek.Morph.Rounds as Rounds

wordProperties :: RoundContext ctx Void Void
  Word
  ( InitialAspiration
  :*
    [ [ConsonantRho]
    :* VocalicSyllable
    ]
  :* [ConsonantRho]
  :* Maybe
    ( BasicAccent
    :* AccentPosition
    :* ForceAcute
    :* ExtraAccents
    )
  :* Crasis
  :* Elision
  :* MarkPreservation
  :* Capitalization
  :* HasWordPunctuation
  )
wordProperties = Round
  (traverseWithItemContext $ liftRoundIdTo Rounds.wordProperties)
  (traverseWithItemContext $ liftRoundIdFrom Rounds.wordProperties)

morph = wordProperties
