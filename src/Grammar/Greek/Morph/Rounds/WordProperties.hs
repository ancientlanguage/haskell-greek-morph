module Grammar.Greek.Morph.Rounds.WordProperties where

import Prelude hiding (Word)
import Control.Lens (over, _Just)
import Grammar.Common
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word

explodeSyllable :: Syllable -> [ConsonantRho] :* VocalicSyllable
explodeSyllable (Syllable cs v) = cs :^ v

explodeWordAccent :: WordAccent -> BasicAccent :* AccentPosition :* ForceAcute :* ExtraAccents
explodeWordAccent (WordAccent a b c d) = a :^ b :^ c :^ d

explodeWord
  :: Word
  -> InitialAspiration
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
explodeWord (Word a b c d e f g h i) = a :^ fmap explodeSyllable b :^ c :^ over _Just explodeWordAccent d :^ e :^ f :^ g :^ h :^ i

implodeSyllable :: [ConsonantRho] :* VocalicSyllable -> Syllable
implodeSyllable = uncurry Syllable

implodeWordAccent :: BasicAccent :* AccentPosition :* ForceAcute :* ExtraAccents -> WordAccent
implodeWordAccent (a, (b, (c, d))) = WordAccent a b c d

implodeWord
  :: InitialAspiration
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
  -> Word
implodeWord (a, (b, (c, (d, (e, (f, (g, (h, i)))))))) = Word a (fmap implodeSyllable b) c (over _Just implodeWordAccent d) e f g h i

wordProperties :: RoundId Word
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
wordProperties = RoundId explodeWord implodeWord
