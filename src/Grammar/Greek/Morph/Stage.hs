module Grammar.Greek.Morph.Stage where

import Prelude hiding (Word)
import Control.Lens (over, _Just, _2)
import Grammar.Common.Types
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

start :: [SourceId :* [Milestone :* Word]]
  ->
    [ SourceId :*
      [ Milestone
      :* InitialAspiration
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
      ]
    ]
start = over (traverse . _2 . traverse . _2) explodeWord
