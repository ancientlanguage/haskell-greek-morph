{-# LANGUAGE DeriveGeneric #-}

module Grammar.Greek.Morph.Types where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word

data ShouldElide = ShouldElide | ShouldNotElide
  deriving (Eq, Ord, Show, Generic)
instance Serialize ShouldElide

data CoreWord = CoreWord
  { coreWordAspiration :: InitialAspiration
  , coreWordSyllables :: [Syllable]
  , coreWordFinalConsonants :: [ConsonantRho]
  }
  deriving (Eq, Ord, Show, Generic)
instance Serialize CoreWord
