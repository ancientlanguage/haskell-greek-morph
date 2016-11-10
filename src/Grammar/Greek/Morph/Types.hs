{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Grammar.Greek.Morph.Types where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Data
import Data.Serialize (Serialize)
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word

data ShouldElide = ShouldElide | ShouldNotElide
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize ShouldElide

data CoreWord = CoreWord
  { coreWordAspiration :: InitialAspiration
  , coreWordSyllables :: [Syllable]
  , coreWordFinalConsonants :: [ConsonantRho]
  }
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize CoreWord

wordCore :: Word -> CoreWord
wordCore (Word asp ss fc _ _ _ _ _ _) = CoreWord asp ss fc

data WordShouldElide = WordShouldElide
  CoreWord
  (Maybe WordAccent)
  Crasis
  ShouldElide
  MarkPreservation
  Capitalization
  HasWordPunctuation
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize WordShouldElide

wordShouldElideCoreWord :: WordShouldElide -> CoreWord
wordShouldElideCoreWord (WordShouldElide x _ _ _ _ _ _) = x
wordShouldElideShouldElide :: WordShouldElide -> ShouldElide
wordShouldElideShouldElide (WordShouldElide _ _ _ x _ _ _) = x
