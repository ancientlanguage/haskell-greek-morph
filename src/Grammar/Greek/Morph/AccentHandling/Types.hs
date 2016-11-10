{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Grammar.Greek.Morph.AccentHandling.Types where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Data
import Data.Serialize (Serialize)
import Grammar.Greek.Morph.Types
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word

data WordAccentHandling = WordAccentHandling
  CoreWord
  (Maybe WordAccent)
  AccentHandling
  Crasis
  ShouldElide
  MarkPreservation
  Capitalization
  HasWordPunctuation
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize WordAccentHandling
