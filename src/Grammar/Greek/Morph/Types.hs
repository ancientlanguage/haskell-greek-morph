{-# LANGUAGE DeriveGeneric #-}

module Grammar.Greek.Morph.Types where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)

data ShouldElide = ShouldElide | ShouldNotElide
  deriving (Eq, Ord, Show, Generic)
instance Serialize ShouldElide
