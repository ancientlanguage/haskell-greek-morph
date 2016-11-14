{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Grammar.Greek.Morph.Paradigm.Types where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Data
import Data.Serialize (Serialize)
import Data.Text (Text)
import Grammar.Greek.Morph.Types
import Primary ()

data ParadigmExemplar = ParadigmExemplar
  { paradigmExemplarText :: Text
  , paradigmExemplarWord :: AccentedWord
  }
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize ParadigmExemplar

data Declension = Declension1 | Declension2 | Declension3 | DeclensionIrregular
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Declension

data Conjugation = Conjugation1Omega | Conjugation2MI | ConjugationIrregularMI
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Conjugation

data FormKind
  = NounFormKind Declension
  | PronounFormKind
  | AdjectiveFormKind
  | AdverbFormKind
  | NumeralFormKind
  | VerbFormKind Conjugation
  | ParticipleFormKind
  | InfinitiveFormKind
  | ParticleFormKind
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize FormKind

data ParadigmEnding = ParadigmEnding
  { paradigmEndingExemplar :: ParadigmExemplar
  , paradigmEndingMorph :: Morph
  , paradigmEndingEnding :: [Phoneme]
  }
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize ParadigmEnding

data NounParadigm = NounParadigm
  { nounParadigmSgNom :: Maybe ParadigmExemplar
  , nounParadigmSgGen :: Maybe ParadigmExemplar
  , nounParadigmSgDat :: Maybe ParadigmExemplar
  , nounParadigmSgAcc :: Maybe ParadigmExemplar
  , nounParadigmSgVoc :: Maybe ParadigmExemplar

  , nounParadigmDualNomAccVoc :: Maybe ParadigmExemplar
  , nounParadigmDualGenDat :: Maybe ParadigmExemplar

  , nounParadigmPlNomVoc :: Maybe ParadigmExemplar
  , nounParadigmPlGen :: Maybe ParadigmExemplar
  , nounParadigmPlDat :: Maybe ParadigmExemplar
  , nounParadigmPlAcc :: Maybe ParadigmExemplar
  }
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize NounParadigm

data VerbParadigm = VerbParadigm
  { verbParadigmSg1 :: Maybe ParadigmExemplar
  , verbParadigmSg2 :: Maybe ParadigmExemplar
  , verbParadigmSg3 :: Maybe ParadigmExemplar

  , verbParadigmDual2 :: Maybe ParadigmExemplar
  , verbParadigmDual3 :: Maybe ParadigmExemplar

  , verbParadigmPl1 :: Maybe ParadigmExemplar
  , verbParadigmPl2 :: Maybe ParadigmExemplar
  , verbParadigmPl3 :: Maybe ParadigmExemplar
  }
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize VerbParadigm
