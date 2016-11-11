{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Grammar.Greek.Morph.Paradigm.Types where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Data
import Data.Serialize (Serialize)
import Data.Text (Text)
import Grammar.Greek.Morph.Types
import Grammar.Greek.Script.Types hiding (Case)
import Grammar.Greek.Script.Word

data ParadigmForm = ParadigmForm
  { paradigmFormText :: Text
  , paradigmFormWord :: AccentedWord
  }
  deriving (Data, Typeable)

data NounParadigm = NounParadigm
  { nounParadigmSgNom :: Maybe ParadigmForm
  , nounParadigmSgGen :: Maybe ParadigmForm
  , nounParadigmSgDat :: Maybe ParadigmForm
  , nounParadigmSgAcc :: Maybe ParadigmForm
  , nounParadigmSgVoc :: Maybe ParadigmForm

  , nounParadigmDualNomAccVoc :: Maybe ParadigmForm
  , nounParadigmDualGenDat :: Maybe ParadigmForm

  , nounParadigmPlNomVoc :: Maybe ParadigmForm
  , nounParadigmPlGen :: Maybe ParadigmForm
  , nounParadigmPlDat :: Maybe ParadigmForm
  , nounParadigmPlAcc :: Maybe ParadigmForm
  }
  deriving (Data, Typeable)

data VerbParadigm = VerbParadigm
  { verbParadigmSg1 :: Maybe ParadigmForm
  , verbParadigmSg2 :: Maybe ParadigmForm
  , verbParadigmSg3 :: Maybe ParadigmForm

  , verbParadigmDual2 :: Maybe ParadigmForm
  , verbParadigmDual3 :: Maybe ParadigmForm

  , verbParadigmPl1 :: Maybe ParadigmForm
  , verbParadigmPl2 :: Maybe ParadigmForm
  , verbParadigmPl3 :: Maybe ParadigmForm
  }
  deriving (Data, Typeable)
