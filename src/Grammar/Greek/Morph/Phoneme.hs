{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Grammar.Greek.Morph.Phoneme where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Data
import Data.Serialize (Serialize)
import Grammar.Common
import Grammar.Greek.Morph.Types
import Grammar.Greek.Script.Types hiding (Case)
import Grammar.Greek.Script.Word
import qualified Grammar.Common.Round as Round

data Phoneme
  = Ph_Aspiration
  | Ph_Consonant ConsonantRho
  | Ph_Vocalic VocalicSyllable
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Phoneme

coreWordToPhonemes :: CoreWord -> [Phoneme]
coreWordToPhonemes (CoreWord asp ss fc) = addAsp asp $ concatMap doSyll ss ++ doCons fc
  where
  addAsp NoInitialAspiration xs = xs
  addAsp HasInitialAspiration xs = Ph_Aspiration : xs
  doSyll (Syllable cs v) = doCons cs ++ [Ph_Vocalic v]
  doCons = fmap Ph_Consonant

simplePhonemesToSyllables :: [ConsonantRho :+ VocalicSyllable] -> [[ConsonantRho] :* VocalicSyllable] :* [ConsonantRho]
simplePhonemesToSyllables = roundIdTo Round.groupLeft

simplePhoneme :: Phoneme -> [ConsonantRho :+ VocalicSyllable]
simplePhoneme Ph_Aspiration = []
simplePhoneme (Ph_Consonant x) = [Left x]
simplePhoneme (Ph_Vocalic x) = [Right x]

phonemesToCoreWord :: [Phoneme] -> CoreWord
phonemesToCoreWord [] = CoreWord NoInitialAspiration [] []
phonemesToCoreWord xs@(x : _) =
  let
    ps = concatMap simplePhoneme xs
    (sps, fc) = simplePhonemesToSyllables ps
    ss = fmap (uncurry Syllable) sps
    asp = case x of
      Ph_Aspiration -> HasInitialAspiration
      _ -> NoInitialAspiration
  in CoreWord asp ss fc

phonemeRound :: RoundId CoreWord [Phoneme]
phonemeRound = RoundId coreWordToPhonemes phonemesToCoreWord
