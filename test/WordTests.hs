{-# LANGUAGE QuasiQuotes #-}

module WordTests where

import Test.Framework
import Grammar.Greek.Morph.Phoneme (phonemeRound)
import Grammar.Greek.Morph.QuasiQuoters
import Grammar.Greek.Morph.Types
import Grammar.Test.Round

coreWordPhonemeRound = testGroup "coreWordPhonemeRound" $ fmap (uncurry $ testRoundId phonemeRound)
  [coreWordPairs|
  ἀντί
  ἀπό
  διά
  ἐπί
  κατά
  μετά
  παρά
  ὑπό

  ἀλλά

  δέ
  μηδέ
  οὐδέ

  τοῦτο
  φυγῆς
  |]
