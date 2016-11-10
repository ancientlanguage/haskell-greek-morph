{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Smyth.Verbs.Verbs where

import Grammar.Greek.Morph.QuasiQuoters

--  i. (b) vowel verbs:  contracted verbs
-- Smyth 385
luo =
  -- indicative present
  [ [verbParadigm|
      λύ̄ω
      λύ̄εις
      λύ̄ει
      λύ̄ετον
      λύ̄ετον
      λύ̄ομεν
      λύ̄ετε
      λύ̄ουσι
    |]

  -- indicative imperfect
  , [verbParadigm|
      ἔλῡον
      ἔλῡες
      ἔλῡε
      ἐλύ̄ετον
      ἐλῡέτην
      ἐλύ̄ομεν
      ἐλύ̄ετε
      ἔλῡον
    |]

  -- indicativeFuture
  , [verbParadigm|
      λύ̄σω
      λύ̄σεις
      λύ̄σει
      λύ̄σετον
      λύ̄σετον
      λύ̄σομεν
      λύ̄σετε
      λύ̄σουσι
    |]
  ]
