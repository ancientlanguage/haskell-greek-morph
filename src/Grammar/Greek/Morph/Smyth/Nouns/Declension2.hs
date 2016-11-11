{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Smyth.Nouns.Declension2 where

import Grammar.Greek.Morph.QuasiQuoters

-- Smyth 230
declension2 =
  [ [nounParadigm|
ἵππο-ς
ἵππου
ἵππῳ
ἵππο-ν
ἵππε

ἵππω
ἵπποιν

ἵπποι
ἵππων
ἵπποις
ἵππους

    |]
  , [nounParadigm|
ἄνθρωπο-ς
ἀνθρώπου
ἀνθρώπῳ
ἄνθρωπο-ν
ἄνθρωπε

ἀνθρώπω
ἀνθρώποιν

ἄνθρωποι
ἀνθρώπων
ἀνθρώποις
ἀνθρώπους

    |]
  , [nounParadigm|
ὁδό-ς
ὁδοῦ
ὁδῷ
ὁδό-ν
ὁδέ

ὁδώ
ὁδοῖν

ὁδοί
ὁδῶν
ὁδοῖς
ὁδούς
    |]
  , [nounParadigm|
δῶρο-ν
δώρου
δώρῳ
δῶρο-ν
δῶρο-ν

δώρω
δώροιν

δῶρα
δώρων
δώροις
δῶρα
    |]
  ]

