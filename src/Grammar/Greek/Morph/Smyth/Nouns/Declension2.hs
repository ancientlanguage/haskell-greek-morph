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

-- Smyth 235. Contracts
secondDeclensionContracts =
  [ [nounParadigm|
νοῦ
νῷ
νοῦ-ν
νοῦ
  *

νώ
νοῖν

νοῖ
νῶν
νοῖς
νοῦς
    |]
    , [nounParadigm|
περίπλου
περίπλῳ
περίπλου-ν
περίπλου
  *

περίπλω
περίπλοιν

περίπλοι
περίπλων
περίπλοις
περίπλους
    |]
    , [nounParadigm|
ὀστοῦ
ὀστῷ
ὀστοῦ-ν
ὀστοῦ-ν
  *

ὀστώ
ὀστοῖν

ὀστᾶ
ὀστῶν
ὀστοῖς
ὀστᾶ
    |]
  ]