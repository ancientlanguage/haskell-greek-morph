{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Smyth.Nouns.Nouns where

import Grammar.Greek.Morph.QuasiQuoters

-- Smyth 216. Feminines
firstDeclensionFeminines =
  [ [nounParadigm|
      χώρᾱ
      χώρᾱς
      χώρᾳ
      χώρᾱν
      χώρᾱ

      χώρᾱ
      χώραιν

      χῶραι
      χωρῶν
      χώραις
      χώρᾱς
    |]
  , [nounParadigm|
      νί̄κη
      νί̄κης
      νί̄κῃ
      νί̄κην
      νί̄κη

      νί̄κᾱ
      νί̄καιν

      νῖκαι
      νῑκῶν
      νί̄καις
      νί̄κᾱς
    |]
  , [nounParadigm|
      φυγή
      φυγῆς
      φυγῇ
      φυγήν
      φυγή

      φυγά̄
      φυγαῖν

      φυγαί
      φυγῶν
      φυγαῖς
      φυγά̄ς
    |]
  ]
