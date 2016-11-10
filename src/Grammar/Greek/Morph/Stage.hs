{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Grammar.Greek.Morph.Stage where

import Grammar.Greek.Morph.Stage.ShouldElide (shouldElide)

morph = shouldElide
