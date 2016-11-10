{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.ProcliticForms where

import Grammar.Greek.Morph.QuasiQuoters
import Grammar.Greek.Morph.Types

-- Smyth 179

procliticForms :: [ CoreWord ]
procliticForms =
  [ [coreWord| ὁ |]
  , [coreWord| ἡ |]
  , [coreWord| οἱ |]
  , [coreWord| αἱ |]
  , [coreWord| ἐν |]
  , [coreWord| εἰς |]
  , [coreWord| ἐς |]
  , [coreWord| ἐξ |]
  , [coreWord| ἐκ |]
  , [coreWord| εἰ |]
  , [coreWord| ὡς |]
  , [coreWord| οὐ |]
  , [coreWord| οὐκ |]
  , [coreWord| οὐχ |]
  ]
