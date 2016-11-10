{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.EncliticForms where

import Grammar.Greek.Morph.QuasiQuoters
import Grammar.Greek.Morph.Types

-- Smyth 181

encliticForms :: [ AccentedWord ]
encliticForms =
  [ [accentedWord| μοῦ |]
  , [accentedWord| μοί |]
  , [accentedWord| μέ |]
  , [accentedWord| σοῦ |]
  , [accentedWord| σοί |]
  , [accentedWord| σέ |]
  , [accentedWord| οὗ |]
  , [accentedWord| οἷ |]
  , [accentedWord| ἕ |]

  , [accentedWord| τίς |]
  , [accentedWord| τί |]
  , [accentedWord| τοῦ |]
  , [accentedWord| τῷ |]
  , [accentedWord| τινός |]
  , [accentedWord| τινί |]

  , [accentedWord| πού |]
  , [accentedWord| ποθί |]
  , [accentedWord| πῄ |]
  , [accentedWord| ποί |]
  , [accentedWord| ποθέν |]
  , [accentedWord| ποτέ |]
  , [accentedWord| πώ |]
  , [accentedWord| πώς |]

  -- εἰμί pres indic except εἶ
  , [accentedWord| ἐστί |]
  , [accentedWord| ἐστίν |]
  , [accentedWord| ἐσμέν |]
  , [accentedWord| ἐστέ |]
  , [accentedWord| εἰσί |]
  , [accentedWord| εἰσίν |]

  -- φημί pres indic except φῄς
  , [accentedWord| φημί |]
  , [accentedWord| φησί |]
  , [accentedWord| φαμέν |]
  , [accentedWord| φατέ |]
  , [accentedWord| φασί |]
  , [accentedWord| φασίν |]

  , [accentedWord| γέ |]
  , [accentedWord| τέ |]
  , [accentedWord| τοί |]
  , [accentedWord| πέρ |]
  -- the inseparable -δε in ὅδε, τοσόσδε
  ]
