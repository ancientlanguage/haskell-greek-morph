{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.ElidingForms where

import Grammar.Greek.Morph.QuasiQuoters
import Grammar.Greek.Morph.Types

elidingForms :: [ CoreWord ]
elidingForms =
  [ [coreWord| ἀντί |]
  , [coreWord| ἀπό |]
  , [coreWord| διά |]
  , [coreWord| ἐπί |]
  , [coreWord| κατά |]
  , [coreWord| μετά |]
  , [coreWord| παρά |]
  , [coreWord| ὑπό |]

  , [coreWord| ἀλλά |]
  , [coreWord| δέ |]
  , [coreWord| μηδέ |]
  , [coreWord| οὐδέ |]

  , [coreWord| τοῦτο |]
  ]
