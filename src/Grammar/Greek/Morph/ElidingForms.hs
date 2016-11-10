{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.ElidingForms where

import Grammar.Greek.Morph.QuasiQuoters
import Grammar.Greek.Morph.Types

elidingForms :: [ CoreWord ]
elidingForms =
  [ [coreWord| ἀλλά |]
  , [coreWord| ἀντί |]
  , [coreWord| ἀπό |]
  , [coreWord| δέ |]
  , [coreWord| διά |]
  , [coreWord| ἐπί |]
  , [coreWord| κατά |]
  , [coreWord| μετά |]
  , [coreWord| μηδέ |]
  , [coreWord| οὐδέ |]
  , [coreWord| παρά |]
  , [coreWord| τοῦτο |]
  , [coreWord| ὑπό |]
  ]
