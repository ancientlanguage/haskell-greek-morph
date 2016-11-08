module Grammar.Greek.Morph.Serialize where

import Prelude hiding (Word)
import Grammar.CommonTypes
import Grammar.Greek.Script.Word
import Grammar.Prepare
import Grammar.Serialize

scriptPath :: FilePath
scriptPath = "./modules/binary-greek-script/data"

readScript :: IO [SourceId :* [Milestone :* Word]]
readScript = loadStage scriptPath
