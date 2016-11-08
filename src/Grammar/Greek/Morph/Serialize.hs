module Grammar.Greek.Morph.Serialize where

import Prelude hiding (Word)
import qualified Data.ByteString as BS
import qualified Data.Serialize as Serialize
import Grammar.CommonTypes
import Grammar.Greek.Script.Word
import Grammar.Prepare

decodeScript :: BS.ByteString -> Either String [SourceId :* [Milestone :* Word]]
decodeScript = Serialize.decode

scriptPath :: FilePath
scriptPath = "./modules/binary-greek-script/data"

readScript :: IO (Either String [SourceId :* [Milestone :* Word]])
readScript = decodeScript <$> BS.readFile scriptPath
