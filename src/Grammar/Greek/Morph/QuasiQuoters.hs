{-# LANGUAGE TemplateHaskell #-}

module Grammar.Greek.Morph.QuasiQuoters where

import Prelude hiding (Word)
import Data.Data
import Data.Either.Validation
import Data.Generics (extQ)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Grammar.Common
import Grammar.Common.Decompose
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word
import Grammar.Greek.Morph.Types
import qualified Grammar.Greek.Script.Stage as Stage

handleText :: T.Text -> Maybe ExpQ
handleText x = Just $ appE (varE 'T.pack) $ litE $ StringL $ T.unpack x

trim :: String -> String
trim = T.unpack . T.strip . T.pack

parseWord :: String -> Validation String Word
parseWord x = case (roundTo Stage.script) [() :^ (trim . decompose) x :^ NoWordPunctuation] of
  Failure e -> Failure (show e)
  Success [(_, w)] -> Success w
  Success w -> Failure $ "Unexpected word: " ++ show w

wordExp :: Data a => (Word -> a) -> String -> Q Exp
wordExp f x = case parseWord x of
  Failure e -> fail e
  Success w -> dataToExpQ (const Nothing `extQ` handleText) $ f w

coreWord :: QuasiQuoter
coreWord = QuasiQuoter
  { quoteExp = wordExp wordToCoreWord
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

accentedWord :: QuasiQuoter
accentedWord = QuasiQuoter
  { quoteExp = wordExp wordToAccentedWord
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
