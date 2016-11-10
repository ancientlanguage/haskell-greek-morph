{-# LANGUAGE TemplateHaskell #-}

module Grammar.Greek.Morph.QuasiQuoters where

import Data.Either.Validation
import Data.Generics (extQ)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Grammar.Common
import Grammar.Common.Decompose
import Grammar.Greek.Script.Types
import Grammar.Greek.Morph.Types
import qualified Grammar.Greek.Script.Stage as Stage

handleText :: T.Text -> Maybe ExpQ
handleText x = Just $ appE (varE 'T.pack) $ litE $ StringL $ T.unpack x

trim :: String -> String
trim = T.unpack . T.strip . T.pack

coreWordExp :: String -> Q Exp
coreWordExp x = case (roundTo Stage.script) [() :^ (trim . decompose) x :^ NoWordPunctuation] of
  Failure e -> fail (show e)
  Success [(_, w)] -> dataToExpQ (const Nothing `extQ` handleText) $ wordCore w
  Success w -> fail $ "Unexpected word: " ++ show w

coreWord :: QuasiQuoter
coreWord = QuasiQuoter
  { quoteExp = coreWordExp
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
