{-# LANGUAGE TemplateHaskell #-}

module Grammar.Greek.Morph.QuasiQuoters where

import Prelude hiding (Word)
import Data.Data
import Data.Either.Validation
import Data.Generics (extQ)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Grammar.Common
import Grammar.Common.Decompose
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word
import Grammar.Greek.Morph.Types
import qualified Grammar.Greek.Script.Stage as Stage

handleText :: Text -> Maybe ExpQ
handleText x = Just $ appE (varE 'Text.pack) $ litE $ StringL $ Text.unpack x

trim :: String -> String
trim = Text.unpack . Text.strip . Text.pack

parseWordMap :: (String -> String) -> String -> Validation String Word
parseWordMap f x = case (roundTo Stage.script) [x :^ (trim . f . decompose) x :^ NoWordPunctuation] of
  Failure es -> Failure $ concatMap (\(c, e) -> c ++ " -- " ++ show e ++ "\n") es
  Success [(_, w)] -> Success w
  Success w -> Failure $ "Unexpected word: " ++ show w

parseWordsMap :: (String -> String) -> String -> Validation String [Word]
parseWordsMap f = traverse (parseWordMap f) . words

parseWords :: String -> Validation String [Word]
parseWords = parseWordsMap id

wordsExp :: Data a => (Word -> a) -> String -> Q Exp
wordsExp f x = case parseWords x of
  Failure e -> fail e
  Success ws -> dataToExpQ (const Nothing `extQ` handleText) $ fmap f ws

ignoreMacron :: String -> String
ignoreMacron = filter ('\772' /=)

wordsExpLen :: Data a => Int -> (Word -> a) -> String -> Q Exp
wordsExpLen len f x = case parseWordsMap ignoreMacron x of
  Failure e -> fail e
  Success ws -> case length ws == len of
    True -> dataToExpQ (const Nothing `extQ` handleText) $ fmap f ws
    False -> fail $ "expected " ++ show len ++ " words but found " ++ show (length ws)

coreWords :: QuasiQuoter
coreWords = QuasiQuoter
  { quoteExp = wordsExp wordToCoreWord
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

accentedWords :: QuasiQuoter
accentedWords = QuasiQuoter
  { quoteExp = wordsExp wordToAccentedWord
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

nounParadigm :: QuasiQuoter
nounParadigm = QuasiQuoter
  { quoteExp = wordsExpLen 11 wordToAccentedWord
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
