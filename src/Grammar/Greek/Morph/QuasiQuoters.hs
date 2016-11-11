{-# LANGUAGE TemplateHaskell #-}

module Grammar.Greek.Morph.QuasiQuoters where

import Prelude hiding (Word)
import Control.Applicative
import Control.Lens (over)
import Data.Data
import Data.Either.Validation
import Data.Generics (extQ)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Grammar.Common
import Grammar.Common.Decompose
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word
import Grammar.Greek.Morph.Paradigm.Types
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

parseStar :: String -> Validation String (Maybe a)
parseStar "*" = Success Nothing
parseStar x = Failure $ "not a star: " ++ x

parseWords :: String -> Validation String [Word]
parseWords = traverse (parseWordMap id) . words

ignoreParadigmChars :: String -> String
ignoreParadigmChars = filter (flip Set.notMember ignoreChars)
  where
  ignoreChars = Set.fromList
    [ '\772' -- macron
    , '-'
    , '('
    , ')'
    ]

parseParadigmForms :: String -> Validation String [Maybe ParadigmForm]
parseParadigmForms = traverse (\x -> parseStar x <|> doWord x) . words
  where
  doWord y = over _Success (pure . ParadigmForm (Text.pack y) . wordToAccentedWord) . parseWordMap ignoreParadigmChars $ y

wordsExp :: Data a => (Word -> a) -> String -> Q Exp
wordsExp f x = case parseWords x of
  Failure e -> fail e
  Success ws -> dataToExpQ (const Nothing `extQ` handleText) $ fmap f ws

nounParadigmParser :: String -> Q Exp
nounParadigmParser x = case parseParadigmForms x of
  Failure e -> fail e
  Success [a,b,c,d,e, f,g, h,i,j,k] -> dataToExpQ (const Nothing `extQ` handleText) $ NounParadigm a b c d e f g h i j k
  Success ws -> fail $ "Incorrect number of noun forms: " ++ show (length ws)

verbParadigmParser :: String -> Q Exp
verbParadigmParser x = case parseParadigmForms x of
  Failure e -> fail e
  Success [a,b,c, d,e, f,g,h] -> dataToExpQ (const Nothing `extQ` handleText) $ VerbParadigm a b c d e f g h
  Success ws -> fail $ "Incorrect number of verb forms: " ++ show (length ws)

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
  { quoteExp = nounParadigmParser
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

verbParadigm :: QuasiQuoter
verbParadigm = QuasiQuoter
  { quoteExp = verbParadigmParser
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
