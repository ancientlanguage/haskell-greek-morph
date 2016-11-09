import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertFailure, assertEqual)
import Control.Lens (over, _2, (^?), _Right)
import Data.Either.Validation (Validation(..), _Failure)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Grammar.Common.Prepare
import Grammar.Common.Pretty
import Grammar.Common.Round
import Grammar.Common.Types
import Grammar.IO.Serialize
import Grammar.Greek.Morph.Types
import Grammar.Greek.Script.Types
import qualified Grammar.Greek.Morph.Rounds as Rounds
import qualified Grammar.Greek.Morph.Serialize as Serialize
import qualified Grammar.Greek.Morph.Stage as Stage
import Grammar.Test.Round
import Grammar.Test.Stage

shouldElideGroup = testGroup "shouldElide" $
  [ testDest "δ’"
    (NoInitialAspiration :^ [] :^ [CR_δ] :^ IsElided :^ NoInitialAspiration)
    (NoInitialAspiration :^ [[CR_δ] :^ VS_Vowel V_ε] :^ [] :^ ShouldElide :^ NoInitialAspiration)
  , testDest "ὑπ’"
    (HasInitialAspiration :^ [[] :^ VS_Vowel V_υ] :^ [CR_π] :^ IsElided :^ NoInitialAspiration)
    (HasInitialAspiration :^ [[] :^ VS_Vowel V_υ, [CR_π] :^ VS_Vowel V_ο] :^ [] :^ ShouldElide :^ NoInitialAspiration)
  , testDest "ὑφ’"
    (HasInitialAspiration :^ [[] :^ VS_Vowel V_υ] :^ [CR_φ] :^ IsElided :^ HasInitialAspiration)
    (HasInitialAspiration :^ [[] :^ VS_Vowel V_υ, [CR_π] :^ VS_Vowel V_ο] :^ [] :^ ShouldElide :^ HasInitialAspiration)
  ]
  where
  testDest n vs ds = testRoundDest n Rounds.shouldElide vs ds

main :: IO ()
main = defaultMain
  [ shouldElideGroup
  , testGroupStages "morph stage" Stage.morph id (pure <$> Serialize.readScript)
  ]
