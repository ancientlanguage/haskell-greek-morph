module Grammar.Greek.Morph.Rounds.ShouldElide where

import Control.Lens (over)
import Data.Either.Validation
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Grammar.Common
import Grammar.Greek.Morph.Aspirate
import Grammar.Greek.Morph.Types
import Grammar.Greek.Script.Types

data InvalidElisionForm =
  InvalidElisionForm (InitialAspiration :* [ [ConsonantRho] :* VocalicSyllable ] :* [ConsonantRho] :* Elision :* InitialAspiration)
  deriving (Show)
data InvalidElisionCandidate =
  InvalidElisionCandidate (InitialAspiration :* [ [ConsonantRho] :* VocalicSyllable ] :* [ConsonantRho] :* ShouldElide :* InitialAspiration)
  deriving (Show)

elide
  :: InitialAspiration :* [ [ConsonantRho] :* VocalicSyllable ] :* [ConsonantRho] :* InitialAspiration
  -> InitialAspiration :* [ [ConsonantRho] :* VocalicSyllable ] :* [ConsonantRho] :* InitialAspiration
elide (asp :^ ss :^ [] :^ nasp) | ((fc :^ _) : rss) <- reverse ss = asp :^ reverse rss :^ f fc :^ nasp
  where
  f = case nasp of
    HasInitialAspiration -> aspirateList
    NoInitialAspiration -> id
elide x = x

elidingForms :: [ InitialAspiration :* [ [ConsonantRho] :* VocalicSyllable ] ]
elidingForms =
  [ NoInitialAspiration :^ [[CR_δ] :^ VS_Vowel V_ε]
  , NoInitialAspiration :^ [[] :^ VS_Vowel V_α, [CR_λ, CR_λ] :^ VS_Vowel V_α]
  , NoInitialAspiration :^ [[] :^ VS_Vowel V_α, [CR_ν, CR_τ] :^ VS_Vowel V_ι]
  , NoInitialAspiration :^ [[] :^ VS_Vowel V_α, [CR_π] :^ VS_Vowel V_ο]
  , NoInitialAspiration :^ [[] :^ VS_Vowel V_ε, [CR_π] :^ VS_Vowel V_ι]
  , HasInitialAspiration :^ [[] :^ VS_Vowel V_υ, [CR_π] :^ VS_Vowel V_ο]
  , NoInitialAspiration :^ [[] :^ VS_Diphthong D_ου, [CR_δ] :^ VS_Vowel V_ε]
  , NoInitialAspiration :^ [[CR_δ] :^ VS_Vowel V_ι, [] :^ VS_Vowel V_α]
  , NoInitialAspiration :^ [[CR_κ] :^ VS_Vowel V_α, [CR_τ] :^ VS_Vowel V_α]
  , NoInitialAspiration :^ [[CR_μ] :^ VS_Vowel V_ε, [CR_τ] :^ VS_Vowel V_α]
  , NoInitialAspiration :^ [[CR_μ] :^ VS_Vowel V_η, [CR_δ] :^ VS_Vowel V_ε]
  , NoInitialAspiration :^ [[CR_π] :^ VS_Vowel V_α, [CR_ρ B_Smooth] :^ VS_Vowel V_α]
  , NoInitialAspiration :^ [[CR_τ] :^ VS_Diphthong D_ου, [CR_τ] :^ VS_Vowel V_ο]
  ]

reverseElisionForms :: Map
  (InitialAspiration :* [ [ConsonantRho] :* VocalicSyllable ] :* [ConsonantRho] :* InitialAspiration)
  (InitialAspiration :* [ [ConsonantRho] :* VocalicSyllable ] :* [ConsonantRho] :* InitialAspiration)
reverseElisionForms = Map.fromList $ do
  asp :^ ss <- elidingForms
  nasp <- [ NoInitialAspiration, HasInitialAspiration ]
  let form = asp :^ ss :^ [] :^ nasp
  return (elide form, form)

shouldElide :: Round InvalidElisionForm InvalidElisionCandidate
  (InitialAspiration :* [ [ConsonantRho] :* VocalicSyllable ] :* [ConsonantRho] :* Elision :* InitialAspiration)
  (InitialAspiration :* [ [ConsonantRho] :* VocalicSyllable ] :* [ConsonantRho] :* ShouldElide :* InitialAspiration)
shouldElide = Round (over _Failure pure . to) (over _Failure pure . from)
  where
  to (asp :^ ss :^ fc :^ IsElided :^ nasp)
    | Just (asp' :^ ss' :^ fc' :^ nasp') <- Map.lookup (asp :^ ss :^ fc :^ nasp) reverseElisionForms
    = Success $ asp' :^ ss' :^ fc' :^ ShouldElide :^ nasp'
  to (asp :^ ss :^ fc :^ NotElided :^ nasp) = Success $ asp :^ ss :^ fc :^ ShouldNotElide :^ nasp
  to x = Failure $ InvalidElisionForm x

  from (asp :^ ss :^ fc :^ ShouldElide :^ nasp) =
    let asp' :^ ss' :^ fc' :^ nasp' = elide $ asp :^ ss :^ fc :^ nasp
    in Success $ asp' :^ ss' :^ fc' :^ IsElided :^ nasp'
  from (asp :^ ss :^ fc :^ ShouldNotElide :^ nasp) = Success $ asp :^ ss :^ fc :^ NotElided :^ nasp
  from x = Failure $ InvalidElisionCandidate x
