{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Executable checks for the adhesive and PBPO rewriting laws.
module Moonlight.Category.Effect.Harness.Adhesive
  ( adhesiveWitnessMonicSound,
    pushoutComplementSquareCommutes,
    pushoutComplementUniversal,
    pbpoPullbackSquareCommutes,
    pbpoPushoutSquareCommutes,
    pbpoComplementUniversal,
    pullbackMediatorCommutes,
  )
where

import Moonlight.Category.Pure.Adhesive
  ( AdhesiveCategory,
    PBPOAdhesiveCategory,
    PBPOComplementWitness,
    PushoutComplementWitness,
    monicMatchArrow,
    pbpoComplement,
    pushoutComplement,
    witnessMonic,
  )
import Moonlight.Category.Pure.Adhesive qualified as Adhesive
import Moonlight.Category.Pure.Category (Category (..), composeMor)
import Moonlight.Category.Pure.Limits (HasPullbacks (..))
import Prelude hiding (Functor)

adhesiveWitnessMonicSound :: forall c. (AdhesiveCategory c, Eq (Mor c)) => c -> (Mor c -> Bool) -> Mor c -> Bool
adhesiveWitnessMonicSound categoryValue isMonic morphism =
  case witnessMonic @c categoryValue morphism of
    Nothing ->
      True
    Just witness ->
      monicMatchArrow witness == morphism && isMonic morphism

pushoutComplementSquareCommutes :: forall c. (AdhesiveCategory c, Eq (Mor c)) => c -> Mor c -> Mor c -> Bool
pushoutComplementSquareCommutes categoryValue ruleLeg matchArrow =
  maybe True (Adhesive.pushoutComplementSquareCommutes categoryValue) (pushoutComplementWitness @c categoryValue ruleLeg matchArrow)

pushoutComplementUniversal :: forall c. AdhesiveCategory c => c -> (PushoutComplementWitness c -> Bool) -> Mor c -> Mor c -> Bool
pushoutComplementUniversal categoryValue isUniversal ruleLeg matchArrow =
  maybe True isUniversal (pushoutComplementWitness @c categoryValue ruleLeg matchArrow)

pbpoPullbackSquareCommutes :: forall c. (PBPOAdhesiveCategory c, Eq (Mor c)) => c -> Mor c -> Mor c -> Bool
pbpoPullbackSquareCommutes categoryValue ruleLeg matchArrow =
  maybe True (Adhesive.pbpoPullbackSquareCommutes categoryValue) (pbpoComplementWitness @c categoryValue ruleLeg matchArrow)

pbpoPushoutSquareCommutes :: forall c. (PBPOAdhesiveCategory c, Eq (Mor c)) => c -> Mor c -> Mor c -> Bool
pbpoPushoutSquareCommutes categoryValue ruleLeg matchArrow =
  maybe True (Adhesive.pbpoPushoutSquareCommutes categoryValue) (pbpoComplementWitness @c categoryValue ruleLeg matchArrow)

pbpoComplementUniversal :: forall c. PBPOAdhesiveCategory c => c -> (PBPOComplementWitness c -> Bool) -> Mor c -> Mor c -> Bool
pbpoComplementUniversal categoryValue isUniversal ruleLeg matchArrow =
  maybe True isUniversal (pbpoComplementWitness @c categoryValue ruleLeg matchArrow)

pullbackMediatorCommutes ::
  forall c.
  (HasPullbacks c, Eq (Mor c)) =>
  c ->
  Mor c ->
  Mor c ->
  Mor c ->
  Mor c ->
  Bool
pullbackMediatorCommutes categoryValue leftBase rightBase coneLeft coneRight =
  case pullback @c categoryValue leftBase rightBase of
    Nothing ->
      False
    Just (_, projLeft, projRight) ->
      case (composeMor @c categoryValue leftBase coneLeft, composeMor @c categoryValue rightBase coneRight) of
        (Right leftComposite, Right rightComposite)
          | leftComposite == rightComposite ->
              case pullbackMediator @c categoryValue leftBase rightBase coneLeft coneRight of
                Just mediator ->
                  rightEquals (composeMor @c categoryValue projLeft mediator) coneLeft
                    && rightEquals (composeMor @c categoryValue projRight mediator) coneRight
                Nothing ->
                  False
        _ ->
          False

pushoutComplementWitness :: forall c. AdhesiveCategory c => c -> Mor c -> Mor c -> Maybe (PushoutComplementWitness c)
pushoutComplementWitness categoryValue ruleLeg matchArrow =
  witnessMonic @c categoryValue matchArrow >>= pushoutComplement @c categoryValue ruleLeg

pbpoComplementWitness :: forall c. PBPOAdhesiveCategory c => c -> Mor c -> Mor c -> Maybe (PBPOComplementWitness c)
pbpoComplementWitness categoryValue ruleLeg matchArrow =
  witnessMonic @c categoryValue matchArrow >>= pbpoComplement @c categoryValue ruleLeg

rightEquals :: Eq value => Either err value -> value -> Bool
rightEquals eitherValue expected =
  case eitherValue of
    Right value -> value == expected
    Left _ -> False
