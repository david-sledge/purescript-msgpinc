module Test.Main
  ( main
  )
  where

import Prelude

import Control.MsgPinc.Deserializer (BitSize(..), DataSize(..), DataType(..), DeserialError(..), Event(..), ExtSubState(..), MapSubState(..), NumberType(..), SubState(..), caseDeserialState, deserializeMsgPackStreamT, initState)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as D
import Data.ArrayBuffer.Typed as A
import Data.ArrayBuffer.Types (DataView)
import Data.Either (Either(Left, Right))
import Data.ListLike as L
import Data.MsgPinc.Spec (array16, array32, bin16, bin32, bin8, ext16, ext32, ext8, fals, fixarray, fixext1, fixext16, fixext2, fixext4, fixext8, fixmap, fixstr, float32, float64, int16, int32, int64, int8, map16, map32, nil, posFixint, str16, str32, str8, tru, uint16, uint32, uint64, uint8)
import Data.Tuple (Tuple(Tuple))
import Data.UInt as U
import Data.UInt (UInt)
import Debug (trace)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Spec.Assertions (fail)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Web.Encoding.TextDecoder (new)
import Web.Encoding.UtfLabel (utf8)
import Data.Int.Bits ((.|.))

main ∷ Effect Unit
main = do
  test

compareDataViews :: DataView -> DataView -> Effect Boolean
compareDataViews dv1 dv2 = do
  let byteLen = D.byteLength dv1
  if D.byteLength dv2 == byteLen
    then
      let
        recurse ndx =
          let restLen = byteLen - ndx in
          if restLen >= 4
          then do
            val1 <- D.getInt32be dv1 ndx
            val2 <- D.getInt32be dv2 ndx
            if val1 == val2
              then recurse $ ndx + 4
              else pure false
          else if restLen >= 2
            then do
              val1 <- D.getInt16be dv1 ndx
              val2 <- D.getInt16be dv2 ndx
              if val1 == val2
                then recurse $ ndx + 2
                else pure false
            else if restLen > 0
              then do
                val1 <- D.getInt8 dv1 ndx
                val2 <- D.getInt8 dv2 ndx
                if val1 == val2
                  then recurse $ ndx + 1
                  else pure false
              else pure true
      in
      recurse 0
    else pure false

eqEvent :: forall f. Applicative f => MonadEffect f => Event -> Event -> f Boolean
eqEvent ENil ENil = pure true
eqEvent (EBool val1) (EBool val2) = pure $ val1 == val2
eqEvent (EFloat val1) (EFloat fl2) = pure $ val1 == fl2
eqEvent (EDouble val1) (EDouble val2) = pure $ val1 == val2
eqEvent (EInt val1) (EInt val2) = pure $ val1 == val2
eqEvent (EInt64 val1) (EInt64 val2) = pure $ val1 == val2
eqEvent (EUInt val1) (EUInt val2) = pure $ val1 == val2
eqEvent (EUInt64 val1) (EUInt64 val2) = pure $ val1 == val2
eqEvent (EBinaryStart val1) (EBinaryStart val2) = pure $ val1 == val2
eqEvent (EBinary val1) (EBinary val2) = liftEffect $ A.eq val1 val2
eqEvent EBinaryEnd EBinaryEnd = pure true
eqEvent (EExt val1) (EExt val2) = liftEffect $ A.eq val1 val2
eqEvent EExtEnd EExtEnd = pure true
eqEvent (EStringStart val1) (EStringStart val2) = pure $ val1 == val2
eqEvent (EString val1) (EString val2) = pure $ val1 == val2
eqEvent EStringEnd EStringEnd = pure true
eqEvent (EArrayStart val1) (EArrayStart val2) = pure $ val1 == val2
eqEvent EArrayEnd EArrayEnd = pure true
eqEvent (EMapStart val1) (EMapStart val2) = pure $ val1 == val2
eqEvent EMapEnd EMapEnd = pure true
eqEvent _ _ = pure false

eqNestEither :: forall f a. Applicative f => Eq a => MonadEffect f => Either DeserialError (Either a Event) -> Either DeserialError (Either a Event) -> f Boolean
eqNestEither (Left UnusedByte) (Left UnusedByte) = pure true
eqNestEither (Right (Left n1)) (Right (Left n2)) = pure $ n1 == n2
eqNestEither (Right (Right event1)) (Right (Right event2)) = eqEvent event1 event2
eqNestEither _ _ = pure false

data ParentStateExpectation
  = PRoot
  | DataRead UInt UInt ParentStateExpectation
  | MapRead UInt Boolean UInt ParentStateExpectation

data StateExpectation
  = Root
  | Num NumberType ParentStateExpectation
  | Dat DataType SubState ParentStateExpectation
  | Ext ExtSubState ParentStateExpectation
  | Maap MapSubState ParentStateExpectation

test :: Effect Unit
test = do
  decoder ← new utf8
  runSpecAndExitProcess [consoleReporter] do
    describe "Deserilization start tests" do
      let
        parentStateTest parentStateExpectation deserialState =
          let
            fale _ = trace deserialState \ _ -> fail $ "Invalid parent state!"
          in
          if initState == deserialState
            then
              case parentStateExpectation of
              PRoot -> pure unit
              _ -> trace deserialState \ _ -> fail $ "Expected Root as the parent state!"
            else caseDeserialState
              (pure unit)
              (\ _ _ -> fale 1)
              (\ dataType subState parentState -> do
                case dataType of
                  DTArray -> pure unit
                  _ -> trace dataType \ _ -> fail "DSData as a parent state must have DTArray as the DataType!"
                case subState of
                  SSRead len itemsRead ->
                    case parentStateExpectation of
                      DataRead expLen expItemsRead gParentsateExpectation ->
                        let
                          exp = (Tuple expLen expItemsRead)
                          act = (Tuple len itemsRead)
                        in
                        if exp == act
                        then parentStateTest gParentsateExpectation parentState
                        else trace (Tuple exp act) \ _ -> fail "Unexpected SSRead sub state"
                      MapRead _ _ _ _ -> trace parentState \ _ -> fail "Expected DSData (DTArray) as a parent state!"
                      _ -> trace deserialState \ _ -> fail "Error in the test logic for parent state!"
                  _ -> trace subState \ _ -> fail "DSData (DTArray) as a parent state must have SSRead as a substate!"
              )
              (\ _ _ -> fale 1)
              (\ mapSubState parentState ->
                case mapSubState of
                MSRead entryLen readVal entriesRead ->
                  case parentStateExpectation of
                    DataRead _ _ _ -> trace parentState \ _ -> fail "Expected DSMap as a parent state!"
                    MapRead expEntryLen expReadVal expEntriesRead gParentStateExpectation ->
                      let
                        exp = (Tuple expEntryLen $ Tuple expReadVal expEntriesRead)
                        act = (Tuple entryLen $ Tuple readVal entriesRead)
                      in
                      if exp == act
                      then parentStateTest gParentStateExpectation parentState
                      else trace (Tuple exp act) \ _ -> fail "Unexpected MSRead sub state"
                    _ -> trace deserialState \ _ -> fail "Error in the test logic for parent state!"
                _ -> trace mapSubState \ _ -> fail "DSMap as a parent state must have MSRead as a substate!"
              )
              deserialState
        itTest description len leadByte expRes resStateDesc stateExpectation =
          it description do
            -- create one-byte DataView
            dv <- liftEffect $ D.whole <$> AB.empty len
            -- set the byte to MessagePack true
            void <<< liftEffect <<< D.setUint8 dv 0 $ U.fromInt leadByte
            -- deserialize the DataView contents
            Tuple mResult (Tuple deserialState dv') <- deserializeMsgPackStreamT $ Tuple initState dv
            bo <- eqNestEither expRes mResult
            if bo
              then pure unit
              else trace (Tuple expRes mResult) \ _ -> fail "Unexpected result!"
            -- check the end state
            let
              fale _ = trace deserialState \ _ -> fail $ "Resulting deserial state is not " <> resStateDesc <> "!"
            if initState == deserialState
              then
                case stateExpectation of
                Root -> pure unit
                _ -> fale 1
              else caseDeserialState
                (pure unit)
                (\ numberType parentState -> case stateExpectation of
                  Num expNumberType parentStateExpectation ->
                    if expNumberType == numberType
                    then parentStateTest parentStateExpectation parentState
                    else trace (Tuple expNumberType numberType) \ _ -> fail "Unexpected NumberType!"
                  _ -> fale 1
                )
                (\ dataType subState parentState -> case stateExpectation of
                  Dat expDataType expSubState parentStateExpectation ->
                    let
                      exp = Tuple expDataType expSubState
                      act = Tuple dataType subState
                    in
                    if exp == act
                    then parentStateTest parentStateExpectation parentState
                    else trace (Tuple exp act) \ _ -> fail "Unexpected DataType or SubState!"
                  _ -> fale 1
                )
                (\ extSubState parentState -> case stateExpectation of
                  Ext expExtSubState parentStateExpectation ->
                    if expExtSubState == extSubState
                    then parentStateTest parentStateExpectation parentState
                    else trace (Tuple expExtSubState extSubState) \ _ -> fail "Unexpected ExtSubState!"
                  _ -> fale 1
                )
                (\ mapSubState parentState -> case stateExpectation of
                  Maap expMapSubState parentStateExpectation ->
                    if expMapSubState == mapSubState
                    then parentStateTest parentStateExpectation parentState
                    else trace (Tuple expMapSubState mapSubState) \ _ -> fail "Unexpected MapSubState!"
                  _ -> fale 1
                )
                deserialState
            b <- liftEffect $ compareDataViews dv' =<< L.empty
            if b
              then pure unit
              else trace dv' \ _ -> fail "Resulting DataView is not empty!"
      itTest
        "deserializes a nil value"
        1
        nil
        (Right $ Right ENil)
        "DSRoot"
        Root
      itTest
        "deserializes a boolean false"
        1
        fals
        (Right <<< Right $ EBool false)
        "DSRoot"
        Root
      itTest
        "deserializes a boolean true"
        1
        tru
        (Right <<< Right $ EBool true)
        "DSRoot"
        Root
      itTest
        "starts to deserialize binary 8-bit data (lenth is specified in an 8-bit word)"
        1
        bin8
        (Right $ Left 1)
        "DTBinary (SSLength Data8) DSRoot"
        $ Dat DTBinary (SSLength Data8) PRoot
      itTest
        "starts to deserialize binary 8-bit data (lenth is specified in an 16-bit word)"
        1
        bin16
        (Right $ Left 2)
        "DTBinary (SSLength Data16) DSRoot"
        $ Dat DTBinary (SSLength Data16) PRoot
      itTest
        "starts to deserialize binary 8-bit data (lenth is specified in an 32-bit word)"
        1
        bin32
        (Right $ Left 4)
        "DTBinary (SSLength Data32) DSRoot"
        $ Dat DTBinary (SSLength Data32) PRoot
      itTest
        "starts to deserialize ext 8-bit data (lenth is specified in an 8-bit word)"
        1
        ext8
        (Right $ Left 1)
        "DSExt (ESLength Data8) DSRoot"
        $ Ext (ESLength Data8) PRoot
      itTest
        "starts to deserialize ext 8-bit data (lenth is specified in an 16-bit word)"
        1
        ext16
        (Right $ Left 2)
        "DSExt (ESLength Data16) DSRoot"
        $ Ext (ESLength Data16) PRoot
      itTest
        "starts to deserialize ext 8-bit data (lenth is specified in an 32-bit word)"
        1
        ext32
        (Right $ Left 4)
        "DSExt (ESLength Data32) DSRoot"
        $ Ext (ESLength Data32) PRoot
      itTest
        "starts to deserialize 32-bit float"
        1
        float32
        (Right $ Left 4)
        "DTNumber NFloat DSRoot"
        $ Num NFloat PRoot
      itTest
        "starts to deserialize 64-bit float"
        1
        float64
        (Right $ Left 8)
        "DTNumber NDouble DSRoot"
        $ Num NDouble PRoot
      itTest
        "puts the lotion into the basket"
        1
        uint8
        (Right $ Left 1)
        "DTNumber (NUInt Bit8) DSRoot"
        $ Num (NUInt Bit8) PRoot
      itTest
        "starts to deserialize 16-bit unsigned integer"
        1
        uint16
        (Right $ Left 2)
        "DTNumber (NUInt Bit16) DSRoot"
        $ Num (NUInt Bit16) PRoot
      itTest
        "starts to deserialize 32-bit unsigned integer"
        1
        uint32
        (Right $ Left 4)
        "DTNumber (NUInt Bit32) DSRoot"
        $ Num (NUInt Bit32) PRoot
      itTest
        "starts to deserialize 64-bit unsigned integer"
        1
        uint64
        (Right $ Left 8)
        "DTNumber (NUInt Bit64) DSRoot"
        $ Num (NUInt Bit64) PRoot
      itTest
        "starts to deserialize 8-bit integer"
        1
        int8
        (Right $ Left 1)
        "DTNumber (NInt Bit8) DSRoot"
        $ Num (NInt Bit8) PRoot
      itTest
        "starts to deserialize 16-bit integer"
        1
        int16
        (Right $ Left 2)
        "DTNumber (NInt Bit16) DSRoot"
        $ Num (NInt Bit16) PRoot
      itTest
        "starts to deserialize 32-bit integer"
        1
        int32
        (Right $ Left 4)
        "DTNumber (NInt Bit32) DSRoot"
        $ Num (NInt Bit32) PRoot
      itTest
        "starts to deserialize 64-bit integer"
        1
        int64
        (Right $ Left 8)
        "DTNumber (NInt Bit64) DSRoot"
        $ Num (NInt Bit64) PRoot
      itTest
        "starts to deserialize a string (lenth is specified in an 8-bit word)"
        1
        str8
        (Right $ Left 1)
        "(DTString decoder) (SSLength Data8) DSRoot"
        $ Dat (DTString decoder) (SSLength Data8) PRoot
      itTest
        "starts to deserialize a string (lenth is specified in an 16-bit word)"
        1
        str16
        (Right $ Left 2)
        "(DTString decoder) (SSLength Data16) DSRoot"
        $ Dat (DTString decoder) (SSLength Data16) PRoot
      itTest
        "starts to deserialize a string (lenth is specified in an 32-bit word)"
        1
        str32
        (Right $ Left 4)
        "(DTString decoder) (SSLength Data32) DSRoot"
        $ Dat (DTString decoder) (SSLength Data32) PRoot
      itTest
        "starts to deserialize an array (lenth is specified in an 16-bit word)"
        1
        array16
        (Right $ Left 2)
        "DTArray (SSLength Data16) DSRoot"
        $ Dat DTArray (SSLength Data16) PRoot
      itTest
        "starts to deserialize an array (lenth is specified in an 32-bit word)"
        1
        array32
        (Right $ Left 4)
        "DTArray (SSLength Data32) DSRoot"
        $ Dat DTArray (SSLength Data32) PRoot
      itTest
        "starts to deserialize a map (lenth is specified in an 16-bit word)"
        1
        map16
        (Right $ Left 2)
        "(MSLength Data16) DSRoot"
        $ Maap (MSLength Data16) PRoot
      itTest
        "starts to deserialize a map (lenth is specified in an 32-bit word)"
        1
        map32
        (Right $ Left 4)
        "(MSLength Data32) DSRoot"
        $ Maap (MSLength Data32) PRoot
      itTest
        "throws a DeserialError on the never used byte"
        1
        0xc1
        (Left UnusedByte)
        "DSRoot"
        Root
      itTest
        "deserializes a fixint value"
        1
        (posFixint .|. 42)
        (Right <<< Right $ EInt 42)
        "DSRoot"
        Root
      itTest
        "deserializes a negative fixint value"
        1
        0xe0
        (Right <<< Right $ EInt (-32))
        "DSRoot"
        Root
      let len = U.fromInt 1
      itTest
        "starts to deserializes a fixmap"
        1
        (fixmap .|. 1)
        (Right <<< Right $ EMapStart len)
        "MSRead 1 false 0"
        $ Maap (MSRead len false $ U.fromInt 0) PRoot
      itTest
        "starts to deserializes a fixarray"
        1
        (fixarray .|. 1)
        (Right <<< Right $ EArrayStart len)
        "DTArray (MSRead 1 false 0)"
        $ Dat DTArray (SSRead len $ U.fromInt 0) PRoot
      itTest
        "starts to deserializes a fixstr"
        1
        (fixstr .|. 1)
        (Right <<< Right $ EStringStart len)
        "DTString (MSRead 1 false 0)"
        $ Dat (DTString decoder) (SSRead len $ U.fromInt 0) PRoot
      itTest
        "starts to deserializes a fixext1"
        1
        fixext1
        (Right $ Left 1)
        "(ESType 1) DSRoot"
        $ Ext (ESType len) PRoot
      itTest
        "starts to deserializes a fixext2"
        1
        fixext2
        (Right $ Left 1)
        "(ESType 2) DSRoot"
        $ Ext (ESType $ U.fromInt 2) PRoot
      itTest
        "starts to deserializes a fixext4"
        1
        fixext4
        (Right $ Left 1)
        "(ESType 4) DSRoot"
        $ Ext (ESType $ U.fromInt 4) PRoot
      itTest
        "starts to deserializes a fixext8"
        1
        fixext8
        (Right $ Left 1)
        "(ESType 8) DSRoot"
        $ Ext (ESType $ U.fromInt 8) PRoot
      itTest
        "starts to deserializes a fixext16"
        1
        fixext16
        (Right $ Left 1)
        "(ESType 16) DSRoot"
        $ Ext (ESType $ U.fromInt 16) PRoot
