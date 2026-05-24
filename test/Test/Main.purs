module Test.Main
  ( main
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader
import Control.Monad.State
import Control.MsgPinc.Deserializer
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as D
import Data.ArrayBuffer.Types
import Data.ArrayBuffer.Typed as A
import Data.Either
import Data.Identity
import Data.List
import Data.ListLike as L
import Data.MsgPinc.Spec
import Data.Tuple
import Data.UInt as U
import Debug (trace)
import Effect
import Effect.Class
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Test.Spec
import Test.Spec.Assertions
import Effect.Aff (launchAff_, delay)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main ∷ Effect Unit
main = do
  test

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

-- eqEvent ENil ENil = pure true
-- eqEvent (EBool val1) (EBool val2) = pure $ val1 == val2
-- eqEvent (EFloat val1) (EFloat fl2) = pure $ val1 == fl2
-- eqEvent (EDouble val1) (EDouble val2) = pure $ val1 == val2
-- eqEvent (EInt val1) (EInt val2) = pure $ val1 == val2
-- eqEvent (EInt64 val1) (EInt64 val2) = pure $ val1 == val2
-- eqEvent (EUInt val1) (EUInt val2) = pure $ val1 == val2
-- eqEvent (EUInt64 val1) (EUInt64 val2) = pure $ val1 == val2
-- eqEvent (EBinaryStart val1) (EBinaryStart val2) = pure $ val1 == val2
-- eqEvent (EBinary val1) (EBinary val2) = A.eq val1 val2
-- eqEvent EBinaryEnd EBinaryEnd = pure true
-- eqEvent (EExt val1) (EExt val2) = A.eq val1 val2
-- eqEvent EExtEnd EExtEnd = pure true
-- eqEvent (EStringStart val1) (EStringStart val2) = pure $ val1 == val2
-- eqEvent (EString val1) (EString val2) = pure $ val1 == val2
-- eqEvent EStringEnd EStringEnd = pure true
-- eqEvent (EArrayStart val1) (EArrayStart val2) = pure $ val1 == val2
-- eqEvent EArrayEnd EArrayEnd = pure true
-- eqEvent (EMapStart val1) (EMapStart val2) = pure $ val1 == val2
-- eqEvent EMapEnd EMapEnd = pure true
-- eqEvent _ _ = pure false

test = runSpecAndExitProcess [consoleReporter] do
  describe "Deserilization tests" do
    it "deserializes a nil value" do
      -- create one-byte DataView
      dv <- liftEffect $ D.whole <$> AB.empty 1
      -- set the byte to MessagePack nil
      void <<< liftEffect <<< D.setUint8 dv 0 $ U.fromInt nil
      -- deserialize the DataView contents
      Tuple mResult (Tuple deserialState dv') <- deserializeMsgPackStreamT $ Tuple initState dv
      case mResult of
        Right (Right ENil) -> pure unit
        _ -> trace mResult \ _ -> fail "Did not deserialize an ENil event!"
      let
        fale :: forall a b m. MonadThrow Error m => a -> b -> m Unit
        fale _ _ = trace deserialState \ _ -> fail "Resulting deserial state is not DSRoot!"
      -- check the end state
      caseDeserialState
        (pure unit)
        fale
        (\ _ -> fale)
        fale
        fale
        deserialState
      b <- liftEffect $ compareDataViews dv' =<< L.empty
      if b
        then pure unit
        else trace dv' \ _ -> fail "Resulting DataView is not empty!"
    it "deserializes a boolean false" do
      -- create one-byte DataView
      dv <- liftEffect $ D.whole <$> AB.empty 1
      -- set the byte to MessagePack false
      void <<< liftEffect <<< D.setUint8 dv 0 $ U.fromInt fals
      -- deserialize the DataView contents
      Tuple mResult (Tuple deserialState dv') <- deserializeMsgPackStreamT $ Tuple initState dv
      case mResult of
        Right (Right (EBool false)) -> pure unit
        _ -> trace mResult \ _ -> fail "Did not deserialize an EBool false event!"
      let
        fale :: forall a b m. MonadThrow Error m => a -> b -> m Unit
        fale _ _ = trace deserialState \ _ -> fail "Resulting deserial state is not DSRoot!"
      -- check the end state
      caseDeserialState
        (pure unit)
        fale
        (\ _ -> fale)
        fale
        fale
        deserialState
      b <- liftEffect $ compareDataViews dv' =<< L.empty
      if b
        then pure unit
        else trace dv' \ _ -> fail "Resulting DataView is not empty!"
    it "deserializes a boolean true" do
      -- create one-byte DataView
      dv <- liftEffect $ D.whole <$> AB.empty 1
      -- set the byte to MessagePack true
      void <<< liftEffect <<< D.setUint8 dv 0 $ U.fromInt tru
      -- deserialize the DataView contents
      Tuple mResult (Tuple deserialState dv') <- deserializeMsgPackStreamT $ Tuple initState dv
      case mResult of
        Right (Right (EBool true)) -> pure unit
        _ -> trace mResult \ _ -> fail "Did not deserialize an EBool true event!"
      let
        fale :: forall a b m. MonadThrow Error m => a -> b -> m Unit
        fale _ _ = trace deserialState \ _ -> fail "Resulting deserial state is not DSRoot!"
      -- check the end state
      caseDeserialState
        (pure unit)
        fale
        (\ _ -> fale)
        fale
        fale
        deserialState
      b <- liftEffect $ compareDataViews dv' =<< L.empty
      if b
        then pure unit
        else trace dv' \ _ -> fail "Resulting DataView is not empty!"
    let
      itTest description len leadByte askByteLen resStateDesc handler =
        it description do
          -- create one-byte DataView
          dv <- liftEffect $ D.whole <$> AB.empty len
          -- set the byte to MessagePack true
          void <<< liftEffect <<< D.setUint8 dv 0 $ U.fromInt leadByte
          -- deserialize the DataView contents
          Tuple mResult (Tuple deserialState dv') <- deserializeMsgPackStreamT $ Tuple initState dv
          case mResult of
            Right (Left askByteLen) -> pure unit
            _ -> trace mResult \ _ -> fail $ "Did not ask for " <> show askByteLen <> " more byte(s) of data!"
          let
            one = 1
            fale :: forall a b m. MonadThrow Error m => a -> b -> m Unit
            fale _ _ = trace deserialState \ _ -> fail $ "Resulting deserial state is not " <> resStateDesc <> "!"
          -- check the end state
          caseDeserialState
            -- trick trace from executing unless it needs to
            (case handler of
              Root -> pure unit
              _ -> fale 1 2
            )
            (case handler of
              Num f -> f
              _ -> fale
            )
            (case handler of
              Dat f -> f
              _ -> \ _ -> fale
            )
            (case handler of
              Ext f -> f
              _ -> fale
            )
            (case handler of
              Maap f -> f
              _ -> fale
            )
            deserialState
          b <- liftEffect $ compareDataViews dv' =<< L.empty
          if b
            then pure unit
            else trace dv' \ _ -> fail "Resulting DataView is not empty!"
    itTest
      "starts to deserialize binary 8-bit data (lenth is specified in an 8-bit word)"
      1
      bin8
      1
      "DTBinary (SSLength Data8) DSRoot"
      $ Dat \ dataType subState parentState -> do
          if dataType == DTBinary && subState == SSLength Data8
            then pure unit
            else fail "Resulting deserial state is not DTBinary (SSLength Data8) DSRoot!"
          let
            parentFail :: forall a b m. MonadThrow Error m => a -> b -> m Unit
            parentFail _ _ = trace parentState \ _ -> fail "parent state is not DSRoot!"
          caseDeserialState
            (pure unit)
            parentFail
            (\ _ -> parentFail)
            parentFail
            parentFail
            parentState
    itTest
      "starts to deserialize binary 8-bit data (lenth is specified in an 16-bit word)"
      1
      bin16
      2
      "DTBinary (SSLength Data16) DSRoot"
      $ Dat \ dataType subState parentState -> do
          if dataType == DTBinary && subState == SSLength Data16
            then pure unit
            else fail "Resulting deserial state is not DTBinary (SSLength Data16) DSRoot!"
          let
            parentFail :: forall a b m. MonadThrow Error m => a -> b -> m Unit
            parentFail _ _ = trace parentState \ _ -> fail "parent state is not DSRoot!"
          caseDeserialState
            (pure unit)
            parentFail
            (\ _ -> parentFail)
            parentFail
            parentFail
            parentState
    itTest
      "starts to deserialize binary 8-bit data (lenth is specified in an 32-bit word)"
      1
      bin32
      4
      "DTBinary (SSLength Data32) DSRoot"
      $ Dat \ dataType subState parentState -> do
          if dataType == DTBinary && subState == SSLength Data32
            then pure unit
            else fail "Resulting deserial state is not DTBinary (SSLength Data32) DSRoot!"
          let
            parentFail :: forall a b m. MonadThrow Error m => a -> b -> m Unit
            parentFail _ _ = trace parentState \ _ -> fail "parent state is not DSRoot!"
          caseDeserialState
            (pure unit)
            parentFail
            (\ _ -> parentFail)
            parentFail
            parentFail
            parentState

data StateHandler m
  = Root
  | Num (NumberType -> DeserialState -> m Unit)
  | Dat (DataType -> SubState -> DeserialState -> m Unit)
  | Ext (ExtSubState -> DeserialState -> m Unit)
  | Maap (MapSubState -> DeserialState -> m Unit)
