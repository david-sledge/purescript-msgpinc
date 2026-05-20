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
    it "starts to deserialize binary data" do
      -- create one-byte DataView
      dv <- liftEffect $ D.whole <$> AB.empty 1
      -- set the byte to MessagePack true
      void <<< liftEffect <<< D.setUint8 dv 0 $ U.fromInt bin8
      -- deserialize the DataView contents
      Tuple mResult (Tuple deserialState dv') <- deserializeMsgPackStreamT $ Tuple initState dv
      case mResult of
        Right (Left 1) -> pure unit
        _ -> trace mResult \ _ -> fail "Did not start to deserialize binary data!"
      let
        fale :: forall a b m. MonadThrow Error m => a -> b -> m Unit
        fale _ _ = trace deserialState \ _ -> fail "Resulting deserial state is not DTBinary (SSLength Data8) DSRoot!"
      -- check the end state
      caseDeserialState
        (fale 1 2)
        fale
        (\ dataType subState parentState -> do
          case dataType of
            DTBinary -> pure unit
            _ -> trace dataType \ _ -> fail "Data type is not DTBinary!"
          case subState of
            SSLength dataSize ->
              case dataSize of
              Data8 -> pure unit
              _ -> trace dataSize \ _ -> fail "Data size is not Data8!"
            _ -> trace dataType \ _ -> fail "Sub-state is not SSLength Data8!"
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
        )
        fale
        fale
        deserialState
      b <- liftEffect $ compareDataViews dv' =<< L.empty
      if b
        then pure unit
        else trace dv' \ _ -> fail "Resulting DataView is not empty!"
