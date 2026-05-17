module Test.Main
  ( main
  )
  where

import Prelude

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
      liftEffect <<< unlessM (compareDataViews dv' =<< L.empty) $
        trace dv' \ _ -> fail "Resulting DataView is not empty!"
