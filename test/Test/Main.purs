module Test.Main
  ( main
  ) where

import Prelude

import Control.Monad.Reader
import Control.Monad.State
import Data.Identity
import Data.List
import Effect
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as D
import Data.ArrayBuffer.Types

list onNil onUncons l = case l of
  Nil -> onNil
  Cons head tail -> onUncons head tail

runIdentity (Identity a) = a

main ∷ Effect Unit
main = do
  fst <- D.whole <$> AB.empty 8
  snd <- D.whole <$> AB.empty 4
  thd <- D.whole <$> AB.empty 2
  fth <- D.whole <$> AB.empty 1
  done <- D.whole <$> AB.empty 0
  let
    source = Cons fst <<< Cons snd <<< Cons thd $ Cons fth Nil
    feeder =
      get >>= list
        (pure done)
        ( \head tail -> do
            put tail
            pure head
        )
  -- void <<< pure <<< runIdentity $ runStateT feeder source
  void <<< pure <<< runIdentity $
    ( \m -> runStateT m source
    ) feeder
  pure unit
