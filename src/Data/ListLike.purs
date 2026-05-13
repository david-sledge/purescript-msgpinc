module Data.ListLike
  ( class ListLike
  , append
  , genericLength
  , empty
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
-- import Data.Array as A
-- import Data.Array.NonEmpty as N
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.Builder (execPutM, putDataView)
import Data.ArrayBuffer.DataView as D
import Data.ArrayBuffer.Typed as T
import Data.ArrayBuffer.Typed (class TypedArray)
import Data.ArrayBuffer.Types (ArrayView, DataView)
import Effect.Class (class MonadEffect, liftEffect)

class ListLike m l where
  genericLength ∷ l → m Int
  append ∷ l → l → m l
  empty ∷ m l

-- instance ListLike m (Array i) where
--   genericLength = A.length

-- instance ListLike m (N.NonEmptyArray i) where
--   genericLength = N.length

instance (MonadEffect m, MonadRec m, TypedArray a t) ⇒ ListLike m (ArrayView a) where
  genericLength = pure <<< T.length
  append av1 av2 =
    let
      dataArrayViewM av = liftEffect (D.part (T.buffer av) (T.byteOffset av) (T.byteLength av)) >>= putDataView
    in
      liftEffect <<< T.whole =<< execPutM do
        dataArrayViewM av1
        dataArrayViewM av2
  empty = liftEffect (T.empty 0)

instance (MonadEffect m, MonadRec m) ⇒ ListLike m DataView where
  genericLength = pure <<< D.byteLength
  append dv1 dv2 =
    D.whole <$> execPutM do
      putDataView dv1
      putDataView dv2
  empty = D.whole <$> liftEffect (AB.empty 0)

