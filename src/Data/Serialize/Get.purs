module Data.Serialize.Get
  ( GetT
  , UnGet
  , getFloat32beT
  , getFloat32leT
  , getFloat64beT
  , getFloat64leT
  , getInt16beT
  , getInt16leT
  , getInt32beT
  , getInt32leT
  , getInt64beT
  , getInt64leT
  , getInt8T
  , getUint16beT
  , getUint16leT
  , getUint32beT
  , getUint32leT
  , getUint64beT
  , getUint64leT
  , getUint8T
  , processBytes
  , runGetT
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT, get, put, runStateT)
import Data.ArrayBuffer.DataView as D
import Data.ArrayBuffer.Types (DataView)
import Data.Int64 as I
import Data.Int64 (Int64)
import Data.Either (Either)
import Data.Float32 (Float32)
import Data.ListLike as L
import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)
import Data.Tuple (Tuple)
import Data.UInt (UInt)
import Data.UInt64 as U
import Data.UInt64 (UInt64)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

-- | GetT is the composition of ExceptT and StateT in the given monad stack order.
type GetT e s m = ExceptT e (StateT s m)

type UnGet e a s = Tuple (Either e a) s

runGetT ∷ ∀ s e m a. GetT e s m a → s → m (UnGet e a s)
runGetT = runStateT <<< runExceptT

processBytes :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => Int -> m DataView
processBytes n = do
  dv ← get
  let len = D.byteLength dv
  if len == n then do
    put =<< L.empty
    pure dv
  else if len > n then do
    put =<< liftEffect (D.part (D.buffer dv) (n + D.byteOffset dv) (len - n))
    liftEffect (D.part (D.buffer dv) (D.byteOffset dv) n)
  else do
    throwError $ n - len

bridge ∷ ∀ m a. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => Int -> (DataView -> Int -> Effect (Maybe a)) -> m a
bridge n getNum = processBytes n >>= map (unsafePartial fromJust) <<< liftEffect <<< flip getNum 0

getInt8T :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m Int
getInt8T = bridge 1 D.getInt8

-- | Read a Int16 in big endian format
getInt16beT :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m Int
getInt16beT = bridge 2 D.getInt16be

-- | Read a Int16 in little endian format
getInt16leT :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m Int
getInt16leT = bridge 2 D.getInt16le

-- | Read a Int32 in big endian format
getInt32beT :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m Int
getInt32beT = bridge 4 D.getInt32be

-- | Read a Int32 in little endian format
getInt32leT :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m Int
getInt32leT = bridge 4 D.getInt32le

-- | Read a Int64 in big endian format
getInt64beT :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m Int64
getInt64beT = do
  dv <- processBytes 8
  highBits ← unsafePartial fromJust <$> liftEffect (D.getInt32be dv 0)
  I.fromLowHighBits highBits <<< unsafePartial fromJust <$> liftEffect (D.getInt32be dv 4)

-- | Read a Int64 in little endian format
getInt64leT :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m Int64
getInt64leT = do
  dv <- processBytes 8
  highBits ← unsafePartial fromJust <$> liftEffect (D.getInt32le dv 4)
  I.fromLowHighBits highBits <<< unsafePartial fromJust <$> liftEffect (D.getInt32le dv 0)

-- ------------------------------------------------------------------------

-- | Read a Uint8 from the monad state
getUint8T :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m UInt
getUint8T = bridge 1 D.getUint8

-- | Read a Uint16 in big endian format
getUint16beT :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m UInt
getUint16beT = bridge 2 D.getUint16be

-- | Read a Uint16 in little endian format
getUint16leT :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m UInt
getUint16leT = bridge 2 D.getUint16le

-- | Read a Uint32 in big endian format
getUint32beT :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m UInt
getUint32beT = bridge 4 D.getUint32be

-- | Read a Uint32 in little endian format
getUint32leT :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m UInt
getUint32leT = bridge 4 D.getUint32le

-- | Read a UInt64 in big endian format
getUint64beT :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m UInt64
getUint64beT = do
  dv <- processBytes 8
  highBits ← unsafePartial fromJust <$> liftEffect (D.getInt32be dv 0)
  U.fromLowHighBits highBits <<< unsafePartial fromJust <$> liftEffect (D.getInt32be dv 4)

-- | Read a UInt64 in little endian format
getUint64leT :: forall m. MonadState DataView m => MonadEffect m => MonadRec m => MonadThrow Int m => m UInt64
getUint64leT = do
  dv <- processBytes 8
  highBits ← unsafePartial fromJust <$> liftEffect (D.getInt32le dv 4)
  U.fromLowHighBits highBits <<< unsafePartial fromJust <$> liftEffect (D.getInt32le dv 0)

getFloat32beT :: forall m226. MonadState DataView m226 => MonadEffect m226 => MonadRec m226 => MonadThrow Int m226 => m226 Float32
getFloat32beT = bridge 4 D.getFloat32be

getFloat32leT :: forall m226. MonadState DataView m226 => MonadEffect m226 => MonadRec m226 => MonadThrow Int m226 => m226 Float32
getFloat32leT = bridge 4 D.getFloat32le

getFloat64beT :: forall m226. MonadState DataView m226 => MonadEffect m226 => MonadRec m226 => MonadThrow Int m226 => m226 Number
getFloat64beT = bridge 8 D.getFloat64be

getFloat64leT :: forall m226. MonadState DataView m226 => MonadEffect m226 => MonadRec m226 => MonadThrow Int m226 => m226 Number
getFloat64leT = bridge 8 D.getFloat64le
