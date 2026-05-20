module Control.MsgPinc.Decoder where

import Prelude (bind, discard, pure, ($), (*>), (<<<))

import Control.MsgPinc.Deserializer
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Nope (class MonadNuhUh, MaybeT, nope)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT, get, modify, put)
import Data.ArrayBuffer.Types (DataView)
import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Tuple (Tuple(Tuple))
import Effect.Class (class MonadEffect, liftEffect)

data DecodeException c e
  = ImplementationError (Either c e)
  | DeserialError DeserialError

class MonadThrow (DecodeException c a) m ⇐ DecodeMsgPackStream c a m where
  decodeMsgPackT
    ∷ c
    → Event
    → m (Either c a)

decodeMsgPackStreamT =
  let
    recurse = do
      Tuple state mAcc ← get
      Tuple result state' ← deserializeMsgPackStreamT state
      put (Tuple state' mAcc)
      either
        (throwError <<< DeserialError)
        ( either pure
            ( \ event → either
                ( \ acc → do
                    mAcc' ← decodeMsgPackT acc event
                    modify (\(Tuple state'' _) → Tuple state'' mAcc') *>
                      either (\_ → recurse) pure mAcc'
                )
                ( \ _ → throwError $ ImplementationError mAcc)
                mAcc
            )
        )
        result
  in
    recurse

-- feedMoreData ∷ ∀ m d c b a s. Bind m ⇒ Source s d c m ⇒ Applicative m ⇒ d → Tuple (Tuple a s) b → m (Tuple (Tuple a s) b)
-- feedMoreData msgPackStr (Tuple (Tuple DeserialState srcState) acc) = do
--   srcState' ← refillSource msgPackStr srcState
--   pure $ Tuple (Tuple DeserialState srcState') acc

-- decodeContT
--   ∷ ∀ m a s c r
--   . Source s String Char m
--   ⇒ Monad m
--   ⇒ Source s String Char (MaybeT (ExceptT (DecodeException c a) (StateT (Tuple (Tuple DeserialState s) (Either c a)) m)))
--   ⇒ DecodeMsgPackStream c a (MaybeT (ExceptT (DecodeException c a) (StateT (Tuple (Tuple DeserialState s) (Either c a)) m)))
--   ⇒ EndMsgPackDecode c a (ExceptT (DecodeException c a) (StateT (Tuple (Tuple DeserialState s) (Either c a)) m))
--   ⇒ String
--   → c
--   → (Tuple (Tuple DeserialState s) (Either c a) → (DecodeException c a) → m r)
--   → (Tuple (Tuple DeserialState s) (Either c a) → (Maybe String → m r) → m r)
--   → (a → m r)
--   → m r
-- decodeContT chunk initAcc onError onEmpty f = do
--   psState ← startState
--   state ← feedMoreData chunk <<< Tuple psState $ Left initAcc
--   let
--     recurse state_ = do
--       Tuple result state' ← runDeserializeT decodeMsgPackStreamT state_
--       either
--         (onError state')
--         ( \mA → maybe
--             ( onEmpty state'
--                 $ maybe
--                     ( do
--                         Tuple result' state'' ← runEndDeserializeT endMsgPackStreamDecodeT state'
--                         either (onError state'') f result'
--                     )
--                     (\chunk' → recurse =<< feedMoreData chunk' state')
--             )
--             (\_ → recurse state')
--             mA
--         )
--         result
--   recurse state

-- decodeT
--   ∷ ∀ m a c
--   . Monad m
--   ⇒ DecodeMsgPackStream c a (MaybeT (ExceptT (DecodeException c a) (StateT (Tuple (Tuple DeserialState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c a)) m)))
--   ⇒ EndMsgPackDecode c a (ExceptT (DecodeException c a) (StateT (Tuple (Tuple DeserialState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c a)) m))
--   ⇒ String
--   → c
--   → m (Either (DecodeException c a) a)
-- decodeT msgPackStr initAcc = (decodeContT ∷ String → c → (Tuple (Tuple DeserialState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c a) → (DecodeException c a) → m (Either (DecodeException c a) a)) → (Tuple (Tuple DeserialState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c a) → (Maybe String → m (Either (DecodeException c a) a)) → m (Either (DecodeException c a) a)) → (a → m (Either (DecodeException c a) a)) → m (Either (DecodeException c a) a))
--   msgPackStr
--   initAcc
--   (\_ e → pure $ Left e)
--   (\_ f → f Nothing)
--   (pure <<< Right)
