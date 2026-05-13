module Control.MsgPinc.Decoder where

import Prelude

import Control.MsgPinc.Deserializer
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Nope (class MonadNuhUh, MaybeT, nope)
import Control.Monad.State (class MonadState, StateT, get, modify, put)
import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Tuple (Tuple(Tuple))

-- data DecodeException c e
--   = ImplementationError (Either c e)
--   | DeserializeError DeserializeException
--   | DecodeError String

-- class MonadThrow (DecodeException c a) m ⇐ EndMsgPackDecode c a m where
--   endMsgPackDecodeT
--     ∷ c
--     → Number
--     → m (Either c a)

-- endMsgPackStreamDecodeT
--   ∷ ∀ s a c m
--   . MonadState (Tuple (Tuple DeserializeState s) (Either c a)) m
--   ⇒ MonadThrow (DecodeException c a) m
--   ⇒ EndMsgPackDecode c a m
--   ⇒ m a
-- endMsgPackStreamDecodeT =
--   let
--     recurse = do
--       Tuple state mAcc ← get
--       Tuple result state' ← endMsgPackStreamDeserializeT state
--       put (Tuple state' mAcc)
--       either (throwError <<< DeserializeError)
--         ( maybe
--             -- end of JSON string
--             ( either
--                 -- should not be an accumulator at this point
--                 (\_ → throwError $ ImplementationError mAcc)
--                 pure
--                 mAcc
--             )
--             ( \num → either
--                 ( \acc → do
--                     mAcc' ← endMsgPackDecodeT acc num
--                     modify (\(Tuple state'' _) → Tuple state'' mAcc') *> recurse
--                 )
--                 -- should not have a final decoded value yet
--                 (\_ → throwError $ ImplementationError mAcc)
--                 mAcc
--             )
--         )
--         result
--   in
--     recurse

-- class EndMsgPackDecode c a m ⇐ DecodeMsgPackStream c a m where
--   decodeMsgPackT
--     ∷ c
--     → Event
--     → m (Either c a)

-- decodeMsgPackStreamT
--   ∷ ∀ m a s c
--   . MonadState (Tuple (Tuple DeserializeState s) (Either c a)) m
--   ⇒ Source s String Char m
--   ⇒ MonadThrow (DecodeException c a) m
--   ⇒ MonadNuhUh m
--   ⇒ DecodeMsgPackStream c a m
--   ⇒ m a
-- decodeMsgPackStreamT =
--   let
--     recurse = do
--       Tuple state mAcc ← get
--       Tuple result state' ← deserializeMsgPackStreamT state
--       put (Tuple state' mAcc)
--       either
--         (throwError <<< DeserializeError)
--         ( maybe nope
--             ( \event → either
--                 ( \acc → do
--                     mAcc' ← decodeMsgPackT acc event
--                     modify (\(Tuple state'' _) → Tuple state'' mAcc') *>
--                       either (\_ → recurse) pure mAcc'
--                 )
--                 (\_ → throwError $ ImplementationError mAcc)
--                 mAcc
--             )
--         )
--         result
--   in
--     recurse

-- feedMoreData ∷ ∀ m d c b a s. Bind m ⇒ Source s d c m ⇒ Applicative m ⇒ d → Tuple (Tuple a s) b → m (Tuple (Tuple a s) b)
-- feedMoreData msgPackStr (Tuple (Tuple deserializeState srcState) acc) = do
--   srcState' ← refillSource msgPackStr srcState
--   pure $ Tuple (Tuple deserializeState srcState') acc

-- decodeContT
--   ∷ ∀ m a s c r
--   . Source s String Char m
--   ⇒ Monad m
--   ⇒ Source s String Char (MaybeT (ExceptT (DecodeException c a) (StateT (Tuple (Tuple DeserializeState s) (Either c a)) m)))
--   ⇒ DecodeMsgPackStream c a (MaybeT (ExceptT (DecodeException c a) (StateT (Tuple (Tuple DeserializeState s) (Either c a)) m)))
--   ⇒ EndMsgPackDecode c a (ExceptT (DecodeException c a) (StateT (Tuple (Tuple DeserializeState s) (Either c a)) m))
--   ⇒ String
--   → c
--   → (Tuple (Tuple DeserializeState s) (Either c a) → (DecodeException c a) → m r)
--   → (Tuple (Tuple DeserializeState s) (Either c a) → (Maybe String → m r) → m r)
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
--   ⇒ DecodeMsgPackStream c a (MaybeT (ExceptT (DecodeException c a) (StateT (Tuple (Tuple DeserializeState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c a)) m)))
--   ⇒ EndMsgPackDecode c a (ExceptT (DecodeException c a) (StateT (Tuple (Tuple DeserializeState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c a)) m))
--   ⇒ String
--   → c
--   → m (Either (DecodeException c a) a)
-- decodeT msgPackStr initAcc = (decodeContT ∷ String → c → (Tuple (Tuple DeserializeState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c a) → (DecodeException c a) → m (Either (DecodeException c a) a)) → (Tuple (Tuple DeserializeState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c a) → (Maybe String → m (Either (DecodeException c a) a)) → m (Either (DecodeException c a) a)) → (a → m (Either (DecodeException c a) a)) → m (Either (DecodeException c a) a))
--   msgPackStr
--   initAcc
--   (\_ e → pure $ Left e)
--   (\_ f → f Nothing)
--   (pure <<< Right)
