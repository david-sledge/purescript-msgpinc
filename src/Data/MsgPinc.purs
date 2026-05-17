module Data.MsgPinc where

import Prelude

import Control.MsgPinc.Decoder
import Control.MsgPinc.Deserializer
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (snoc)
import Data.Either (Either(Left, Right))
import Data.Identity (Identity(Identity))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Tuple (Tuple(Tuple))
import Foreign.Object (fromFoldable)

-- data JAccumulator
--   = RootAcc
--   | StringAcc JAccumulator String
--   | ArrayAcc JAccumulator (Array MsgPack)
--   | ObjectAcc JAccumulator (Array (Tuple String MsgPack)) (Maybe String)

-- err ∷ ∀ m t c a. MonadThrow (DecodeException c a) m ⇒ m t
-- err = throwError $ DecodeError "Programmatic error: notify the maintainers with the string that was being deserialized."

-- instance MonadThrow (DecodeException JAccumulator MsgPack) m ⇒ EndMsgPackDecode JAccumulator MsgPack m where
--   endMsgPackDecodeT acc num =
--     let
--       f = pure <<< Left
--       jVal = fromNumber num
--     in
--       case acc of
--         RootAcc → pure $ Right jVal
--         ArrayAcc acc' values → f <<< ArrayAcc acc' $ snoc values jVal
--         ObjectAcc acc' props mName →
--           maybe
--             err
--             (\name → f $ ObjectAcc acc' (snoc props $ Tuple name jVal) Nothing)
--             mName
--         _ → err

-- instance MonadThrow (DecodeException JAccumulator MsgPack) m ⇒ DecodeMsgPackStream JAccumulator MsgPack m where
--   decodeMsgPackT acc event =
--     let
--       f = pure <<< Left
--       processValue mStr acc' jVal =
--         case acc' of
--           RootAcc → pure $ Right jVal
--           ArrayAcc acc'' values → f <<< ArrayAcc acc'' $ snoc values jVal
--           ObjectAcc acc'' props mName →
--             maybe
--               (maybe err (\str → f <<< ObjectAcc acc'' props $ Just str) mStr)
--               (\name → f $ ObjectAcc acc'' (snoc props $ Tuple name jVal) Nothing)
--               mName
--           _ → err
--     in
--       case event of
--         ENumber num → processValue Nothing acc $ fromNumber num
--         ENull → processValue Nothing acc msgPackNull
--         EBool bool → processValue Nothing acc $ fromBoolean bool
--         EStringStart →
--           case acc of
--             StringAcc _ _ → err
--             _ → f $ StringAcc acc ""
--         EString str →
--           case acc of
--             StringAcc acc' str' → f <<< StringAcc acc' $ str' <> str
--             _ → err
--         EStringEnd →
--           case acc of
--             StringAcc acc' str → processValue (Just str) acc' $ fromString str
--             _ → err
--         EArrayStart → f $ ArrayAcc acc []
--         EArrayEnd →
--           case acc of
--             ArrayAcc acc' elems → processValue Nothing acc' $ fromArray elems
--             _ → err
--         EObjectStart → f $ ObjectAcc acc [] Nothing
--         EObjectEnd →
--           case acc of
--             ObjectAcc acc' props mName →
--               case mName of
--                 Nothing → processValue Nothing acc' <<< fromObject $ fromFoldable props
--                 Just _ → err
--             _ → err

-- deserializeMsgPack ∷ String → Either (DecodeException JAccumulator MsgPack) MsgPack
-- deserializeMsgPack msgPackStr = case decodeT msgPackStr RootAcc of
--   Identity r → r
