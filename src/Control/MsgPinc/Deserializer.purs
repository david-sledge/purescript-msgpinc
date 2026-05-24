module Control.MsgPinc.Deserializer
  ( BitSize(..)
  , DataSize(..)
  , DataType(..)
  , DeserialError(..)
  , DeserialMsgPackT
  , DeserialState
  , Event(..)
  , ExtSubState(..)
  , MapSubState(..)
  , NumberType(..)
  , SubState(..)
  , UnDeserialMsgPack
  , caseDeserialState
  , deserializeMsgPackStreamT
  , initState
  )
  where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (get, modify, put)
import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.Typed as A
import Data.ArrayBuffer.Types (DataView, Uint8Array)
import Data.ArrayBuffer.DataView as D
import Data.Either (Either, either)
import Data.Float32 (Float32)
import Data.Functor (voidRight)
import Data.Generic.Rep (class Generic)
import Data.Int.Bits (complement, (.&.))
import Data.Int64 (Int64)
import Data.ListLike as L
import Data.MsgPinc.Spec (fixarray, fixarrayMask, fixmap, fixmapMask, fixstr, fixstrMask, negFixint, negFixintMask, posFixint, posFixintMask)
import Data.Serialize.Get (GetT, UnGet, getFloat32beT, getFloat64beT, getInt16beT, getInt32beT, getInt64beT, getInt8T, getUint16beT, getUint32beT, getUint64beT, getUint8T, processBytes, runGetT)
import Data.Tuple (Tuple(Tuple), snd)
import Data.UInt64 (UInt64)
import Data.UInt as U
import Data.UInt (UInt)
import Effect.Class (class MonadEffect, liftEffect)
import Web.Encoding.TextDecoder (TextDecoder, new, decodeWithOptions)
import Web.Encoding.UtfLabel (utf8)

data BitSize
  = Bit8
  | Bit16
  | Bit32
  | Bit64

derive instance eqBitSize ∷ Eq BitSize
derive instance ordBitSize ∷ Ord BitSize
derive instance genericBitSize ∷ Generic BitSize _

data NumberType
  = NUInt BitSize
  | NInt BitSize
  | NFloat
  | NDouble

derive instance eqNumberType ∷ Eq NumberType
derive instance ordNumberType ∷ Ord NumberType
derive instance genericNumberType ∷ Generic NumberType _

data DataSize
  = Data8
  | Data16
  | Data32

derive instance eqDataSize ∷ Eq DataSize
derive instance ordDataSize ∷ Ord DataSize
derive instance genericDataSize ∷ Generic DataSize _

data SubState
  = SSLength DataSize
  | SSRead UInt UInt

derive instance eqSubState ∷ Eq SubState
derive instance ordSubState ∷ Ord SubState
derive instance genericSubState ∷ Generic SubState _

data DataType
  = DTString TextDecoder
  | DTBinary
  | DTArray

instance eqDataType :: Eq DataType where
  eq (DTString _) (DTString _) = true
  eq DTBinary DTBinary = true
  eq DTArray DTArray = true
  eq _ _ = false

data ExtSubState
  = ESLength DataSize
  | ESType UInt
  | ESRead UInt UInt

derive instance eqExtSubState ∷ Eq ExtSubState
derive instance ordExtSubState ∷ Ord ExtSubState
derive instance genericExtSubState ∷ Generic ExtSubState _

data MapSubState
  = MSLength DataSize
  | MSRead UInt Boolean UInt

derive instance eqMapSubState ∷ Eq MapSubState
derive instance ordMapSubState ∷ Ord MapSubState
derive instance genericMapSubState ∷ Generic MapSubState _

-- The only valid options for parent DeserialState values is DSRoot and
-- DSData (DataType = Array) or DSMap.
data DeserialState
  = DSRoot
  | DSNumber NumberType DeserialState
  | DSData DataType SubState DeserialState
  | DSExt ExtSubState DeserialState
  | DSMap MapSubState DeserialState

derive instance eqDeserialState ∷ Eq DeserialState

caseDeserialState :: forall s. s -> (NumberType -> DeserialState -> s) -> (DataType -> SubState -> DeserialState -> s) -> (ExtSubState -> DeserialState -> s) -> (MapSubState -> DeserialState -> s) -> DeserialState -> s
caseDeserialState root numbr dat ext maap desState =
  case desState of
  DSRoot -> root
  DSNumber numberType parentState -> numbr numberType parentState
  DSData dataType subState parentState -> dat dataType subState parentState
  DSExt extSubState parentState -> ext extSubState parentState
  DSMap mapSubState parentState -> maap mapSubState parentState

initState :: DeserialState
initState = DSRoot

data DeserialError
  = UnusedByte
  | FlogTheDeveloper DeserialState

data Event
  = ENil
  | EBool Boolean
  | EFloat Float32
  | EDouble Number
  | EInt Int
  | EInt64 Int64
  | EUInt UInt
  | EUInt64 UInt64
  | EBinaryStart UInt
  | EBinary Uint8Array
  | EBinaryEnd
  | EExtStart UInt Int
  | EExt Uint8Array
  | EExtEnd
  | EStringStart UInt
  | EString String
  | EStringEnd
  | EArrayStart UInt
  | EArrayEnd
  | EMapStart UInt
  | EMapEnd

-- derive instance eqEvent ∷ Eq Event
-- derive instance ordEvent ∷ Ord Event
-- derive instance genericEvent ∷ Generic Event _

-- instance showEvent ∷ Show Event where
--   show = genericShow

type DeserialMsgPackT m = ExceptT Int (GetT DeserialError (Tuple DeserialState DataView) m)

type UnDeserialMsgPack = UnGet DeserialError (Either Int Event) (Tuple DeserialState DataView)

deserializeMsgPackStreamT :: forall m. MonadEffect m => MonadRec m => Tuple DeserialState DataView -> m UnDeserialMsgPack
deserializeMsgPackStreamT =
  (runGetT <<< runExceptT)
    let
      getDataState = snd <$> get
      putDeserialState deserialState = modify (\(Tuple _ dataState) → Tuple deserialState dataState) # void
      throwDeserialError = lift <<< throwError
      throwFlogger = throwDeserialError <<< FlogTheDeveloper
      maxInt = 2147483647

      dataToArray :: DataView -> DeserialMsgPackT m Uint8Array
      dataToArray dv = liftEffect $ A.part (D.buffer dv) (D.byteOffset dv) (D.byteLength dv)

      stateTransitionFromEndValue deserialState =
        case deserialState of
          DSRoot → putDeserialState deserialState
          DSMap mapSubState parentState →
            case mapSubState of
              MSLength _ → void $ throwFlogger deserialState
              MSRead len readVal read →
                putDeserialState $ DSMap
                  ( if readVal then MSRead len false $ read + (U.fromInt 1)
                    else MSRead len true read
                  )
                  parentState
          DSData dataType subState parentState →
            case dataType of
              DTArray →
                case subState of
                  SSLength _ → void $ throwFlogger deserialState
                  SSRead len read → putDeserialState $ DSData dataType (SSRead len $ read + (U.fromInt 1)) parentState
              _ → void $ throwFlogger deserialState
          _ → void $ throwFlogger deserialState

      -- body of work
      deserialize = do
        Tuple deserialState d ← get
        let
          next
            :: forall r
             . GetT Int DataView (DeserialMsgPackT m) r
            -> DeserialMsgPackT m r
          next getter = do
            Tuple mA d' ← runGetT getter d
            either
              throwError
              (\d'' -> modify (\(Tuple desSt _) → Tuple desSt d') *> pure d'')
              mA
          strDeserSt subState = do
            decoder ← liftEffect $ new utf8
            putDeserialState (DSData (DTString decoder) subState deserialState)
          valueStart = do
            uint8 <- next getUint8T
            case U.toInt uint8 of
              -- nil
              0xc0 → ENil <$ stateTransitionFromEndValue deserialState
              -- false
              0xc2 → EBool false <$ stateTransitionFromEndValue deserialState
              -- true
              0xc3 → EBool true <$ stateTransitionFromEndValue deserialState
              -- bin8
              0xc4 → putDeserialState (DSData DTBinary (SSLength Data8) deserialState) *> deserialize
              -- bin16
              0xc5 → putDeserialState (DSData DTBinary (SSLength Data16) deserialState) *> deserialize
              -- bin16
              0xc6 → putDeserialState (DSData DTBinary (SSLength Data32) deserialState) *> deserialize
              -- ext8
              0xc7 → putDeserialState (DSExt (ESLength Data8) deserialState) *> deserialize
              -- ext16
              0xc8 → putDeserialState (DSExt (ESLength Data16) deserialState) *> deserialize
              -- ext32
              0xc9 → putDeserialState (DSExt (ESLength Data32) deserialState) *> deserialize
              -- float32
              0xca → putDeserialState (DSNumber NFloat deserialState) *> deserialize
              -- float64
              0xcb → putDeserialState (DSNumber NDouble deserialState) *> deserialize
              -- uint8
              0xcc → putDeserialState (DSNumber (NUInt Bit8) deserialState) *> deserialize
              -- uint16
              0xcd → putDeserialState (DSNumber (NUInt Bit16) deserialState) *> deserialize
              -- uint32
              0xce → putDeserialState (DSNumber (NUInt Bit32) deserialState) *> deserialize
              -- uint64
              0xcf → putDeserialState (DSNumber (NUInt Bit64) deserialState) *> deserialize
              -- int8
              0xd0 → putDeserialState (DSNumber (NInt Bit8) deserialState) *> deserialize
              -- int16
              0xd1 → putDeserialState (DSNumber (NInt Bit16) deserialState) *> deserialize
              -- int32
              0xd2 → putDeserialState (DSNumber (NInt Bit32) deserialState) *> deserialize
              -- int64
              0xd3 → putDeserialState (DSNumber (NInt Bit64) deserialState) *> deserialize
              -- fixext1
              0xd4 → putDeserialState (DSExt (ESType $ U.fromInt 1) deserialState) *> deserialize
              -- fixext2
              0xd5 → putDeserialState (DSExt (ESType $ U.fromInt 2) deserialState) *> deserialize
              -- fixext4
              0xd6 → putDeserialState (DSExt (ESType $ U.fromInt 4) deserialState) *> deserialize
              -- fixext8
              0xd7 → putDeserialState (DSExt (ESType $ U.fromInt 8) deserialState) *> deserialize
              -- fixext16
              0xd8 → putDeserialState (DSExt (ESType $ U.fromInt 16) deserialState) *> deserialize
              -- str8
              0xd9 → strDeserSt (SSLength Data8) *> deserialize
              -- str16
              0xda → strDeserSt (SSLength Data16) *> deserialize
              -- str32
              0xdb → strDeserSt (SSLength Data32) *> deserialize
              -- array16
              0xdc → putDeserialState (DSData DTArray (SSLength Data16) deserialState) *> deserialize
              -- array32
              0xdd → putDeserialState (DSData DTArray (SSLength Data32) deserialState) *> deserialize
              -- map16
              0xde → putDeserialState (DSMap (MSLength Data16) deserialState) *> deserialize
              -- map32
              0xdf → putDeserialState (DSMap (MSLength Data32) deserialState) *> deserialize
              -- the rest
              byte →
                if byte .&. posFixintMask == posFixint
                  then pure $ EInt byte
                else if byte .&. negFixintMask == negFixint
                  then pure <<< EInt $ byte - 256
                else if byte .&. fixstrMask == fixstr
                  then
                    let len = U.fromInt $ byte .&. complement fixstrMask in
                    EStringStart len <$ strDeserSt (SSRead len $ U.fromInt 0)
                else if byte .&. fixarrayMask == fixarray
                  then
                    let len = U.fromInt $ byte .&. complement fixarrayMask in
                    EArrayStart len <$ putDeserialState (DSData DTArray (SSRead len $ U.fromInt 0) deserialState)
                else if byte .&. fixmapMask == fixmap
                  then
                    let len = U.fromInt $ byte .&. complement fixmapMask in
                    EMapStart len <$ putDeserialState (DSMap (MSRead len false $ U.fromInt 0) deserialState)
                else throwDeserialError UnusedByte
          getteryGen m stateF resK = do
            uint <- next m
            putDeserialState $ stateF uint
            resK uint
        case deserialState of
          DSRoot → valueStart
          DSData dType subState parentState →
            case dType of
              DTBinary →
                case subState of
                  SSLength dataSize →
                    let
                      gettery m = getteryGen m (\uint -> DSData dType (SSRead uint (U.fromInt 0)) parentState) $ pure <<< EBinaryStart
                    in
                      case dataSize of
                        Data8 → gettery getUint8T
                        Data16 → gettery getUint16beT
                        Data32 → gettery getUint32beT
                  SSRead len read →
                    if len == read then EBinaryEnd <$ stateTransitionFromEndValue parentState
                    else do
                      let
                        remain = len - read
                        nextRead =
                          if U.fromInt maxInt < remain then maxInt
                          else U.toInt remain
                      Tuple mA dv ← runGetT (processBytes nextRead) =<< getDataState
                      either
                        ( \n -> do
                            let byteLen = D.byteLength dv
                            if byteLen == 0 then throwError n
                            else (EBinary <$> dataToArray dv) <* (put <<< Tuple (DSData dType (SSRead len $ read + U.fromInt byteLen) parentState) =<< L.empty)
                        )
                        (\dv' -> (EBinary <$> dataToArray dv') <* put (Tuple (DSData dType (SSRead len $ read + U.fromInt nextRead) parentState) dv))
                        mA
              DTString decoder →
                case subState of
                  SSLength dataSize →
                    let
                      gettery m = getteryGen m (\uint -> DSData dType (SSRead uint (U.fromInt 0)) parentState) $ pure <<< EStringStart
                    in
                      case dataSize of
                        Data8 → gettery getUint8T
                        Data16 → gettery getUint16beT
                        Data32 → gettery getUint32beT
                  SSRead len read →
                    if len == read then EStringEnd <$ stateTransitionFromEndValue parentState
                    else do
                      let
                        remain = len - read
                        nextRead =
                          if U.fromInt maxInt < remain then maxInt
                          else U.toInt remain
                      Tuple mA dv ← runGetT (processBytes nextRead) =<< getDataState
                      let
                        f totalRead dvFront dvBack = do
                          av <- dataToArray dvFront
                          str ← liftEffect $ decodeWithOptions av { stream: totalRead < len } decoder
                          EString str <$ put (Tuple (DSData dType (SSRead len totalRead) parentState) dvBack)
                      either
                        ( \n -> do
                            let byteLen = D.byteLength dv
                            if byteLen == 0 then throwError n
                            else f (read + U.fromInt byteLen) dv =<< L.empty
                        )
                        (\dv' -> f (read + U.fromInt nextRead) dv' dv)
                        mA
              DTArray →
                case subState of
                  SSLength dataSize →
                    let
                      gettery m = getteryGen m (\uint -> DSData dType (SSRead uint (U.fromInt 0)) parentState) $ pure <<< EArrayStart
                    in
                      case dataSize of
                        Data8 → throwFlogger deserialState
                        Data16 → gettery getUint16beT
                        Data32 → gettery getUint32beT
                  SSRead len read →
                    if len == read then EArrayEnd <$ stateTransitionFromEndValue parentState
                    else valueStart
          DSExt subState parentState →
            case subState of
              ESLength dataSize →
                let
                  gettery m = getteryGen m (\uint -> DSExt (ESType uint) parentState) $ const deserialize
                in
                  case dataSize of
                    Data8 → gettery getUint8T
                    Data16 → gettery getUint16beT
                    Data32 → gettery getUint32beT
              ESType len → do
                typ <- next getInt8T
                putDeserialState $ DSExt (ESRead len $ U.fromInt 0) parentState
                pure $ EExtStart len typ
              ESRead len read →
                if len == read then EExtEnd <$ stateTransitionFromEndValue parentState
                else do
                  let
                    remain = len - read
                    nextRead =
                      if U.fromInt maxInt < remain then maxInt
                      else U.toInt remain
                  Tuple mA dv ← runGetT (processBytes nextRead) =<< getDataState
                  either
                    ( \n -> do
                        let byteLen = D.byteLength dv
                        if byteLen == 0 then throwError n
                        else (EExt <$> dataToArray dv) <* (put <<< Tuple (DSExt (ESRead len $ read + U.fromInt byteLen) parentState) =<< L.empty)
                    )
                    (\dv' -> (EExt <$> dataToArray dv') <* put (Tuple (DSExt (ESRead len $ read + U.fromInt nextRead) parentState) dv))
                    mA
          DSNumber numType parentState →
            let
              numValEnd :: forall n. GetT Int DataView (DeserialMsgPackT m) n -> (n -> Event) -> DeserialMsgPackT m Event
              numValEnd f g = next f >>=
                flip voidRight (stateTransitionFromEndValue parentState) <<< g
            in
              case numType of
                NFloat → numValEnd getFloat32beT EFloat
                NDouble → numValEnd getFloat64beT EDouble
                NUInt bitSize →
                  case bitSize of
                    Bit8 → numValEnd getUint8T EUInt
                    Bit16 → numValEnd getUint16beT EUInt
                    Bit32 → numValEnd getUint32beT EUInt
                    Bit64 → numValEnd getUint64beT EUInt64
                NInt bitSize →
                  case bitSize of
                    Bit8 → numValEnd getInt8T EInt
                    Bit16 → numValEnd getInt16beT EInt
                    Bit32 → numValEnd getInt32beT EInt
                    Bit64 → numValEnd getInt64beT EInt64
          DSMap subState parentState →
            case subState of
              MSLength dataSize →
                let
                  gettery m = getteryGen m (\uint -> DSMap (MSRead uint false (U.fromInt 0)) parentState) $ pure <<< EMapStart
                in
                  case dataSize of
                    Data8 → throwFlogger deserialState
                    Data16 → gettery getUint16beT
                    Data32 → gettery getUint32beT
              MSRead len readVal read →
                if len == read then
                  if readVal then throwFlogger deserialState
                  else EMapEnd <$ stateTransitionFromEndValue parentState
                else valueStart
    in
      deserialize
