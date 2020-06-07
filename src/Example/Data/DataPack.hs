{-# LANGUAGE Rank2Types #-}

module Example.Data.DataPack (
    fixintMask,
    nilByte,
    collectionEndByte,
    falseByte,
    trueByte,
    uint8Byte,
    uint16Byte,
    uint32Byte,
    uint64Byte,
    int8Byte,
    int16Byte,
    int32Byte,
    int64Byte,
    floatByte,
    doubleByte,
    bin8Byte,
    bin16Byte,
    bin32Byte,
    str8Byte,
    str16Byte,
    str32Byte,
    ns8Byte,
    ns16Byte,
    ns32Byte,
    classnameByte,
    sequenceByte,
    assortmentByte,
    objectByte,
    noKeyValueByte,
    fixbinMask,
    fixstrMask,
    fixnsMask,
    fixMask,
    lenMask,

    DataSourceOp(..),
    UnpackState(..),
    Catchers(..),
    Callbacks(..),
    defaultCallbacks,
    unpackDataT,
  ) where

import Prelude hiding ((>>=), (>>), pure, (<*>), (<$>))
import Control.MonadOp
import Control.Monad.Trans.ClassOp
import Data.Bits
import Data.Utils
import Data.Int
import qualified Data.ByteString.Lazy as C
import Data.Word
import qualified Data.Map.Lazy as Map
import Control.Monad.Trans.Reader (ReaderT(..), Reader, runReader)
import Control.Monad.Trans.State.Strict (StateT(..), State, runState, evalState,
    execState)
import Control.Monad.Trans.ReaderOp hiding (ask)
import Control.Monad.Trans.StateOp hiding (get, modifyAnd, modify, putAnd, put)
import Control.Monad.Trans.Cont (ContT(..), Cont, cont, runCont)
import Control.Monad.Trans.ContOp

fixintMask        = 0xc0::Word8
nilByte           = 0x40::Word8
collectionEndByte = 0x41::Word8
falseByte         = 0x42::Word8
trueByte          = 0x43::Word8
uint8Byte         = 0x44::Word8
uint16Byte        = 0x45::Word8
uint32Byte        = 0x46::Word8
uint64Byte        = 0x47::Word8
int8Byte          = 0x48::Word8
int16Byte         = 0x49::Word8
int32Byte         = 0x4a::Word8
int64Byte         = 0x4b::Word8
floatByte         = 0x4c::Word8
doubleByte        = 0x4d::Word8
bin8Byte          = 0x4e::Word8
bin16Byte         = 0x4f::Word8
bin32Byte         = 0x50::Word8
str8Byte          = 0x51::Word8
str16Byte         = 0x52::Word8
str32Byte         = 0x53::Word8
ns8Byte           = 0x54::Word8
ns16Byte          = 0x55::Word8
ns32Byte          = 0x56::Word8
classnameByte     = 0x57::Word8
sequenceByte      = 0x58::Word8
assortmentByte    = 0x59::Word8
objectByte        = 0x5a::Word8
noKeyValueByte    = 0x5b::Word8
--unusedBytes = [0x5c..0x5f]
fixbinMask        = 0x60::Word8
fixstrMask        = 0x80::Word8
fixnsMask         = 0xa0::Word8
fixMask           = 0xe0::Word8
lenMask           = 0x1f::Word8

data DataSourceOp s e = DataSourceOp {
    _take :: forall r .
      Word32
       -> s -- stream value
       -> (e -> (C.ByteString, s) -> r) -- error handler
       -> ((C.ByteString, s) -> r) -- sucess handler
       -> r -- result of error or success handler
    }

data UnpackState =
    Sequence
  | Assortment
  | Object
  | SequenceStart
  | ObjectStart
  | Classname
  | EntryValue
  | LocalName
  deriving (Show, Ord, Eq)

data States s = States {
    unpackStack :: [UnpackState],
    source :: s }
  deriving (Show, Ord, Eq)

type Callback s r = s -> (s -> r) -> r

-- exception handlers
data Catchers e s r = Catchers {
    stream :: e -> C.ByteString -> s -> ((C.ByteString, s) -> r) -> r,
    invalidByte :: Word8 -> [UnpackState] -> [(Word8, Word8)] -> Callback s r,
    unusedByte :: Word8 -> Callback s r,
    programmatic :: r }

data Callbacks s r = Callbacks {
    nil :: Callback s r,
    collectionEnd :: Callback s r,
    boolean :: Bool -> Callback s r,
    int :: Int -> Callback s r,
    uint32 :: Word32 -> Callback s r,
    uint64 :: Word64 -> Callback s r,
    int64 :: Int64 -> Callback s r,
    float :: Float -> Callback s r,
    double :: Double -> Callback s r,
    binStart :: Word32 -> Callback s r,
    strStart :: Word32 -> Callback s r,
    nsStart :: Word32 -> Callback s r,
    dat :: C.ByteString -> Callback s r,
    classname :: Callback s r,
    noKeyValue :: Callback s r,
    sequenceD :: Callback s r,
    assortment :: Callback s r,
    object :: Callback s r }

callbk = \s f -> f s

callbk' = const callbk

defaultCallbacks = Callbacks {
    nil = callbk,
    collectionEnd = callbk,
    boolean = callbk',
    int = callbk',
    uint32 = callbk',
    uint64 = callbk',
    int64 = callbk',
    float = callbk',
    double = callbk',
    binStart = callbk',
    strStart = callbk',
    nsStart = callbk',
    dat = callbk',
    classname = callbk,
    noKeyValue = callbk,
    sequenceD = callbk,
    assortment = callbk,
    object = callbk }

data Env e s r = Env {
  catchers :: Catchers e s r,
  callbacks :: Callbacks s r }

infixl 1 >>, >>=
infixl 4 <$>, <*>, <%>, <#>

monad'ReaderContTOp = monad'ReaderTOp monad'ContTOp

msOp@(MonadStateOp {
    _modifyAnd = modifyAnd,
    _get = get,
    _state = state,
    _bind'MS = (>>=),
    _pure'MS = pure,
    _ap'MS = (<*>),
    _fmap'MS = (<$>) }) =
  monadState'StateTOp $ monad'ReaderContTOp

mrOp@(MonadReaderOp {
    _ask = ask,
    _local = local,
    _reader = reader{-,
    _bind'MS = (>>=),
    _pure'MS = pure,
    _ap'MS = (<*>),
    _fmap'MS = (<$>)-} }) =
  monadReader'StateTOp $ monadReader'ReaderTOp monad'ContTOp

k <%> x = k <*> pure x

f <#> x = pure $ f x

modify = _modify msOp

m >> k = m >>= const k

stateReaderContT = _lift monadTrans'StateTOp monad'ReaderContTOp .
  _lift monadTrans'ReaderTOp monad'ContTOp . ContT

catcherAsk f = ask >>= f . catchers

callbackAsk f = ask >>= f . callbacks

putSourceAnd s a = modifyAnd (\states -> states { source = s }) (const a)

putSource s = putSourceAnd s ()

putStateStack stack = modify (\states -> states { unpackStack = stack })

getSource f = get >>= f . source

getStateStack f = get >>= f . unpackStack

callInvalidByteErrorHandler byte allowedBytes = getStateStack $ \stack ->
  catcherAsk $ \catchers ->
  callWithSource $ invalidByte catchers byte stack allowedBytes

callUnusedByteErrorHandler byte = catcherAsk $ \catchers ->
  callWithSource $ unusedByte catchers byte

callProgrammaticErrorHandler = catcherAsk $
  stateReaderContT . const . programmatic

-- interprets byte as a signed 8-bit integer
fromByteToInt byte = fromIntegral (fromIntegral byte :: Int8)

validBytesDict = let
    valueRanges = [
        (fixintMask, fixintMask `xor` 0xff),
        (nilByte, nilByte),
        (falseByte, trueByte),
        (uint8Byte, uint64Byte),
        (int8Byte, int64Byte),
        (floatByte, doubleByte),
        (bin8Byte, bin32Byte),
        (str8Byte, str32Byte),
        (sequenceByte, sequenceByte),
        (assortmentByte, assortmentByte),
        (objectByte, objectByte),
        (fixbinMask, fixbinMask .|. lenMask),
        (fixstrMask, fixstrMask .|. lenMask) ]
    sequenceRanges = (collectionEndByte, collectionEndByte):valueRanges
    localnameRanges = [
        (bin8Byte, bin32Byte),
        (str8Byte, str32Byte),
        (fixbinMask, fixbinMask .|. lenMask),
        (fixstrMask, fixstrMask .|. lenMask) ]
    qnameRanges =
        (ns8Byte, ns32Byte):(fixnsMask, fixstrMask .|. lenMask):localnameRanges
    propNameRanges = (noKeyValueByte, noKeyValueByte):
        (collectionEndByte, collectionEndByte):qnameRanges
    keyValueRanges = (noKeyValueByte, noKeyValueByte):sequenceRanges
  in
  Map.fromList [
    (Just Sequence, sequenceRanges),
    (Just SequenceStart, (classnameByte, classnameByte):sequenceRanges),
    (Just Assortment, keyValueRanges),
    (Just Object, propNameRanges),
    (Just ObjectStart, (classnameByte, classnameByte):propNameRanges),
    (Just Classname, qnameRanges),
    (Just EntryValue, keyValueRanges),
    (Just LocalName, localnameRanges),
    (Nothing, valueRanges) ]

headSafe (x:xs) = Just x
headSafe _ = Nothing

callWithSource cb = getSource $ (\r -> stateReaderContT r >>= putSource) . cb

--{-
takeUnpack dsOp n =
  getSource $ \s ->
  _take dsOp n s (
      \e (dat, s') ->
        catcherAsk $ \catchers ->
        stateReaderContT (stream catchers e dat s') >>= \(dat', s'') ->
        putSourceAnd s'' dat' )
    $ \(dat, s') -> putSourceAnd s' dat

unpackT dsOp =
  takeUnpack dsOp 1 >>= \dat ->
    case C.uncons dat of
    Just (byte, _) ->
      let value = byte .&. fixintMask in
      -- is fixint?
      if value == 0 || value == fixintMask
      -- yes: 
      then
        callbackAsk $ \callbacks ->
        -- validate the byte against current state, read any additional data,
        -- and call corresponding callback after validation.
        validateValueState dsOp byte . callWithSource .
          int callbacks $ fromByteToInt byte
      else
        let mask = byte .&. fixMask in
        -- in fixDict?
        case Map.lookup mask fixDict of
        -- yes: call mapped function with length argument
        Just f -> f dsOp byte . fromIntegral $ byte .&. lenMask
        _ ->
        -- in formatDict?
          case Map.lookup byte formatDict of
          -- call mapped function
          Just g -> g dsOp
          -- unused byte
          _ -> callUnusedByteErrorHandler byte >> unpackT dsOp
    _ -> callProgrammaticErrorHandler

readByteString dsOp n c =
  takeUnpack dsOp n >>= \byteString ->
  c byteString >>
  let len = fromIntegral $ C.length byteString in
  if len < n
  then readByteString dsOp (n - len) c
  else pure ()

toWord n byteString =
  let n' = n - 1 in
  if n > 0
  then
    case C.uncons byteString of
    Just (byte, byteString') ->
      shiftL (fromIntegral byte) (fromIntegral $ n' * 8) .|.
        toWord n' byteString'
    _ -> 0
  else 0

readBytesToWord dsOp n =
  takeUnpack dsOp n >>= \byteString ->
  let
    len = fromIntegral $ C.length byteString
    word = toWord n byteString
  in
  if len < n
  then (word .|.) <$> readBytesToWord dsOp (n - len)
  else pure word

readNum dsOp n f = f <$> readBytesToWord dsOp n

read64 dsOp = readNum dsOp 8 (fromIntegral::Num a => Word64 -> a)

readUint8 dsOp = readNum dsOp 1 (fromIntegral::Num a => Word8 -> a)

readUint16 dsOp = readNum dsOp 2 (fromIntegral::Num a => Word16 -> a)

readUint32 dsOp = readNum dsOp 4 (fromIntegral::Num a => Word32 -> a)

{-
More than just validation:
1. validate against current state
2. transition state (sometimes)
3. additional processing which includes
  a. reading additional data (sometimes)
  b. making callbacks (every time)
4. mutually recurse (unless the resulting state stack is empty)
-}
validateValueState dsOp byte procMore = let
    unpack = unpackT dsOp
    onward = procMore >> unpack
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> let
      callInvalidByte =
        callInvalidByteErrorHandler byte validBytes >> unpack
    in
    case stateStack of
    Sequence:_ -> onward
    Assortment:_ -> putStateStack (EntryValue:stateStack) >> onward
    Object:_ -> callInvalidByte
    SequenceStart:stateStack' -> putStateStack (Sequence:stateStack') >> onward
    ObjectStart:_ -> callInvalidByte
    Classname:_ -> callInvalidByte
    EntryValue:stateStack' -> putStateStack stateStack' >> onward
    LocalName:_ -> callInvalidByte
    _ -> procMore
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpack

validateCollectionEndState dsOp byte procMore = let
    unpack = unpackT dsOp
    onward = procMore >> unpack
    parent stateStack =
      case stateStack of
      Sequence:_ -> putStateStack stateStack >> onward
      Assortment:_ -> putStateStack (EntryValue:stateStack) >> onward
      Object:_ -> callProgrammaticErrorHandler >> unpack
      SequenceStart:_ -> callProgrammaticErrorHandler >> unpack
      ObjectStart:_ -> callProgrammaticErrorHandler >> unpack
      Classname:_ -> callProgrammaticErrorHandler >> unpack
      EntryValue:stateStack' -> putStateStack stateStack' >> onward
      LocalName:_ -> callProgrammaticErrorHandler >> unpack
      _ -> putStateStack [] >> procMore
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> let
      callInvalidByte =
        callInvalidByteErrorHandler byte validBytes >> unpack
      pop stack = putStateStack stack >> onward
    in
    case stateStack of
    Sequence:stateStack' -> parent stateStack' >> onward
    Assortment:stateStack' -> parent stateStack'
    Object:stateStack' -> parent stateStack'
    SequenceStart:stateStack' -> parent stateStack'
    ObjectStart:stateStack' -> parent stateStack'
    Classname:_ -> callInvalidByte
    EntryValue:stateStack'' -> (let
        extraCall stack = callbackAsk $ \callbacks ->
          (callWithSource $ noKeyValue callbacks) >> parent stack
      in
      case stateStack'' of
      Assortment:stateStack' -> extraCall stateStack'
      Object:stateStack' -> extraCall stateStack'
      _ -> callProgrammaticErrorHandler >> unpack )
    LocalName:_ -> callInvalidByte
    _ -> callInvalidByte
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpack

validateStringState dsOp byte procMore = let
    unpack = unpackT dsOp
    onward = procMore >> unpack
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> case stateStack of
    Sequence:_ -> onward
    Assortment:_ -> putStateStack (EntryValue:stateStack) >> onward
    Object:_ -> putStateStack (EntryValue:stateStack) >> onward
    SequenceStart:stateStack' -> putStateStack (Sequence:stateStack') >> onward
    ObjectStart:stateStack' ->
      putStateStack (EntryValue:Object:stateStack') >> onward
    Classname:stateStack' -> putStateStack stateStack' >> onward
    EntryValue:stateStack' -> putStateStack stateStack' >> onward
    LocalName:stateStack' -> (
        case stateStack' of
        -- transition to entry value (object) or
        Object:_ -> putStateStack (EntryValue:stateStack')
        -- pop Classname (object/sequence) off the stack
        Classname:ss -> putStateStack ss
        -- flog the developer!
        _ -> callProgrammaticErrorHandler ) >> onward
    _ -> procMore
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpack

validateNamespaceState dsOp byte procMore = let
    unpack = unpackT dsOp
    onward = procMore >> unpack
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> let
      callInvalidByte = callInvalidByteErrorHandler byte validBytes >> unpack
    in
    case stateStack of
    Sequence:_ -> callInvalidByte
    Assortment:_ -> callInvalidByte
    Object:_ -> putStateStack (LocalName:stateStack) >> onward
    SequenceStart:_ -> callInvalidByte
    ObjectStart:stateStack' ->
      putStateStack (LocalName:Object:stateStack') >> onward
    Classname:_ -> putStateStack (LocalName:stateStack) >> onward
    EntryValue:_ -> callInvalidByte
    LocalName:_ -> callInvalidByte
    _ -> callInvalidByte
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpack

validateClassnameState dsOp byte procMore = let
    unpack = unpackT dsOp
    onward = procMore >> unpack
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> let
      callInvalidByte = callInvalidByteErrorHandler byte validBytes >> unpack
    in
    case stateStack of
    Sequence:_ -> callInvalidByte
    Assortment:_ -> callInvalidByte
    Object:_ -> callInvalidByte
    SequenceStart:stack -> putStateStack (Classname:Sequence:stack) >> onward
    ObjectStart:stack -> putStateStack (Classname:Object:stack) >> onward
    Classname:_ -> callInvalidByte
    EntryValue:_ -> callInvalidByte
    LocalName:_ -> callInvalidByte
    _ -> callInvalidByte
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpack

validateNoKeyValueState dsOp byte procMore = let
    unpack = unpackT dsOp
    onward = procMore >> unpack
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> let
      callInvalidByte = callInvalidByteErrorHandler byte validBytes >> unpack
    in
    case stateStack of
    Sequence:_ -> callInvalidByte
    Assortment:_ -> putStateStack (EntryValue:stateStack) >> onward
    Object:_ -> putStateStack (EntryValue:stateStack) >> onward
    SequenceStart:stack -> callInvalidByte
    ObjectStart:stack -> putStateStack (EntryValue:Object:stack) >> onward
    Classname:_ -> callInvalidByte
    EntryValue:stack -> putStateStack stack >> onward
    LocalName:_ -> callInvalidByte
    _ -> callInvalidByte
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpack

validateCollectionState dsOp unpackState byte procMore = let
    unpack = unpackT dsOp
    onward = procMore >> unpack
  in
  getStateStack $ \stateStack ->
  case Map.lookup (headSafe stateStack) validBytesDict of
  Just validBytes -> let
      callInvalidByte = callInvalidByteErrorHandler byte validBytes >>
        unpack
    in
    case stateStack of
    Sequence:_ -> putStateStack (unpackState:stateStack) >> onward
    Assortment:_ -> putStateStack (unpackState:stateStack) >> onward
    Object:_ -> callInvalidByte
    SequenceStart:stack -> putStateStack (unpackState:Sequence:stack) >>
      onward
    ObjectStart:stack -> callInvalidByte
    Classname:_ -> callInvalidByte
    EntryValue:stack -> putStateStack (unpackState:stateStack) >> onward
    LocalName:_ -> callInvalidByte
    _ -> putStateStack (unpackState:stateStack) >> onward
  -- flog the developer!
  _ -> callProgrammaticErrorHandler >> unpack

formatDictEntry byte valid more = (
    byte,
    \dsOp -> callbackAsk $ valid dsOp byte . more dsOp )

formatDict = Map.fromList [
    formatDictEntry nilByte validateValueState . const $
      \callbacks -> callWithSource $ nil callbacks,
    formatDictEntry collectionEndByte validateCollectionEndState . const $
      \callbacks -> callWithSource $ collectionEnd callbacks,
    formatDictEntry falseByte validateValueState . const $
      \callbacks -> callWithSource $ boolean callbacks False,
    formatDictEntry trueByte validateValueState . const $
      \callbacks -> callWithSource $ boolean callbacks True,
    formatDictEntry uint8Byte validateValueState $ \dsOp callbacks ->
      readUint8 dsOp >>= \integer ->
      callWithSource $ int callbacks integer,
    formatDictEntry uint16Byte validateValueState $ \dsOp callbacks ->
      readUint16 dsOp >>= \integer ->
      callWithSource $ int callbacks integer,
    formatDictEntry uint32Byte validateValueState $ \dsOp callbacks ->
      readUint32 dsOp >>= \integer ->
      callWithSource $ uint32 callbacks integer,
    formatDictEntry uint64Byte validateValueState $ \dsOp callbacks ->
      read64 dsOp >>= \integer ->
      callWithSource $ uint64 callbacks integer,
    formatDictEntry int8Byte validateValueState $ \dsOp callbacks ->
      readNum dsOp 1 (fromByteToInt::Num a => Word8 -> a) >>= \integer ->
      callWithSource $ int callbacks integer,
    formatDictEntry int16Byte validateValueState $ \dsOp callbacks ->
      readNum dsOp 2 ((\byte ->
          fromIntegral (fromIntegral byte :: Int16))::Num a => Word16 -> a
        ) >>= \integer ->
      callWithSource $ int callbacks integer,
    formatDictEntry int32Byte validateValueState $ \dsOp callbacks ->
      readNum dsOp 4 ((\byte ->
          fromIntegral (fromIntegral byte :: Int))::Num a => Word32 -> a
        ) >>= \integer ->
      callWithSource $ int callbacks integer,
    formatDictEntry int64Byte validateValueState $ \dsOp callbacks ->
      read64 dsOp >>= \integer ->
      callWithSource $ int64 callbacks integer,
    formatDictEntry floatByte validateValueState $ \dsOp callbacks ->
      readNum dsOp 4 wordToFloat >>= \num ->
      callWithSource $ float callbacks num,
    formatDictEntry doubleByte validateValueState $ \dsOp callbacks ->
      readNum dsOp 8 wordToDouble >>= \num ->
      callWithSource $ double callbacks num,
    formatDictEntry bin8Byte validateStringState $ \dsOp callbacks ->
      readUint8 dsOp >>= \length ->
      (callWithSource $ binStart callbacks length) >>
      readByteString dsOp length (callWithSource . dat callbacks),
    formatDictEntry bin16Byte validateStringState $ \dsOp callbacks ->
      readUint16 dsOp >>= \length ->
      (callWithSource $ binStart callbacks length) >>
      readByteString dsOp length (callWithSource . dat callbacks),
    formatDictEntry bin32Byte validateStringState $ \dsOp callbacks ->
      readUint32 dsOp >>= \length ->
      (callWithSource $ binStart callbacks length) >>
      readByteString dsOp length (callWithSource . dat callbacks),
    formatDictEntry str8Byte validateStringState $ \dsOp callbacks ->
      readUint8 dsOp >>= \length ->
      (callWithSource $ strStart callbacks length) >>
      readByteString dsOp length (callWithSource . dat callbacks),
    formatDictEntry str16Byte validateStringState $ \dsOp callbacks ->
      readUint16 dsOp >>= \length ->
      (callWithSource $ strStart callbacks length) >>
      readByteString dsOp length (callWithSource . dat callbacks),
    formatDictEntry str32Byte validateStringState $ \dsOp callbacks ->
      readUint32 dsOp >>= \length ->
      (callWithSource $ strStart callbacks length) >>
      readByteString dsOp length (callWithSource . dat callbacks),
    formatDictEntry ns8Byte validateNamespaceState $ \dsOp callbacks ->
      readUint8 dsOp >>= \length ->
      (callWithSource $ nsStart callbacks length) >>
      readByteString dsOp length (callWithSource . dat callbacks),
    formatDictEntry ns16Byte validateNamespaceState $ \dsOp callbacks ->
      readUint16 dsOp >>= \length ->
      (callWithSource $ nsStart callbacks length) >>
      readByteString dsOp length (callWithSource . dat callbacks),
    formatDictEntry ns32Byte validateNamespaceState $ \dsOp callbacks ->
      readUint32 dsOp >>= \length ->
      (callWithSource $ nsStart callbacks length) >>
      readByteString dsOp length (callWithSource . dat callbacks),
    formatDictEntry classnameByte validateClassnameState . const $
      \callback -> callWithSource $ classname callback,
    formatDictEntry sequenceByte
      (\dsOp -> validateCollectionState dsOp SequenceStart) . const $
      \callback -> callWithSource $ sequenceD callback,
    formatDictEntry assortmentByte
      (\dsOp -> validateCollectionState dsOp Assortment) . const $
      \callback -> callWithSource $ assortment callback,
    formatDictEntry objectByte
      (\dsOp -> validateCollectionState dsOp ObjectStart) . const $
      \callback -> callWithSource $ object callback,
    formatDictEntry noKeyValueByte validateNoKeyValueState . const $
      \callback -> callWithSource $ noKeyValue callback ]

fixDictEntry byteMask validator startCb = (byteMask, \dsOp byte length ->
      callbackAsk $ \callbacks ->
      validator dsOp byte $
        (callWithSource $ startCb callbacks length) >>
        readByteString dsOp length (callWithSource . dat callbacks)
    )

fixDict = Map.fromList [
    fixDictEntry fixbinMask validateStringState binStart,
    fixDictEntry fixstrMask validateStringState strStart,
    fixDictEntry fixnsMask validateNamespaceState nsStart ]

{-
unpackT dsOp = unpack
  where
    takeUnpack n = let
        take = _take dsOp
      in
      getSource $ \s ->
      take n s (\e (dat, s') -> catcherAsk $ \catchers ->
          stateReaderContT (stream catchers e dat s') >>= \(dat', s'') ->
          putSourceAnd s'' dat')
        (\(dat, s') ->
          -- a lens would be useful here
          putSourceAnd s' dat)

    readByteString n c =
      takeUnpack n >>= \byteString ->
      c byteString >>
      let len = fromIntegral $ C.length byteString in
      if len < n
      then readByteString (n - len) c
      else pure ()

    toWord n byteString =
      let n' = n - 1 in
      if n > 0
      then
        case C.uncons byteString of
        Just (byte, byteString') ->
          shiftL (fromIntegral byte) (fromIntegral $ n' * 8)
            .|. toWord n' byteString'
        _ -> 0
      else 0

    readBytesToWord n =
      takeUnpack n >>= \byteString ->
      let
        len = fromIntegral $ C.length byteString
        word = toWord n byteString
      in
      if len < n
      then (word .|.) <$> readBytesToWord (n - len)
      else pure word

    readNum n f = f <$> readBytesToWord n
    read64 = readNum 8 (fromIntegral::Num a => Word64 -> a)
    readUint8 = readNum 1 (fromIntegral::Num a => Word8 -> a)
    readUint16 = readNum 2 (fromIntegral::Num a => Word16 -> a)
    readUint32 = readNum 4 (fromIntegral::Num a => Word32 -> a)

    unpack =
      takeUnpack 1 >>= \dat ->
        case C.uncons dat of
        Just (byte, _) ->
          let
            value = byte .&. fixintMask
          in
          -- is fixint?
          if value == 0 || value == fixintMask
          -- yes: 
          then
            callbackAsk $ \callbacks ->
            -- validate the byte against current state, read any additional
            -- data, and call corresponding callback after validation.
            validateValueState byte . callWithSource . int callbacks $
              fromByteToInt byte
          else
            let mask = byte .&. fixMask in
            -- in fixDict?
            case Map.lookup mask fixDict of
            -- yes: call mapped function with length argument
            Just f -> f byte . fromIntegral $ byte .&. lenMask
            _ ->
            -- in formatDict?
              case Map.lookup byte formatDict of
              -- call mapped function
              Just g -> g
              -- unused byte
              _ -> callUnusedByteErrorHandler byte >> unpack
        _ -> callProgrammaticErrorHandler

    {-
    More than just validation:
    1. validate against current state
    2. transition state (sometimes)
    3. additional processing which includes
      a. reading additional data (sometimes)
      b. making callbacks (every time)
    4. mutually recurse (unless the resulting state stack is empty)
    -}
    validateValueState byte procMore = let
        onward = procMore >> unpack
      in
      getStateStack $ \stateStack ->
      case Map.lookup (headSafe stateStack) validBytesDict of
      Just validBytes -> let
          callInvalidByte =
            callInvalidByteErrorHandler byte validBytes >> unpack
        in
        case stateStack of
        Sequence:_ -> onward
        Assortment:_ -> putStateStack (EntryValue:stateStack) >> onward
        Object:_ -> callInvalidByte
        SequenceStart:stateStack' ->
          putStateStack (Sequence:stateStack') >> onward
        ObjectStart:_ -> callInvalidByte
        Classname:_ -> callInvalidByte
        EntryValue:stateStack' -> putStateStack stateStack' >> onward
        LocalName:_ -> callInvalidByte
        _ -> procMore
      -- flog the developer!
      _ -> callProgrammaticErrorHandler >> unpack

    validateCollectionEndState byte procMore = let
        onward = procMore >> unpack
        parent stateStack =
          case stateStack of
          Sequence:_ -> putStateStack stateStack >> onward
          Assortment:_ -> putStateStack (EntryValue:stateStack) >> onward
          Object:_ -> callProgrammaticErrorHandler >> unpack
          SequenceStart:_ -> callProgrammaticErrorHandler >> unpack
          ObjectStart:_ -> callProgrammaticErrorHandler >> unpack
          Classname:_ -> callProgrammaticErrorHandler >> unpack
          EntryValue:stateStack' -> putStateStack stateStack' >> onward
          LocalName:_ -> callProgrammaticErrorHandler >> unpack
          _ -> putStateStack [] >> procMore
      in
      getStateStack $ \stateStack ->
      case Map.lookup (headSafe stateStack) validBytesDict of
      Just validBytes -> let
          callInvalidByte =
            callInvalidByteErrorHandler byte validBytes >> unpack
          pop stack = putStateStack stack >> onward
        in
        case stateStack of
        Sequence:stateStack' -> parent stateStack' >> onward
        Assortment:stateStack' -> parent stateStack'
        Object:stateStack' -> parent stateStack'
        SequenceStart:stateStack' -> parent stateStack'
        ObjectStart:stateStack' -> parent stateStack'
        Classname:_ -> callInvalidByte
        EntryValue:stateStack'' -> (let
            extraCall stack = callbackAsk $ \callbacks ->
              callWithSource (noKeyValue callbacks) >> parent stack
          in
          case stateStack'' of
          Assortment:stateStack' -> extraCall stateStack'
          Object:stateStack' -> extraCall stateStack'
          _ -> callProgrammaticErrorHandler >> unpack )
        LocalName:_ -> callInvalidByte
        _ -> callInvalidByte
      -- flog the developer!
      _ -> callProgrammaticErrorHandler >> unpack

    validateStringState byte procMore = let
        onward = procMore >> unpack
      in
      getStateStack $ \stateStack ->
      case Map.lookup (headSafe stateStack) validBytesDict of
      Just validBytes -> case stateStack of
        Sequence:_ -> onward
        Assortment:_ -> putStateStack (EntryValue:stateStack) >> onward
        Object:_ -> putStateStack (EntryValue:stateStack) >> onward
        SequenceStart:stateStack' ->
          putStateStack (Sequence:stateStack') >> onward
        ObjectStart:stateStack' ->
          putStateStack (EntryValue:Object:stateStack') >> onward
        Classname:stateStack' -> putStateStack stateStack' >> onward
        EntryValue:stateStack' -> putStateStack stateStack' >> onward
        LocalName:stateStack' -> (
            case stateStack' of
            -- transition to entry value (object) or
            Object:_ -> putStateStack (EntryValue:stateStack')
            -- pop Classname (object/sequence) off the stack
            Classname:ss -> putStateStack ss
            -- flog the developer!
            _ -> callProgrammaticErrorHandler ) >> onward
        _ -> procMore
      -- flog the developer!
      _ -> callProgrammaticErrorHandler >> unpack

    validateNamespaceState byte procMore = let
        onward = procMore >> unpack
      in
      getStateStack $ \stateStack ->
      case Map.lookup (headSafe stateStack) validBytesDict of
      Just validBytes -> let
          callInvalidByte =
            callInvalidByteErrorHandler byte validBytes >> unpack
        in
        case stateStack of
        Sequence:_ -> callInvalidByte
        Assortment:_ -> callInvalidByte
        Object:_ -> putStateStack (LocalName:stateStack) >> onward
        SequenceStart:_ -> callInvalidByte
        ObjectStart:stateStack' ->
          putStateStack (LocalName:Object:stateStack') >> onward
        Classname:_ -> putStateStack (LocalName:stateStack) >> onward
        EntryValue:_ -> callInvalidByte
        LocalName:_ -> callInvalidByte
        _ -> callInvalidByte
      -- flog the developer!
      _ -> callProgrammaticErrorHandler >> unpack

    validateClassnameState byte procMore = let
        onward = procMore >> unpack
      in
      getStateStack $ \stateStack ->
      case Map.lookup (headSafe stateStack) validBytesDict of
      Just validBytes -> let
          callInvalidByte =
            callInvalidByteErrorHandler byte validBytes >> unpack
        in
        case stateStack of
        Sequence:_ -> callInvalidByte
        Assortment:_ -> callInvalidByte
        Object:_ -> callInvalidByte
        SequenceStart:stack -> putStateStack (Classname:Sequence:stack) >>
          onward
        ObjectStart:stack -> putStateStack (Classname:Object:stack) >> onward
        Classname:_ -> callInvalidByte
        EntryValue:_ -> callInvalidByte
        LocalName:_ -> callInvalidByte
        _ -> callInvalidByte
      -- flog the developer!
      _ -> callProgrammaticErrorHandler >> unpack

    validateNoKeyValueState byte procMore = let
        onward = procMore >> unpack
      in
      getStateStack $ \stateStack ->
      case Map.lookup (headSafe stateStack) validBytesDict of
      Just validBytes -> let
          callInvalidByte = callInvalidByteErrorHandler byte validBytes
            >> unpack
        in
        case stateStack of
        Sequence:_ -> callInvalidByte
        Assortment:_ -> putStateStack (EntryValue:stateStack) >> onward
        Object:_ -> putStateStack (EntryValue:stateStack) >> onward
        SequenceStart:stack -> callInvalidByte
        ObjectStart:stack -> putStateStack (EntryValue:Object:stack) >> onward
        Classname:_ -> callInvalidByte
        EntryValue:stack -> putStateStack stack >> onward
        LocalName:_ -> callInvalidByte
        _ -> callInvalidByte
      -- flog the developer!
      _ -> callProgrammaticErrorHandler >> unpack

    validateCollectionState unpackState byte procMore = let
        onward = procMore >> unpack
      in
      getStateStack $ \stateStack ->
      case Map.lookup (headSafe stateStack) validBytesDict of
      Just validBytes -> let
          callInvalidByte = callInvalidByteErrorHandler byte validBytes >>
            unpack
        in
        case stateStack of
        Sequence:_ -> putStateStack (unpackState:stateStack) >> onward
        Assortment:_ -> putStateStack (unpackState:stateStack) >> onward
        Object:_ -> callInvalidByte
        SequenceStart:stack -> putStateStack (unpackState:Sequence:stack) >>
          onward
        ObjectStart:stack -> callInvalidByte
        Classname:_ -> callInvalidByte
        EntryValue:stack -> putStateStack (unpackState:stateStack) >> onward
        LocalName:_ -> callInvalidByte
        _ -> putStateStack (unpackState:stateStack) >> onward
      -- flog the developer!
      _ -> callProgrammaticErrorHandler >> unpack

    formatDictEntry byte valid more = (
        byte,
        callbackAsk $ valid byte . more )

    formatDict = Map.fromList [
        formatDictEntry nilByte validateValueState $ callWithSource . nil,
        formatDictEntry collectionEndByte validateCollectionEndState $
          callWithSource . collectionEnd,
        formatDictEntry falseByte validateValueState (
          \callbacks -> callWithSource $ boolean callbacks False),
        formatDictEntry trueByte validateValueState (
          \callbacks -> callWithSource $ boolean callbacks True),
        formatDictEntry uint8Byte validateValueState (\callbacks ->
            readUint8 >>= \integer ->
            callWithSource $ int callbacks integer),
        formatDictEntry uint16Byte validateValueState (\callbacks ->
            readUint16 >>= \integer ->
            callWithSource $ int callbacks integer),
        formatDictEntry uint32Byte validateValueState (\callbacks ->
            readUint32 >>= \integer ->
            callWithSource $ uint32 callbacks integer),
        formatDictEntry uint64Byte validateValueState (\callbacks ->
            read64 >>= \integer ->
            callWithSource $ uint64 callbacks integer),
        formatDictEntry int8Byte validateValueState (\callbacks ->
            readNum 1 (fromByteToInt::Num a => Word8 -> a) >>= \integer ->
            callWithSource $ int callbacks integer),
        formatDictEntry int16Byte validateValueState (\callbacks ->
            readNum 2 ((\byte ->
                fromIntegral (fromIntegral byte :: Int16)
              )::Num a => Word16 -> a) >>= \integer ->
            callWithSource $ int callbacks integer),
        formatDictEntry int32Byte validateValueState (\callbacks ->
            readNum 4 ((\byte ->
                fromIntegral (fromIntegral byte::Int))::Num a => Word32 -> a
              ) >>= \integer ->
            callWithSource $ int callbacks integer),
        formatDictEntry int64Byte validateValueState (\callbacks ->
            read64 >>= \integer ->
            callWithSource $ int64 callbacks (fromIntegral integer)),
        formatDictEntry floatByte validateValueState (\callbacks ->
            readNum 4 wordToFloat >>= \num ->
            callWithSource $ float callbacks num),
        formatDictEntry doubleByte validateValueState (\callbacks ->
            readNum 8 wordToDouble >>= \num ->
            callWithSource $ double callbacks num),
        formatDictEntry bin8Byte validateStringState (
            \callbacks ->
            readUint8 >>= \length ->
            (callWithSource . binStart callbacks $ fromIntegral length) >>
            readByteString (fromIntegral length)
              (callWithSource . dat callbacks) ),
        formatDictEntry bin16Byte validateStringState (\callbacks ->
            readUint16 >>= \length ->
            (callWithSource $ binStart callbacks (fromIntegral length)) >>
            readByteString (fromIntegral length)
              (callWithSource . dat callbacks) ),
        formatDictEntry bin32Byte validateStringState (\callbacks ->
            readUint32 >>= \length ->
            (callWithSource $ binStart callbacks length) >>
            readByteString length (callWithSource . dat callbacks)),
        formatDictEntry str8Byte validateStringState (\callbacks ->
            readUint8 >>= \length ->
            (callWithSource $ strStart callbacks (fromIntegral length)) >>
            readByteString (fromIntegral length)
              (callWithSource . dat callbacks) ),
        formatDictEntry str16Byte validateStringState (\callbacks ->
            readUint16 >>= \length ->
            (callWithSource $ strStart callbacks (fromIntegral length)) >>
            readByteString (fromIntegral length)
              (callWithSource . dat callbacks) ),
        formatDictEntry str32Byte validateStringState (\callbacks ->
            readUint32 >>= \length ->
            (callWithSource $ strStart callbacks length) >>
            readByteString length (callWithSource . dat callbacks)),
        formatDictEntry ns8Byte validateNamespaceState (\callbacks ->
            readUint8 >>= \length ->
            (callWithSource $ nsStart callbacks (fromIntegral length)) >>
            readByteString (fromIntegral length)
              (callWithSource . dat callbacks) ),
        formatDictEntry ns16Byte validateNamespaceState (\callbacks ->
            readUint16 >>= \length ->
            (callWithSource $ nsStart callbacks (fromIntegral length)) >>
            readByteString (fromIntegral length)
              (callWithSource . dat callbacks) ),
        formatDictEntry ns32Byte validateNamespaceState (\callbacks ->
            readUint32 >>= \length ->
            (callWithSource $ nsStart callbacks length) >>
            readByteString length (callWithSource . dat callbacks)),
        formatDictEntry classnameByte validateClassnameState $
          callWithSource . classname,
        formatDictEntry sequenceByte (validateCollectionState SequenceStart) $
          callWithSource . sequenceD,
        formatDictEntry assortmentByte (validateCollectionState Assortment) $
          callWithSource . assortment,
        formatDictEntry objectByte (validateCollectionState ObjectStart) $
          callWithSource . object,
        formatDictEntry noKeyValueByte validateNoKeyValueState $
          callWithSource . noKeyValue ]

    fixDictEntry byteMask validator startCb = (byteMask, \byte length ->
          callbackAsk $ \callbacks ->
          validator byte $
            (callWithSource $ startCb callbacks length) >>
            readByteString length (callWithSource . dat callbacks)
        )

    fixDict = Map.fromList [
        fixDictEntry fixbinMask validateStringState binStart,
        fixDictEntry fixstrMask validateStringState strStart,
        fixDictEntry fixnsMask validateNamespaceState nsStart ]
--}

unpackDataT dsOp s catchers callbacks = \c ->
  (runContT . runReaderT (runStateT (unpackT dsOp) $ States [] s) $
    Env catchers callbacks) ( \(_, states) ->
    case unpackStack states of
    [] -> c $ source states
    _ -> programmatic catchers )
