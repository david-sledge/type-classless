{-# LANGUAGE Rank2Types #-}

module Example.Data.DataPackTest where

import Prelude hiding ((>>=), (>>))
import Control.MonadOp
import Data.FixOp
import Control.Monad.Misc
import Example.Data.DataPack
import qualified Data.ByteString.Lazy as C
import Data.Int
import System.Exit (exitFailure)

(>>=) = _bind monad'IOOp

m >> k = m >>= const k

failure str = const . const $ putStrLn str >> exitFailure

failure' str = const $ failure str

failCallbacks = Callbacks {
    nil = failure "nil",
    collectionEnd = failure "collectionEnd",
    boolean = failure' "boolean",
    int = failure' "int",
    uint32 = failure' "uint32",
    uint64 = failure' "uint64",
    int64 = failure' "int64",
    float = failure' "float",
    double = failure' "double",
    binStart = failure' "binStart",
    strStart = failure' "strStart",
    nsStart = failure' "nsStart",
    dat = failure' "dat",
    classname = failure "classname",
    sequenceD = failure "sequence",
    dictionary = failure "dictionary",
    object = failure "object" }

dataSource'ByteStringOp = DataSourceOp {
    _take = \n bstr bad good ->
      if C.length bstr == 0
      then bad () (bstr, bstr)
      else
        let (body, tail) = C.splitAt (fromIntegral n) bstr in
        good (body, tail) }

myCatchers = Catchers {
  stream = \e _ _ _ -> (putStrLn $ "stream fail " ++ show e) >>
    exitFailure,
  invalidByte = \byte stack allowedByteRanges s f ->
    (putStrLn $ "fail " ++
      show byte ++ " is not allowed with state of " ++ show stack ++
      ". Expected values within the ranges of "
        ++ show allowedByteRanges ++ ".") >>
    exitFailure,
  unusedByte = \byte s f ->
    (putStrLn $ "fail " ++ show byte ++ " is not a usable byte.") >>
    exitFailure,
  programmatic =
    putStrLn "Super fail! Programmatic error! Flog the developer!" >>
    exitFailure }

myCallbacks = failCallbacks {
    nil = \s f -> putStrLn "pass" >> f s }

nilSource = C.pack [classnameByte, classnameByte + 1, nilByte]

testUnpackT = (
    unpackDataT dataSource'ByteStringOp (C.pack [nilByte]) myCatchers
    (failCallbacks {
    nil = nil defaultCallbacks })
    . const $ putStrLn "pass" ){- >>
  unpackDataT
  dataSource'ByteStringOp
  nilSource
  myCatchers
  myCallbacks (
    \endStream ->
    unpackDataT dataSource'ByteStringOp endStream myCatchers myCallbacks
      . const $ print "fail" ) >>
  (unpackDataT dataSource'ByteStringOp nilSource myCatchers myCallbacks
    . const $ print "fail")
--}
