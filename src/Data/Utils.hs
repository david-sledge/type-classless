{-# LANGUAGE FlexibleContexts #-}
module Data.Utils where

import Data.Word (Word32, Word64)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)
