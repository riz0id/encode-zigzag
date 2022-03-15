{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-- | Fast Zigzag encoding and decoding defined via primitive GHC PrimOps.
--
-- @since 1.0.0
module Data.Encode.ZigZag.Prim
  ( toZigZagI#,
    fromZigZagI#,
    toZigZagI8#,
    fromZigZagI8#,
    toZigZagI16#,
    fromZigZagI16#,
    toZigZagI32#,
    fromZigZagI32#,
  )
where

import GHC.Exts (Int#, Word#, and#, int2Word#, not#, plusWord#, xor#, (-#))
import qualified GHC.Exts as GHC
import GHC.Int (uncheckedIShiftRA64#)
import GHC.Word (uncheckedShiftL64#, uncheckedShiftRL64#)

#if MIN_VERSION_base(4,16,0)

import GHC.Exts (Int16#, Int32#, Int8#)

#endif

import Data.Encode.ZigZag.Compat
  ( int16ToInt#,
    int32ToInt#,
    int8ToInt#,
    intToInt16#,
    intToInt32#,
    intToInt8#,
  )

-- defines the `WORD_SIZE_IN_BITS` macro
#include "MachDeps.h"

-- -----------------------------------------------------------------------------
--
-- Int#
--

toZigZagI# :: Int# -> Word#
toZigZagI# x =
  let zig = uncheckedShiftL64# (GHC.int2Word# x) 1#
      zag = int2Word# (uncheckedIShiftRA64# x (WORD_SIZE_IN_BITS# -# 1#))
   in xor# zig zag

fromZigZagI# :: Word# -> Int#
fromZigZagI# x =
  let zig = uncheckedShiftRL64# x 1#
      zag = not# (x `and#` one) `plusWord#` one
      one = int2Word# 1#
   in GHC.word2Int# (xor# zig zag)

-- -----------------------------------------------------------------------------
--
-- Int8#
--

#if MIN_VERSION_base(4,16,0)

toZigZagI8# :: Int8# -> Word#
toZigZagI8# x = toZigZagI# (int8ToInt# x)

fromZigZagI8# :: Word# -> Int8#
fromZigZagI8# w = intToInt8# (fromZigZagI# w)

#else

toZigZagI8# :: Int# -> Word#
toZigZagI8# x = toZigZagI# (int8ToInt# x)

fromZigZagI8# :: Word# -> Int#
fromZigZagI8# w = intToInt8# (fromZigZagI# w)

#endif

-- -----------------------------------------------------------------------------
--
-- Int16#
--

#if MIN_VERSION_base(4,16,0)

toZigZagI16# :: Int16# -> Word#
toZigZagI16# x = toZigZagI# (int16ToInt# x)

fromZigZagI16# :: Word# -> Int16#
fromZigZagI16# w = intToInt16# (fromZigZagI# w)

#else

toZigZagI16# :: Int# -> Word#
toZigZagI16# x = toZigZagI# (int16ToInt# x)

fromZigZagI16# :: Word# -> Int#
fromZigZagI16# w = intToInt16# (fromZigZagI# w)

#endif

-- -----------------------------------------------------------------------------
--
-- Int32#
--

#if MIN_VERSION_base(4,16,0)

toZigZagI32# :: Int32# -> Word#
toZigZagI32# x = toZigZagI# (int32ToInt# x)

fromZigZagI32# :: Word# -> Int32#
fromZigZagI32# w = intToInt32# (fromZigZagI# w)

#else

toZigZagI32# :: Int# -> Word#
toZigZagI32# x = toZigZagI# (int32ToInt# x)

fromZigZagI32# :: Word# -> Int#
fromZigZagI32# w = intToInt32# (fromZigZagI# w)

#endif
