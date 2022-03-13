{-# LANGUAGE MagicHash #-}

-- | Fast Zigzag encoding and decoding defined via primitive GHC PrimOps.
--
-- @since 1.0.0
module Data.Encode.ZigZag.Prim where

import qualified GHC.Exts as GHC
import GHC.Int (uncheckedIShiftRA64#)
import GHC.Word (uncheckedShiftL64#, uncheckedShiftRL64#)

import GHC.Exts
  ( Int#,
    Int16#,
    Int32#,
    Int8#,
    Word#,
    and#,
    int16ToInt#,
    int8ToInt#,
    intToInt16#,
    intToInt8#,
    int8ToInt#,
    int16ToInt#,
    int32ToInt#,
    int2Word#,
    intToInt32#,
    not#,
    plusWord#,
    xor#,
    (-#),
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

toZigZagI8# :: Int8# -> Word#
toZigZagI8# x = toZigZagI# (int8ToInt# x)

fromZigZagI8# :: Word# -> Int8#
fromZigZagI8# w = intToInt8# (fromZigZagI# w)

-- -----------------------------------------------------------------------------
--
-- Int16#
--

toZigZagI16# :: Int16# -> Word#
toZigZagI16# x = toZigZagI# (int16ToInt# x)

fromZigZagI16# :: Word# -> Int16#
fromZigZagI16# w = intToInt16# (fromZigZagI# w)

-- -----------------------------------------------------------------------------
--
-- Int32#
--

toZigZagI32# :: Int32# -> Word#
toZigZagI32# x = toZigZagI# (int32ToInt# x)

fromZigZagI32# :: Word# -> Int32#
fromZigZagI32# w = intToInt32# (fromZigZagI# w)
