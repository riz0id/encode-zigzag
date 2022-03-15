{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-- | PrimOp compatibility wrapper.
--
-- @since 1.0.0
module Data.Encode.ZigZag.Compat
  ( -- * Int8#
    int8ToInt#,
    intToInt8#,

    -- * Int16#
    int16ToInt#,
    intToInt16#,

    -- * Int32#
    int32ToInt#,
    intToInt32#,
  )
where

#if MIN_VERSION_base(4,16,0)

import GHC.Exts (Int#, Int16#, Int32#, Int8#)
import qualified GHC.Exts as GHC

#elif MIN_VERSION_base(4,14,3)

import GHC.Exts (Int#)

#elif MIN_VERSION_base(4,13,0)

import GHC.Exts (Int#)

#endif

-- -----------------------------------------------------------------------------
--
-- 'Int8#'
--

#if MIN_VERSION_base(4,16,0)

int8ToInt# :: Int8# -> Int#
int8ToInt# = GHC.int8ToInt#

intToInt8# :: Int# -> Int8#
intToInt8# = GHC.intToInt8#

#else

int8ToInt# :: Int# -> Int#
int8ToInt# x = x

intToInt8# :: Int# -> Int#
intToInt8# x = x

#endif

-- -----------------------------------------------------------------------------
--
-- 'Int16#'
--

#if MIN_VERSION_base(4,16,0)

int16ToInt# :: Int16# -> Int#
int16ToInt# = GHC.int16ToInt#

intToInt16# :: Int# -> Int16#
intToInt16# = GHC.intToInt16#

#else

int16ToInt# :: Int# -> Int#
int16ToInt# x = x

intToInt16# :: Int# -> Int#
intToInt16# x = x

#endif

-- -----------------------------------------------------------------------------
--
-- 'Int32#'
--

#if MIN_VERSION_base(4,16,0)

int32ToInt# :: Int32# -> Int#
int32ToInt# = GHC.int32ToInt#

intToInt32# :: Int# -> Int32#
intToInt32# = GHC.intToInt32#

#else

int32ToInt# :: Int# -> Int#
int32ToInt# x = x

intToInt32# :: Int# -> Int#
intToInt32# x = x

#endif
