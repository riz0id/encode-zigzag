-- | Zigzag encoding of signed integers.
--
-- @since 1.0.0
module Data.Encode.ZigZag
  ( -- * ZigZag Encoding
    ZigZag,
    toZigZag,
    fromZigZag,
  )
where

import GHC.Int (Int64 (I64#), Int32 (I32#), Int16 (I16#), Int8 (I8#), Int (I#))
import GHC.Word (Word (W#))

import Data.Encode.ZigZag.Prim qualified as Prim

-- -----------------------------------------------------------------------------

-- | Instances of 'ZigZag' are signed 'Integral' types that can be encoded and
-- decoded from "Zigzag" form.
--
-- @since 1.0.0
class ZigZag a where
  toZigZag :: a -> Word

  fromZigZag :: Word -> a

-- | @since 1.0.0
instance ZigZag Int where
  toZigZag (I# n) = W# (Prim.toZigZagI# n)
  {-# INLINE toZigZag #-}

  fromZigZag (W# n) = I# (Prim.fromZigZagI# n)
  {-# INLINE fromZigZag #-}

-- | @since 1.0.0
instance ZigZag Int8 where
  toZigZag (I8# n) = W# (Prim.toZigZagI8# n)
  {-# INLINE toZigZag #-}

  fromZigZag (W# n) = I8# (Prim.fromZigZagI8# n)
  {-# INLINE fromZigZag #-}

-- | @since 1.0.0
instance ZigZag Int16 where
  toZigZag (I16# n) = W# (Prim.toZigZagI16# n)
  {-# INLINE toZigZag #-}

  fromZigZag (W# n) = I16# (Prim.fromZigZagI16# n)
  {-# INLINE fromZigZag #-}

-- | @since 1.0.0
instance ZigZag Int32 where
  toZigZag (I32# n) = W# (Prim.toZigZagI32# n)
  {-# INLINE toZigZag #-}

  fromZigZag (W# n) = I32# (Prim.fromZigZagI32# n)
  {-# INLINE fromZigZag #-}

-- | @since 1.0.0
instance ZigZag Int64 where
  toZigZag (I64# n) = W# (Prim.toZigZagI# n)
  {-# INLINE toZigZag #-}

  fromZigZag (W# n) = I64# (Prim.fromZigZagI# n)
  {-# INLINE fromZigZag #-}
