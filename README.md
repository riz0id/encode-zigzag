# `encode-zigzag` - `1.0.0`

An implementation of "[Zigzag encoding](https://en.wikipedia.org/wiki/Variable-length_quantity#Zigzag_encoding)" of signed integer types in Haskell. The encoder and decoder pairs provided here are fast (implemented via GHC PrimOps) and cautious of integer underflow/overflow. 

## Requirements 

`encode-zigzag` is compatible with `GHC` versions as far back as `8.8.4` with `base-4.13.0`.

## Example

The `ZigZag` typeclass provides instances for `Int`, `Int8`, `Int16`, and `Int32`.

```haskell
class ZigZag a where
  toZigZag :: a -> Word
  fromZigZag :: Word -> a
  
instance ZigZag Int   where ...
instance ZigZag Int8  where ...
instance ZigZag Int16 where ...
instance ZigZag Int32 where ...
```

`toZigZag` encodes the signed integer type as 32-bit or 64-bit word (depending on the value of `WORD_SIZE_IN_BITS` in `#include "MachDeps.h"`, except for GHC-9.2.1 which always use 64-bit word size to represent `Word`).

```haskell
>>> :set -XTypeApplications
>>> toZigZag (maxBound @Int)
18446744073709551614
>>> map (toZigZag @Int) [0, 1, -1, 2, -2, 3, -3, 4, -4, 5, -5]
[0,2,1,4,3,6,5,8,7,10,9]
```

`fromZigZag` can then be used to decode.

```haskell
>>> fromZigZag @Int 18446744073709551614
9223372036854775807
>>> map (fromZigZag @Int) [0,2,1,4,3,6,5,8,7,10,9]
[0,1,-1,2,-2,3,-3,4,-4,5,-5]
```
