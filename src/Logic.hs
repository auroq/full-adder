module Logic
  ( xor
  , Logic.or
  , Logic.and
  , Logic.not
  , nand
  , nor
  ) where

import Data.List
import Binary (fromBool, toBool, Binary(..))

and :: Binary -> Binary -> Binary
and a b = fromBool $ toBool a && toBool b

or :: Binary -> Binary -> Binary
or a b = fromBool $ toBool a || toBool b

nand :: Binary -> Binary -> Binary
nand a b = Logic.not $ a `Logic.and` b

nor :: Binary -> Binary -> Binary
nor a b = Logic.not $ a `Logic.or` b

xor :: Binary -> Binary -> Binary
xor a b = fromBool $ a /= b

not :: Binary -> Binary
not T = F
not F = T
