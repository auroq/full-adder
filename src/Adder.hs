module Adder
  ( halfAdder
  , fullAdder
  , add
  ) where

import Data.List

import Binary (Binary (..))
import Logic (or, xor, and)

halfAdder :: Binary -> Binary -> [Binary]
halfAdder a b = [Logic.and a b, Logic.xor a b]

add :: [Binary] -> [Binary] -> [Binary]
add as bs = dropWhile (== F) $ reverse $ add' F (reverse as) (reverse bs)
  where add' :: Binary -> [Binary] -> [Binary] -> [Binary]
        add' acc []  []  = [acc]
        add' acc as  []  = add' acc as [F]
        add' acc []  bs  = add' acc [F] bs
        add' acc (a:as') (b:bs') =
          let (l:r:_) = fullAdder a b acc
          in r : add' l as' bs'

fullAdder :: Binary -> Binary -> Binary -> [Binary]
fullAdder a b c =
  let (a':b':_) = halfAdder a b
      ha2 = halfAdder b' c
      l   = a' `Logic.or` head ha2
      r   = (head . tail) ha2
  in [l, r]
