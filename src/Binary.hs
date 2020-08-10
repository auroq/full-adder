module Binary
  ( toString
  , fromString
  , fromBool
  , toBool
  , toDecimal
  , Binary (..)
  ) where

import Data.List

class AsBool a where
  toBool :: a -> Bool
  fromBool :: Bool -> a

data Binary = T | F deriving Eq
instance Show Binary where
  show T = "1"
  show F = "0"
instance AsBool Binary where
  toBool T = True
  toBool F = False
  fromBool True = T
  fromBool False = F

toString :: [Binary] -> String
toString lst = intercalate "" $ map show lst

fromString :: String -> [Binary]
fromString = map toBin
  where toBin '1' = T
        toBin  _  = F

toDecimal :: [Binary] -> Integer
toDecimal = toDecimal' 1 . reverse
  where toDecimal' :: Integer -> [Binary] -> Integer
        toDecimal' acc (T:bs) = acc + toDecimal' (acc * 2) bs
        toDecimal' acc (F:bs) = 0 + toDecimal' (acc * 2) bs
        toDecimal' acc [] = 0
