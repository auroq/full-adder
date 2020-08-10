module BinarySpec where

import Test.Hspec
import Adder
import Binary


spec :: Spec
spec = do
  describe "when converting" $ do
    let bin = [T, F, T, T, F, T, F, F, T, F, T, T, F, T, F, F]
    let str = "1011010010110100"
    context (str ++ " to string") $
      it ("should equal " ++ str) $
        let actual = toString bin
        in actual `shouldBe` str
    context (str ++ " from string") $
      it ("should equal " ++ str) $
        let actual = fromString str
        in actual `shouldBe` bin

  describe "when converting to decimal" $ do
    toDecimalSpec [F] 0
    toDecimalSpec [T] 1
    toDecimalSpec [T, F] 2
    toDecimalSpec [T, T] 3
    toDecimalSpec [T, F, F] 4
    toDecimalSpec [T, F, T] 5
    toDecimalSpec [T, T, F] 6
    toDecimalSpec [T, T, T] 7
    toDecimalSpec [T, F, F, F] 8
    toDecimalSpec [T, F, F, F, F] 16
    toDecimalSpec [T, F, F, F, F, F] 32
    toDecimalSpec [T, F, F, F, F, F, F] 64
    toDecimalSpec [T, F, F, F, F, F, F, F] 128
    toDecimalSpec [T, F, F, F, F, F, F, F, F] 256
    toDecimalSpec [T, F, F, F, F, F, F, F, F, F] 512
    toDecimalSpec [T, F, F, F, F, F, F, F, F, F, F] 1024
    toDecimalSpec [T, F, F, F, F, F, F, F, F, F, F, F] 2048
    toDecimalSpec [T, F, F, F, F, F, F, F, F, F, F, F, F] 4096
    toDecimalSpec [T, F, F, F, F, F, F, F, F, F, F, F, F, F] 8192
    toDecimalSpec [T, F, F, F, F, F, F, F, F, F, F, F, F, F, F] 16384
    toDecimalSpec [T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F] 32768
    toDecimalSpec [T, F, T, T, F, T, F, F, T, F, T, T, F, T, F, F] 46260

  describe "when converting to decimal" $ do
    fromDecimalSpec 0 [F]
    fromDecimalSpec 1 [T]
    fromDecimalSpec 2 [T, F]
    fromDecimalSpec 3 [T, T]
    fromDecimalSpec 4 [T, F, F]
    fromDecimalSpec 5 [T, F, T]
    fromDecimalSpec 6 [T, T, F]
    fromDecimalSpec 7 [T, T, T]
    fromDecimalSpec 8 [T, F, F, F]
    fromDecimalSpec 16 [T, F, F, F, F]
    fromDecimalSpec 32 [T, F, F, F, F, F]
    fromDecimalSpec 64 [T, F, F, F, F, F, F]
    fromDecimalSpec 128 [T, F, F, F, F, F, F, F]
    fromDecimalSpec 256 [T, F, F, F, F, F, F, F, F]
    fromDecimalSpec 512 [T, F, F, F, F, F, F, F, F, F]
    fromDecimalSpec 1024 [T, F, F, F, F, F, F, F, F, F, F]
    fromDecimalSpec 2048 [T, F, F, F, F, F, F, F, F, F, F, F]
    fromDecimalSpec 4096 [T, F, F, F, F, F, F, F, F, F, F, F, F]
    fromDecimalSpec 8192 [T, F, F, F, F, F, F, F, F, F, F, F, F, F]
    fromDecimalSpec 16384 [T, F, F, F, F, F, F, F, F, F, F, F, F, F, F]
    fromDecimalSpec 32768 [T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F]
    fromDecimalSpec 46260 [T, F, T, T, F, T, F, F, T, F, T, T, F, T, F, F]


toDecimalSpec :: [Binary] -> Integer -> SpecWith ()
toDecimalSpec bin expected =
  it (toString bin ++ " should equal " ++ show expected) $
    let actual = toDecimal bin
    in actual `shouldBe` expected

fromDecimalSpec :: Integer -> [Binary] -> SpecWith ()
fromDecimalSpec int expected =
  it (show int ++ " should equal " ++ toString expected) $
    let actual = fromDecimal int
    in actual `shouldBe` expected
