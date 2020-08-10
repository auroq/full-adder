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
    decimalSpec [F] 0
    decimalSpec [T] 1
    decimalSpec [T, F] 2
    decimalSpec [T, T] 3
    decimalSpec [T, F, F] 4
    decimalSpec [T, F, T] 5
    decimalSpec [T, T, F] 6
    decimalSpec [T, T, T] 7
    decimalSpec [T, F, F, F] 8
    decimalSpec [T, F, F, F, F] 16
    decimalSpec [T, F, F, F, F, F] 32
    decimalSpec [T, F, F, F, F, F, F] 64
    decimalSpec [T, F, F, F, F, F, F, F] 128
    decimalSpec [T, F, F, F, F, F, F, F, F] 256
    decimalSpec [T, F, F, F, F, F, F, F, F, F] 512
    decimalSpec [T, F, F, F, F, F, F, F, F, F, F] 1024
    decimalSpec [T, F, F, F, F, F, F, F, F, F, F, F] 2048
    decimalSpec [T, F, F, F, F, F, F, F, F, F, F, F, F] 4096
    decimalSpec [T, F, F, F, F, F, F, F, F, F, F, F, F, F] 8192
    decimalSpec [T, F, F, F, F, F, F, F, F, F, F, F, F, F, F] 16384
    decimalSpec [T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F] 32768
    decimalSpec [T, F, T, T, F, T, F, F, T, F, T, T, F, T, F, F] 46260


decimalSpec :: [Binary] -> Integer -> SpecWith ()
decimalSpec bin expected =
  it (toString bin ++ " should equal " ++ show expected) $
    let actual = toDecimal bin
    in actual `shouldBe` expected
