module AdderSpec where

import Test.Hspec
import Adder
import Binary


spec :: Spec
spec = do
  describe "when using half adder" $ do
    halfAdderSpec F F [F, F]
    halfAdderSpec T F [F, T]
    halfAdderSpec F T [F, T]
    halfAdderSpec T T [T, F]

  describe "when using full adder" $ do
    fullAdderSpec T T T [T, T]
    fullAdderSpec T T F [T, F]
    fullAdderSpec T F T [T, F]
    fullAdderSpec F T T [T, F]
    fullAdderSpec T F F [F, T]
    fullAdderSpec F F T [F, T]
    fullAdderSpec F T F [F, T]
    fullAdderSpec F F F [F, F]

  describe "when adding two numbers" $ do
    context "and numbers are the same length" $
      addSpec [T, T, F, T, T, T, F, T, F, T, F] [T, T, T, T, F, T, F, T, T, F, F] [T, T, T, F, T, F, F, T, F, T, T, F]
    context "and numbers are different length" $ do
      addSpec [T, F, T] [F, T, F] [T, T, T]
      addSpec [T, T, T] [T, T, F] [T, T, F, T]
      addSpec [] [T, T, F] [T, T, F]
      addSpec [T, F, F, T] [] [T, F, F, T]
      addSpec [T, T, T, T] [T] [T, F, F, F, F]
      addSpec [T] [T, T, T, T] [T, F, F, F, F]


halfAdderSpec :: Binary -> Binary -> [Binary] -> SpecWith ()
halfAdderSpec a b expected =
  context ("and adding " ++ show a ++ " and " ++ show b) $
    it ("should return " ++ show expected) $
      let actual = halfAdder a b
      in actual `shouldBe` expected


fullAdderSpec :: Binary -> Binary -> Binary -> [Binary] -> SpecWith ()
fullAdderSpec a b c expected =
  context ("and adding " ++ show a ++ " and " ++ show b ++ " and " ++ show c) $
    it ("should return " ++ toString expected) $
      let actual = fullAdder a b c
      in actual `shouldBe` expected


addSpec :: [Binary] -> [Binary] -> [Binary] -> SpecWith ()
addSpec a b expected =
  it (toString a ++ " plus " ++ toString b ++ " should equal " ++ toString expected) $
    let actual = add a b
    in actual `shouldBe` expected
