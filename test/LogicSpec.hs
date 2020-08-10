module LogicSpec where

import Test.Hspec
import Data.String

import Binary (Binary (..))
import Logic


spec :: Spec
spec = do
  context "when calling and" $ do
    logicSpec "and" Logic.and F F F
    logicSpec "and" Logic.and T F F
    logicSpec "and" Logic.and F T F
    logicSpec "and" Logic.and T T T

  context "when calling or" $ do
    logicSpec "or" Logic.or F F F
    logicSpec "or" Logic.or T F T
    logicSpec "or" Logic.or F T T
    logicSpec "or" Logic.or T T T

  context "when calling nand" $ do
    logicSpec "nand" Logic.nand F F T
    logicSpec "nand" Logic.nand T F T
    logicSpec "nand" Logic.nand F T T
    logicSpec "nand" Logic.nand T T F

  context "when calling nor" $ do
    logicSpec "nor" Logic.nor F F T
    logicSpec "nor" Logic.nor T F F
    logicSpec "nor" Logic.nor F T F
    logicSpec "nor" Logic.nor T T F

  context "when calling xor" $ do
    logicSpec "xor" Logic.xor F F F
    logicSpec "xor" Logic.xor T F T
    logicSpec "xor" Logic.xor F T T
    logicSpec "xor" Logic.xor T T F

  describe "when calling not" $ do
    notSpec Logic.not F T
    notSpec Logic.not T F


logicSpec :: String -> (Binary -> Binary -> Binary) -> Binary -> Binary -> Binary -> SpecWith ()
logicSpec description func a b expected =
  context (show a ++ " " ++ description ++ " " ++ show b) $
    it ("should resolve to " ++ show expected) $
      let actual = func a b
      in actual `shouldBe` expected

notSpec :: (Binary -> Binary) -> Binary -> Binary -> SpecWith ()
notSpec func a expected =
  context ("not" ++ show a) $
    it ("should resolve to " ++ show expected) $
      let actual = func a
      in actual `shouldBe` expected
