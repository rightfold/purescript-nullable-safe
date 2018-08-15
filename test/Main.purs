module Test.Main
  ( main
  ) where

import Data.Nullable.Safe

import Effect (Effect)
import Prelude (Unit, ($), (+), discard)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Tap (tapReporter)

main :: Effect Unit
main = run [tapReporter] $
  describe "Data.Nullable.Safe" do
    describe "eq" $ do
      it "null == null" $ (null :: Nullable Int) `shouldEqual` null
      it "just == just" $ just 1 `shouldEqual` just 1
      it "just /= just" $ just 1 `shouldNotEqual` just 2
      it "just /= null" $ just 1 `shouldNotEqual` null
    describe "nullable" $ do
      it "null" $ nullable 1 (_ + 1) null `shouldEqual` 1
      it "just" $ nullable 1 (_ + 1) (just 1) `shouldEqual` 2
    describe "neverNull" $ do
      it "null" $ neverNull (null :: Nullable Int) 1 `shouldEqual` 1
      it "just" $ neverNull (just 1) 1 `shouldEqual` 1
    describe "mapNullable" $ do
      it "null" $ mapNullable (_ + 1) null `shouldEqual` null
      it "just" $ mapNullable (_ + 1) (just 1) `shouldEqual` just 2
