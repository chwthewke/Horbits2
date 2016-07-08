module Horbits.MatchersSpec where

import           Test.Hspec
import           Test.HUnit

import           Horbits.Matchers

spec :: Spec
spec =
    describe "Core matchers" $
        describe "equals" $ do
            it "should accept equal arguments" $
                pending -- 1 `should` equal 1
            it "should fail with different arguments" $
                pending -- (1 `should` equal 2) >>= (`shouldThrow` anyException)



