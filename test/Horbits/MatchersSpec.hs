module Horbits.MatchersSpec where

import           Test.Hspec
import           Test.HUnit
import           Test.HUnit.Lang

import           Horbits.Matchers

shouldSucceed :: Expectation -> () -> Expectation
shouldSucceed test _ = do
    r <- performTestCase test
    r `shouldBe` Success

shouldFailWith :: Expectation -> String -> Expectation
shouldFailWith test msg = do
    r <- performTestCase test
    case r of
        Failure _ m -> m `shouldBe` msg
        res         -> assertFailure ("Expected Failure (" ++ msg ++ "), got " ++ show res)

spec :: Spec
spec =
    describe "Core matchers" $
        describe "equals" $ do
            it "should accept equal arguments" $
                (1 `should` equal 1) `shouldSucceed` ()
            it "should fail with different arguments" $
                (1 `should` equal 2) `shouldFailWith` "Expected: 2, got: 1"



