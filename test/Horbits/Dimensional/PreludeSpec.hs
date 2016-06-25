{-# LANGUAGE NoImplicitPrelude #-}

module Horbits.Dimensional.PreludeSpec where

import           Control.Monad.Identity             (Identity (..))
import           Numeric.Units.Dimensional.NonSI
import           Test.Hspec
import           Test.QuickCheck

import           Horbits.Dimensional.Prelude
import           Numeric.Units.Dimensional.Coercion (Dimensional (Quantity),
                                                     coerce)



valueOf :: Quantity d a -> a
valueOf = coerce

spec :: Spec
spec = describe "Prelude" $ do
    describe "^*~" $ do
        it "wraps functor with a unit" $
            (runIdentity . valueOf $ Identity 1.0 ^*~ meter) `shouldBe` 1.0
        it "preserves the value passed" $
            (runIdentity . valueOf $ Identity 10.0 ^*~ meter) `shouldBe` 10.0
        it "applies an SI unit's scale" $
            (runIdentity . valueOf $ Identity 1.0 ^*~ kilo meter) `shouldBe` 1000.0
        it "applies a non-SI unit's scale" $
            (runIdentity . valueOf $ Identity 1.0 ^*~ yard) `shouldBe` 0.9144
    describe "^/~" $ do
        it "gives back a non-dimensional value" $
            (Identity 1.0 ^*~ second) ^/~ second `shouldBe` 1.0
        it "preserves the dimensional's (scaled) value" $
            (Identity 2.0 ^*~ minute) ^/~ second `shouldBe` 120.0
