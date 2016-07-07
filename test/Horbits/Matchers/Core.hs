{-# LANGUAGE TypeSynonymInstances #-}

module Horbits.Matchers.Core where

import           Control.Monad
import           Data.Functor.Contravariant
import           Test.Hspec.Expectations
import           Test.HUnit

-- TODO Profunctor?
data Matcher1 a b = Matcher { verifyMatch      :: a -> Maybe b
                            , describeMismatch :: a -> String
                            , describeMatch    :: a -> String
                            }

andThen :: Matcher1 a b -> Matcher1 b c -> Matcher1 a c
andThen m m' = Matcher
    (verifyMatch m >=> verifyMatch m')
    mismatch
    match
  where
    mismatch a = combine (describeMismatch m a) (describeMismatch m') a
    match a = combine (describeMatch m a) (describeMatch m') a
    combine desc desc' = maybe desc (\b -> desc ++ " " ++ desc' b) . verifyMatch m

transform :: (b -> c) -> Matcher1 a b -> Matcher1 a c
transform f m = Matcher (fmap f . verifyMatch m) (describeMismatch m) (describeMatch m)

type Matcher a = Matcher1 a ()

--instance Contravariant Matcher where
contramap :: (b -> a) -> Matcher a -> Matcher b
contramap f m = Matcher
        (verifyMatch m . f)
        (describeMismatch m . f)
        (describeMatch m . f)

expect :: Matcher a -> a -> Expectation
expect m a =
    unless (verifyMatch m a == Just ()) . assertFailure $ describeMismatch m a

equal :: (Show a, Eq a) => a -> Matcher a
equal expected = Matcher (\a -> when (a == expected) $ return ()) undefined undefined

should :: a -> Matcher a -> Expectation
should = flip expect

isA :: Show a => String -> (a -> Maybe b) -> Matcher1 a b
isA name unap = Matcher
    unap
    (\a -> show a ++ " is not a " ++ name)
    (\a -> show a ++ " is a " ++ name)

-- matchTestResultWith :: Matcher String -> Matcher Result
-- matchTestResultWith
--
-- failWith :: Matcher String -> IO (Matcher Expectation)
-- failWith messageMatcher = Matcher v d d' >$$< evalTest
--   where evalTest t =
