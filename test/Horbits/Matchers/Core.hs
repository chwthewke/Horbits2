{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Horbits.Matchers.Core(
    MatchResult(..), type Matcher1, type Matcher, verifyMatch, describeMismatch, describeMatch
  , matcher, should) where

import           Control.Applicative
import           Control.Arrow           hiding ((|||))
import           Control.Lens
import           Control.Monad
import           Data.List               (intercalate)
import           Test.Hspec.Expectations
import           Test.HUnit
import           Test.HUnit.Lang         (Result (..), performTestCase)

-- Data

data MatchResult a = MatchResult { matchResult :: Maybe a
                                 , mismatch    :: [String]
                                 , match       :: [String]
                                 } deriving (Eq, Show)

instance Functor MatchResult where
    fmap = liftM

instance Applicative MatchResult where
    pure = return
    (<*>) = ap

instance Monad MatchResult where
    return a = MatchResult (Just a) [] []
    ra >>= f = maybe failed (combineResult . f) (matchResult ra)
      where
        failed = MatchResult Nothing (mismatch ra) (match ra)
        combineResult rb =
            MatchResult
                (matchResult rb)
                (mismatch rb)
                (match ra ++ match rb)


type Matcher1 = Kleisli MatchResult

type Matcher a = Matcher1 a ()

verifyMatch :: Matcher1 a b -> a -> Maybe b
verifyMatch = fmap matchResult . runKleisli

describe :: (a -> [String]) -> a -> String
describe f = intercalate ", " . f

describeMismatch :: Matcher1 a b -> a -> String
describeMismatch = fmap (describe mismatch) . runKleisli

describeMatch :: Matcher1 a b -> a -> String
describeMatch = fmap (describe match) . runKleisli

matcher :: (a -> Maybe b) -> (a -> String) -> (a -> String) -> Matcher1 a b
matcher verify descMismatch descMatch =
    Kleisli $ liftA3 MatchResult verify (pure . descMismatch) (pure . descMatch)

-- Run

toExpectation :: MatchResult a -> Expectation
toExpectation r = maybe (assertFailure . describe mismatch $ r) (void . return) (matchResult r)

should :: a -> Matcher a -> Expectation
should a m = toExpectation $ runKleisli m a

-- meta expectations

shouldSucceed :: Expectation -> () -> Expectation
shouldSucceed test _ = do
    r <- performTestCase test
    r `should` equal Success

-- Combinators

not' :: Matcher1 a b -> Matcher a
not' (Kleisli m) = Kleisli $ \a -> MatchResult
    (matchResult (m a) ^? _Nothing)
    (match $ m a)
    (mismatch $ m a)

(|||) :: Matcher1 a b -> Matcher1 a b' -> Matcher1 a (Either b b')
(|||) = undefined

(<&&&>) :: Matcher a -> Matcher a -> Matcher a
m <&&&> m' = void $ m &&& m'

(<|||>) :: Matcher a -> Matcher a -> Matcher a
m <|||> m' = void $ m ||| m'


-- Lib

equal :: (Show a, Eq a) => a -> Matcher a
equal expected = matcher (\a -> when (a == expected) $ return ()) undefined undefined

isA :: Show a => String -> (a -> Maybe b) -> Matcher1 a b
isA name f = matcher
    f
    (\a -> show a ++ " is not a " ++ name)
    (\a -> show a ++ " is a " ++ name)

--
-- -- matchTestResultWith :: Matcher String -> Matcher Result
-- -- matchTestResultWith
-- --
-- -- failWith :: Matcher String -> IO (Matcher Expectation)
-- -- failWith messageMatcher = Matcher v d d' >$$< evalTest
-- --   where evalTest t =
