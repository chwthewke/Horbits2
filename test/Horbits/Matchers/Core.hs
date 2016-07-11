{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Horbits.Matchers.Core(
    MatchResult(..), type Matcher1, type Matcher, verifyMatch, describeMismatch
  , defaultMatcher, should, equal, not', (<&&>), (<||>), (>&&<), (>||<)) where

import           Control.Applicative
import           Control.Arrow           hiding ((|||))
import           Control.Lens
import           Control.Monad
import           Data.List               (intercalate)
import           Test.Hspec.Expectations
import           Test.HUnit
import           Test.HUnit.Lang         (Result (..), performTestCase)

-- Data

data MatchResult a = MatchResult { matchResult         :: Maybe a
                                 , description         :: String
                                 , mismatchDescription :: String
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
        failed = MatchResult Nothing (description ra) (mismatchDescription ra)
        combineResult rb =
            let combinedDescription = description ra ++ ", such that " ++ description rb in
            MatchResult (matchResult rb) combinedDescription combinedDescription

type Matcher1 = Kleisli MatchResult

type Matcher a = Matcher1 a ()

verifyMatch :: Matcher1 a b -> a -> Maybe b
verifyMatch = fmap matchResult . runKleisli


defaultMatcher :: (a -> Maybe b) -> String -> Matcher1 a b
defaultMatcher verify descr =
    Kleisli $ \a -> MatchResult (verify a) descr descr

-- Run

describeMismatch :: Show a => a -> MatchResult b -> String
describeMismatch a (MatchResult _ _ d) = "Expected: " ++ d ++ ", got: " ++ show a


toExpectation :: Show a => a -> MatchResult b -> Expectation
toExpectation a r = maybe
    (assertFailure $ describeMismatch a r)
    (void . return)
    (matchResult r)

should :: Show a => a -> Matcher a -> Expectation
should a m = toExpectation a $ runKleisli m a


-- Combinators

not' :: Matcher1 a b -> Matcher a
not' (Kleisli m) = Kleisli $ \a ->
    MatchResult
        (matchResult (m a) ^? _Nothing)
        ("not " ++ description (m a))
        ("not " ++ mismatchDescription (m a))


(<&&>) :: Matcher1 a b -> Matcher1 a b' -> Matcher1 a (b, b')
(<&&>) m m' = Kleisli $ \a ->
    let MatchResult r d mis = runKleisli m a in
    let MatchResult r' d' mis' = runKleisli m' a in
    let desc = d ++ " and " ++ d' in
    maybe
        (MatchResult Nothing desc mis)
        (\res -> MatchResult (fmap (res,) r') desc (mis ++ ", " ++ mis'))
        r

(<||>) :: Matcher1 a b -> Matcher1 a b' -> Matcher1 a (Either b b')
(<||>) m m' = Kleisli $ \a ->
    let MatchResult r d mis = runKleisli m a in
    let MatchResult r' d' mis' = runKleisli m' a in
    let desc = d ++ " or " ++ d' in
    maybe
        (MatchResult (fmap Right r') desc (mis ++ ", " ++ mis'))
        (\res -> MatchResult (Just $ Left res) desc mis)
        r

asMatcher :: Matcher1 a b -> Matcher a
asMatcher m = m & _Wrapped %~ fmap void


(>&&<) :: Matcher a -> Matcher a -> Matcher a
m >&&< m' = asMatcher $ m <&&> m'

(>||<) :: Matcher a -> Matcher a -> Matcher a
m >||< m' = asMatcher $ m <||> m'


-- Lib

equal :: (Show a, Eq a) => a -> Matcher a
equal expected =
    defaultMatcher
        (\a -> if (a == expected) then Just () else Nothing)
        (show expected)

isA :: Show a => String -> (a -> Maybe b) -> Matcher1 a b
isA name f = defaultMatcher f ("a " ++ name)

--
-- -- matchTestResultWith :: Matcher String -> Matcher Result
-- -- matchTestResultWith
-- --
-- -- failWith :: Matcher String -> IO (Matcher Expectation)
-- -- failWith messageMatcher = Matcher v d d' >$$< evalTest
-- --   where evalTest t =
