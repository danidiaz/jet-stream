{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Jet.Internal where

import Control.Applicative
import Control.Monad hiding (replicateM, zipWithM)
import Control.Monad.IO.Class
import Data.Foldable qualified
import Data.Foldable (for_)
import Prelude hiding (drop, dropWhile, fold, foldM, take, takeWhile, unfold, zip, zipWith)

newtype Jet a = Jet {runJet :: forall s. (s -> Bool) -> (s -> a -> IO s) -> s -> IO s} deriving (Functor)

instance Applicative Jet where
  pure i = Jet \stop step initial ->
    if
        | stop initial -> pure initial
        | otherwise -> step initial i
  Jet left <*> Jet right = Jet \stop step initial ->
    -- Here we assume that the first Jet correctly handles the stop signal.
    let step' f s a = step s (f a)
     in left stop (\s f -> right stop (step' f) s) initial

instance Monad Jet where
  return = pure
  Jet m >>= k = Jet \stop step initial ->
    m stop (\s a -> runJet (k a) stop step s) initial

instance MonadIO Jet where
  liftIO action = Jet \stop step initial ->
    if
        | stop initial -> pure initial
        | otherwise -> do
          a <- action
          step initial a

instance Semigroup (Jet a) where
  Jet f1 <> Jet f2 = Jet \stop step s0 -> do
    if
        | stop s0 ->
          pure s0
        | otherwise -> do
          !s1 <- f1 stop step s0
          if
              | stop s1 ->
                pure s1
              | otherwise -> do
                !s2 <- f2 stop step s1
                pure s2

instance Monoid (Jet a) where
  mempty = Jet \_ _ initial -> pure initial

instance Alternative Jet where
  (<|>) = (<>)
  empty = mempty

instance MonadPlus Jet where
  mzero = mempty
  mplus = (<>)

instance MonadFail Jet where
  fail _ = mzero

each :: Foldable f => f a -> Jet a
each (Data.Foldable.toList -> seed) = Jet \stop step ->
  let go b s =
        if
            | stop s ->
              pure s
            | otherwise ->
              case b of
                [] ->
                  pure s
                -- see corresponding comment in unfold.
                x : xs -> do
                  !s' <- step s x
                  go xs s'
   in go seed

repeat :: a -> Jet a
repeat a = repeatM (pure a)

repeatM :: IO a -> Jet a
repeatM ioa = Jet \stop step -> do
  let go s =
        if
            | stop s ->
              pure s
            | otherwise -> do
              a <- ioa
              !s' <- step s a
              go s'
  go

replicate :: Int -> a -> Jet a
replicate n a = replicateM n (pure a)

replicateM :: Int -> IO a -> Jet a
replicateM n ioa = take n (repeatM ioa)

iterate :: (a -> a) -> a -> Jet a
iterate h = iterateM (fmap pure h)

iterateM :: (a -> IO a) -> a -> Jet a
iterateM h a0 = Jet \stop step ->
  let go a s =
        if
            | stop s ->
              pure s
            | otherwise -> do
              !s' <- step s a
              !a' <- h a
              go a' s'
   in go a0

unfold :: (b -> Maybe (a, b)) -> b -> Jet a
unfold h = unfoldM (fmap pure h)

unfoldM :: (b -> IO (Maybe (a, b))) -> b -> Jet a
unfoldM f seed = Jet \stop step ->
  let go b s =
        if
            | stop s ->
              pure s
            | otherwise -> do
              next <- f b
              case next of
                Nothing ->
                  pure s
                -- strictness only on the states. Good idea, or bad?
                Just !(a, !b') -> do
                  !s' <- step s a
                  go b' s'
   in go seed

untilEOF :: (h -> IO Bool) -> (h -> IO a) -> h -> Jet a
untilEOF hIsEOF' hGetLine' handle = Jet \stop step ->
  let go s =
        if
            | stop s -> do
              pure s
            | otherwise -> do
              eof <- hIsEOF' handle
              if
                  | eof -> do
                    pure s
                  | otherwise -> do
                    line <- hGetLine' handle
                    s' <- step s line
                    go s'
   in go


-- | Convert to a regular list. This breaks streaming.
--
-- Equivalent to 
--
-- > Control.Foldl.purely Jet.fold Control.Foldl.list
--
-- which is more composable.
toList :: Jet a -> IO [a]
toList (Jet f) = do
    as <- f (const False) (\xs x -> pure (x : xs)) []
    pure (reverse as)

-- | Returns the number of elements yielded by the 'Jet'.
--
-- Equivalent to 
--
-- > Control.Foldl.purely Jet.fold Control.Foldl.length
--
-- which is more composable.
length :: Jet a -> IO Int
length (Jet f) = do
    l <- f (const False) (\s _ -> pure (succ s)) 0
    pure l

data Pair a b = Pair !a !b

pairExtract (Pair _ b) = b

pairEnv (Pair a _) = a

-- fromTuple :: (a, b) -> Pair a b
-- fromTuple (a, b) -> Pair a b

drop :: Int -> Jet a -> Jet a
drop limit (Jet f) = Jet \stop step initial -> do
  let stop' = stop . pairExtract
      step' (Pair count s) a =
        if
            | count < limit -> do
              pure (Pair (succ count) s)
            | otherwise -> do
              !s' <- step s a
              pure (Pair count s')
      initial' = Pair 0 initial
  Pair _ final <- f stop' step' initial'
  pure final

data DropState = StillDropping | DroppingNoMore

dropWhile :: (a -> Bool) -> Jet a -> Jet a
dropWhile p = dropWhileM (fmap pure p)

dropWhileM :: (a -> IO Bool) -> Jet a -> Jet a
dropWhileM p (Jet f) = Jet \stop step initial -> do
  let stop' = stop . pairExtract
      step' (Pair DroppingNoMore s) a = do
        !s' <- step s a
        pure (Pair DroppingNoMore s')
      step' (Pair StillDropping s) a = do
        keepDropping <- p a
        if
            | keepDropping ->
              pure (Pair StillDropping s)
            | otherwise -> do
              !s' <- step s a
              pure (Pair DroppingNoMore s')
      initial' = (Pair StillDropping initial)
  Pair _ final <- f stop' step' initial'
  pure final

take :: Int -> Jet a -> Jet a
take limit (Jet f) = Jet \stop step initial -> do
  let stop' (Pair count s) =
        count >= limit || stop s
      step' (Pair count s) a = do
        !s' <- step s a
        pure (Pair (succ count) s')
      initial' = Pair 0 initial
  Pair _ final <- f stop' step' initial'
  pure final

data TakeState = StillTaking | TakingNoMore

takeWhile :: (a -> Bool) -> Jet a -> Jet a
takeWhile p = takeWhileM (fmap pure p)

takeWhileM :: (a -> IO Bool) -> Jet a -> Jet a
takeWhileM p (Jet f) = Jet \stop step initial -> do
  let stop' (Pair TakingNoMore _) =
        True
      stop' (Pair StillTaking s) =
        stop s
      step' (Pair internal s) a = do
        keepTaking <- p a
        if
            | keepTaking -> do
              !s' <- step s a
              pure (Pair internal s')
            | otherwise ->
              pure (Pair TakingNoMore s)
      initial' = Pair StillTaking initial
  Pair _ final <- f stop' step' initial'
  pure final

-- | Behaves like a combination of 'fmap' and 'foldl'; it applies a function to
-- each element of a structure passing an accumulating parameter from left to right.
--
-- The resulting 'Jet' has the same number of elements as the original one.
--
-- One difference with 'Data.Traversable.mapAccumL' is that the current value
-- of the accumulator is yielded along with each element, instead of only
-- returning the final value at the end.
mapAccum :: (a -> b -> (a, c)) -> a -> Jet b -> Jet (a, c)
mapAccum stepAcc = mapAccumM (fmap (fmap pure) stepAcc)

mapAccumM :: (a -> b -> IO (a, c)) -> a -> Jet b -> Jet (a, c)
mapAccumM stepAcc initialAcc (Jet f) = Jet \stop step initial -> do
  let stop' = stop . pairExtract
      step' (Pair acc s) b = do
        pair@(acc', _) <- stepAcc acc b
        !s' <- step s pair
        pure (Pair acc' s')
      initial' = Pair initialAcc initial
  Pair _ final <- f stop' step' initial'
  pure final

zip :: [a] -> Jet b -> Jet (a, b)
zip = zipWith (,)

zipWith :: (a -> b -> c) -> [a] -> Jet b -> Jet c
zipWith zf as0 = zipWithM (fmap (fmap pure) zf) (map pure as0)

zipM :: [IO a] -> Jet b -> Jet (a, b)
zipM = zipWithM (\x y -> pure (x, y))

zipWithM :: (a -> b -> IO c) -> [IO a] -> Jet b -> Jet c
zipWithM zf ioas0 (Jet f) = Jet \stop step initial -> do
  let stop' (Pair [] _) = True
      stop' (Pair _ s) = stop s
      step' (Pair (ioa : ioas) s) b = do
        a <- ioa
        z <- zf a b
        !s' <- step s z
        pure (Pair ioas s')
      step' (Pair [] _) _ = error "never happens"
      initial' = Pair ioas0 initial
  Pair _ final <- f stop' step' initial'
  pure final

control :: forall s a resource. (forall x. (resource -> IO x) -> IO x) -> Jet resource
control f =
  Jet \stop step initial ->
    if
        | stop initial ->
          pure initial
        | otherwise -> do
          f (step initial)

control_ :: forall s a. (forall x. IO x -> IO x) -> Jet ()
control_ f =
  Jet \stop step initial ->
    if
        | stop initial -> do
          pure initial
        | otherwise -> do
          f (step initial ())

fold :: Jet a -> (s -> a -> s) -> s -> (s -> r) -> IO r
fold (Jet f) step initial coda = do
  r <- f (const False) (fmap (fmap pure) step) initial
  pure $ coda r

foldM :: Jet a -> (s -> a -> IO s) -> IO s -> (s -> IO r) -> IO r
foldM (Jet f) step initialM coda = do
  initial <- initialM
  r <- f (const False) step initial
  coda r
