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

import Control.Monad.IO.Class
import Data.Foldable (for_, toList)
import Prelude hiding (drop, fold, foldM, take)

newtype Jet i = Jet {runJet :: forall s. (s -> Bool) -> (s -> i -> IO s) -> s -> IO s} deriving (Functor)

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
          r <- action
          step initial r

each :: Foldable f => f a -> Jet a
each (toList -> seed) = Jet \stop step ->
  let go b s =
        if
            | stop s ->
              pure s
            | otherwise ->
              case b of
                [] ->
                  pure s
                -- see corresponding comment in unfold.
                x : xs ->
                  do
                    !s' <- step s x
                    go xs s'
   in go seed

unfold :: (b -> IO (Maybe (a, b))) -> b -> Jet a
unfold f seed = Jet \stop step ->
  let go b s =
        if
            | stop s ->
              pure s
            | otherwise ->
              do
                next <- f b
                case next of
                  Nothing ->
                    pure s
                  -- strictness only on the states. Good idea, or bad?
                  Just !(a, !b') ->
                    do
                      !s' <- step s a
                      go b' s'
   in go seed

untilEOF :: (h -> IO Bool) -> (h -> IO a) -> h -> Jet a
untilEOF hIsEOF hGetLine h = Jet \stop step ->
  let go s =
          if
              | stop s -> do
                pure s
              | otherwise -> do
                eof <- hIsEOF h
                if
                    | eof -> do
                      pure s
                    | otherwise -> do
                      line <- hGetLine h
                      s' <- step s line
                      go s'
   in go

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
