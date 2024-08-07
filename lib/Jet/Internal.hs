{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Tampering with the internals lets you write invalid 'Jet's that don't
-- respect stop signals from consumers, so be careful.
--
-- Also, the internals expose 'Line' and 'ByteBundle' as thin coats of paint
-- over lazy text and lazy bytestring, respectively.
module Jet.Internal where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable qualified
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef
import Data.List qualified
import Data.Maybe
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Traversable qualified
import Data.Typeable
import System.Exit
import System.IO (Handle, IOMode (..), hClose, openBinaryFile)
import System.IO qualified
import System.Process
import Prelude hiding
  ( drop,
    dropWhile,
    filter,
    filterM,
    fold,
    for_,
    intersperse,
    lines,
    take,
    takeWhile,
    traverse_,
    unfold,
    unlines,
    zip,
    zipWith,
  )
import Prelude qualified

-- import Debug.Trace

-- $setup
--
-- >>> :set -XTypeApplications
-- >>> :set -XImportQualifiedPost
-- >>> :set -XScopedTypeVariables
-- >>> :set -XLambdaCase
-- >>> :set -XNumDecimals
-- >>> import Jet (Jet, (&))
-- >>> import Jet qualified as J
-- >>> import Control.Foldl qualified as L
-- >>> import Control.Concurrent
-- >>> import Data.IORef
-- >>> import Data.Text qualified as T

-- | A 'Jet' is a sequence of values produced through 'IO' effects.
--
-- It allows consuming the elements as they are produced and doesn't force them
-- to be present in memory all at the same time, unlike functions like
-- 'Control.Monad.replicateM' from @base@.
newtype Jet a = Jet
  { runJet :: forall s. (s -> Bool) -> (s -> a -> IO s) -> s -> IO s
  }

-- | Maps over the yielded elements. '(<&>)' can be used to put the function last.
--
-- >>> J.each "aa" <&> succ & J.toList
-- "bb"
deriving stock instance Functor Jet

-- | Go through the elements produced by a 'Jet', while threading an
-- state @s@ and possibly performing some effect.
--
-- The caller is the one who chooses the type of the state @s@, and must pass
-- an initial value for it. The state is kept in [weak-head normal form](https://en.wikibooks.org/wiki/Haskell/Graph_reduction#Weak_Head_Normal_Form).
--
-- The caller must also provide a predicate on the state that informs the `Jet`
-- when to stop producing values: whenever the predicate returns
-- @True@.
run :: forall a s. Jet a -> (s -> Bool) -> (s -> a -> IO s) -> s -> IO s
run j = runJet j

-- | Like 'run', but always goes through all elements produced by the 'Jet'.
--
-- Equivalent to @run (const False)@.
consume :: forall a s. Jet a -> (s -> a -> IO s) -> s -> IO s
consume j = run j (const False)

for :: Jet a -> (a -> IO b) -> Jet b
for j k = zipWithIO (\() -> k) (Prelude.repeat (pure ())) j

for_ :: Jet a -> (a -> IO b) -> IO ()
for_ j k = consume j (\() -> void <$> k) ()

-- | Apply an effectful transformation to each element in a 'Jet'.
--
-- >>> :{
-- J.each "abc"
-- & J.traverse (\c -> let c' = succ c in putStrLn ([c] ++ " -> " ++ [c']) *> pure c')
-- & J.toList
-- :}
-- a -> b
-- b -> c
-- c -> d
-- "bcd"
traverse :: (a -> IO b) -> Jet a -> Jet b
traverse = flip for

traverse_ :: (a -> IO b) -> Sink a
traverse_ = flip for_

-- | Go through the 'Jet' only for the 'IO' effects, discarding all yielded elements.
drain :: Sink a
drain = traverse_ pure

-- | Similar to the instance for pure lists, that generates combinations.
--
-- >>> (,) <$> J.each "ab" <*> J.each "cd" & J.toList
-- [('a','c'),('a','d'),('b','c'),('b','d')]
instance Applicative Jet where
  pure i = Jet \stop step initial ->
    if
      | stop initial -> pure initial
      | otherwise -> step initial i
  Jet left <*> Jet right = Jet \stop step initial ->
    -- Here we assume that the first Jet correctly handles the stop signal.
    let step' f s a = step s (f a)
     in left stop (\s f -> right stop (step' f) s) initial

-- | Similar to the instance for pure lists, that does search.
--
-- >>> :{
-- do string <- J.each ["ab","cd"]
--    J.each string
-- &
-- J.toList
-- :}
-- "abcd"
instance Monad Jet where
  return = pure
  Jet m >>= k = Jet \stop step initial ->
    m stop (\s a -> runJet (k a) stop step s) initial

-- |
-- >>> liftIO (putStrLn "foo") <> liftIO (putStrLn "bar") & J.toList
-- foo
-- bar
-- [(),()]
instance MonadIO Jet where
  liftIO action = Jet \stop step initial ->
    if
      | stop initial -> pure initial
      | otherwise -> do
          a <- action
          step initial a

-- | 'Jet' concatenation.
--
-- >>> J.each "ab" <> J.each "cd" & J.toList
-- "abcd"
instance Semigroup (Jet a) where
  Jet f1 <> Jet f2 = Jet \stop step s0 -> do
    -- perhaps some of the stop checks are redundant, the first one in particular?
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

-- | 'mempty' is the empty 'Jet'.
--
-- >>> mempty <> J.each "ab" <> mempty & J.toList
-- "ab"
instance Monoid (Jet a) where
  mempty = Jet \_ _ initial -> pure initial

-- | Same as 'Monoid'.
instance Alternative Jet where
  (<|>) = (<>)
  empty = mempty

-- | Same as 'Monoid'
instance MonadPlus Jet where
  mzero = mempty
  mplus = (<>)

-- | A failed pattern-match in a do-block produces 'mzero'.
--
-- >>> :{
-- do Just c <- J.each [Nothing, Just 'a', Nothing, Just 'b']
--    pure c
-- & J.toList
-- :}
-- "ab"
instance MonadFail Jet where
  fail _ = mzero

-- | Build a 'Jet' from any 'Foldable' container
--
-- >>> J.each [True,False] & J.toList
-- [True,False]
each :: forall a f. (Foldable f) => f a -> Jet a
each (Data.Foldable.toList -> seed) = Jet \stop step ->
  -- This could be done with Jet.unfold, but let's leave as it is.
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

-- |
--
-- >>> J.repeat True & J.take 2 & J.toList
-- [True,True]
repeat :: a -> Jet a
repeat a = repeatIO (pure a)

-- |
--
-- >>> J.repeatIO (putStrLn "hi" *> pure True) & J.take 2 & J.toList
-- hi
-- hi
-- [True,True]
repeatIO :: IO a -> Jet a
repeatIO action = untilNothing (fmap Just action)

-- |
--
-- >>> J.replicate 2 True & J.toList
-- [True,True]
replicate :: Int -> a -> Jet a
replicate n a = replicateIO n (pure a)

-- |
-- >>> J.replicateIO 2 (putStrLn "hi" *> pure True) & J.toList
-- hi
-- hi
-- [True,True]
--
-- Don't confuse this with @Control.Monad.replicateM :: Int -> Jet a -> Jet [a]@ which has a combinatorial behavior.
replicateIO :: Int -> IO a -> Jet a
replicateIO n ioa = take n (repeatIO ioa)

-- |
--
-- >>> J.iterate succ (1 :: Int) & J.take 2 & J.toList
-- [1,2]
iterate :: (a -> a) -> a -> Jet a
iterate h = iterateIO (fmap pure h)

-- |
--
-- >>> J.iterateIO (\x -> putStrLn "hi" *> pure (succ x)) (1 :: Int) & J.take 2 & J.toList
-- hi
-- [1,2]
iterateIO :: (a -> IO a) -> a -> Jet a
iterateIO h a = pure a <> unfoldIO (fmap (fmap (\x -> Just (x, x))) h) a

-- |
-- >>> J.unfold (\case [] -> Nothing ; c : cs -> Just (c,cs)) "abc" & J.toList
-- "abc"
unfold :: (b -> Maybe (a, b)) -> b -> Jet a
unfold h = unfoldIO (fmap pure h)

-- |
-- >>> :{
-- J.unfoldIO (\x -> do putStrLn "hi"
--                      pure $ case x of
--                         [] -> Nothing
--                         c : cs -> Just (c,cs))
--            "abc"
-- & J.toList
-- :}
-- hi
-- hi
-- hi
-- hi
-- "abc"
unfoldIO :: (b -> IO (Maybe (a, b))) -> b -> Jet a
unfoldIO h seed = Jet \stop step ->
  let go b s =
        if
          | stop s ->
              pure s
          | otherwise -> do
              next <- h b
              case next of
                Nothing ->
                  pure s
                -- strictness only on the states. Good idea, or bad?
                Just (a, !b') -> do
                  !s' <- step s a
                  go b' s'
   in go seed

-- |
-- >>> j = J.untilEOF System.IO.hIsEOF System.IO.hGetLine :: Handle -> Jet String
untilEOF :: (handle -> IO Bool) -> (handle -> IO a) -> handle -> Jet a
untilEOF hIsEOF' hGetLine' handle = untilNothing do
  eof <- hIsEOF' handle
  if
    | eof ->
        pure Nothing
    | otherwise ->
        Just <$> hGetLine' handle

-- |
--
-- >>> :{
-- do ref <- newIORef "abc"
--    let pop = atomicModifyIORef ref (\case [] -> ([], Nothing)
--                                           x : xs -> (xs, Just x))
--    J.untilNothing pop & J.toList
-- :}
-- "abc"
untilNothing :: IO (Maybe a) -> Jet a
untilNothing action = unfoldIO (\() -> fmap (fmap (,())) action) ()

-- | Convert to a regular list. This breaks streaming.
--
-- >>> J.each "abc" & J.toList
-- "abc"
--
-- Alternatively, we can use 'fold' in combination with 'Control.Foldl.list' form the [foldl](https://hackage.haskell.org/package/foldl) library:
--
-- >>> L.purely (J.fold (J.each "abc")) L.list
-- "abc"
--
-- which is more verbose, but more composable.
toList :: Jet a -> IO [a]
toList (Jet f) = do
  as <- f (const False) (\xs x -> pure (x : xs)) []
  pure (reverse as)

-- | Returns the number of elements yielded by the 'Jet', exhausting it in the process.
--
-- >>> J.each "abc" & J.length
-- 3
--
-- Alternatively, we can use 'fold' in combination with 'Control.Foldl.length' form the [foldl](https://hackage.haskell.org/package/foldl) library:
--
-- >>> L.purely (J.fold (J.each "abc")) L.length
-- 3
--
-- which is more verbose, but more composable.
length :: Jet a -> IO Int
length (Jet f) = do
  l <- f (const False) (\s _ -> pure (succ s)) 0
  pure l

data Pair a b = Pair !a !b deriving (Show)

pairExtract (Pair _ b) = b

pairEnv (Pair a _) = a

data Triple a b c = Triple !a !b !c

tripleExtract (Triple _ _ c) = c

-- fromTuple :: (a, b) -> Pair a b
-- fromTuple (a, b) -> Pair a b

-- | >>> J.each "abc" & J.drop 2 & J.toList
-- "c"
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

-- | >>> J.each [1..5] & J.dropWhile (<3) & J.toList
-- [3,4,5]
dropWhile :: (a -> Bool) -> Jet a -> Jet a
dropWhile p = dropWhileIO (fmap pure p)

dropWhileIO :: (a -> IO Bool) -> Jet a -> Jet a
dropWhileIO p (Jet f) = Jet \stop step initial -> do
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

-- | >>> J.each "abc" & J.take 2 & J.toList
-- "ab"
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

-- | Synonym for 'take'.
limit :: Int -> Jet a -> Jet a
limit = take

data TakeState = StillTaking | TakingNoMore

-- | >>> J.each [1..] & J.takeWhile (<5) & J.toList
-- [1,2,3,4]
takeWhile :: (a -> Bool) -> Jet a -> Jet a
takeWhile p = takeWhileIO (fmap pure p)

takeWhileIO :: (a -> IO Bool) -> Jet a -> Jet a
takeWhileIO p (Jet f) = Jet \stop step initial -> do
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

-- |
-- >>> J.each "abc" & J.filter (=='a') & J.toList
-- "a"
filter :: (a -> Bool) -> Jet a -> Jet a
filter p = filterIO (fmap pure p)

filterIO :: (a -> IO Bool) -> Jet a -> Jet a
filterIO p (Jet f) = Jet \stop step initial -> do
  let step' s a = do
        shouldPass <- p a
        if
          | shouldPass -> do
              !s' <- step s a
              pure s'
          | otherwise ->
              pure s
  f stop step' initial

-- | Behaves like a combination of 'fmap' and 'foldl'; it applies a function to
-- each element of a structure passing an accumulating parameter from left to right.
--
-- The resulting 'Jet' has the same number of elements as the original one.
--
-- Unlike 'Data.Traversable.mapAccumL', it doesn't make the final state available.
--
-- >>> J.each [1,2,3,4] & J.mapAccum (\a b -> (a + b,a)) 0 & J.toList
-- [0,1,3,6]
mapAccum :: (a -> b -> (a, c)) -> a -> Jet b -> Jet c
mapAccum stepAcc = mapAccumIO (fmap (fmap pure) stepAcc)

mapAccumIO :: (a -> b -> IO (a, c)) -> a -> Jet b -> Jet c
mapAccumIO stepAcc initialAcc (Jet f) = Jet \stop step initial -> do
  let stop' = stop . pairExtract
      step' (Pair acc s) b = do
        (acc', c) <- stepAcc acc b
        !s' <- step s c
        pure (Pair acc' s')
      initial' = Pair initialAcc initial
  Pair _ final <- f stop' step' initial'
  pure final

data Touched
  = NotYetTouched
  | AlreadyTouched

-- TODO: there's a bug here!!!!

-- |
-- >>> J.each "abc" & J.intersperse '-' & J.toList
-- "a-b-c"
intersperse :: a -> Jet a -> Jet a
intersperse intrusion (Jet upstream) = Jet \stop step initial -> do
  let stop' = stop . pairExtract
      step' (Pair AlreadyTouched s) a = do
        !s' <- step s intrusion
        if
          | stop s' ->
              pure (Pair AlreadyTouched s')
          | otherwise -> do
              !s'' <- step s' a
              pure (Pair AlreadyTouched s'')
      step' (Pair NotYetTouched s) a = do
        !s' <- step s a
        pure (Pair AlreadyTouched s')
      initial' = Pair NotYetTouched initial
  Pair _ final <- upstream stop' step' initial'
  pure final

-- |
-- >>> J.each "abc" & J.zip [1..] & J.toList
-- [(1,'a'),(2,'b'),(3,'c')]
--
-- >>> J.each [1..] & J.zip "abc" & J.toList
-- [('a',1),('b',2),('c',3)]
zip :: (Foldable f) => f a -> Jet b -> Jet (a, b)
zip = zipWith (,)

zipWith :: (Foldable f) => (a -> b -> c) -> f a -> Jet b -> Jet c
zipWith zf (Data.Foldable.toList -> as0) = zipWithIO (fmap (fmap pure) zf) (fmap pure as0)

zipIO :: (Foldable f) => f (IO a) -> Jet b -> Jet (a, b)
zipIO = zipWithIO (\x y -> pure (x, y))

-- |
-- Zips a list of 'IO' actions with a 'Jet', where the combining function can also have effects.
--
-- If the list of actions is exhausted, the 'Jet' stops:
--
-- >>> J.each [1..] <&> show & zipWithIO (\c1 c2 -> putStrLn (c1 ++ c2)) [pure "a", pure "b"] & J.toList
-- a1
-- b2
-- [(),()]
zipWithIO :: (Foldable f) => (a -> b -> IO c) -> f (IO a) -> Jet b -> Jet c
zipWithIO zf (Data.Foldable.toList -> ioas0) (Jet f) = Jet \stop step initial -> do
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

-- | Opens a file and makes the 'Handle' available to all following statements
-- in the do-block.
--
-- Notice that it's often simpler to use the 'JetSource' (for reading) and
-- 'JetSink' (for writing) instances of 'File'.
withFile :: FilePath -> IOMode -> Jet Handle
withFile path iomode = control @Handle (System.IO.withFile path iomode)

-- |
--
-- >>> :{
-- do r <- J.bracket (putStrLn "allocating" *> pure "foo") (\r -> putStrLn $ "deallocating " ++ r)
--    liftIO $ putStrLn $ "using resource " ++ r
-- & drain
-- :}
-- allocating
-- using resource foo
-- deallocating foo
bracket ::
  forall a b.
  -- | allocator
  IO a ->
  -- | finalizer
  (a -> IO b) ->
  Jet a
bracket allocate free = control @a (Control.Exception.bracket allocate free)

bracket_ ::
  forall a b.
  -- | allocator
  IO a ->
  -- | finalizer
  IO b ->
  Jet ()
bracket_ allocate free = control_ (Control.Exception.bracket_ allocate free)

bracketOnError ::
  forall a b.
  -- | allocator
  IO a ->
  -- | finalizer
  (a -> IO b) ->
  Jet a
bracketOnError allocate free = control @a (Control.Exception.bracketOnError allocate free)

-- |
--
-- Notice how the finalizer runs even when we limit the 'Jet':
--
-- >>> :{
-- do J.finally (putStrLn "hi") -- protects statements below
--    liftIO (putStrLn "hey")
--    J.each "abc"
-- & J.limit 2
-- & J.toList
-- :}
-- hey
-- hi
-- "ab"
--
-- But if the protected 'Jet' is not consumed at all, the finalizer might not run.
--
-- >>> :{
-- do J.finally (putStrLn "hi") -- protects statements below
--    liftIO (putStrLn "hey")
--    J.each "abc"
-- & J.limit 0
-- & J.toList
-- :}
-- ""
finally :: IO a -> Jet ()
finally afterward =
  control_ (flip Control.Exception.finally afterward)

onException :: IO a -> Jet ()
onException afterward =
  control_ (flip Control.Exception.onException afterward)

-- | Lift a control operation (like 'Control.Exception.bracket') for which the
-- callback uses the allocated resource.
--
-- __BEWARE__: the control operation shouldn't do weird things like executing
-- the callback twice.
control :: forall resource. (forall x. (resource -> IO x) -> IO x) -> Jet resource
control f =
  Jet \stop step initial ->
    if
      | stop initial ->
          pure initial
      | otherwise -> do
          f (step initial)

-- | Lift a control operation (like 'Control.Exception.finally') for which the
-- callback doesn't use the allocated resource.
--
-- __BEWARE__: the control operation shouldn't do weird things like executing
-- the callback twice.
control_ :: (forall x. IO x -> IO x) -> Jet ()
control_ f =
  Jet \stop step initial ->
    if
      | stop initial -> do
          pure initial
      | otherwise -> do
          f (step initial ())

-- |
--
-- >>> L.purely (J.fold (J.each "abc")) ((,) <$> L.list <*> L.length)
-- ("abc",3)
fold :: Jet a -> (s -> a -> s) -> s -> (s -> r) -> IO r
fold (Jet f) step initial coda = do
  r <- f (const False) (fmap (fmap pure) step) initial
  pure $ coda r

-- |
-- >>> L.impurely (J.foldIO (J.each "abc")) (L.FoldM (\() c -> putStrLn [c]) (pure ()) pure *> L.generalize L.length)
-- a
-- b
-- c
-- 3
foldIO :: Jet a -> (s -> a -> IO s) -> IO s -> (s -> IO r) -> IO r
foldIO (Jet f) step initialIO coda = do
  initial <- initialIO
  r <- f (const False) step initial
  coda r

-- Byte Jets

-- https://stackoverflow.com/questions/49852060/how-to-choose-chunk-size-when-reading-a-large-file
-- https://askubuntu.com/questions/641900/how-file-system-block-size-works
-- https://stackoverflow.com/questions/1111661/8192-bytes-when-creating-file
data ChunkSize
  = DefaultChunkSize
  | ChunkSize Int
  | ChunkSize1K
  | ChunkSize4K
  | ChunkSize8K
  | ChunkSize16K
  | ChunkSize1M
  | ChunkSize2M
  deriving (Show)

chunkSize :: ChunkSize -> Int
chunkSize = \case
  DefaultChunkSize -> 8192
  ChunkSize c -> c
  ChunkSize1K -> 1024
  ChunkSize4K -> 4096
  ChunkSize8K -> 8192
  ChunkSize16K -> 16384
  ChunkSize1M -> 1048576
  ChunkSize2M -> 2097152

-- | Helper multi-parameter typeclass for creating 'Jet' values out of a
--   variety of common sources.
--
--   Because there's no functional dependency, sometimes we need to use
--   @TypeApplications@ to give the compiler a hint about the type of elements
--   we want to produce. For example, here we want 'Line's and not, say,
--   'ByteString's:
--
-- >>> action = J.jet @Line (File "foo.txt") & J.sink J.stdout
class JetSource a source where
  jet :: source -> Jet a

bytes :: ChunkSize -> Handle -> Jet ByteString
bytes (chunkSize -> count) handle =
  untilEOF System.IO.hIsEOF (flip B.hGetSome count) handle

instance JetSource ByteString Handle where
  jet = bytes DefaultChunkSize

instance (JetSource a Handle) => JetSource a File where
  jet (File path) = do
    handle <- withFile path ReadMode
    jet handle

accumByteLengths :: Jet ByteString -> Jet (Int, ByteString)
accumByteLengths = mapAccum (\acc bytes -> let acc' = acc + B.length bytes in (acc', (acc', bytes))) (0 :: Int)

data AmIContinuing
  = Continuing
  | NotContinuing
  deriving (Show)

-- | Splits a stream of bytes into groups bounded by maximum byte sizes. When
-- one group \"fills up\", the next one is started.
--
-- When the list of buckets sizes is exhausted, all incoming bytes are put into
-- the same unbounded group.
--
-- Useful in combination with 'recast'.
bytesOverBuckets :: [Int] -> Splitter ByteString ByteString
bytesOverBuckets buckets0 = MealyIO step mempty (pure (Pair NotContinuing buckets0))
  where
    -- logStep s@(Pair c zzz) a = do
    --     putStrLn "foooo!"
    --     System.IO.hFlush System.IO.stdout
    --     traceIO ("state: " ++ show c)
    --     traceIO ("bucket: " ++ show (Prelude.take 2 zzz))
    --     traceIO ("input: " ++ show a)
    --     r@(nexts, _) <- step s a
    --     traceIO ("output: " ++ show nexts)
    --     pure r
    step :: Pair AmIContinuing [Int] -> ByteString -> IO (SplitStepResult ByteString, Pair AmIContinuing [Int])
    step splitterState b = do
      (continueResult, Pair continuing' buckets', b') <- continue splitterState b
      if
        | B.null b' ->
            pure (continueResult, Pair continuing' buckets')
        | otherwise -> do
            (entiresResult, splitterState') <- makeEntires mempty b' buckets'
            pure (continueResult <> entiresResult, splitterState')
    continue :: Pair AmIContinuing [Int] -> ByteString -> IO (SplitStepResult ByteString, Pair AmIContinuing [Int], ByteString)
    continue (Pair NotContinuing []) b = pure (nextWith b, Pair NotContinuing [], B.empty)
    continue (Pair Continuing []) b = pure (continueWith b, Pair Continuing [], B.empty)
    continue (Pair NotContinuing (bucket : buckets)) b = do
      let blen = B.length b
      -- traceIO ("b = " ++ show b ++ " bucket size= " ++ show bucket)
      pure case compare blen bucket of
        LT -> (nextWith b, Pair Continuing (bucket - blen : buckets), B.empty)
        EQ -> (entireWith (singleton b), Pair NotContinuing buckets, B.empty)
        GT ->
          let (left, right) = B.splitAt bucket b
           in (entireWith (singleton left), Pair NotContinuing buckets, right)
    continue (Pair Continuing (bucket : buckets)) b = do
      let blen = B.length b
      pure case compare blen bucket of
        LT -> (continueWith b, Pair Continuing (bucket - blen : buckets), B.empty)
        EQ -> (continueWith b, Pair NotContinuing buckets, B.empty)
        GT ->
          let (left, right) = B.splitAt bucket b
           in (continueWith left, Pair NotContinuing buckets, right)
    makeEntires :: DList ByteString -> ByteString -> [Int] -> IO (SplitStepResult ByteString, Pair AmIContinuing [Int])
    makeEntires acc b [] = pure (entireWith acc <> nextWith b, Pair Continuing [])
    makeEntires acc b (bucket : buckets) = do
      let blen = B.length b
      case compare blen bucket of
        LT -> pure (entireWith acc <> nextWith b, Pair Continuing (bucket - blen : buckets))
        EQ -> pure (entireWith (acc <> singleton b), Pair NotContinuing buckets)
        GT -> do
          let (left, right) = B.splitAt bucket b
          makeEntires (acc <> singleton left) right buckets -- non-terminal
    continueWith b = mempty {continuationOfPreviouslyStartedGroup = [b]}
    entireWith bdf = mempty {entireGroups = fmap pure (closeDList bdf)}
    nextWith b = mempty {startOfNewGroup = [b]}

-- | A sequence of bytes that we might want to keep together.
newtype ByteBundle = ByteBundle BL.ByteString deriving newtype (Show, Semigroup, Monoid)

-- | Constructs a 'ByteBundle' out of the bytes of some 'Foldable' container.
bundle :: (Foldable f) => f ByteString -> ByteBundle
bundle = ByteBundle . BL.fromChunks . Data.Foldable.toList

-- | Length in bytes.
bundleLength :: ByteBundle -> Int
bundleLength (ByteBundle value) = fromIntegral (BL.length value) -- Int64, but unlikely we'll reach the limit

bundleBytes :: ByteBundle -> Jet ByteString
bundleBytes (ByteBundle value) = each (BL.toChunks value)

-- | Exception thrown when we try to write too much data in a size-bounded destination.
data BucketOverflow = BucketOverflow
  deriving (Show, Typeable)

instance Exception BucketOverflow

-- | Splits a stream of 'ByteBundles' into groups bounded by maximum byte
-- sizes.  Bytes belonging to the same 'ByteBundle' are always put in the same
-- group. When one group \"fills up\", the next one is started.
--
-- When the list of buckets sizes is exhausted, all incoming bytes are put into
-- the same unbounded group.
--
-- Useful in combination with 'recast'.
--
-- __THROWS__:
--
-- * 'BucketOverflow' exception if the size bound of a group turns out to be
-- too small for holding even a single 'ByteBundle' value.
byteBundlesOverBuckets :: [Int] -> Splitter ByteBundle ByteString
byteBundlesOverBuckets buckets0 = MealyIO step mempty (pure (Pair NotContinuing buckets0))
  where
    step :: Pair AmIContinuing [Int] -> ByteBundle -> IO (SplitStepResult ByteString, Pair AmIContinuing [Int])
    step (Pair splitterState []) (ByteBundle pieces) =
      -- We assume [] means "infinite bucket" so once we enter it we'll only be able to continue.
      pure
        ( case splitterState of
            Continuing -> continueWith pieces
            NotContinuing -> nextWith pieces,
          Pair Continuing []
        )
    step (Pair splitterState (bucket : buckets)) e@(ByteBundle pieces) = do
      let elen = bundleLength e
      case compare elen bucket of
        LT ->
          pure
            ( case splitterState of
                Continuing -> continueWith pieces
                NotContinuing -> nextWith pieces,
              Pair Continuing (bucket - elen : buckets)
            )
        EQ ->
          pure
            ( case splitterState of
                Continuing -> continueWith pieces
                NotContinuing -> entireWith pieces,
              Pair NotContinuing buckets
            )
        -- NB: It's possible to close a bucket and open the next one in the same iteration.
        GT -> case splitterState of
          Continuing -> step (Pair NotContinuing buckets) e
          -- If we are not continuing, that means that the brand-new bucket hasn't
          -- enough space to hold a single entity.
          NotContinuing -> throwIO BucketOverflow
    continueWith bs = mempty {continuationOfPreviouslyStartedGroup = BL.toChunks bs}
    entireWith pieces = mempty {entireGroups = [BL.toChunks pieces]}
    nextWith bs = mempty {startOfNewGroup = BL.toChunks bs}

-- | Uses the default system locale.
instance JetSource Line Handle where
  jet handle =
    textToLine <$> untilEOF System.IO.hIsEOF T.hGetLine handle

--
--
-- Text Jets

-- |
-- __THROWS__:
--
-- * 'T.UnicodeException'
decodeUtf8 :: Jet ByteString -> Jet Text
decodeUtf8 (Jet f) = Jet \stop step initial -> do
  let stop' = stop . pairExtract
      step' (Pair leftovers s) bytes = do
        T.Some !text !_ !leftovers' <- pure $ T.streamDecodeUtf8 bytes
        !s' <- step s text
        pure (Pair leftovers' s')
      initial' = Pair leftovers0 initial
  Pair leftovers final <- f stop' step' initial'
  T.Some !_ !bytes !_ <- pure $ T.streamDecodeUtf8 B.empty
  if
    | not (B.null bytes) ->
        throwIO (T.DecodeError "Unconsumed leftovers at end." Nothing)
    | otherwise ->
        pure final
  where
    leftovers0 =
      let T.Some _ _ g = T.streamDecodeUtf8 B.empty
       in g

encodeUtf8 :: Jet Text -> Jet ByteString
encodeUtf8 = fmap T.encodeUtf8

-- | A line of text.
--
-- While it is guaranteed that the 'Line's coming out of the 'lines' function
-- do not contain newlines, that invariant is not otherwise enforced.
newtype Line = Line_ TL.Text
  deriving newtype (Eq, Ord, Semigroup, Monoid, Show, IsString)

-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html

-- | Unidirectional pattern that allows converting a 'Line' into a 'Text'
-- during pattern-matching.
pattern Line text <- Line_ (TL.toStrict -> text)

-- | Converts a 'Line' back to text, without adding the newline.
lineToText :: Line -> Text
lineToText (Line_ text) = TL.toStrict text

-- | Converts a 'Line' to an utf8-encdoed 'ByteBundle', without adding the newline.
lineToUtf8 :: Line -> ByteBundle
lineToUtf8 (Line_ l) = TL.toChunks l <&> T.encodeUtf8 & bundle

textToLine :: Text -> Line
textToLine = Line_ . TL.fromStrict

-- | @Data.Text.singleton '\\n'@
newline :: Text
newline = T.singleton '\n'

textToUtf8 :: Text -> ByteBundle
textToUtf8 t = ByteBundle (t & T.encodeUtf8 & BL.fromStrict)

lineContains :: Text -> Line -> Bool
lineContains t (Line_ l) = TL.isInfixOf (TL.fromStrict t) l

lineBeginsWith :: Text -> Line -> Bool
lineBeginsWith t (Line_ l) = TL.isPrefixOf (TL.fromStrict t) l

-- | Adds the 'Text' to the beginning of the 'Line'.
prefixLine :: Text -> Line -> Line
prefixLine t (Line_ l) = Line_ (TL.fromChunks (t : TL.toChunks l))

-- textToLine :: Text -> Line
-- textToLine text
--     | Just _ <- T.find (=='\n') text = throw NewlineForbidden
--     | otherwise = Line_ (removeTrailingCarriageReturn text)

stringToLine :: String -> Line
stringToLine = Line_ . TL.pack

-- withLineText :: (Text -> r) -> Line -> r
-- withLineText f (Line text) = f text

isEmptyLine :: Line -> Bool
isEmptyLine (Line_ text) = TL.null text

emptyLine :: Line
emptyLine = Line_ TL.empty

-- | Exception thrown when we find newlines in functions which don't accept them.
--
-- A direct copy of the @NewlineForbidden@ exception from the [turtle](https://hackage.haskell.org/package/turtle) package.
data NewlineForbidden = NewlineForbidden
  deriving (Show, Typeable)

instance Exception NewlineForbidden

removeTrailingCarriageReturn :: Text -> Text
removeTrailingCarriageReturn text
  | T.null text = text
  | T.last text == '\r' = T.init text
  | otherwise = text

lines :: Jet Text -> Jet Line
lines (Jet f) = Jet \stop step initial -> do
  let stop' = stop . pairExtract
      findLinesInCurrentBlock text
        | T.null text =
            []
        | otherwise =
            map (textToLine . removeTrailingCarriageReturn) (T.lines text)
              ++ if
                | T.last text == '\n' ->
                    [mempty]
                | otherwise ->
                    []
      step' (Pair lineUnderConstruction s) (findLinesInCurrentBlock -> linesInCurrentBlock) = do
        case linesInCurrentBlock of
          [] -> do
            pure (Pair lineUnderConstruction s)
          [l] -> do
            pure (Pair (lineUnderConstruction <> singleton l) s)
          l : rest@(x : xs) -> do
            -- Ineficcient mconcat, better strictify a lazy text here?
            let completedLine = mconcat $ runDList lineUnderConstruction [l]
            s' <- downstream stop step (completedLine : init rest) s
            pure (Pair (singleton (last linesInCurrentBlock)) s')
      initial' = Pair mempty initial
  Pair (mconcat . closeDList -> lineUnderConstruction) final <- f stop' step' initial'
  if
    | stop final ->
        pure final
    | isEmptyLine lineUnderConstruction ->
        pure final
    | otherwise ->
        step final lineUnderConstruction

unlines :: Jet Line -> Jet Text
unlines j = do
  Line text <- j
  pure text <> pure (T.singleton '\n')

downstream :: (s -> Bool) -> (s -> x -> IO s) -> [x] -> s -> IO s
downstream stop step = go
  where
    go [] s =
      pure s
    go (x : xs) s
      | stop s =
          pure s
      | otherwise = do
          !s' <- step s x
          go xs s'

-- General sinks

-- | A function that consumes a 'Jet' totally or partially, without returning a result.
type Sink a = Jet a -> IO ()

-- | Helper multi-parameter typeclass for creating 'Jet'-consuming functions
-- out of a variety of common destinations.
--
-- >>> J.each ["aaa","bbb","ccc"] <&> J.stringToLine & J.sink J.stdout
-- aaa
-- bbb
-- ccc
class JetSink a target where
  sink :: target -> Sink a

instance JetSink ByteString Handle where
  sink handle j = for_ j (B.hPut handle)

instance (JetSink a Handle) => JetSink a File where
  sink (File path) j = System.IO.withFile path System.IO.WriteMode \handle ->
    sink handle j

-- | Uses the default system locale. Adds newlines.
instance JetSink Line Handle where
  sink handle = traverse_ (T.hPutStrLn handle . lineToText)

-- | Uses the default system locale.
instance JetSink Text Handle where
  sink handle = traverse_ (T.hPutStr handle)

-- | 'FilePaths' are plain strings. This newtype provides a small measure of
-- safety over them.
newtype File = File {getFilePath :: FilePath} deriving (Show)

-- | The maximum size in bytes of some destination into which we write the
-- bytes produced by a 'Jet'.
data BoundedSize x = BoundedSize Int x deriving stock (Show, Read)

instance JetSink ByteBundle Handle where
  sink handle j = traverse_ (B.hPut handle) do
    s <- j
    bundleBytes s

-- | Distributes incoming bytes through a sequence of files. Once a file is
-- full, we start writing the next one.
instance JetSink ByteString [BoundedSize File] where
  sink bucketFiles j =
    withCombiners_
      (\handle b -> B.hPut handle b)
      hClose
      (makeAllocator <$> bucketFiles)
      (\combiners -> drain $ recast (bytesOverBuckets bucketSizes) combiners j)
    where
      bucketSizes = map (\(BoundedSize size _) -> size) bucketFiles

-- | Distributes incoming bytes through a sequence of files. Once a file is
-- full, we start writing the next one.
--
-- Each 'ByteBundle' value is garanteed to be written to a single file. If a
-- file turns out to be too small for even a single 'ByteBundle' value, a
-- 'BucketOverflow' exception is thrown.
instance JetSink ByteBundle [BoundedSize File] where
  sink bucketFiles j =
    withCombiners_
      (\handle b -> B.hPut handle b)
      hClose
      (makeAllocator <$> bucketFiles)
      (\combiners -> drain $ recast (byteBundlesOverBuckets bucketSizes) combiners j)
    where
      bucketSizes = map (\(BoundedSize size _) -> size) bucketFiles

makeAllocator :: BoundedSize File -> IO Handle
makeAllocator (BoundedSize _ (File path)) = openBinaryFile path WriteMode

-- DList helper
newtype DList a = DList {runDList :: [a] -> [a]}

instance Semigroup (DList a) where
  DList a1 <> DList a2 = DList (a1 . a2)

instance Monoid (DList a) where
  mempty = DList id

makeDList :: [a] -> DList a
makeDList as = DList \xs -> as ++ xs

closeDList :: DList a -> [a]
closeDList (DList f) = f []

singleton :: a -> DList a
singleton a = DList $ (a :)

--
-- concurrency

-- | Process the values yielded by the upstream 'Jet' in a concurrent way,
-- and return the results in the form of another 'Jet' as they are produced.
--
-- __NB__: this function might scramble the order of the returned values. Right
-- now there isn't a function for unscrambling them.
--
-- >>> :{
--  J.each [(3,'a'), (2,'b'), (1,'c')]
--  & J.traverseConcurrently (numberOfWorkers 10) (\(d,c) -> threadDelay (d*1e5) *> pure c)
--  & J.toList
-- :}
-- "cba"
--
-- What happens if we 'limit' the resulting 'Jet' and we reach that limit, or
-- if we otherwise stop consuming the 'Jet' before it gets exhausted? In those
-- cases, all pending @IO b@ tasks are cancelled.
--
-- >>> :{
--  J.each [(9999,'a'), (2,'b'), (1,'c')]
--  & J.traverseConcurrently (numberOfWorkers 10) (\(d,c) -> threadDelay (d*1e5) *> pure c)
--  & J.take 2
--  & J.toList
-- :}
-- "cb"
traverseConcurrently :: (PoolConf -> PoolConf) -> (a -> IO b) -> Jet a -> Jet b
-- TODO:
-- It would be nice to have 0-lengh channels for which one side blocks until
-- the other side takes the job.
traverseConcurrently adaptConf makeTask upstream = Jet \stop step initial -> do
  if
    -- If we know we aren't going to do any work, don't bother starting the
    -- whole boondoggle.
    | stop initial ->
        pure initial
    | otherwise -> do
        -- At this point we know we should do at least one step.
        let PoolConf {_inputQueueSize, _numberOfWorkers, _outputQueueSize} = adaptConf defaultPoolConf
        input <- newTBMQueueIO _inputQueueSize
        inputQueueWriterShouldStop <- newIORef False
        aliveWorkers <- newIORef _numberOfWorkers
        output <- newTBMQueueIO _outputQueueSize
        let -- The inputQueueWriter should *not* be interrupted aynchronously.
            -- After each iteration, it reads the IORef to see if it should stop.
            -- Once it stops, it closes the input queue.
            inputQueueWriter = do
              run
                upstream
                id
                ( \_ a -> do
                    atomically $ writeTBMQueue input (makeTask a)
                    readIORef inputQueueWriterShouldStop
                )
                False
              atomically $ closeTBMQueue input
            -- Workers *can* be interrupted asynchronously.
            worker = do
              mtask <- atomically $ readTBMQueue input
              case mtask of
                Nothing -> do
                  remaining <- do
                    atomicModifyIORef' aliveWorkers \count ->
                      let count' = pred count
                       in (count', count')
                  if
                    | remaining == 0 -> do
                        atomically $ closeTBMQueue output
                    | otherwise -> do
                        pure ()
                Just task -> do
                  result <- task
                  atomically $ writeTBMQueue output result
                  worker
            outputQueueReader s = do
              if
                | stop s -> do
                    -- tell the inserter from upstream that it should stop. is this enough?
                    writeIORef inputQueueWriterShouldStop True
                    atomically $ closeTBMQueue input -- perhaps unnecessary?
                    pure s
                | otherwise -> do
                    mresult <- atomically $ readTBMQueue output
                    case mresult of
                      Nothing -> do
                        pure s
                      Just result -> do
                        !s' <- step s result
                        outputQueueReader s'
        runConcurrently $
          Concurrently do
            inputQueueWriter
            *> Concurrently do
              finalLeft <- do
                runConcurrentlyE $
                  -- The worker pool is always killed when the output reader finishes,
                  -- but for the "happy path" the workers will already be dead.
                  ConcurrentlyE (Right <$> replicateConcurrently_ _numberOfWorkers worker)
                    *>
                    -- This Left is what kills the worker pool.
                    ConcurrentlyE (Left <$> outputQueueReader initial)
              case finalLeft of
                Right () -> do
                  error "never happens, the Left always wins"
                Left final -> do
                  pure final

-- | Configuration record for the worker pool.
data PoolConf = PoolConf
  { _inputQueueSize :: Int,
    _numberOfWorkers :: Int,
    _outputQueueSize :: Int
  }
  deriving (Show)

defaultPoolConf =
  PoolConf
    { _inputQueueSize = 1,
      _numberOfWorkers = 1,
      _outputQueueSize = 1
    }

-- | Size of the waiting queue into the worker pool. The default is @1@.
inputQueueSize :: Int -> PoolConf -> PoolConf
inputQueueSize size poolConf = poolConf {_inputQueueSize = size}

-- | The size of the worker pool. The default is @1@.
numberOfWorkers :: Int -> PoolConf -> PoolConf
numberOfWorkers number poolConf = poolConf {_numberOfWorkers = number}

-- | Size of the queue holding results out of the working pool before they
-- are yielded downstream. The default is @1@.
outputQueueSize :: Int -> PoolConf -> PoolConf
outputQueueSize size poolConf = poolConf {_outputQueueSize = size}

-- | An alias for 'id'. Useful with functions like 'traverseConcurrently' and
-- 'throughProcess', for which it means \"use the default configuration\".
defaults :: a -> a
defaults = id

--
-- process invocation

-- | Feeds the upstream 'Jet' to an external process' @stdin@ and returns the
-- process' @stdout@ as another @Jet@. The feeding and reading of the standard
-- streams is done concurrently in order to avoid deadlocks.
--
-- What happens if we 'limit' the resulting 'Jet' and we reach that limit, or
-- if we otherwise stop consuming the 'Jet' before it gets exhausted? In those
-- cases, the external process is promptly terminated.
throughProcess :: (ProcConf -> ProcConf) -> CreateProcess -> Jet ByteString -> Jet ByteString
throughProcess adaptConf = throughProcess_ (adaptConf defaultProcConf)

-- | Like 'throughProcess', but feeding and reading 'Line's using the default
-- system encoding.
--
-- >>> :{
-- J.each ["aaa","bbb","ccc"]
-- <&> J.stringToLine
-- & linesThroughProcess defaults (shell "cat")
-- & J.toList
-- :}
-- ["aaa","bbb","ccc"]
--
-- An example of not reading all the lines from a long-lived process that gets cancelled:
--
-- >>> :{
-- mempty
-- & linesThroughProcess defaults (shell "{ printf \"aaa\\nbbb\\nccc\\n\" ; sleep infinity ; }")
-- & J.limit 2
-- & J.toList
-- :}
-- ["aaa","bbb"]
linesThroughProcess :: (ProcConf -> ProcConf) -> CreateProcess -> Jet Line -> Jet Line
linesThroughProcess adaptConf procSpec = do
  let textLinesProcConf =
        (adaptConf defaultProcConf)
          { _writeToStdIn = T.hPutStrLn,
            _readFromStdout = T.hGetLine
          }
  fmap textToLine . throughProcess_ textLinesProcConf procSpec . fmap lineToText

-- | Like 'throughProcess', but feeding and reading 'Line's encoded in UTF8.
utf8LinesThroughProcess :: (ProcConf -> ProcConf) -> CreateProcess -> Jet Line -> Jet Line
utf8LinesThroughProcess adaptConf procSpec = do
  lines . decodeUtf8 . throughProcess adaptConf procSpec . encodeUtf8 . unlines

throughProcess_ :: forall a b. ProcConf_ a b -> CreateProcess -> Jet a -> Jet b
throughProcess_ procConf procSpec upstream = Jet \stop step initial -> do
  let ProcConf_ {_bufferStdin, _writeToStdIn, _readFromStdout, _readFromStderr, _handleExitCode} = procConf
  if
    -- If we know we aren't going to do any work, don't bother starting the
    -- whole boondoggle.
    | stop initial ->
        pure initial
    | otherwise -> do
        let procSpec' =
              procSpec
                { std_in = CreatePipe,
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }
        input <- newTBMQueueIO @a 1
        inputQueueWriterShouldStop <- newIORef False
        -- remember to drain stderr concurrently with stdout...
        let inputQueueWriter = do
              run
                upstream
                id
                ( \_ a -> do
                    atomically $ writeTBMQueue input a
                    readIORef inputQueueWriterShouldStop
                )
                False
              atomically $ closeTBMQueue input
        finalEither <-
          runConcurrently $
            Concurrently do
              inputQueueWriter
              *> Concurrently do
                withCreateProcess procSpec' \(Just stdin') (Just stdout') (Just stderr') phandle -> do
                  when (not _bufferStdin) (System.IO.hSetBuffering stdin' System.IO.NoBuffering)
                  let stdinWriter = do
                        ma <- atomically $ readTBMQueue input
                        case ma of
                          Nothing -> do
                            hClose stdin'
                          Just a -> do
                            _writeToStdIn stdin' a
                            stdinWriter
                      stderrReader = do
                        untilEOF System.IO.hIsEOF _readFromStdout stderr' & drain
                      stdoutReader s = do
                        if
                          | stop s -> do
                              writeIORef inputQueueWriterShouldStop True
                              pure (Left s)
                          | otherwise -> do
                              eof <- System.IO.hIsEOF stdout'
                              if
                                | eof -> do
                                    writeIORef inputQueueWriterShouldStop True
                                    exitCode <- waitForProcess phandle
                                    _handleExitCode exitCode
                                    pure (Right s)
                                | otherwise -> do
                                    b <- _readFromStdout stdout'
                                    !s' <- step s b
                                    stdoutReader s'
                  runConcurrentlyE $
                    ConcurrentlyE do Right <$> stdinWriter
                      *> ConcurrentlyE do Right <$> stderrReader
                      *> ConcurrentlyE do stdoutReader initial
        pure (either id id finalEither)

-- | Configuration record with some extra options in addition to those in "CreateProcess".
type ProcConf = ProcConf_ ByteString ByteString

data ProcConf_ a b = ProcConf_
  { _bufferStdin :: Bool,
    _writeToStdIn :: Handle -> a -> IO (),
    _readFromStdout :: Handle -> IO b,
    _readFromStderr :: Handle -> IO (),
    _handleExitCode :: ExitCode -> IO ()
  }

defaultProcConf :: ProcConf
defaultProcConf =
  ProcConf_
    { _bufferStdin = False,
      _writeToStdIn = B.hPut,
      _readFromStdout = flip B.hGetSome 8192,
      _readFromStderr = void . T.hGetLine,
      _handleExitCode = \exitCode -> case exitCode of
        ExitFailure _ -> throwIO exitCode
        ExitSuccess -> pure ()
    }

-- | Should we buffer the process' @stdin@? Usually should be 'True' for
-- interactive scenarios.
--
-- By default, 'False'.
bufferStdin :: Bool -> ProcConf -> ProcConf
bufferStdin doBuffering procConf = procConf {_bufferStdin = doBuffering}

-- | Sets the function that reads a single line of output from the process
-- @stderr@.  It's called repeatedly until @stderr@ is exhausted. The reads are
-- done concurrently with the reads from @stdout@.
--
-- By default, lines of text are read using the system's default encoding.
--
-- This is a good place to throw an exception if we don't like what comes out
-- of @stderr@.
readFromStderr :: (Handle -> IO ()) -> ProcConf -> ProcConf
readFromStderr readFunc procConf = procConf {_readFromStderr = readFunc}

-- | Sets the function that handles the final `ExitCode` of the process.
--
-- The default behavior is to throw the `ExitCode` as an exception if it's not
-- a success.
handleExitCode :: (ExitCode -> IO ()) -> ProcConf -> ProcConf
handleExitCode handler procConf = procConf {_handleExitCode = handler}

--
--
-- complicated stufff

data AreWeInsideGroup foldState
  = OutsideGroup
  | InsideGroup !foldState

data RecastState foldState = RecastState !(AreWeInsideGroup foldState) [IO foldState]

-- | This is a complex, unwieldly, yet versatile function. It can be used to
-- define grouping operations, but also for decoding and other purposes.
--
-- Groups are delimited in the input 'Jet' using the 'Splitter', and the
-- contents of those groups are then combined using 'Combiners'. The result of
-- each combiner is yielded by the return 'Jet'.
--
-- If the list of combiners is finite and becomes exhausted, we stop splitting
-- and the return 'Jet' stops.
recast :: forall a b c. Splitter a b -> Combiners b c -> Jet a -> Jet c
recast
  (MealyIO splitterStep splitterCoda splitterAlloc)
  (Combiners foldStep foldCoda foldAllocs0)
  (Jet upstream) = Jet \stop step initial -> do
    initialSplitterState <- splitterAlloc
    let -- When to stop? Either downstream says we need to stop,
        -- or we are outside a group and there isn't another group consumer we
        -- can use to process the next one.
        stop' (Triple _ (RecastState OutsideGroup []) _) = True
        stop' (Triple _ _ s) = stop s

        step' (Triple splitterState recastState s) a = do
          (splitResult, splitterState') <- splitterStep splitterState a
          Pair recastState' s' <- advanceRecast splitResult recastState s
          pure (Triple splitterState' recastState' s')

        advanceRecast ssr@(SplitStepResult {continuationOfPreviouslyStartedGroup, entireGroups, startOfNewGroup}) (RecastState areWeInside foldAllocs) s = do
          case (areWeInside, entireGroups, startOfNewGroup) of
            -- If there aren't any new groups and we don't start an incomplete one, just advance the current fold
            (InsideGroup foldState, [], []) -> do
              -- traceIO $ "recast inside group just continuing"
              foldState' <- advanceGroupWithougClosing foldState continuationOfPreviouslyStartedGroup
              pure (Pair (RecastState (InsideGroup foldState') foldAllocs) s) -- main state didn't change
            (InsideGroup foldState, _, _) -> do
              -- traceIO $ "recast inside group closing"
              !c <- processSingleGroup foldState continuationOfPreviouslyStartedGroup
              !s' <- step s c
              if
                | stop s' -> do
                    -- traceIO $ "recast inside group pure"
                    pure (Pair (RecastState OutsideGroup foldAllocs) s')
                | otherwise -> do
                    -- traceIO $ "recast inside group advancing"
                    advanceRecast ssr (RecastState OutsideGroup foldAllocs) s'
            -- if we are outside of a group, the "continuationOfPreviouslyStartedGroup" is ignored.
            (OutsideGroup, _, _) -> do
              -- traceIO $ "recast outside group"
              -- doens't return foldState becasue we close the groups
              Pair foldAllocs' s' <- processEntireGroups foldAllocs s entireGroups
              bail <- pure (Pair (RecastState OutsideGroup foldAllocs') s')
              if
                | stop s' -> do
                    pure bail
                | otherwise -> do
                    case startOfNewGroup of
                      [] -> do
                        pure bail
                      (_ : _) -> do
                        case foldAllocs of
                          [] -> do
                            pure bail
                          alloc : allocs -> do
                            -- traceIO $ "recast we should be allocating here"
                            -- there is a next group, so let's begin it
                            !foldState0 <- alloc
                            foldState <- processBeginNextGroup foldState0 startOfNewGroup
                            pure (Pair (RecastState (InsideGroup foldState) allocs) s')
        -- foldM ?
        advanceGroupWithougClosing :: _ -> [b] -> IO _
        advanceGroupWithougClosing foldState [] =
          pure foldState
        advanceGroupWithougClosing foldState (b : bs) = do
          !foldState' <- foldStep foldState b
          advanceGroupWithougClosing foldState' bs
        processEntireGroups :: [IO _] -> _ -> [[b]] -> IO (Pair [IO _] _)
        -- We can't go on if there aren't any more groups
        processEntireGroups allocs s [] = do
          pure (Pair allocs s)
        -- We can't go on if there aren't any more fold initial state allocs
        processEntireGroups [] s _ = do
          pure (Pair [] s)
        processEntireGroups (alloc : allocs) s (bs : bss) = do
          !foldState0 <- alloc
          !c <- processSingleGroup foldState0 bs -- a single step downstream
          !s' <- step s c
          if
            | stop s' -> do
                pure (Pair allocs s')
            | otherwise -> do
                processEntireGroups allocs s' bss
        -- a whole fold is processed here
        processSingleGroup :: _ -> [b] -> IO c
        processSingleGroup foldState [] = do
          foldCoda foldState
        processSingleGroup foldState (b : bs) = do
          !foldState' <- foldStep foldState b
          processSingleGroup foldState' bs
        processBeginNextGroup :: _ -> [b] -> IO _
        processBeginNextGroup foldState [] = do
          pure foldState
        processBeginNextGroup foldState (b : bs) = do
          !foldState' <- foldStep foldState b
          processBeginNextGroup foldState' bs
        initial' = Triple initialSplitterState (RecastState OutsideGroup foldAllocs0) initial
    Triple splitterState recastState final <- upstream stop' step' initial'
    -- What happens if there's a fold ongoing when we stop? Right now we always close it, which seems to be a reasonable
    -- action (because the fold coda might hide a finalizer).
    --
    -- Also, when can it happen that we reach this point with an ongoing fold?
    -- If I understand correctly:
    --    - it can only happen when the upstream closes and leaves the fold open.
    --    - it can't (?) happen when the consumer stops early.
    let closePendingFold = \case
          RecastState OutsideGroup _ -> do
            pure ()
          RecastState (InsideGroup foldState) _ -> do
            _ <- foldCoda foldState
            pure ()
    if
      | stop final -> do
          closePendingFold recastState
          pure final
      | otherwise -> do
          splitResult <- splitterCoda splitterState
          -- We discard the "begins next group"; it doesn't make sense in this final step.
          Pair recastState' final' <- advanceRecast (splitResult {startOfNewGroup = []}) recastState final
          if
            | stop final' -> do
                -- TODO:
                -- should we dealloc here? Maybe there's a fold reaminging... we should close it. See below.
                closePendingFold recastState'
                pure final'
            | otherwise -> do
                case recastState' of
                  RecastState OutsideGroup _ -> do
                    -- traceIO $ "final! outside group"
                    pure final'
                  RecastState (InsideGroup foldState) _ -> do
                    -- traceIO $ "final! inside group"
                    c <- foldCoda foldState
                    final'' <- step final' c
                    pure final''

-- | A 'Combiners' value knows how to process a sequence of groups, while
-- keeping a (existentially hidden) state for each group.
--
-- Very much like a @FoldM IO@  from the
-- [foldl](https://hackage.haskell.org/package/foldl-1.4.12/docs/Control-Foldl.html#t:FoldM)
-- library, but \"restartable\" with a list of starting states.
--
-- For converting one into the other, this function should do the trick:
--
-- > \(L.FoldM step allocator coda) -> combiners step coda (Prelude.repeat allocator)
data Combiners a b where
  Combiners :: (s -> a -> IO s) -> (s -> IO b) -> [IO s] -> Combiners a b

deriving stock instance Functor (Combiners a)

-- | Constructor for 'Combiners' values.
combiners ::
  forall s a b r.
  -- \^ foo

  -- | Step function that threads the state @s@.
  (s -> a -> IO s) ->
  -- | Coda invoked when a group closes.
  (s -> IO b) ->
  -- | Actions that produce the initial states @s@ for processing each group.
  [IO s] ->
  Combiners a b
combiners = Combiners

-- | A simpler version of 'withCombiners' that doen't thread a state; it merely
-- allocates and deallocates the resource @h@.
withCombiners_ ::
  forall h a r.
  -- | Step function that accesses the resource @h@.
  (h -> a -> IO ()) ->
  -- | Finalizer to run after closing each group, and also in the case of an exception.
  (h -> IO ()) ->
  -- | Actions that allocate a sequence of resources @h@.
  [IO h] ->
  -- | The 'Combiners' value should be consumed linearly.
  (Combiners a () -> IO r) ->
  IO r
withCombiners_ step finalize allocators = do
  withCombiners
    (\h () a -> step h a)
    (\_ () -> pure ())
    finalize
    ( do
        allocator <- allocators
        pure (allocator, \_ -> pure ())
    )

-- | 'Combiners' thread a state @s@ while processing each group. Sometimes, in
-- addition to that, we want to allocate a resource @h@ when we start
-- processing a group, and deallocate it after we finish processing the group
-- or an exception is thrown. The typical example is allocating a 'Handle' for
-- writing the elements of the group as they arrive.
withCombiners ::
  forall h s a b r.
  -- | Step function that accesses the resource @h@ and threads the state @s@.
  (h -> s -> a -> IO s) ->
  -- | Coda invoked when a group closes.
  (h -> s -> IO b) ->
  -- | Finalizer to run after each coda, and also in the case of an exception.
  (h -> IO ()) ->
  -- | Actions that allocate a sequence of resources @h@ and produce initial states @s@ for processing each group.
  [(IO h, h -> IO s)] ->
  -- | The 'Combiners' value should be consumed linearly.
  (Combiners a b -> IO r) ->
  IO r
withCombiners step coda finalize allocators continuation = do
  resourceRef <- newEmptyMVar @h
  let step' (Pair h s) a = do
        s' <- step h s a
        pure (Pair h s')
      tryFinalize = do
        tryTakeMVar resourceRef >>= \case
          Nothing -> pure ()
          Just resource -> finalize resource
      adaptAllocator :: (IO h, h -> IO s) -> IO (Pair h s)
      adaptAllocator (allocate, makeInitialState) = do
        h <- mask_ do
          h <- allocate
          putMVar resourceRef h
          pure h
        s <- makeInitialState h
        pure (Pair h s)
      coda' :: Pair h s -> IO b
      coda' (Pair h s) = do
        b <- coda h s
        -- this always succeeds, we store the resource at the beginning!
        mask_ tryFinalize
        pure b
  r <-
    (continuation (combiners step' coda' (adaptAllocator <$> allocators)))
      `Control.Exception.finally` tryFinalize
  pure r

-- | Puts the elements of each group into a list that is kept in memory. This breaks streaming within the group.
--
-- Useful with 'recast'.
combineIntoLists :: Combiners a [a]
combineIntoLists =
  combiners
    (\s a -> pure (s <> singleton a))
    (pure . closeDList)
    (Prelude.repeat (pure mempty))

-- | Delimits groups in the values yielded by a 'Jet', and can also transform
-- those values.
type Splitter a b = MealyIO a (SplitStepResult b)

-- | A [Mealy machine](https://en.wikipedia.org/wiki/Mealy_machine) with an
-- existentially hidden state.
--
-- Very much like a @FoldM IO@  from the
-- [foldl](https://hackage.haskell.org/package/foldl-1.4.12/docs/Control-Foldl.html#t:FoldM)
-- library, but it emits an output at each step, not only at the end.
data MealyIO a b where
  MealyIO ::
    -- | The step function which threads the state.
    (s -> a -> IO (b, s)) ->
    -- | The final output, produced from the final state.
    (s -> IO b) ->
    -- | An action that produces the initial state.
    IO s ->
    MealyIO a b

deriving stock instance Functor (MealyIO a)

-- | For each value coming from upstream, what has the 'Splitter' learned?
--
-- * Perhaps we should continue some group we have already started in a previous step.
--
-- * Perhaps we have found entire groups that we should emit in one go, groups we know are already complete.
--
-- * Perhaps we should start a new group that will continue in the next steps.
data SplitStepResult b = SplitStepResult
  { -- | The continued group will be \"closed"\ if in the current step we emit
    -- an entire group or we begin a new group.
    --
    -- __INVARIANT__: we should only continue a group if we have already
    -- opened a \"new one\" with one or more elements in an earlier step.
    continuationOfPreviouslyStartedGroup :: [b],
    -- | It's ok if the groups we find are empty.
    entireGroups :: [[b]],
    -- | __INVARIANT__: when we are in the final step, we should not yield elements
    -- for the beginning of a new one.
    startOfNewGroup :: [b]
  }
  deriving (Functor, Show)

instance Semigroup (SplitStepResult b) where
  SplitStepResult c1 e1 b1 <> SplitStepResult c2 e2 b2 =
    SplitStepResult (c1 <> c2) (e1 <> e2) (b1 <> b2)

instance Monoid (SplitStepResult b) where
  mempty = SplitStepResult [] [] []

-- TODO: bring back some linear stuff? Perhaps adding a linearFmap ?
--
