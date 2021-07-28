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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Jet.Internal where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.Foldable qualified
import Prelude hiding (traverse_, for_, filter, drop, dropWhile, fold, take, takeWhile, unfold, zip, zipWith, filterM, lines, intersperse)
import Unsafe.Coerce qualified
import System.IO (Handle, IOMode)
import System.IO qualified
import Data.Function ((&))
import Data.Functor ((<&>))

import Data.Bifunctor
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.ByteString (ByteString)
import Data.ByteString qualified as B

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent.Conceit
import Control.Concurrent.STM.TBMQueue

newtype Jet a = Jet {
        runJet :: forall s. (s -> Bool) -> (s -> a -> IO s) -> s -> IO s
    } deriving (Functor)

-- | Go through the elements produced by a 'Jet', while keeping an
-- state @s@ and possibly performing some effect.
--
-- The caller is the one who chooses the type of the state @s@, and
-- must pass an initial value for it.
--
-- He must also provide a predicate on the state that informs the `Jet`
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

traverse :: (a -> IO b) -> Jet a -> Jet b
traverse =  flip for

-- data ConcLevel = ConcLevel Int
-- traverseCo

traverse_ :: (a -> IO b) -> Jet a -> IO ()
traverse_  = flip for_

drain :: Jet a -> IO ()
drain = traverse_ pure

-- -- | Synonym for '(=<<)'. Might be occasionally useful when building pipelines with '(&)'.
-- flatMap :: (a -> Jet b) -> Jet a -> Jet b
-- flatMap = (=<<)

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

repeat :: a -> Jet a
repeat a = repeatIO (pure a)

repeatIO :: IO a -> Jet a
repeatIO action = untilNothing (fmap Just action)

replicate :: Int -> a -> Jet a
replicate n a = replicateIO n (pure a)

replicateIO :: Int -> IO a -> Jet a
replicateIO n ioa = take n (repeatIO ioa)

iterate :: (a -> a) -> a -> Jet a
iterate h = iterateIO (fmap pure h)

iterateIO :: (a -> IO a) -> a -> Jet a
iterateIO h a = unfoldIO (fmap (fmap (\x -> Just (x,x))) h) a     

unfold :: (b -> Maybe (a, b)) -> b -> Jet a
unfold h = unfoldIO (fmap pure h)

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
                Just !(a, !b') -> do
                  !s' <- step s a
                  go b' s'
   in go seed

untilEOF :: (handle -> IO Bool) -> (handle -> IO a) -> handle -> Jet a
untilEOF hIsEOF' hGetLine' handle = untilNothing do
      eof <- hIsEOF' handle
      if
          | eof -> 
            pure Nothing
          | otherwise ->
            Just <$> hGetLine' handle

untilNothing :: IO (Maybe a) -> Jet a
untilNothing action = unfoldIO (\() -> fmap (fmap (,())) action) ()

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

data Touched = 
      NotYetTouched
    | AlreadyTouched

intersperse :: a -> Jet a -> Jet a
intersperse intrusion (Jet upstream) = Jet \stop step initial -> do
  let stop' = stop . pairExtract
      step' (Pair AlreadyTouched s) a = do
        !s' <- step s a
        pure (Pair AlreadyTouched s')
      step' (Pair NotYetTouched s) a = do
        !s' <- step s intrusion
        if 
            | stop s' ->
                pure (Pair AlreadyTouched s')
            | otherwise -> do
                !s'' <- step s' a
                pure (Pair AlreadyTouched s'')
      initial' = Pair NotYetTouched initial
  Pair _ final <- upstream stop' step' initial'
  pure final

zip :: Foldable f => f a -> Jet b -> Jet (a, b)
zip = zipWith (,)

zipWith :: Foldable f => (a -> b -> c) -> f a -> Jet b -> Jet c
zipWith zf (Data.Foldable.toList -> as0) = zipWithIO (fmap (fmap pure) zf) (fmap pure as0)

zipIO :: Foldable f => f (IO a) -> Jet b -> Jet (a, b)
zipIO = zipWithIO (\x y -> pure (x, y))

zipWithIO :: Foldable f => (a -> b -> IO c) -> f (IO a) -> Jet b -> Jet c
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


withFile :: FilePath -> Jet Handle
withFile path = control @Handle (unsafeCoerceControl @Handle (System.IO.withFile path System.IO.ReadMode))

bracket :: forall a b . IO a -> (a -> IO b) -> Jet a
bracket allocate free = control @a (unsafeCoerceControl @a (Control.Exception.bracket allocate free))

bracket_ :: forall a b . IO a -> IO b -> Jet ()
bracket_ allocate free = control_ (unsafeCoerceControl_ (Control.Exception.bracket_ allocate free))

bracketOnError :: forall a b . IO a -> (a -> IO b) -> Jet a
bracketOnError allocate free = control @a (unsafeCoerceControl @a (Control.Exception.bracketOnError allocate free))

finally :: IO a -> Jet ()
finally afterward =
    control_ (unsafeCoerceControl_ (flip Control.Exception.finally afterward))

onException :: IO a -> Jet ()
onException afterward =
    control_ (unsafeCoerceControl_ (flip Control.Exception.onException afterward))

control :: forall resource. (forall x. (resource -> IO x) %1 -> IO x) -> Jet resource
control f =
  Jet \stop step initial ->
    if
        | stop initial ->
          pure initial
        | otherwise -> do
          f (step initial)

control_ :: (forall x. IO x %1-> IO x) -> Jet ()
control_ f =
  Jet \stop step initial ->
    if
        | stop initial -> do
          pure initial
        | otherwise -> do
          f (step initial ())

unsafeCoerceControl :: forall resource . (forall x. (resource -> IO x) -> IO x) -> (forall x. (resource -> IO x) %1 -> IO x)
unsafeCoerceControl f = Unsafe.Coerce.unsafeCoerce f

unsafeCoerceControl_ :: (forall x. IO x -> IO x) -> (forall x. IO x %1 -> IO x)
unsafeCoerceControl_ f = Unsafe.Coerce.unsafeCoerce f

fold :: Jet a -> (s -> a -> s) -> s -> (s -> r) -> IO r
fold (Jet f) step initial coda = do
  r <- f (const False) (fmap (fmap pure) step) initial
  pure $ coda r

foldIO :: Jet a -> (s -> a -> IO s) -> IO s -> (s -> IO r) -> IO r
foldIO (Jet f) step initialIO coda = do
  initial <- initialIO
  r <- f (const False) step initial
  coda r


-- Byte Jets

-- https://stackoverflow.com/questions/49852060/how-to-choose-chunk-size-when-reading-a-large-file
-- https://askubuntu.com/questions/641900/how-file-system-block-size-works
-- https://stackoverflow.com/questions/1111661/8192-bytes-when-creating-file
data ChunkSize =
      DefaultChunkSize
    | ChunkSize Int
    | ChunkSize1K
    | ChunkSize4K
    | ChunkSize8K
    | ChunkSize16K
    | ChunkSize1M
    | ChunkSize2M
    deriving Show

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

class JetSource a source where
    jet :: source -> Jet a 

bytes :: ChunkSize -> Handle -> Jet ByteString
bytes (chunkSize -> count) handle =
    untilEOF System.IO.hIsEOF (flip B.hGetSome count) handle

instance JetSource ByteString Handle where
    jet = bytes DefaultChunkSize

instance JetSource a Handle => JetSource a File where
    jet (File path) = do
        handle <- withFile path 
        jet handle

-- | Uses the default system locale.
instance JetSource Line Handle where
    jet handle = 
        textToLine <$> untilEOF System.IO.hIsEOF T.hGetLine handle

--
--
-- Text Jets

decodeUtf8 :: Jet ByteString -> Jet Text
decodeUtf8 (Jet f) = Jet \stop step initial -> do
    let stop' = stop . pairExtract
        step' (Pair leftovers s) bytes = do
            T.Some !text !_ !leftovers' <- pure $ T.streamDecodeUtf8 bytes
            !s' <- step s text
            pure (Pair leftovers' s')
        initial' = Pair leftovers0 initial
    Pair leftovers final <-  f stop' step' initial'  
    T.Some !_ !bytes !_ <- pure $ T.streamDecodeUtf8 B.empty
    if | not (B.null bytes) -> 
         throwIO (T.DecodeError "Unconsumed leftovers at end." Nothing)
       | otherwise -> 
         pure final
  where 
    leftovers0 = 
        let T.Some _ _ g = T.streamDecodeUtf8 B.empty
         in g

encodeUtf8 :: Jet Text -> Jet ByteString
encodeUtf8 = fmap T.encodeUtf8

newtype Line = Line_ Text
    deriving newtype (Eq,Ord,Semigroup,Monoid,Show)

-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html
pattern Line text <- Line_ text

lineToText :: Line -> Text
lineToText (Line_ text) = text

textToLine :: Text -> Line
textToLine text 
    | Just _ <- T.find (=='\n') text = error "text for a line can't contain newlines!"
    | otherwise = Line_ (removeTrailingCarriageReturn text)

withLineText :: (Text -> r) -> Line -> r
withLineText f (Line text) = f text 

isEmptyLine :: Line -> Bool
isEmptyLine (Line_ text) = T.null text 

emptyLine :: Line
emptyLine = Line_ T.empty

newtype Utf8 a = Utf8 a

instance JetSource Text (Utf8 source) => JetSource Line (Utf8 source) where
    jet source = do
          jet @Text source
        & lines

instance JetSource ByteString source => JetSource Text (Utf8 source) where
    jet (Utf8 source) = do
          jet @ByteString source
        & decodeUtf8  

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
              ++ 
              if
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
        
downstream :: (s -> Bool) -> (s -> x -> IO s) -> [x] -> s -> IO s
downstream stop step = go
  where
    go [] s = 
        pure s
    go (x : xs) s 
        | stop s =
          pure s
        | otherwise = do
            s' <- step s x
            go xs s'

-- General funnels

type Funnel a = Jet a -> IO ()

class JetTarget a target where
    funnel :: target -> Funnel a

instance JetTarget ByteString Handle where
    funnel handle j = for_ j (B.hPut handle)

instance JetTarget a Handle => JetTarget a File where
    funnel (File path) j = System.IO.withFile path System.IO.WriteMode \handle ->
        funnel handle j

instance JetTarget ByteString target => JetTarget Text (Utf8 target) where 
    funnel (Utf8 target) j =
        j & encodeUtf8
          & funnel target

instance JetTarget Text (Utf8 target) => JetTarget Line (Utf8 target) where 
    funnel target j =
        j & fmap lineToText
          & intersperse (T.singleton '\n') 
          & funnel target

-- TODO: remove this.
-- Perhaps add a "locale" wrapper newtype, alternative to utf8. (but what about input?)
-- data StdStream = StdOut | StdErr deriving Show
-- 
-- stdStreamToHandle :: StdStream -> Handle
-- stdStreamToHandle StdOut = System.IO.stdout
-- stdStreamToHandle StdErr = System.IO.stderr
-- 
-- instance JetTarget Text StdStream where
--     funnel (stdStreamToHandle -> handle) = traverse_ T.putStr

-- | Uses the default system locale.
instance JetTarget Line Handle where
    funnel handle = traverse_ (T.hPutStrLn handle . lineToText)

-- | Uses the default system locale.
instance JetTarget Text Handle where
    funnel handle = traverse_ (T.hPutStr handle)

newtype File = File { getFilePath :: FilePath } deriving Show

-- DList helper
newtype DList a = DList { runDList :: [a] -> [a] }

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

traverseConcurrently :: (PoolConf -> PoolConf) -> (a -> IO b) -> Jet a -> Jet b
traverseConcurrently adaptConf makeTask upstream = Jet \stop step initial -> do
    let PoolConf {_inputQueueSize,_numberOfWorkers,_outputQueueSize} = adaptConf defaultPoolConf
    input <- newTBMQueueIO _inputQueueSize
    output <- newTBMQueueIO _outputQueueSize
    let worker = do
            mtask <- atomically $ readTBMQueue input
            case mtask of
                Nothing -> pure ()
                Just task -> do
                        result <- task
                        atomically $ writeTBMQueue output result
        inputReader = do
            for_ upstream \a -> do
                atomically $ writeTBMQueue input (makeTask a)
    undefined

data PoolConf = PoolConf {
        _inputQueueSize :: Int,
        _numberOfWorkers :: Int,
        _outputQueueSize :: Int
    } deriving Show

defaultPoolConf = PoolConf {
        _inputQueueSize = 1,
        _numberOfWorkers = 1,
        _outputQueueSize = 1
 }

inputQueueSize :: Int -> PoolConf -> PoolConf
inputQueueSize size poolConf = poolConf { _inputQueueSize = size }

numberOfWorkers :: Int -> PoolConf -> PoolConf
numberOfWorkers number poolConf = poolConf { _numberOfWorkers = number }

outputQueueSize :: Int -> PoolConf -> PoolConf 
outputQueueSize size poolConf = poolConf { _outputQueueSize = size }

