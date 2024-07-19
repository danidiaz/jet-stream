{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}

-- | A streaming library build around the 'Jet' type, which behaves as a kind of \"effectful list\".
--
-- For example, here's a way to print the first ten lines of a file to @stdout@:
--
-- >>> action = J.jet @Line (File "foo.txt") & J.limit 10 & J.sink stdout
--
-- The code is using the 'J.jet' function to create a 'Jet' of 'Line' values
-- (read using the default system encoding). 'J.jet' is part of the
-- 'J.JetSource' helper typeclass. Meanwhile, 'J.sink' is part of the
-- complementary 'J.JetSink' typeclass.
--
-- Note also the use of '(&)', which is simply a flipped '($)'. I've found it
-- useful to define forward-chained pipelines.
--
-- If instead of printing to @stdout@ we wanted to store the lines in a list:
--
-- >>> action = J.jet @Line (File "foo.txt") & J.limit 10 & J.toList
--
-- Imagine we wanted to print the combined lines of two files, excepting the
-- first 10 lines of each:
--
-- >>> :{
-- action =
--  do file <- J.each [File "foo.txt", File "bar.txt"]
--     jet @Line file & J.drop 10
--  & J.sink stdout
-- :}
--
-- Here we are making use of the 'Monad' instance of 'Jet', which resembles
-- that of conventional lists. We are mixing monadic do-blocks and conventional
-- function application. Also we use 'J.each', a function which creates a 'Jet'
-- out of any 'Foldable' container.
--
-- 'Jet's are 'Monoid's too, so we could have written:
--
-- >>> action = [File "foo.txt", File "bar.txt"] & foldMap (J.drop 10 . J.jet @Line) & J.sink stdout
--
-- Here's an interesting use of 'sink'. Imagine we have a big utf8-encoded file
-- and we want to split it into a number of files of no more than 100000 bytes
-- each, with the extra condition that we don't want to split any line between
-- two files. We could do it like this:
--
-- >>> :{
-- action =
--    let buckets = BoundedSize 100000 . File . ("result.txt." ++) . show <$> [1..]
--     in jet (File "12999.txt.utf-8")
--        & J.decodeUtf8
--        & J.lines
--      <&> (\line -> J.lineToUtf8 line <> J.textToUtf8 J.newline)
--        & J.sink buckets
-- :}
--
-- In this example we aren't using the default system encoding: instead of
-- that, we are reading bytes, explicity decoding them with 'J.decodeUtf8' and
-- finding 'J.lines'. Then we create a 'ByteBundle' for each 'Line' to signify
-- that it shouldn't be broken, and end by writing to a sequence of
-- 'BoundedSize' 'File's.
module Jet
  ( -- * The Jet type
    Jet,
    J.run,
    J.consume,
    J.drain,
    -- J.flatMap,

    -- * Building Jets
    J.each,
    J.repeat,
    J.repeatIO,
    J.replicate,
    J.replicateIO,
    J.iterate,
    J.iterateIO,
    J.unfold,
    J.unfoldIO,
    J.untilEOF,
    J.untilNothing,

    -- * List-like functions
    -- $listlike
    J.toList,
    J.length,
    J.traverse,
    J.traverse_,
    J.for,
    J.for_,
    J.filter,
    J.filterIO,
    J.take,
    J.limit,
    J.takeWhile,
    J.takeWhileIO,
    J.drop,
    J.dropWhile,
    J.dropWhileIO,
    J.mapAccum,
    J.mapAccumIO,
    J.intersperse,

    -- * Zips
    -- $zips
    J.zip,
    J.zipWith,
    J.zipIO,
    J.zipWithIO,

    -- * Control operations
    -- $control
    J.withFile,
    J.bracket,
    J.bracket_,
    J.bracketOnError,
    J.finally,
    J.onException,

    -- ** Building your own
    -- $doityourself
    J.control,
    J.control_,

    -- * Folding Jets
    -- $folding
    J.fold,
    J.foldIO,

    -- * Byte utils
    J.bytes,
    J.ChunkSize (..),
    J.ByteBundle,
    J.bundle,
    J.bundleLength,
    J.bundleBytes,

    -- * Text and line utils
    J.decodeUtf8,
    J.encodeUtf8,
    J.Line (Line),
    J.lines,
    J.unlines,
    J.newline,
    J.lineToText,
    J.lineToUtf8,
    J.textToLine,
    J.textToUtf8,
    J.stringToLine,
    J.lineContains,
    J.lineBeginsWith,
    J.prefixLine,

    -- * Concurrency
    traverseConcurrently,
    PoolConf,
    defaults,
    inputQueueSize,
    numberOfWorkers,
    outputQueueSize,

    -- * Process invocation
    throughProcess,
    linesThroughProcess,
    utf8LinesThroughProcess,
    ProcConf,
    bufferStdin,
    readFromStderr,
    handleExitCode,

    -- * Conversion helpers
    J.JetSource (..),
    J.JetSink (..),
    J.Sink (..),
    J.File (..),
    J.BoundedSize (..),
    J.BucketOverflow (..),

    -- * Some complicated stuff
    -- $complicated
    recast,
    Splitter (..),
    MealyIO (..),
    SplitStepResult (..),
    bytesOverBuckets,
    byteBundlesOverBuckets,
    Combiners,
    combiners,
    withCombiners,
    withCombiners_,
    combineIntoLists,

    -- * Re-exports
    -- $pipelines
    (&),
    (<&>),
    -- $standardstreams
    stdin,
    stdout,
    stderr,
    -- $exceptions
    T.UnicodeException,
    -- $process
    proc,
    shell,
  )
where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text.Encoding.Error qualified as T
import Jet.Internal
import Jet.Internal qualified as J
import System.IO (stderr, stdin, stdout)
import System.Process

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

-- $zips
--
-- It's not possible to zip two 'Jet's together. But 'Jet's can be zipped with
-- pure lists, or with lists of 'IO' actions.

-- $complicated
--
-- I didn't manage to make this stuff simpler.

-- $pipelines
-- I've found that the 'Data.Function.&' (reverse application) and 'Data.Functor.<&>' (reverse 'fmap')
-- operators feel quite natural for building pipelines.

-- $standardstreams
-- The standard streams, useful with functions like 'sink'.

-- $exceptions
-- Thrown when decoding UTF8.

-- $process
-- Functions that create process specs for use with 'throughProcess'. For more control, import the whole of "System.Process".

-- $folding These functions can be used directly, but they're also useful for
-- interfacing with the @Applicative@ folds from the
-- [foldl](https://hackage.haskell.org/package/foldl) library, with the help of
-- functions like @Control.Foldl.purely@ and @Control.Foldl.impurely@.
--
-- @Applicative@ folds are useful because they let you run multiple
-- \"analyses\" of a 'Jet' while going through it only once.

-- $doityourself
-- These are for advanced usage.
--
-- Sometimes we want to lift some existing
-- resource-handling operation not already covered, one that works with plain
-- 'IO' values. These functions help with that.

-- $control
-- Some 'Jet's must allocate resources to do its work. For example, opening a
-- text file and yielding its lines. These resources must be promptly released
-- when the 'Jet' itself finishes or the consumers stops it (for example, by
-- using 'limit' on the 'Jet'). They must also be released in the face of
-- exceptions.
--
-- Here are various control operations like those from "Control.Exception", but
-- lifted to work on 'Jet's.
--
-- When put in a do-block, these operations \"protect\" every statement in the
-- do-block below the operation itself.

-- $listlike
--
-- In these functions, the 'Jet' is working as a kind of \"effectful list\".
-- The effects which produce the elements, and the effects with which we
-- transform and consume the elements, are always 'IO' effects.
--
-- Don't confuse these functions with similarly named functions from
-- 'Data.Traversable' or 'Control.Monad', for which 'Jet' doesn't work as the
-- \"container\", but as the Applicative/Monadic effect itself.
