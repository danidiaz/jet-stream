{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
module Jet (
        -- * The Jet type
        Jet,
        J.run,
        J.consume,
        J.traverse,
        J.traverse_,
        J.for,
        J.for_,
        J.drain,
        defaults,
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
        J.toList,
        J.length,
        J.filter,
        J.filterIO,
        J.take,
        J.limit,
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
        J.withFile, 
        J.bracket,
        J.bracket_,
        J.bracketOnError,
        J.finally,
        J.onException, 
        -- ** Building your own
        J.control,
        J.unsafeCoerceControl,
        J.control_,
        J.unsafeCoerceControl_,
        -- * Folding Jets
        J.fold,
        J.foldIO,
        -- * Byte utils
        J.bytes,
        J.ChunkSize (..),
        J.Serialized,
        J.serialized,
        J.serializedLength,
        J.serializedBytes,
        -- * Text and line utils
        J.decodeUtf8,
        J.encodeUtf8,
        J.lines,
        J.unlines,
        J.linesUtf8,
        J.unlinesUtf8,
        J.Line (Line),
        J.lineToText,
        J.textToLine,
        J.stringToLine,
        J.lineContains,
        J.lineBeginsWith,
        J.prefixLine,
        -- * Concurrency
        traverseConcurrently,
        PoolConf,
        inputQueueSize,
        numberOfWorkers,
        outputQueueSize,
        -- * Process invocation
        throughProcess,
        linesThroughProcess,
        linesUtf8ThroughProcess,
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
        MealyIO(..),
        SplitStepResult(..),
        Combiners,
        combiners,
        withCombiners,
        -- * Re-exports
        -- $pipelines
        (&),
        (<&>),
        -- $standardstreams
        stdin,
        stdout,
        stderr,
        -- $process
        proc,
        shell,
    ) where

import System.IO (stdin, stdout, stderr)
import System.Process

import Jet.Internal
import Jet.Internal qualified as J

import Data.Function ((&))
import Data.Functor ((<&>))

-- $zips
--
-- It's not possible to zip two 'Jet's together. But 'Jet's can be zipped with
-- pure lists, or with lists of 'IO' actions.
--
--

-- $complicated
--
-- I didn't manage to make this stuff simpler.
--

-- $pipelines
-- I've found that the 'Data.Function.&' (reverse application) and 'Data.Functor.<&>' (reverse 'fmap')
-- operators feel quite natural for building pipelines.

-- $standardstreams
-- The standard streams, useful with functions like 'sink'.
--

-- $process
-- Functions that create process specs for use with 'throughProcess'. For more control, import the whole of "System.Process".
--
