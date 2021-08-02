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
        -- * Text and line utils
        J.decodeUtf8,
        J.encodeUtf8,
        J.lines,
        J.unlines,
        J.Line (Line),
        J.lineToText,
        J.withLineText,
        -- ** These are partial functions
        J.textToLine,
        J.stringToLine,
        -- * Concurrency
        traverseConcurrently,
        PoolConf,
        inputQueueSize,
        numberOfWorkers,
        outputQueueSize,
        defaults,
        -- * Process invocation
        throughProcess,
        utf8LinesThroughProcess,
        ProcConf,
        bufferStdin,
        -- * Conversion helpers
        J.JetSource (..),
        J.JetSink (..),
        J.Sink (..),
        J.File (..),
        J.Utf8 (..),
        pattern J.Utf8File,
        -- * Some complicated stuff
        -- $complicated
        recast,
        Splitter (..),
        MealyIO(..),
        SplitStepResult(..),
        Combiners,
        combiners,
        -- * Re-exports
        stdin,
        stdout,
        stderr,
        proc,
        shell,
        (&),
        (<&>),
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
