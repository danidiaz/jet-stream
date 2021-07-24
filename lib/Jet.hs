{-# LANGUAGE ImportQualifiedPost #-}
module Jet (
        -- * The Jet type
        Jet,
        J.runJet,
        J.exhaustJet,
        J.effects,
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
        J.takeWhileIO,
        J.drop,
        J.dropWhile,
        J.dropWhileIO,
        J.mapAccum,
        J.mapAccumIO,
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
    ) where

import Jet.Internal (Jet)
import Jet.Internal qualified as J

-- $zips
--
-- It's not possible to zip two 'Jet's together. But 'Jet's can be zipped with
-- pure lists, or with lists of 'IO' actions.
--
--
