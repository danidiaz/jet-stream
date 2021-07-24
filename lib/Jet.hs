{-# LANGUAGE ImportQualifiedPost #-}
module Jet (
        -- * The Jet type
        Jet,
        -- * Building Jets
        J.each,
        J.repeat,
        J.repeatM,
        J.replicate,
        J.replicateM,
        J.iterate,
        J.iterateM,
        J.unfold,
        J.unfoldM,
        J.untilEOF,
        -- * List-like functions
        J.drop,
        J.dropWhile,
        J.take,
        J.takeWhile,
        J.mapAccum,
        J.mapAccumM,
        -- * Zips
        J.zip,
        J.zipWith,
        J.zipM,
        J.zipWithM,
        -- * Control operations
        J.control,
        J.control_,
        -- * Folding Jets
        J.fold,
        J.foldM,
    ) where

import Jet.Internal (Jet)
import Jet.Internal qualified as J

