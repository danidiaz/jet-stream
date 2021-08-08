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
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures  #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL

import Jet
import Jet qualified as J

tests :: TestTree
tests =
  testGroup
    "All"
    [
        testGroup "byteSplitter" $
            let tests = do
                    -- splitSize <- [1]
                    -- bucketSize <- [2]
                    splitSize <- [1..7]
                    bucketSize <- [1..10]
                    pure $ 
                        testCase ("splitter splitSize=" ++ show splitSize ++ " bucketSize=" ++ show bucketSize) $ 
                            assertBytesCorrectlySplit (Prelude.repeat bucketSize) (bytePieces splitSize az)
             in tests
    ]

az :: ByteString
az = ['a'..'z']  & T.pack & T.encodeUtf8

bytePieces :: Int -> ByteString -> [ByteString]
bytePieces size =
    let go b =
            if B.null b
            then []
            else let (left,right) = B.splitAt size b
                 in left : go right
    in go

assertBytesCorrectlySplit :: [Int] -> [ByteString] -> IO ()
assertBytesCorrectlySplit buckets inputs = do
    let groupsJet = J.recast (J.bytesOverBuckets buckets) combineIntoLists (J.each inputs)
    groups :: [ByteString] <- mconcat <$> J.toList groupsJet 
    let concatenatedInput = mconcat inputs
        concatenatedOutput = mconcat groups
        allButLastGroups = Prelude.init groups
    assertEqual "combined inputs and result" (T.decodeUtf8 concatenatedInput) (T.decodeUtf8 concatenatedOutput)
    pure ()

main :: IO ()
main = defaultMain tests

-- TODO
-- - test byteBundlesOverBuckets
-- - test newlines
-- - test throughProcess
-- - test traverseConcurrently
--
