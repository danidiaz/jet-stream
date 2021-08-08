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
import Data.Foldable
import Debug.Trace
import Data.Functor.Identity

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
                        assertBytesCorrectlySplit bucketSize (bytePieces splitSize az)
         in tests
    ,   
        testGroup "byteBundleSplitter" $
            let tests = do
                    splitSize <- [1..7]
                    bucketSize <- [splitSize..13]
                    pure $ 
                        testCase ("splitter splitSize=" ++ show splitSize ++ " bucketSize=" ++ show bucketSize) $ 
                            assertByteBundlesCorrectlySplit bucketSize (bytePieces splitSize az)
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

assertBytesCorrectlySplit :: Int -> [ByteString] -> IO ()
assertBytesCorrectlySplit bucketSize inputs = do
    let buckets = Prelude.repeat bucketSize
        j = J.recast (J.bytesOverBuckets buckets) combineIntoLists (J.each inputs)
    fragmentedGroups <- J.toList j 
    let groups :: [ByteString] = mconcat <$> fragmentedGroups
        concatenatedInput = T.decodeUtf8 $ mconcat inputs
        concatenatedOutput = T.decodeUtf8 $ mconcat groups
    assertEqual "combined inputs and result" concatenatedInput concatenatedOutput
    -- traceIO "--------------------------"
    -- traceIO $ "+ original groups = " ++ show fragmentedGroups
    -- traceIO $ "+ collected groups = " ++ show groups
    -- traceIO $ "* bucket size = " ++ show bucketSize
    -- traceIO $ show $ B.length <$> Prelude.init groups
    -- traceIO "--------------------------"
    assertBool "group sizes are wrong" $ all (\g -> B.length g == bucketSize) (Prelude.init groups)
    pure ()

assertByteBundlesCorrectlySplit :: Int -> [ByteString] -> IO ()
assertByteBundlesCorrectlySplit bucketSize inputs = do
    let buckets = Prelude.repeat bucketSize
        j = J.recast (J.byteBundlesOverBuckets buckets) combineIntoLists (bundle . Identity <$> J.each inputs)
    fragmentedGroups <- J.toList j 
    let groups :: [ByteString] = mconcat <$> fragmentedGroups
        concatenatedInput = T.decodeUtf8 $ mconcat inputs
        concatenatedOutput = T.decodeUtf8 $ mconcat groups
    assertEqual "combined inputs and result" concatenatedInput concatenatedOutput
    -- traceIO "--------------------------"
    -- traceIO $ "+ inputs = " ++ show inputs
    -- traceIO $ "+ original groups = " ++ show fragmentedGroups
    -- traceIO $ "+ collected groups = " ++ show groups
    -- traceIO $ "* bucket size = " ++ show bucketSize
    -- traceIO $ show $ B.length <$> Prelude.init groups
    -- traceIO "--------------------------"
    assertBool "group sizes are wrong" $ all (\g -> B.length g <= bucketSize) (Prelude.init groups)
    pure ()


main :: IO ()
main = defaultMain tests

-- TODO
-- - test byteBundlesOverBuckets
-- - test newlines
-- - test throughProcess
-- - test traverseConcurrently
--
