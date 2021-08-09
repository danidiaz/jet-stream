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
{-# LANGUAGE NumDecimals #-}
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
import Data.Time.Clock
import Data.Foldable
import Debug.Trace
import Data.Functor.Identity
import Control.Concurrent
import Data.List

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
    ,   
        testGroup "lines" $
            let tests = do
                    fragmentSize <- [1..13]
                    pure $ 
                        testCase ("fragment size " ++ show fragmentSize) $
                           assertLines fragmentSize lineData01 lineExpected01 
             in tests
    ,
        testGroup "process" $ 
            [
                testCase "simple" $ 
                    do resultLines <-
                             mempty
                           & linesThroughProcess defaults (shell "echo foo")
                           & J.toList
                       assertEqual "input and output lines don't match" (textToLine . T.pack <$> ["foo"]) resultLines
                -- testCase "simple" $ 
                --     do let inputLines = textToLine . T.pack <$> ["aaa","bbb","ccc"]
                --        resultLines <-
                --              J.each inputLines
                --            & linesThroughProcess defaults (shell "cat")
                --            & J.toList
                --        assertEqual "input and output lines don't match" inputLines resultLines
            ]
    , 
        testGroup "concurrency" $ 
            [
                testCase "compare" $ do
                    let yieldAfter d x = sleep d *> pure x 
                        delay = cents 1  
                        upstream = J.each "abcde"
                    (ts, rsequential) <- upstream & J.traverse (yieldAfter delay) & J.toList & time
                    (t1, rconc1) <- upstream & J.traverseConcurrently defaults (yieldAfter delay) & J.toList & time
                    (t2, rconc2) <- upstream & J.traverseConcurrently (numberOfWorkers 10) (yieldAfter delay) & J.toList & time
                    let (rsequential', rconc1', rconc2') = (sort rsequential, sort rconc1, sort rconc2)
                    assertEqual "sequential != conc" rsequential' rconc1'
                    assertEqual "conc != conc 2" rconc1' rconc2'
                    assertBool "conc not faster" (t1 < ts)
                    assertBool "conc2 not faster" (t2 < t1)
                    pure ()
            ]
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


lineData01 :: Text
lineData01 = T.pack "aaa\nbb\nccc\ndddd\n\neee\n\n\nfffffffff\ng\niiiii"

lineExpected01 :: [Line] 
lineExpected01 = textToLine . T.pack <$> ["aaa","bb","ccc","dddd","","eee","","","fffffffff", "g", "iiiii"]

textPieces :: Int -> Text -> [Text]
textPieces size =
    let go t =
            if T.null t
            then []
            else let (left,right) = T.splitAt size t
                 in left : go right
    in go

assertLines :: Int -> Text -> [Line] -> IO ()
assertLines textFragmentSize input expected = do
    let pieces = textPieces textFragmentSize input
    ls <- J.each pieces & J.lines & J.toList
    assertEqual "lines do not match expected" expected ls

sleep :: Delay -> IO ()
sleep (Delay d) = threadDelay d

newtype Delay = Delay Int

cents :: Int -> Delay 
cents i = Delay $ i * 1e4 

main :: IO ()
main = defaultMain tests

time :: IO a -> IO (NominalDiffTime, a)
time action = do
    start <- getCurrentTime
    a <- action
    stop <- getCurrentTime
    pure (diffUTCTime stop start, a)

-- TODO
-- - test byteBundlesOverBuckets
-- - test newlines
-- - test throughProcess
-- - test traverseConcurrently
--
