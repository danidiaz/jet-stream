module Main (main) where

import Test.DocTest
main = doctest ["-ilib", "lib/Jet.hs", "lib/Jet/Internal.hs"]
