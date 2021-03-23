module Main where

import Prelude
import Gauge.Main
import qualified Cases as A
import qualified Data.Text as B


main =
  defaultMain $
  [
    bench "camelize" $ nf A.camelize $! B.replicate 100 "Abc 123 / dsf asdf ;lkj. "
  ]
