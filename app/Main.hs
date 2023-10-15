{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import FirstHw.CalculateFunc
import Lib (someFunc)
import SecondHw.PrintResults qualified
import ThirdHw.PrintResults qualified
import ThirdHw.RevInterpolation

main :: IO ()
main = someFunc
