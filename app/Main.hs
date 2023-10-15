module Main (main) where

import FirstHwEquation.CalculateFunc ()
import Lib (someFunc)
import SecondHwInterpolation.Interpolation ()

main :: IO ()
main = someFunc
