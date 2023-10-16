module Main (main) where

import Lib ()
import SecondHw.PrintResults (mainInterpolation)
import ThirdHw.PrintResults (mainNumDiff, mainRevInterpol)

main :: IO ()
main = do
    mainInterpolation
    mainRevInterpol
    mainNumDiff
