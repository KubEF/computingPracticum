module Main (main) where

import FourthHw.PrintResults (mainIntegration)
import Lib ()
import SecondHw.PrintResults (mainInterpolation)
import ThirdHw.PrintResults (mainNumDiff, mainRevInterpol)

main :: IO ()
main = do
    mainInterpolation
    mainRevInterpol
    mainNumDiff
    mainIntegration