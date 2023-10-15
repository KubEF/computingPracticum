module ThirdHw.RevInterpolation (firstVariant, secondVariant) where

import FirstHw.CalculateFunc (root, rootSeparation, secantsMethod)
import SecondHw.Interpolation (polynomialLagrange)

firstVariant :: (Eq a, Floating a) => (a -> a) -> [a] -> Int -> a -> a
firstVariant f ys n = polynomialLagrange f n ys

secondVariant :: (Double -> Double) -> [Double] -> Int -> Double -> Double -> Double -> Double -> [Double]
secondVariant f xs n a b eps mainParam = map root roots
    where
        poly = polynomialLagrange f n xs
        polyWithDiff x = poly x - mainParam
        rootsIntervals = rootSeparation a b polyWithDiff 1000
        roots = map (\rootInterval -> uncurry secantsMethod rootInterval polyWithDiff eps) rootsIntervals