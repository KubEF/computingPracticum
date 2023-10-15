module ThirdHw.RevInterpolation (firstVariant, secondVariant) where

import FirstHw.CalculateFunc (root, rootSeparation, secantsMethod)
import SecondHw.Interpolation (polynomialLagrange)

f :: (Floating a) => a -> a
f x = sin x + x ** 2 / 2

firstVariant :: (Eq a, Floating a) => [a] -> Int -> a -> a
firstVariant ys n = polynomialLagrange f n ys

secondVariant :: [Double] -> Int -> Double -> Double -> Double -> Double -> [Double]
secondVariant xs n a b eps mainParam = map root roots
    where
        poly = polynomialLagrange f n xs
        polyWithDiff = ((-) . poly) mainParam
        rootsIntervals = rootSeparation a b polyWithDiff 1000
        roots = map (\rootInterval -> uncurry secantsMethod rootInterval polyWithDiff eps) rootsIntervals