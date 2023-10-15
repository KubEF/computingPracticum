module SecondHw.Interpolation (polynomialNewton, polynomialLagrange) where

-- число значений в таблице m+1
-- концы отрезка [a, b]
-- x -- точка интерполирования, значение в котором хотим найти
-- n -- степень интерполяционного многочлена
-- мой вариант: f(x) = sin(x) + x^2/2, a = 0.4, b = 0.9, m + 1 = 21, n = 11

splitDiff :: (Fractional a, Eq a) => [a] -> (a -> a) -> a
splitDiff xs f = sum [f xj / product [xj - xi | xi <- xs, xi /= xj] | xj <- xs]

polynomialNewton :: (Fractional b, Eq b) => (b -> b) -> Int -> [b] -> b -> b
polynomialNewton f n xs x = helper n
    where
        helper 1 = f $ head xs
        helper deg = (product [x - xi | xi <- take (deg - 1) xs]) * splitDiff (take deg xs) f + helper (deg - 1)

polynomialLagrange :: (Eq a, Fractional a) => (a -> a) -> Int -> [a] -> a -> a
polynomialLagrange f n xs x = sum [f xi * linearComb xi | xi <- take n xs]
    where
        linearComb xi = product [(x - xj) / (xi - xj) | xj <- take n xs, xi /= xj]
