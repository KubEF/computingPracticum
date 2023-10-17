module ThirdHw.NumericalDiff (numericalDifferentiationFirst, numericalDifferentiationSecond) where

-- будем пользоваться следующими формулами для таблично заданной функции:
-- для центра: f'(a) = (f(a + h) - f (a - h))/(2*h)
-- для начала: f'(a) = (-3*f(a) + 4* f(a + h) - f(a + 2*h))/(2*h)
-- для конца:  f'(a) = (3*f(a) - 4* f(a - h) + f(a - 2*h))/(2*h)
--             f''(a)= (f(a+h) -2*f(a) + f(a-h))/(h^2)

numericalDifferentiationFirst :: (Fractional b) => [b] -> b -> [b]
numericalDifferentiationFirst funValues h = firstF : map centerDiff triples ++ [lastF]
    where
        triples = zip3 funValues (tail funValues) (tail $ tail funValues)
        centerDiff (f1, _, f3) = (f3 - f1) / (2 * h)
        firstF = ((-3) * head funValues + (4 * funValues !! 1) - (funValues !! 2)) / (2 * h)
        lastF = (3 * last funValues - (4 * funValues !! (length funValues - 2)) + (funValues !! (length funValues - 3))) / (2 * h)

numericalDifferentiationSecond :: (Floating b) => [b] -> b -> [b]
numericalDifferentiationSecond funValues h = map centerDiff triples
    where
        triples = zip3 funValues (tail funValues) (tail $ tail funValues)
        centerDiff (f1, f2, f3) = (f1 - (2 * f2) + f3) / (h ** 2)