module FirstHw.CalculateFunc (rootSeparation, bisectionMethod, newtonMethod, newtonMethodModified, secantsMethod, root, delta, discrepancy, counter) where

f' :: (Floating a) => a -> a
f' x = 4 * x - log 2 * 2 ** x

-- f'' :: Floating a => a -> a
-- f'' x = 4 - log 2 ** 2 * 2 ** x

data MyResult = MyResult
    { root :: Double
    , delta :: Double
    , discrepancy :: Double
    , counter :: Int
    }

instance Show MyResult where
    show (MyResult root delta disp counter) =
        "\t\tНайденный корень с точностью eps: "
            ++ show root
            ++ "\n\t\tВеличина достигнутой точности: "
            ++ show delta
            ++ "\n\t\tМодуль невязки: "
            ++ show disp
            ++ "\n\t\tКоличество шагов, за которое был достигнут результат: "
            ++ show counter
            ++ "\n"

-- findNewtonStartingPoints :: (Ord p, Floating p) => (p -> p) -> p -> p -> p -> p
-- findNewtonStartingPoints f leftBound rightBound precision =
--     helper leftBound
--     where
--         helper leftBound
--             | leftBound > rightBound =
--                 error "Не могу найти начальную точку для метода Ньютона."
--             | f leftBound * f'' leftBound > 0.0 =
--                 leftBound
--             | otherwise =
--                 helper (leftBound + precision)

rootSeparation
    :: (Ord t, Ord a, Num a, Fractional t)
    => t
    -> t
    -> (t -> a)
    -> t
    -> [(t, t)]
rootSeparation a b f n = helper a b f h
    where
        h = (b - a) / n
        helper a' b' func h'
            | a' >= b' = []
            | (func a' * func (a' + h')) <= 0 = (a', a' + h') : helper (a' + h') b' func h'
            | otherwise = helper (a' + h') b' func h'

bisectionMethod
    :: Double
    -> Double
    -> (Double -> Double)
    -> Double
    -> Int
    -> MyResult
bisectionMethod a b f epsilon counter
    | f a == 0 = MyResult a 0 0 (counter + 1)
    | f c == 0 = MyResult c 0 0 (counter + 1)
    | (f a * f c) < 0 =
        if c - a > 2 * epsilon
            then bisectionMethod a c f epsilon (counter + 1)
            else MyResult ((a + c) / 2) ((c - a) / 2) (abs (f ((a + c) / 2))) (counter + 1)
    | otherwise =
        if b - c > 2 * epsilon
            then bisectionMethod c b f epsilon (counter + 1)
            else MyResult ((c + b) / 2) ((b - c) / 2) (abs (f ((c + b) / 2))) (counter + 1)
    where
        c = (a + b) / 2

newtonMethod
    :: Double
    -> Double
    -> (Double -> Double)
    -> Double
    -> MyResult
newtonMethod a b f epsilon = helper x f epsilon 0
    where
        x = (a + b) / 2
        helper arg func eps counter
            -- \| f' x == 0 || f x * f'' x <= 0 = helper (findNewtonStartingPoints f a b (10 ** (-6))) f epsilon counter
            | func x_next == 0 = MyResult x_next 0 0 (counter + 1)
            | abs (x_next - arg) <= eps = MyResult x_next (abs (x_next - arg)) (abs (func x_next)) (counter + 1)
            | otherwise = helper x_next func eps (counter + 1)
            where
                x_next = arg - func arg / f' arg

newtonMethodModified
    :: Double
    -> Double
    -> (Double -> Double)
    -> Double
    -> MyResult
newtonMethodModified a b f epsilon = helper x f epsilon 0 x
    where
        x = (a + b) / 2
        helper arg func eps counter x_0
            | func x_next == 0 = MyResult x_next 0 0 (counter + 1)
            | abs (x_next - arg) <= eps = MyResult x_next (abs (x_next - arg)) (abs (func x_next)) (counter + 1)
            | otherwise = helper x_next func eps (counter + 1) arg
            where
                x_next = arg - func arg / f' x_0

secantsMethod
    :: Double
    -> Double
    -> (Double -> Double)
    -> Double
    -> MyResult
secantsMethod a b f epsilon = helper a b f epsilon 0
    where
        helper x x_pred func eps counter
            | func x_next == 0 = MyResult x_next 0 0 (counter + 1)
            | abs (x_next - x) < eps = MyResult x_next (abs (x_next - x)) (abs (func x_next)) (counter + 1)
            | otherwise = helper x_next x func eps (counter + 1)
            where
                x_next = x - (func x / (func x - func x_pred)) * (x - x_pred)
