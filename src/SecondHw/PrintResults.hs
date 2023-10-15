module SecondHwInterpolation.PrintResults(main) where

import SecondHw.Interpolation (polynomialLagrange, polynomialNewton)
import qualified Control.Monad

-- число значений в таблице m+1
-- концы отрезка [a, b]
-- x -- точка интерполирования, значение в котором хотим найти
-- n -- степень интерполяционного многочлена
-- мой вариант: f(x) = sin(x) + x^2/2, a = 0.4, b = 0.9, m + 1 = 21, n = 11

f :: (Floating a) => a -> a
f x = sin x + (x ** 2) / 2

showOneLineOfTable :: (Show a1, Show a2) => (a1, a2) -> IO ()
showOneLineOfTable (a, b) = do
    putStrLn $ "(" ++ show a ++ ",\n\t" ++ show b ++ ")"

readN :: Int -> IO Int
readN m1 = do
    putStr $ "Введите значение n <= " ++ show (m1 - 1) ++ "\nn = "
    ns <- getLine
    let n = read ns :: Int
    if n > m1 - 1
        then do
            putStrLn "Вы ввели некорректное значение n"
            readN m1
        else return n

main :: IO ()
main = do
    putStr "Тема: Задача алгебраического интерполирования\nВариант №11\nВведите число значений в таблице\nm + 1 = "
    m <- getLine
    let m1 = read m :: Int
    putStr "Введите границы интервала\na = "
    as <- getLine
    let a = read as :: Double
    putStr "b = "
    bs <- getLine
    let b = read bs :: Double
    let listOfVar =
            [ (x, f x)
            | let h = (b - a) / fromIntegral (m1 - 1)
            , j <- [0 .. (m1 - 1)]
            , let x = a + fromIntegral j * h
            ]
    putStrLn "Таблица значений функции в формате: \n(x,\n\t f(x))"
    mapM_ showOneLineOfTable listOfVar
    cicleOfReadRes m1 listOfVar

cicleOfReadRes :: Int -> [(Double, b)] -> IO ()
cicleOfReadRes m1 listOfVar = do
    putStr "Введите точку интерполирования \nx = "
    xt <- getLine
    let x = read xt :: Double
    n <- readN m1
    printResults n listOfVar x
    putStr "Вы хотите продолжить с другими данными? Введите 'да' или 'y', если хотите\n"
    goToAnotherCicle <- getLine
    Control.Monad.when (goToAnotherCicle == "да" || goToAnotherCicle == "y") $ cicleOfReadRes m1 listOfVar

printResults :: (Eq a, Floating a, Show a) =>Int -> [(a, b)] -> a -> IO ()
printResults n listOfVar x = do
    -- Если очень хочется абстрагироваться от функции и работать именно со значениями из таблицы, то можно завести функцию
    -- let value listOfVar var = snd $ head $ filter (\x -> fst x == var) listOfVar
    -- и передать вместо f частично применённую функцию (value listOfVar)
    let polNewton = polynomialNewton f (n + 1) (map fst listOfVar) x
    putStrLn $ "Значение интерполяционного многочлена в форме Ньютона в точке " ++ show x ++ " равен: " ++ show polNewton
    putStrLn $ "|f(x) - PN(x)| = " ++ show (abs (f x - polNewton))
    let polLagr = polynomialLagrange f (n + 1) (map fst listOfVar) x
    putStrLn $ "Значение интерполяционного многочлена в форме Лагранжа в точке " ++ show x ++ " равен: " ++ show polLagr
    putStrLn $ "|f(x) - PL(x)| = " ++ show (abs (f x - polLagr))
