module SecondHw.PrintResults (mainInterpolation) where

import Control.Monad (when)
import SecondHw.Interpolation
import Text.Layout.Table

-- число значений в таблице m+1
-- концы отрезка [a, b]
-- x -- точка интерполирования, значение в котором хотим найти
-- n -- степень интерполяционного многочлена
-- мой вариант: f(x) = sin(x) + x^2/2, a = 0.4, b = 0.9, m + 1 = 21, n = 11

f :: (Floating a) => a -> a
f x = sin x + (x ** 2) / 2

generateTable :: (Show a1, Show a2) => [(a1, a2)] -> String
generateTable listOfValues =
    tableString $
        columnHeaderTableS [numCol, numCol] unicodeRoundS (titlesH ["xi", "f(xi)"]) $
            map (\(x, fx) -> rowG [show x, show fx]) listOfValues

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

mainInterpolation :: IO ()
mainInterpolation = do
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
    putStrLn $ generateTable listOfVar
    cycleOfReadRes m1 listOfVar

cycleOfReadRes :: Int -> [(Double, b)] -> IO ()
cycleOfReadRes m1 listOfVar = do
    putStr "Введите точку интерполирования \nx = "
    xt <- getLine
    let x = read xt :: Double
    n <- readN m1
    printResults n listOfVar x
    putStr "Вы хотите продолжить с другими данными? Введите 'да' или 'y', если хотите\n"
    goToAnotherCycle <- getLine
    when (goToAnotherCycle == "да" || goToAnotherCycle == "y") $ cycleOfReadRes m1 listOfVar

printResults :: (Eq a, Floating a, Show a) => Int -> [(a, b)] -> a -> IO ()
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
