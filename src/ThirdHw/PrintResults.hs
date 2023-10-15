module ThirdHw.PrintResults (mainRevInterpol, mainNumDiff) where

import Control.Monad qualified
import Data.List (zip4)
import ThirdHw.NumericalDiff (numericalDifferentiationFirst, numericalDifferentiationSecond)
import ThirdHw.RevInterpolation (firstVariant, secondVariant)

f :: (Floating a) => a -> a
f x = sin x + x ** 2 / 2

g :: (Floating a) => a -> a
g x = exp $ 1.5 * x

g' :: (Floating a) => a -> a
g' x = 1.5 * g x

g'' :: (Floating a) => a -> a
g'' x = 1.5 * g' x

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

mainRevInterpol :: IO ()
mainRevInterpol = do
    putStr "Тема: Задача обратного интерполирования\nВариант №11\nВведите число значений в таблице\nm + 1 = "
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
    cycleOfReadRes m1 listOfVar

cycleOfReadRes :: Int -> [(Double, Double)] -> IO ()
cycleOfReadRes m1 listOfVar = do
    putStr "Введите точку обратного интерполирования \nF = "
    xt <- getLine
    let x = read xt :: Double
    n <- readN m1
    putStr "Введите точность \neps = "
    epsStr <- getLine
    let eps = read epsStr :: Double
    printResults n listOfVar x eps
    putStr "Вы хотите продолжить с другими данными? Введите 'да' или 'y', если хотите\n"
    goToAnotherCycle <- getLine
    Control.Monad.when (goToAnotherCycle == "да" || goToAnotherCycle == "y") $ cycleOfReadRes m1 listOfVar

printResults :: Int -> [(Double, Double)] -> Double -> Double -> IO ()
printResults n listOfVar x eps = do
    -- Если очень хочется абстрагироваться от функции и работать именно со значениями из таблицы, то можно завести функцию
    let value listOfPairs var = fst $ head $ filter (\y -> snd y == var) listOfPairs
    -- и передать вместо f частично применённую функцию (value listOfVar)
    let firstVar = firstVariant (value listOfVar) (map snd listOfVar) n x
    putStrLn "Первым методом нашёлся корень:"
    putStrLn $ "X = " ++ show firstVar ++ " \n|f(X) - F| = " ++ show (f firstVar - x)
    let secondVar = secondVariant f (map fst listOfVar) n (fst $ head listOfVar) (fst $ last listOfVar) eps x
    if null secondVar
        then do
            putStrLn $ "Вторым методом на промежутке [" ++ show (fst $ head listOfVar) ++ ", " ++ show (fst $ last listOfVar) ++ "] значение не найдено"
        else do
            putStrLn $ "Вторым методом корней нашлось: " ++ show (length secondVar)
            mapM_ (\root -> putStrLn $ "X = " ++ show root ++ " \n|f(X) - F| = " ++ show (f root - x)) secondVar

readH :: IO Double
readH = do
    putStr "Введите шаг\nh = "
    hs <- getLine
    let h = read hs :: Double
    if h <= 0
        then do
            putStrLn "Вы ввели некорректное число. h должно быть положительным"
            readH
        else return h

printOneLineOfTable :: (Show a1, Show a2, Floating a1) => a1 -> a2 -> a1 -> a1 -> IO ()
printOneLineOfTable xi zeroValue oneValue twoValue = do
    putStrLn $ show xi ++ "\t|\t" ++ show zeroValue ++ "\t|\t" ++ show oneValue ++ "\t|\t" ++ show (abs $ g' xi - oneValue) ++ "\t|\t" ++ show twoValue ++ "\t|\t" ++ show (abs $ g'' xi - twoValue)

mainNumDiff :: IO ()
mainNumDiff = do
    putStr "Тема: Задача численного дифференцирования\nВариант №11\nВведите число значений в таблице\nm + 1 = "
    m <- getLine
    let m1 = read m :: Int
    putStr "Введите начальную точку интервала\na = "
    as <- getLine
    let a = read as :: Double
    h <- readH
    let xis = [a + (fromIntegral i * h) | i <- [0 .. (m1 - 1)]]
    -- mapM_ showOneLineOfTable listOfValuesFirst
    let listOfValuesZero = map g xis
    let listOfValuesFirst = numericalDifferentiationFirst listOfValuesZero h
    let listOfValuesSecond = numericalDifferentiationSecond listOfValuesZero h
    mapM_ (\(xi, zeroValue, oneValue, twoValue) -> printOneLineOfTable xi zeroValue oneValue twoValue) (zip4 xis listOfValuesZero listOfValuesFirst (0 : listOfValuesSecond ++ [0]))
    putStr "Вы хотите продолжить с другими данными? Введите 'да' или 'y', если хотите\n"
    goToAnotherCycle <- getLine
    Control.Monad.when (goToAnotherCycle == "да" || goToAnotherCycle == "y") mainNumDiff