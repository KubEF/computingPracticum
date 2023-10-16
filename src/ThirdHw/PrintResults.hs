module ThirdHw.PrintResults (mainRevInterpol, mainNumDiff) where

import Control.Monad (when)
import Data.List (zip4)
import Text.Layout.Table
import ThirdHw.NumericalDiff
import ThirdHw.RevInterpolation

f :: (Floating a) => a -> a
f x = sin x + x ** 2 / 2

g :: (Floating a) => a -> a
g x = exp $ 1.5 * x

g' :: (Floating a) => a -> a
g' x = 1.5 * g x

g'' :: (Floating a) => a -> a
g'' x = 1.5 * g' x

generateTableToArgsValues :: (Show a1, Show a2) => [(a1, a2)] -> String
generateTableToArgsValues listOfVars =
    tableString $
        columnHeaderTableS [numCol, numCol] unicodeRoundS (titlesH ["xi", "f(xi)"]) $
            map (\(x, fx) -> rowG [show x, show fx]) listOfVars

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
    when (goToAnotherCycle == "да" || goToAnotherCycle == "y") $ cycleOfReadRes m1 listOfVar

printResults :: Int -> [(Double, Double)] -> Double -> Double -> IO ()
printResults n listOfVar x eps = do
    let value listOfPairs var = fst $ head $ filter (\y -> snd y == var) listOfPairs
    let firstVar = firstVariant (value listOfVar) (map snd listOfVar) n x
    putStrLn "Первым методом нашёлся корень:"
    putStrLn $ "X = " ++ show firstVar ++ " \n|f(X) - F| = " ++ show (f firstVar - x)
    let secondVar = secondVariant f (map fst listOfVar) n (fst $ head listOfVar) (fst $ last listOfVar) eps x
    if null secondVar
        then do
            putStrLn $
                "Вторым методом на промежутке ["
                    ++ show (fst $ head listOfVar)
                    ++ ", "
                    ++ show (fst $ last listOfVar)
                    ++ "] значение не найдено"
        else do
            putStrLn $ "Вторым методом корней нашлось: " ++ show (length secondVar)
            mapM_ (\root -> putStrLn $ "X = " ++ show root ++ " \n|f(X) - F| = " ++ show (f root - x)) secondVar

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
    putStrLn $ generateTableToArgsValues listOfVar
    cycleOfReadRes m1 listOfVar

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

generateTableToNumDiff :: (Show a1, Show a2, Floating a1) => [(a1, a2, a1, a1)] -> String
generateTableToNumDiff values =
    tableString
        $ columnHeaderTableS
            [numCol, numCol, numCol, numCol, numCol, numCol, numCol, numCol]
            unicodeRoundS
            ( titlesH
                [ "xi"
                , "f(xi)"
                , "f'(xi)_num"
                , "|f'(xi)_act - f'(x_i)_num|"
                , "|f'(xi)_act - f'(x_i)_num|/|f'(xi)_act|"
                , "f''(xi)_num"
                , "|f''(xi)_act - f''(x_i)_num|"
                , "|f''(xi)_act - f''(x_i)_num|/|f''(xi)_act|"
                ]
            )
        $ map
            ( \(xi, fxi, f'xi, f''xi) ->
                let
                    absError val dif = abs $ dif xi - val
                    relError val dif = absError val dif / abs (dif xi)
                in
                    rowG
                        [ show xi
                        , show fxi
                        , show f'xi
                        , show $ absError f'xi g'
                        , show $ relError f'xi g'
                        , show f''xi
                        , show $ absError f''xi g''
                        , show $ relError f''xi g''
                        ]
            )
            values

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
    let listOfValuesZero = map g xis
    let listOfValuesFirst = numericalDifferentiationFirst listOfValuesZero h
    let listOfValuesSecond = numericalDifferentiationSecond listOfValuesZero h
    putStrLn $ generateTableToNumDiff (zip4 xis listOfValuesZero listOfValuesFirst (0 : listOfValuesSecond ++ [0]))
    putStr "Вы хотите продолжить с другими данными? Введите 'да' или 'y', если хотите\n"
    goToAnotherCycle <- getLine
    when (goToAnotherCycle == "да" || goToAnotherCycle == "y") mainNumDiff