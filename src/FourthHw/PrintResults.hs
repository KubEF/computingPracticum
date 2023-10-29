module FourthHw.PrintResults (mainIntegration) where

import FourthHw.Integrals (leftTriangleFormula, middleTriangleFormula, rightTriangleFormula, simpsonFormula, threeEightFormula, trapezoidFormula)
import Text.Layout.Table

f :: (Floating a) => a -> a
f x = x * exp x * sin x

primF :: (Floating a) => a -> a
primF x = (exp x / 2) * (x * (sin x - cos x) + cos x)

p0 :: (Num a) => p -> a
p0 x = 34

primP0 :: (Num a) => a -> a
primP0 x = 34 * x

p1 :: p -> p
p1 x = x

primP1 :: (Floating a) => a -> a
primP1 x = (x ** 2) / 2

p2 :: (Floating a) => a -> a
p2 x = 12.6 * (x ** 2) + 6.4 * x + 3.2

primP2 :: (Floating t) => t -> t
primP2 x = 4.2 * (x ** 3) + 3.2 * (x ** 2) + 3.2 * x

p3 :: (Floating a) => a -> a
p3 x = x ** 3 - 7 * x + 7

primP3 :: (Floating a) => a -> a
primP3 x = (x ** 4) / 4 - 3.5 * (x ** 2) + 7 * x

calculateActualPrimitiveValue :: (Num a) => (t -> a) -> t -> t -> a
calculateActualPrimitiveValue primitive a b = primitive b - primitive a

generateTable :: (Show a, Num a) => [(String, a)] -> a -> String
generateTable listOfValues actVal =
    tableString
        $ columnHeaderTableS
            [numCol, numCol, numCol]
            unicodeRoundS
            ( titlesH
                [ "Метод"
                , "Получившееся значение интеграла"
                , "Абсолютная фактическая погрешность"
                ]
            )
        $ map (\(name, value) -> rowG [name, show value, show $ abs (actVal - value)]) listOfValues

calculate :: (Show a, Fractional a) => (a -> a) -> (a -> a) -> a -> a -> String -> IO ()
calculate function primitive a b functionStr = do
    putStrLn $ "Для функции: " ++ functionStr
    let leftTriangle = leftTriangleFormula function a b
    let rightTriangle = rightTriangleFormula function a b
    let middleTriangle = middleTriangleFormula function a b
    let trapezoid = trapezoidFormula function a b
    let threeEight = threeEightFormula function a b
    let simpson = simpsonFormula function a b
    let actVal = calculateActualPrimitiveValue primitive a b
    putStrLn $ "Точное значение интеграла: " ++ show actVal
    putStrLn $
        generateTable
            [ ("Левого прямоугольника", leftTriangle)
            , ("Правого прямоугольника", rightTriangle)
            , ("Среднего прямоугольника", middleTriangle)
            , ("Трапеции", trapezoid)
            , ("3/8", threeEight)
            , ("Симпсона", simpson)
            ]
            actVal

mainIntegration :: IO ()
mainIntegration = do
    putStrLn "Тема: Задача приближённого вычисления интеграла по квадратурным формулам"
    putStr "Введите границы интервала\na = "
    as <- getLine
    let a = read as :: Double
    putStr "b = "
    bs <- getLine
    let b = read bs :: Double
    mapM_
        (\(func, primitive, functionStr) -> calculate func primitive a b functionStr)
        [ (p0, primP0, "f(x) = 34")
        , (p1, primP1, "f(x) = x")
        , (p2, primP2, "f(x) = 12.6*x^2 + 6.4*x + 3.2")
        , (p3, primP3, "f(x) = x^3 - 7*x + 7")
        , (f, primF, "f(x) = x * exp x * sin x")
        ]
    mainIntegration
