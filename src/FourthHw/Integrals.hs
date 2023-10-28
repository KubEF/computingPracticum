module FourthHw.Integrals where

leftTriangleFormula :: (Num t) => (t -> t) -> t -> t -> t
leftTriangleFormula f a b = (b - a) * f a

rightTriangleFormula :: (Num t) => (t -> t) -> t -> t -> t
rightTriangleFormula f a b = (b - a) * f b

middleTriangleFormula :: (Fractional t) => (t -> t) -> t -> t -> t
middleTriangleFormula f a b = (b - a) * f ((a + b) / 2)

trapezoidFormula :: (Fractional t) => (t -> t) -> t -> t -> t
trapezoidFormula f a b = ((b - a) / 2) * (f a + f b)

simpsonFormula :: (Fractional t) => (t -> t) -> t -> t -> t
simpsonFormula f a b = ((b - a) / 6) * (f a + (4 * f ((a + b) / 2)) + f b)

threeEightFormula :: (Fractional t) => (t -> t) -> t -> t -> t
threeEightFormula f a b =
    (b - a)
        * ( f a / 8
                + (3 * f (a + h)) / 8
                + (3 * f (a + (2 * h))) / 8
                + f b / 8
          )
    where
        h = (b - a) / 3
