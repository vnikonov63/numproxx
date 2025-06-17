module NumProxx.Numeric
  ( forwardDifference
  , backwardDifference
  , leftRule
  , rightRule
  , midpointRule
  , trapezoidRule
  , simpsons1_3Rule
  , simpsons3_8Rule
  , boolesRule
  ) where

-- === Numeric Differentiation ===

forwardDifference  :: (Double -> Double) -> Double -> Double -> (Double, Double)
forwardDifference f h x  = ((f (x + h) - f x) / h, h)

backwardDifference :: (Double -> Double) -> Double -> Double -> (Double, Double)
backwardDifference f h x = ((f x - f (x - h)) / h, h)

centralDifference  :: (Double -> Double) -> Double -> Double -> (Double, Double)
centralDifference f h x  = ((f (x + h) - f (x - h)) / (2 * h), h^2) 

-- === Numeric Integration ===

-- == Simple Rules == 

leftRule           :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
leftRule f a b 
    | b < a     = Nothing
    | otherwise =
        let h = b - a
        in Just (h * f a, h^2)

rightRule          :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
rightRule f a b
    | b < a     = Nothing
    | otherwise = 
        let h = b - a
        in Just (h * f b, h^2)

midpointRule       :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
midpointRule f a b
    | b < a     = Nothing
    | otherwise =
        let h = b - a
        in Just (h * f ((a + b) / 2), h^3)

trapezoidRule      :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
trapezoidRule f a b
    | b < a     = Nothing
    | otherwise =
        let h = b - a
        in Just ((h / 2) * (f a + f b), h^3)

simpsons1_3Rule    :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
simpsons1_3Rule f a b
    | b < a     = Nothing
    | otherwise =
        let h = (b - a) / 2
            x0 = a
            x1 = a + h
            x2 = b
        in Just ((h / 3) * (f x0 + 4 * f x1 + f x2), h^5)

-- Possibility of non-optimal error here
simpsons3_8Rule    :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
simpsons3_8Rule f a b
    | b < a     = Nothing
    | otherwise =
        let h = (b - a) / 3
            x0 = a 
            x1 = a + h
            x2 = a + 2h
            x3 = b
        in Just ((3 * h / 8) * (f x0 + 3 * f x1 + 3 * f x2 + f x3), h_1^5)

-- Possibility of non-optimal error here
boolesRule         :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
boolesRule f a b
    | b < a     = Nothing
    | otherwise =
        let h  = (b - a) / 4
          x0 = a
          x1 = a + h
          x2 = a + 2 * h
          x3 = a + 3 * h
          x4 = b
          result = (2 * h / 45) * (7 * f x0 + 32 * f x1 + 12 * f x2 + 32 * f x3 + 7 * f x4)
      in Just (result, h ** 6)

-- == Composite Rules == 
