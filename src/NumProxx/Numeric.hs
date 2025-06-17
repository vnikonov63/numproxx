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
  , compositeLeftRightRule
  , compositeMidpointRule
  , compositeTrapezoidRule
  , compositeSimpsons1_3Rule
  , compositeSimpsons3_8Rule
  , compositeBoolesRule
  ) where

-- === Numeric Differentiation ===

forwardDifference :: (Double -> Double) -> Double -> Double -> (Double, Double)
forwardDifference f h x  = ((f (x + h) - f x) / h, h)

backwardDifference :: (Double -> Double) -> Double -> Double -> (Double, Double)
backwardDifference f h x = ((f x - f (x - h)) / h, h)

centralDifference :: (Double -> Double) -> Double -> Double -> (Double, Double)
centralDifference f h x  = ((f (x + h) - f (x - h)) / (2 * h), h^2) 

-- === Numeric Integration ===

-- == Simple Rules == 

leftRule :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
leftRule f a b 
    | b < a = Nothing
    | otherwise =
        let h = b - a
            result = h * f a
        in Just (result, h^2)

rightRule :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
rightRule f a b
    | b < a = Nothing
    | otherwise = 
        let h = b - a
            result = h * f b
        in Just (result, h^2)

midpointRule :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
midpointRule f a b
    | b < a = Nothing
    | otherwise =
        let h = b - a
            result = h * f ((a + b) / 2)
        in Just (result, h^3)

trapezoidRule :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
trapezoidRule f a b
    | b < a = Nothing
    | otherwise =
        let h = b - a
            result = (h / 2) * (f a + f b)
        in Just (result, h^3)

simpsons1_3Rule          :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
simpsons1_3Rule f a b
    | b < a = Nothing
    | otherwise =
        let h = (b - a) / 2
            x0 = a
            x1 = a + h
            x2 = b
            result = (h / 3) * (f x0 + 4 * f x1 + f x2)
        in Just (result, h^5)

-- Possibility of non-optimal error here
simpsons3_8Rule :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
simpsons3_8Rule f a b
    | b < a = Nothing
    | otherwise =
        let h = (b - a) / 3
            x0 = a 
            x1 = a + h
            x2 = a + 2h
            x3 = b
            result = (3 * h / 8) * (f x0 + 3 * f x1 + 3 * f x2 + f x3)
        in Just (result, h^5)

-- Possibility of non-optimal error here
boolesRule :: (Double -> Double) -> Double -> Double -> Maybe (Double, Double)
boolesRule f a b
    | b < a = Nothing
    | otherwise =
        let h = (b - a) / 4
          x0 = a
          x1 = a + h
          x2 = a + 2 * h
          x3 = a + 3 * h
          x4 = b
          result = (2 * h / 45) * (7 * f x0 + 32 * f x1 + 12 * f x2 + 32 * f x3 + 7 * f x4)
        in Just (result, h^6)

-- == Composite Rules == 

compositeLeftRightRule :: (Double -> Double) -> Double -> Double -> Int -> Maybe (Double, Double)
compositeLeftRule f a b n
    | b < a || n <= 0 = Nothing
    | otherwise =
        let h = (b - a) / fromIntegral n
            x_i = [a + fromIntegral i * h | i <- [0 .. n - 1]]
            result = h * sum (map f x_i)
        in Just (result, h)

compositeMidpointRule :: (Double -> Double) -> Double -> Double -> Int -> Maybe (Double, Double)
compositeMidpointRule f a b n
    | b < a || n <= 0 = Nothing
    | otherwise =
        let h = (b - a) / fromIntegral n
            x_i = [a + (fromIntegral i + 0.5) * h | i <- [0 .. n - 1]]
            result = h * sum (map f x_i)
        in Just (result, h^2)

compositeTrapezoidRule   :: (Double -> Double) -> Double -> Double -> Int -> Maybe (Double, Double)
compositeTrapezoidRule f a b n
    | b < a || n <= 0 = Nothing
    | otherwise = 
        let h = (b - a) / fromIntegral n
            x_i = [a + fromIntegral i * h | i <- [1 .. n - 1]]
            midSum = sum (map f x_i)
            result = (h / 2) * (f a + 2 * midSum + f b)
        in Just (result, h^2)
        
compositeSimpsons1_3Rule :: (Double -> Double) -> Double -> Double -> Int -> Maybe (Double, Double)
    | b < a || n <= 0 || odd n = Nothing
    | otherwise = 
        let h = (b - a) / fromIntegral n
            x_i = [a + fromIntegral i * h | i <- [1 .. n - 1]]
            evenSum = sum [f x | (i, x) <- zip [1..] x_i, even i]
            oddSum = sum [f x | (i, x) <- zip [1..] x_i, odd i]
            result = (h / 3) * (f a + 2 * evenSum + 4 * oddSum + f b)
        in Just (result, h^4)

compositeSimpsons3_8Rule :: (Double -> Double) -> Double -> Double -> Int -> Maybe (Double, Double)
    | b < a || n <= 0 || n `mod` 3 /= 0 = Nothing
    | otherwise = 
        let h = (b - a) / fromIntegral n
            x_i = [a + fromIntegral i * h | i <- [1 .. n - 1]]
            div3Sum = sum [f x | (i, x) <- zip [1..] x_i, i `mod` 3 == 0]
            ndiv3Sum = sum [f x | (i, x) <- zip [1..] x_i, i `mod` 3 /= 0]
            result = (3 * h / 8) * (f a + 3 * ndiv3Sum + 2 * div3Sum + f b)
        in Just (result, h^5)


compositeBoolesRule :: (Double -> Double) -> Double -> Double -> Int -> Maybe (Double, Double)
    | b < a || n <= 0 || n `mod` 4 /= 0 = Nothing
    | otherwise =
        let h = (b - a) / fromIntegral n
            x_i = [a + fromIntegral i * h | i <- [0 .. n]]
            oddSum    = sum [f x | (i, x) <- zip [1..] x_i, odd i]
            mod4_2Sum = sum [f x | (i, x) <- zip [1..] x_i, i `mod` 4 == 2]
            mod4_0Sum = sum [f x | (i, x) <- zip [1..] x_i, i `mod` 4 == 0]
            result = (2 * h / 45) * (7 * (f a + f b) + 32 * oddSum + 12 * mod4_2Sum + 14 * mod4_0Sum)
        in Just (result, h^6)

-- === Richardson extrapolation ===
