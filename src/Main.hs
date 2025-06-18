module Main where

import Numeric (forwardDifference)

main :: IO ()
main = do
  let (approx, err) = forwardDifference sin 0.01 0.5
  putStrLn $ "Approximate derivative: " ++ show approx
  putStrLn $ "Error estimate: " ++ show err 