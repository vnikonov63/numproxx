module Main where

main :: IO ()
main = do
  putStrLn "Enter a number:"              
  input <- getLine                        
  let n = read input :: Int               
  putStrLn $ "Square is: " ++ show (n*n)  