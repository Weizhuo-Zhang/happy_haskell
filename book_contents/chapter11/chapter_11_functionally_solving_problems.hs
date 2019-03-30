import Data.List

solveRPN :: (Num a, Read a) => String -> a
solveRPN expression = head . foldl foldingFunction [] . words expression
    where foldingFunction (x:y:xs) "*" = (x * y):ys
          foldingFunction (x:y:ys) "+" = (x + y):ys
          foldingFunction (x:y:ys) "-" = (x - y):ys
          foldingFunction xs numberString = read numberString:xs












