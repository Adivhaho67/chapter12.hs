import System.IO
import Control.Exception (catch, IOException)
import Data.List (sort)
import System.IO.Error (isDoesNotExistError)
import Text.Printf (printf)

---------------------------------------------------------
-- HC12T1: Print a Welcome Message
---------------------------------------------------------
welcomeMessage :: IO ()
welcomeMessage = putStrLn "Welcome to Haskell Programming!"

---------------------------------------------------------
-- HC12T2: Add Two Numbers
---------------------------------------------------------
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

---------------------------------------------------------
-- HC12T3: Factorial Function
---------------------------------------------------------
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

---------------------------------------------------------
-- HC12T4: First 10 Fibonacci Numbers
---------------------------------------------------------
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

first10Fibonacci :: [Integer]
first10Fibonacci = map fibonacci [0..9]

---------------------------------------------------------
-- HC12T5: Palindrome Checker
---------------------------------------------------------
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

---------------------------------------------------------
-- HC12T6: Sort a List of Integers
---------------------------------------------------------
sortList :: [Int] -> [Int]
sortList = sort

---------------------------------------------------------
-- HC12T7: Calculate Circle Area
---------------------------------------------------------
calculateCircleArea :: Floating a => a -> a
calculateCircleArea r = pi * r * r

---------------------------------------------------------
-- HC12T8: Merge Two Sorted Lists
---------------------------------------------------------
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
    | x < y     = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

---------------------------------------------------------
-- HC12T9: Read and Print File Content
---------------------------------------------------------
printFileContent :: FilePath -> IO ()
printFileContent path = catch (do
    contents <- readFile path
    putStrLn "File Content:"
    putStrLn contents
    ) handleError
  where
    handleError :: IOException -> IO ()
    handleError e
      | isDoesNotExistError e = putStrLn "Error: File does not exist."
      | otherwise             = putStrLn ("Error: " ++ show e)

---------------------------------------------------------
-- HC12T10: Mathematical Operations Module
---------------------------------------------------------
module MathOps (add, subtract', multiply, divide) where
add :: Num a => a -> a -> a
add = (+)

subtract' :: Num a => a -> a -> a
subtract' = (-)

multiply :: Num a => a -> a -> a
multiply = (*)

divide :: Fractional a => a -> a -> a
divide = (/)

---------------------------------------------------------
-- MAIN FUNCTION (demonstrates all tasks)
---------------------------------------------------------
main :: IO ()
main = do
    putStrLn "\n--- HC12T1: Welcome Message ---"
    welcomeMessage

    putStrLn "\n--- HC12T2: Add Two Numbers ---"
    print (addTwoNumbers 5 7)

    putStrLn "\n--- HC12T3: Factorial Function ---"
    print (factorial 5)

    putStrLn "\n--- HC12T4: First 10 Fibonacci Numbers ---"
    print first10Fibonacci

    putStrLn "\n--- HC12T5: Palindrome Checker ---"
    putStrLn "Enter a string to check for palindrome:"
    s <- getLine
    print (isPalindrome s)

    putStrLn "\n--- HC12T6: Sort a List of Integers ---"
    putStrLn "Enter a list of integers separated by spaces:"
    input <- getLine
    let nums = map read (words input) :: [Int]
    print (sortList nums)

    putStrLn "\n--- HC12T7: Calculate Circle Area ---"
    putStrLn "Enter radius:"
    r <- fmap read getLine :: IO Double
    printf "Area: %.2f\n" (calculateCircleArea r)

    putStrLn "\n--- HC12T8: Merge Two Sorted Lists ---"
    let list1 = [1,3,5]
        list2 = [2,4,6]
    print (mergeLists list1 list2)

    putStrLn "\n--- HC12T9: Read and Print File Content ---"
    putStrLn "Enter file path:"
    filePath <- getLine
    printFileContent filePath

    putStrLn "\n--- HC12T10: Mathematical Operations ---"
    print (MathOps.add 5 3)
    print (MathOps.subtract' 10 4)
    print (MathOps.multiply 6 7)
    print (MathOps.divide 10 2)
