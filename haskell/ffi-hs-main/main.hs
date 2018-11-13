{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import System.IO
import Control.Monad
import Data.List

foreign import ccall "fib" c_fib :: Int -> Int

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = foldl (+) 0 $ fib <$> [n - 1, n - 2]

printFib :: (Int -> Int) -> IO()
printFib fibImpl =
    mapM_
        (putStrLn
        . (\p -> p!!0 ++ ". Fibonacci number: " ++ p!!1)
        . (\p -> show <$> [p, fib p]))
        $ take 13 [1,3..]

main :: IO()
main = do
    putStrLn "Haskell Fibonacci implementation"
    printFib fib
    putStrLn "\nC Fibonacci implementation"
    printFib c_fib
