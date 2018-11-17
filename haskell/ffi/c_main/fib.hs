{-# LANGUAGE ForeignFunctionInterface #-}

module Fib where

import Foreign.C.Types
import Foreign.Ptr
import System.IO
import Control.Monad
import Data.List

foreign export ccall fib :: Int -> Int
foreign export ccall printFib :: FunPtr (Int -> Int) -> IO()

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = foldl (+) 0 $ fib <$> [n - 1, n - 2]

printFib :: FunPtr (Int -> Int) -> IO()
printFib fibImpl =
    mapM_
        (putStrLn
        . (\p -> p!!0 ++ ". Fibonacci number: " ++ p!!1)
        . (\p -> show <$> [p, fib p]))
        $ take 13 [1,3..]
