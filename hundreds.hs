--
-- Program: A bruteforcer for a funny game called "hundred"
-- Description: Given six numbers in some order and seven arithmetic operations (+, -, *, /, exp, n-th root, parentheses).
--      Paste any sequence of operations between numbers and get 100 (one hundred) as a result.
-- Author: Dmitry A. Paramonov (c) 2013
--
module Main where

import Control.Monad
import Control.Applicative
import Data.Traversable
import Data.Time (diffUTCTime, getCurrentTime)
import System.IO

data Tree = Leaf Int | Tree Ops Tree Tree
data Ops = Add | Sub | Mul | Div | Exp | Rtn
            deriving Show

instance Show Tree where
        show (Leaf x) = show x
        show (Tree Add l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
        show (Tree Sub l r) = "(" ++ show l ++ " - " ++ show r ++ ")"
        show (Tree Mul l r) = "(" ++ show l ++ " * " ++ show r ++ ")"
        show (Tree Div l r) = "(" ++ show l ++ " / " ++ show r ++ ")"
        show (Tree Exp l r) = "(" ++ show l ++ " ^ " ++ show r ++ ")"
        show (Tree Rtn l r) = "(" ++ show l ++ " root " ++ show r ++ ")"

--operator application
apply :: (Double -> Double -> Double) -> Maybe Double -> Maybe Double -> Maybe Double
apply f a b = do
        an <- a
        bn <- b
        return $ f an bn

--inverse Maybe value
inv :: Maybe Double -> Maybe Double
inv Nothing = Nothing
inv (Just 0) = Nothing
inv (Just n) = return (1 / n)

-- tree evaluation
eval :: Tree -> Maybe Double
eval (Leaf x) = return (fromIntegral x :: Double)
eval (Tree Add l r) = apply (+) (eval l) (eval r)
eval (Tree Sub l r) = apply (-) (eval l) (eval r)
eval (Tree Mul l r) = apply (*) (eval l) (eval r)
eval (Tree Div l r) = if n /= Just 0
                         then apply (/) (eval l) n
                         else Nothing
                      where n = eval r
eval (Tree Exp l r) = apply (**) (eval l) (eval r)
eval (Tree Rtn l r) = if n == Just 0
                         then return 1
                         else apply (**) (eval l) (inv n)
                      where n = eval r

--partitioning the list
parts :: [Int] -> [([Int], [Int])]
parts l = [(take i l, drop i l) | i <- [1..(length l) - 1]]

--all possible accomodations of the list of Ops
accom :: Int -> [Ops] -> [[Ops]]
accom n ops = sequenceA $ take n $ cycle [ops]

--split list of operations
splitops :: [Ops] -> [([Ops], Ops, [Ops])]
splitops l = [(take (i - 1) l, l !! (i - 1), drop i l) | i <- [1..length l]]

--generate all possible trees
trees :: [[Ops]] -> [Int] -> [Tree]
trees _ [] = []
trees [] _ = []
trees _ [x] = [(Leaf x)]
trees (ops : opss) l = (\t -> t ++ trees opss l) $ do
        (left, right) <- parts l
        (lops, x, rops) <- splitops ops
        Tree x <$> trees [lops] left <*> trees [rops] right

--find all hundreds
hundreds :: [Int] -> [Tree]
hundreds = filter (\t -> check (eval t) (Just 100)) . trees (accom 5 [Add, Sub, Mul, Div, Exp, Rtn])
           where check a b = (\n -> case n of {(Just x) -> x; Nothing -> False}) $ do
                         an <- a
                         bn <- b
                         return (abs (an - bn) <= epsilon)
                 epsilon = 1.0e-14

-- main routine
main :: IO ()
main = do
        putStr "Input six numbers without spaces > "
        hFlush stdout
        line <- getLine
        if length line /= 6
           then error "Wrong number of digits!"
           else do
                before <- getCurrentTime
                mapM_ (putStrLn . show) $ h line
                after <- getCurrentTime
                putStrLn $ "Hundreds found: " ++ show (length $ h line)
                putStrLn $ "Time taken: " ++ show (diffTime before after) ++ " ms"
        where diffTime t1 t2 = realToFrac (t2 `diffUTCTime` t1) * 1000.0 :: Double
              h l = hundreds (map (read . (:"")) l :: [Int])

