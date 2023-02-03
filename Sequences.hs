module Sequences where

import Data.Char (ord, chr)

-- Returns the first argument if it is larger than the second,
-- the second argument otherwise

maxOf2 :: Int -> Int -> Int --inputs are integer, outputs are integer (what does the middle one mean)
--check if x is larger than y 
maxOf2 x y| x>y = x
-- else return y 
          | otherwise = y

-- Returns the largest of three Ints
maxOf3 :: Int -> Int -> Int -> Int
-- firstly check the bigger one in y and z, then compare the return value with x
maxOf3 x y z = maxOf2 x (maxOf2 y z) 

-- Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit :: Char -> Bool
-- if the ord value of input is between ord value of 0 and 9, then return true
-- else return false 
isADigit x | ((ord '0' <= ord x) && (ord x <= ord '9')) = True
           | otherwise =False 

-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
-- False otherwise
isAlpha :: Char -> Bool
-- if the ord value of input is between a and z or between A and Z, then return true 
-- else return false 
isAlpha x |((ord 'a' <= ord x) && (ord x <= ord 'z')) || ((ord 'A' <= ord x) && (ord x <= ord 'Z')) = True
          |otherwise = False 


-- Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.
digitToInt :: Char -> Int
-- Pre: the character is one of '0'..'9'
-- check if the ord value of input is between 0 and 9, aka it is a digit, then convert the type to integer (i presume this can?)
digitToInt x | (ord '0' <= ord x) && (ord x <= ord '9') = ord x - ord '0'

-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
toUpper :: Char -> Char
-- convert the input to the small case version if only its ord value is between the ord value of A and Z
toUpper x | (ord 'a' <= ord x) && (ord x <= ord 'z') = chr(ord x - (ord 'a' - ord 'A'))
          | (ord 'A' <= ord x) && (ord x <= ord 'Z') = x

--
-- Sequences and series
--

-- Arithmetic sequence
arithmeticSeq :: Double -> Double -> Int -> Double
-- when its the first item in the sequence, then its value is a itself 
arithmeticSeq a d 0 = a
--else using the archimetric equation: Un = a+(n-1)d
arithmeticSeq a d n = a+ d * (fromIntegral n)

-- Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
--when its the first item in the geometric sequence, then the value is a itself 
--else. it follows the geomatric sequence, then it = a*(r^(n-1))
geometricSeq a r 0 = a
geometricSeq a r n = a*(r^(fromIntegral n))

-- Arithmetic series : sum of the arithmetic sequence
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries a d n = (a + (d * fromIntegral n) /2 ) * ((fromIntegral n) + 1)
-- Geometric series
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries a 1 n = a * ((fromIntegral n) + 1)
geometricSeries a r n = a * (1 - r^((fromIntegral n) + 1)) / (1 - r)

