-- Example 1.8

mnmInt :: [Int] -> Int
mnmInt [] = error "empty list" 
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

min' :: Int -> Int -> Int 
min' x y | x <= y    = x
         | otherwise = y

-- Exercise 1.9

maxInt :: [Int] -> Int
maxInt [] = error "empty list" 
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

-- Exercise 1.10

removeFst :: Int -> [Int] -> [Int]
removeFst r [] = []
removeFst r (x:xs) | r      == x  = xs
                   | r      /= x  = x:removeFst r xs

-- Example 1.11

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

srtInts' :: [Int] -> [Int] 
srtInts' [] = []
srtInts' xs = let
                m = mnmInt xs
              in m : (srtInts' (removeFst m xs))

-- Example 1.12

-- average :: [Int] -> Float
average [] = error "empty list"
average xs = sum xs / fromIntegral (length xs)

{- COMPLICATION

Much to my frustration, I could not get this example to work "as is."

The problem seems to be that length returns an Int value, but fromInt is now deprecated, and I couldn't figure how to import it.

In the end removing the type declaration "solved" the problem, but I'm not too happy about it.

Here's the deprecation note:

http://www.haskell.org/ghc/docs/4.08.2/set/sec-int.html

The following functions are deprecated in the new FFI. Do not use the following functions if you are interested in portability. Most of these functions are here for legacy reasons and may just vanish one day. You have been warned.

The Int module also exports the overloaded operations for converting to and from Haskell Ints. However, for coercing between various integer types, better use fromIntegral, which is specialized for all the common cases so should be fast enough.

toInt   :: (Integral a) => a -> Int
fromInt :: (Num a) => Int -> a

Portability note: both Hugs98 and all releases of GHC prior to ghc-4.05 also exports these two via the Prelude. So, to have code that uses toInt and fromInt be maximally portable, make sure you add an import on Int (even if the version of Hugs or GHC you're currently using may not export these two from there.)

-}

