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

{-- As written in the book:

average :: [Int] -> Float
average [] = error "empty list"
average xs = fromInt (sum xs) / fromInt (length xs)

--}

-- Because fromInt is now deprecated and I couldn't
-- figure out how to import it, I changed [Int] to
-- [Integer] in the type declaration, and changed
-- fromInt to fromInteger in the last line:

average :: [Integer] -> Float
average [] = error "empty list"
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

{- Here's the deprecation note:

http://www.haskell.org/ghc/docs/4.08.2/set/sec-int.html

The following functions are deprecated in the new FFI. Do not use the following functions if you are interested in portability. Most of these functions are here for legacy reasons and may just vanish one day. You have been warned.

The Int module also exports the overloaded operations for converting to and from Haskell Ints. However, for coercing between various integer types, better use fromIntegral, which is specialized for all the common cases so should be fast enough.

toInt   :: (Integral a) => a -> Int
fromInt :: (Num a) => Int -> a

Portability note: both Hugs98 and all releases of GHC prior to ghc-4.05 also exports these two via the Prelude. So, to have code that uses toInt and fromInt be maximally portable, make sure you add an import on Int (even if the version of Hugs or GHC you're currently using may not export these two from there.)

-}

-- Further examples from this part of the book

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- Exercise 1.13

count :: Char -> String -> Int
count t [] = 0 
count t target | t == head target = 1 + count t (tail target)
               | t /= head target = 0 + count t (tail target)

-- And I eventually made (x:xs) work

count' :: Char -> String -> Int
count' t [] = 0
count' t (x:xs) | t == x     = 1 + count' t (xs)
                | otherwise  = 0 + count' t (xs)

-- Exercise 1.14

charTimes :: Int -> Char -> String
charTimes m c | m <  1    = error "positive integer required"
charTimes m c | m == 1    = [c]
charTimes m c | m >  1    = [c] ++ charTimes (m - 1) c

blowupGeneric :: Int -> Int -> String -> String
blowupGeneric a b s  | a < 1 = []
blowupGeneric a b []         = []
blowupGeneric a b s          = charTimes a (head(s)) ++ blowupGeneric (a + b) b (tail(s))

blowupGeneric' :: Int -> Int -> String -> String
blowupGeneric' a b s | a < 1 = []
blowupGeneric' a b []        = []
blowupGeneric' a b (x:xs)    = charTimes a x ++ blowupGeneric' (a + b) b xs

blowup :: String -> String
blowup s = blowupGeneric 1 1 s

blowup' :: String -> String
blowup' s = blowupGeneric' 1 1 s






