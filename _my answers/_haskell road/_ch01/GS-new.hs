mnmInt :: [Int] -> Int
mnmInt [] = error "empty list" 
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

maxInt :: [Int] -> Int
maxInt [] = error "empty list" 
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

min' :: Int -> Int -> Int 
min' x y | x <= y    = x
         | otherwise = y

removeFst :: Int -> [Int] -> [Int]
removeFst r [] = []
removeFst r (x:xs) | r      == x  = xs
                   | r      /= x  = x:removeFst r xs
