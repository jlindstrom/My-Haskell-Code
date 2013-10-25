divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ldf k n | divides k n = k
ldf k n | k^2 > n     = n
ldf k n               = ldf (k+1) n

ld n = ldf 2 n

prime0 n | n <  1    = error "not a positive integer"
         | n == 1    = False
         | otherwise = ld n == n

