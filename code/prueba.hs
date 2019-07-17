distintos :: Eq a => [a] -> Bool
distintos []     = True
distintos (x:xs) = (notElem x xs) && distintos xs

replicate' :: Int -> a -> [a]
replicate' 0 _   = []
replicate' n x   = x:replicate (n-1) x


divideA :: Integer -> Integer -> Bool
divideA x y = mod y x == 0
divisores :: Integer -> [Integer]
divisores x = [ y | y <- [1..x], y `divideA`x ]
divisores' :: Integer -> [Integer]
divisores' x | x < 0 = divisores' (-x)
             | otherwise = [ y | y <- [-x..x], y/=0, y `divideA`x ]

esPrimo :: Integer -> Bool
esPrimo x = length (divisores x) == 2

primoHasta :: Integer -> [Integer]
primoHasta x = [y | y <- [2..x], esPrimo y]

primoHasta' :: Integer -> [Integer]
primoHasta' x = filter (esPrimo) [1..x]