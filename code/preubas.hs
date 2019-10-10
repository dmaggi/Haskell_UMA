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

maximoYresto :: Ord a => [a] -> (a,[a])
maximoYresto [] = error "Lista vacia"
maximoYresto xs = (maximum xs, filter (<maximum xs) xs)


maximoYrestoOrd :: Ord a => [a] -> (a,[a])
maximoYrestoOrd []     = error "Lista vacia"
maximoYrestoOrd [x]    = (x,[])
maximoYrestoOrd (x:xs) |  x > y    = (x,xs)
                    | otherwise = (y,x:ys)
    where
        (y,ys) = maximoYrestoOrd (xs)

reverseMierda :: [a]-> [a]
reverseMierda xs = revOnMierda xs []
    where
        revOnMierda [] ys         = ys
        revOnMierda (x:xs) ys    = revOnMierda xs (x:ys)

reparte :: [a] -> ([a],[a])
reparte []          = error "Lista vacia wey"
reparte [x]         = ([x],[])
reparte (x:y:xs)    = (x:par,y:impar)
    where
        (par,impar) = reparte (xs)

distintos :: Eq a => [a] -> Bool
distintos []     = True
distintos (x:xs) = (notElem x xs) && distintos xs

replicate' :: Int -> a -> [a]
replicate' 0 _   = []
replicate' n x   = x:replicate (n-1) x

mcd :: Integer -> Integer -> Integer
mcd x y = maximum [k | k <- divisores x, z <- divisores y, k == z]

pares :: Integer -> [(Integer,Integer)]
pares n  = [ (x,y) | x <- [2..n], y <- [x..n], esPrimo x, esPrimo y, x+y == n]