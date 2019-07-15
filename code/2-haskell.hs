---------------
--Ejercicio 2
---------------

--a)
--maximoYResto :: Ord a => [a] -> (a,[a])

-- b)
maximoYrestoOrd :: Ord a => [a] -> (a,[a])
maximoYrestoOrd []     = error "Lista vacia"
maximoYrestoOrd [x]    = (x,[])
maximoYrestoOrd (x:xs) | x > y = (x,xs)
                    | otherwise = (y,x:ys)
    where
        (y,ys) = maximoYrestoOrd (xs)

---------------
--Ejercicio 3
---------------

reparte :: [a] -> ([a],[a])
reparte []          = ([],[])
reparte [x]         = ([x],[])
reparte (x:y:xs)    = (x:zs,y:ks)
    where
        (zs,ks) = reparte (xs)


---------------
--Ejercicio 4
---------------

distintos :: Eq a => [a] -> Bool
distintos []    = True
distintos (x:xs) = (notElem x xs) && distintos xs


---------------
--Ejercicio 5
---------------

-- a)
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:replicate' (n-1) x

---------------
--Ejercicio 6
---------------

-- Todos los X pertenecientes al intervale [1-100] tales que X sea par
--[x | x <- [1..100] , mod x 2 == 0]

divideA :: Integer -> Integer -> Bool
divideA a b = mod b a == 0

-- a)

divisores :: Integer -> [Integer]
divisores x = [y | y <- [1..x], divideA y x]
divisores' :: Integer -> [Integer]
divisores' x    | x < 0 = divisores' (-x)
                | otherwise = [y | y <- [-x..x], y /= 0, divideA y x]

---------------
--Ejercicio 7
---------------

-- a)

mcd :: Integer -> Integer -> Integer
mcd x y = maximum [ z | z <- divisores x, k <- divisores y, z == k ]


---------------
--Ejercicio 8
---------------

-- a)
esPrimo :: Integer -> Bool
esPrimo n = length (divisores n) == 2

-- b)
primoHasta :: Integer -> [Integer]
primoHasta n = [x | x <- [1..n], esPrimo x]

-- c)
-- primoHasta' :: Integer -> Integer
-- primoHasta' n = [x | x <-[1..n], esPrimo x]

---------------
--Ejercicio 1
---------------

-- a)


---------------
--Ejercicio 1
---------------

-- a)


---------------
--Ejercicio 1
---------------

-- a)


---------------
--Ejercicio 1
---------------

-- a)


---------------
--Ejercicio 1
---------------

-- a)


---------------
--Ejercicio 1
---------------

-- a)


---------------
--Ejercicio 1
---------------

-- a)


---------------
--Ejercicio 1
---------------

-- a)


---------------
--Ejercicio 1
---------------

-- a)


---------------
--Ejercicio 1
---------------

-- a)


