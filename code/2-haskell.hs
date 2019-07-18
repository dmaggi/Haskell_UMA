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
--Ejercicio 9
---------------

-- a)
pares :: Integer -> [(Integer,Integer)]
pares n = [(x,y) | x <- [2..n], y <- [x..n], esPrimo x, esPrimo y, x+y == n]

-- b)
infixl 1 ==>>
(==>>) :: Bool -> Bool -> Bool
a ==>> b = (not a) || b

golbach :: Integer -> Bool
golbach n = n > 2 && even n ==>> length (pares n) > 0

-- c)
golbachHasta :: Integer -> Bool
-- golbachHasta n = and (map (golbach) [1..n])
golbachHasta n = and (map (\x -> golbach x) [1..n])
-- golbachHasta n = and [golbach x | x <- [3..n]] otra forma
-- golbachHasta n = foldl (\x y -> x && golbach y) True [1..n]
-- golbachHasta n = (length (filter (golbach) [1..n])) == n

---------------
--Ejercicio 10
---------------

-- filter (\k -> mod k 4 == 0) [1..100] Me da de la lista todos los que son divisibles por 4
-- foldl (\x y -> [y]++x) [] [3,2,1,0] Le damos la vuelta a la lista

-- a).
--esPerfecto :: Integer -> Bool
--esPerfecto n = sum [x | x <- [1..n], divisoresHasta n] == n
--esPerfecto foldr (+) 0 [x | x <- [1..n], divisoresHasta x] == n

-- b)
--perfectosMenoresQue :: Integer -> [Integer]
--perfectosMenoresQue n = [x | x <- [1..n], esPerfecto x]

---------------
--Ejercicio 11
---------------

-- a)
take' :: Int -> [a] -> [a]
take' n xs = [x | (p,x) <- zip [0..n-1] xs ]

--b)
drop' :: Int -> [a] -> [a]
drop' n xs = [x | (p,x) <- zip [0.. length xs-1] xs, p>=n]
---------------
--Ejercicio 12
---------------

-- a)
concat :: [[a]] -> [a]
--concat xs = foldr (++) [] xs
concat xs = [c | y <- xs, c <- y]



---------------
--Ejercicio 13
---------------

-- Me dice si una funcion esta ordenada 

---------------
--Ejercicio 14
---------------

-- a)
--inserta :: Ord a => a -> [a] -> [a]
--inserta x xs = (takeWhile (<x) xs) ++ [x] ++ (dropWhile (<x) xs)

-- b)
inserta n [] = [n]
inserta n (x:xs) | n <= x = (n:x:xs)
                 | otherwise = x:(inserta n xs)


-- e)
ordena :: Ord a => [a] -> [a]
ordena xs = foldr (inserta) [] xs

---------------
--Ejercicio 15
---------------

-- a)
iterate' :: (a -> a) -> a -> [a]
iterate' f a = a : iterate f (f a)

geometria

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






