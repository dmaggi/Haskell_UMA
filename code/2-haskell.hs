import Data.List
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

-- b)
geometrica :: Integer -> Integer -> [Integer]
geometrica ini razon = iterate (*razon) ini

-- c)
multiplosDe ini = iterate (+ini) 0

-- d)
potenciasDe ini = iterate (*ini) 1

---------------
--Ejercicio 16
---------------

-- a)
--En teoria es hacacer un tail del anterior

-- b)
-- primeroComun :: Ord a => [a] -> [a] -> a
-- primeroComun (x:xs) (y:ys)
--        | x < y primeroComun xs (y:ys)
--        | x > y primeroComun (x:xs) ys
--        | otherwise x

-- c)
-- mcm :: Integer -> Integer -> Integer
-- mcm x y = primeroComun (multiplosDe x) (multiplosDe y)

---------------
--Ejercicio 17
---------------

-- a)
primeroComunDeTres :: Ord a => [a] -> [a] -> [a] -> a
primeroComunDeTres (x:xs) (y:ys) (z:zs)
        | x > y     = primeroComunDeTres (x:xs) ys (z:zs)
        | y > z     = primeroComunDeTres (x:xs) (y:ys) zs
        | otherwise = primeroComunDeTres xs ys zs

---------------
--Ejercicio 18
---------------

-- a)
factPrimos :: Integer -> [Integer]
factPrimos x = fp x 2
        where
            fp x d
                | x' < d    = [x]
                | r == 0    = d : fp x' d
                | otherwise = fp x (d+1)
                    where (x',r) = divMod x d --cociente y resto

---------------
--Ejercicio 19
---------------

-- a)
mezcla :: [Integer] -> [Integer] -> [Integer]
mezcla l [] = l
mezcla [] l = l
mezcla (x:xs) (y:ys)
            | x == y = x : mezcla xs ys
            | x < y  = x : (mezcla xs (y:ys))
            | x > y  = y : (mezcla (x:xs) ys) 
mcm :: Integer -> Integer -> Integer
mcm x y = foldr (*) 1 comunes
    where
        lx = factPrimos x
        ly = factPrimos y
        comunes = mezcla lx ly


---------------
--Ejercicio 21
---------------

-- a)
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' (filter (/=x) xs)
--                     ([y | y <- xs, y /= x])  Otra manera


-- d)
all' :: (a -> Bool) -> [a] -> Bool
all' f l = foldl (\x y -> x && f y ) True l
--all' f l = length (filter (f) l) = length l
--all' f l = nul [x | x <- l, not f x]
--all' f l = and (map (f) l)


---------------
--Ejercicio 22
---------------
{-
-- a)
binarios :: Integer -> [Integer]
binarios 0 = [[]]
binarios n = ['0' : x | x <- xs] ++ ['1' : x | x <- xs]
    where
        xs = binarios (n-1)
-}

---------------
--Ejercicio 23
---------------

-- a)
varRep :: Integer -> [a] -> [[a]]
varRep 0 l = [[]]
varRep n l = [x ++ [y] | y <- l, x <-varRep (n-1) l]
--         = [y:x | y<-l, x <- vvarRep (n-1) l]

---------------
--Ejercicio 24
---------------

-- a)
--var :: Eq a => Integer -> [a] -> [[a]]
--var 0 _ = [[]]
--var n l = [ x:y | y <- (var(n-1) l), x <- l, x notElem y ]


---------------
--Ejercicio 25
---------------

-- a)
intercala :: a -> [a] -> [[a]]
--intercala e []  = [[e]] --Este no se usa, se puede borrar
intercala e l   = [(take i l) ++ [e] ++ (drop i l) | i <- [0..(length l)]]
--    where
--        ins e l i = (take i l) ++ [e] ++ (drop i l)

-- Mis que antes solo que con un contador
--intercala' e l  = intercalaaux e l i
--        where
--            intercalaaux e l ((length l)+1) = []
--            intercalaaux e l i              = [take i l ++ [e] ++ drop i l]:intercalaaux e l (i+1)

-- b)
--perm :: [a] -> [[a]]
--perm [] = [[]]
--perm l = foldr (\x y -> concat [(intercala x z) | z <- y] ) [[]] l
--perm (x:xs) = concat [(intercala x ys) | ys <- perm xs] -- otra forma
---------------
--Ejercicio 26
---------------

-- a)
--comb :: Integer -> [a] -> [[a]]
--comb n l | n == length l = [l]
--         | n > length l = error "hijoeputa"
--         | otherwise =
--            [ (take (i-1) z) ++ (drop i z) | z <- comb (n+1) l, i <- [1..(length z)] ]
--Otra fora
--comb 0 _      = [[]]
--comb _ []     = []
--comb n (x:xs) = (comb n xs) ++ [x : ys | ys <- comb (n-1) xs]

---------------
--Ejercicio 27
---------------

-- a)
esPrefijoDe :: Eq a => [a] -> [a] -> Bool
esPrefijoDe [] _ = True
esPrefijoDe (x:xs) (y:ys) = x==y && esPrefijoDe xs ys

-- b)
--busquedas :: String -> String -> [Integer]
--busquedas x [] = []
--busquedas [] x = []
--busquedas sub fuente = busquedaAux sub fuente 0
--    where
--        busquedaAux _ [] _ = []
--        busquedaAux sub fuente i | esPrefijoDe sub fuente = i : busquedaAux sub (tail fuente) i+1
                                                         -- i : busquedaAux sub (drop (lenght sub) fuente) i+(lenght.sub)
--                                 | otherwise = busquedaAux sub (tail fuente) (i+1)

-- c)
distancia :: String -> String -> Integer
distancia [] x = length x
distancia y [] = length y
distancia (x:xs) (y:ys) | x \= y    = 1 + distancia xs ys
                        | otherwise = distancia xs ys


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





