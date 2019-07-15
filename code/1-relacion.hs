import Test.QuickCheck

---------------
--Ejercicio 1
---------------

-- a)
esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z = x^2 + y^2 == z^2

-- b)
terna :: Integer -> Integer -> (Integer,Integer,Integer)
terna x y = (x^2-y^2, 2*x*y, x^2+y^2)

-- c)
p_ternas x y = x > 0 && y > 0 && x < y ==> esTerna l1 l2 h
    where (l1,l2,h) = terna x y

-- d)
--Ejecutar: quickCheck p_ternas
    
---------------
--Ejercicio 2 
---------------

intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)

---------------
--Ejercicio 3
---------------

-- a)
ordena2 :: Ord a => (a,a) -> (a,a)
ordena2 (x,y)   | y < x     = (y,x)
                | otherwise = (x,y)

-- b)
ordena3 :: Ord a => (a,a,a) -> (a,a,a)
ordena3 (x,y,z) | x > y = ordena3 (y,x,z)
                | y > z = ordena3 (x,z,y)
                | otherwise = (x,y,z)

-- c)

---------------
--Ejercicio 4
---------------
max2 :: Ord a => a -> a -> a
max2 x y  | x > y = x
          | otherwise = y

---------------
--Ejercicio 5
---------------
entre :: Ord a => a -> (a,a) -> Bool
entre x (y,z) = x>y && x<z

---------------
--Ejercicio 6
---------------
iguales3 :: Eq a => (a,a,a) -> Bool
iguales3 (x,y,z) = x==y && y==z

---------------
--Ejercicio 7 
---------------

--a)
type TotalSeg   = Integer
type Horas      = Integer
type Minuto     = Integer
type Segundos   = Integer
descomponer :: TotalSeg -> (Horas,Minuto,Segundos)
descomponer x = (h, m, s)
    where
        h = x `div` 3600
        m = (x `mod` 3600) `div` 60
        s = (x `mod` 3600) `mod` 60

---------------
--Ejercicio 8
---------------

unEuro :: Double
unEuro = 166.386

--a)
pesetasAeuros :: Double -> Double
pesetasAeuros x = x/unEuro

--b)
eurosApesetas :: Double -> Double
eurosApesetas x = x*unEuro

---------------
--Ejercicio 9
---------------

--Definimos el operador y su prioridad
infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs(x-y) < epsilon
    where epsilon = 1/1000


---------------
--Ejercicio 10
---------------


--a) [-b +- sqrl(b^2-4ac)]/2a
raices :: Double -> Double ->  Double -> (Double,Double)
raices a b c | (b)^2-4*a*c >= 0 = (x, y)
             | otherwise = error "NO SE PEUDE WEY, RAICES NEGATIVAS"
    where
        x = (-b + sqrt(b^2-4*a*c))/2*a
        y = (-b - sqrt(b^2-4*a*c))/2*a


---------------
--Ejercicio 11
---------------

esMultiplo :: Integer -> Integer -> Bool
esMultiplo x y = x `mod` y == 0

---------------
--Ejercicio 12
---------------

esBisiesto :: Integer -> Bool
esBisiesto x | x `esMultiplo` 100 && x `esMultiplo` 400 = True
             | x `esMultiplo` 4 = True
             | otherwise = False

---------------
--Ejercicio 13
---------------

--a)
potencia :: Integer -> Integer -> Integer
potencia x n | n == 1 = x
             | n > 1 = x * potencia x (n-1)

--b)
esPar :: Integer -> Bool
esPar x = x `mod` 2 == 0
--potencia' :: Integer -> Integer -> Integer
--potencia' x n | esPar x == True = k * k
--              | otherwise = x * k2 * k2
--    where
--        k = potencia' b*(din n 2)
--        k2 = potencia' b*(div (n-1) 2)

---------------
--Ejercicio 15
---------------

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

---------------
--Ejercicio 16
---------------

mediana Ord a => (a,a,a,a,a) -> a
mediana (x,y,z,t,u) | x > z = mediana (z,y,x,t,u)
                    | y > z = mediana (x,z,y,t,u)
                    | z > t = mediana (x,y,t,z,u)
                    | z > u = mediana (x,y,u,t,z)
                    | otherwise = 

---------------
--Ejercicio 17
---------------


