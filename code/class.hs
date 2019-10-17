import Test.QuickCheck
-- :set +s

--------------------------------
--------------------------------
-- 09/10/2019
--------------------------------
--------------------------------


factorial :: Integer -> Integer
factorial 0 = 1
factorial n
    | n > 0     = n * factorial (n-1)
    | otherwise = error "Factorial: no valido para numeros negativos"

--------------------------------------------------------

circArea :: Double -> Double
circArea r = pi * r^2
rectArea :: Double -> Double -> Double
rectArea b h = b * h
circLength :: Double -> Double
circLength r = 2 * pi * r

cylinderArea :: Double -> Double -> Double
cylinderArea r h = 2 * circ + rect
    where
        circ = circArea r
        l    = circLength r
        rect = rectArea l h

--------------------------------------------------------
--Hablamos de raices
raices :: Double -> Double -> Double -> (Double,Double)
raices a b c
    | a == 0    = error "La ecuacion no es de segundo grado"
    | disc < 0  = error "Raices complejas"
    | otherwise = ((-b-rdisc) / den, (-b+rdisc)/den) 
    where
        disc  = b^2 - 4*a*c
        rdisc = sqrt (disc)
        den = 2*a

square :: Integer -> Integer
square x = x*x
p1 x y = True ==> square x + square y+2*x*y == square(x+y)


--------------------------------
--------------------------------
-- 10/10/2019
--------------------------------
--------------------------------

--Ejemplo de recursión de cola
resto :: Integer -> Integer -> Integer
resto x y 
    | x < y = x
    | otherwise = resto (x-y) y

    --Esto n oes una recursión de cola
cociente :: Integer -> Integer -> Integer
cociente x y
    | x < y = 0 
    | otherwise = 1 + cociente (x-y) y

cocienteYresto :: Integer -> Integer -> (Integer,Integer)
cocienteYresto x y
    | otherwise = (cociente x y, resto x y)

cocienteYresto' :: Integer -> Integer -> (Integer,Integer)
cocienteYresto' x y
    | x < y = (0,x)
    | otherwise = (u+1,v)
        where (u,v) = cocienteYresto (x-y) y

--Función que cuente el número de ceros que tiene un numero al final
cerosDe :: Integer -> Integer
cerosDe x
    | x == 0    = 1                 --Este es el caso base, sin el, se queda en bucle infinito
--  | x < 10    = 0                 --Esta se puede omitir, no es necesaria
    | r == 0    = 1 + cerosDe c     --Si la division me da restor 0 es que es divisible por 10, asi que tiene un 0, luego lo sumo
    | otherwise = 0                 --Por ejemplo si es 75, no seguios buscando, porque no es ninguno de los casos anteriores
        where (c,r) = divMod x 10

--Ejercicio 1

--a)
esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z = x*x + y*y == z*z    --Se coloca asi por una cuestion de eficiencia, si usamos ^ se pasara a un tipo Double

--b)
terna :: Integer -> Integer -> (Integer,Integer,Integer)
terna x y | x > y = (x*x - y*y, 2*x*y, x*x+y*y)

--c)
p_ternas :: Integer -> Integer -> Property
p_ternas x y = x>0 && y>0 && x>y ==> esTerna a b c
    where (a,b,c) = terna x y



--------------------------------
--------------------------------
-- 15/10/2019
--------------------------------
--------------------------------

cerosUnos :: Integral a => a -> (Integer,Integer)
cerosUnos n
    | n == 0 = (1,0)
    | n == 1 = (0,1)
    | r == 0 = (sc+1,sv)
    | r == 1 = (sc,sv+1)
        where (c,r) = divMod n 10 --Esto me saca de: 100100 - (10010,0)
              (sc,sv) = cerosUnos c


-------- RELACION EJERCICIOS EXTRA ----------------


--Ejercicio [esPrimo]

esPrimo n
    | n <= 0 = error "Error numero negativo o cero"
    | n == 1 = False
    | otherwise = esPrimo' 2
        where
            esPrimo' x
                | x == n = True
                | otherwise = mod n x /= 0 && esPrimo' (x+1)

--Ejercicio [libreDeCuadrados]

libreDeCuadrados :: Integer -> Bool
libreDeCuadrados n
    | n <= 0 = error "Error numero negativo o cero"
    | n == 1 = True
    | otherwise = libreDeCuadrados' 2
        where
            libreDeCuadrados' x
                | x * x > n = True
                | otherwise = mod n (x*x) /= 0 && libreDeCuadrados' (x+1)

--Ejercicio [raizEntera]

esRaizEntera:: Integer -> Integer -> Bool
esRaizEntera r n = r*r <= n && (r+1)*(r+1)>n

raizEntera :: Integer -> Integer
-- raizEntera x = truncate (sqrt (fromIntegral x)) --Esto es usando las librerias
raizEntera x
    | x < 0 = error "Error numero negativo o cero"
    | x == 1 = 0
    | otherwise = raizEntera' 1
        where
            raizEntera' r
                | esRaizEntera r x = r
                | otherwise = raizEntera' (r+1)

raizEnteraRapida :: Integer -> Integer
raizEnteraRapida n
    | n <= 0 = error "Error numero negativo o cero"
    | n == 1 = 1
    | otherwise = raizEnteraRapida' 0 n
            where
                raizEnteraRapida' x y
                    | esRaizEntera z n = z
                    | z * z < n = raizEnteraRapida' z y
                    | otherwise = raizEnteraRapida' x z
                        where z = div (x+y) 2


-- Ejercicio [harshad]

sumaDigitos :: Integer -> Integer
sumaDigitos n
    | n < 10 = n
    | otherwise = r + sumaDigitos c
        where (c,r) = divMod n 10

-- Extra un ejercicio que haga sumarDigitos hasta que se quede solo con 1 numero

sumaDigitosRec :: Integer -> Integer
sumaDigitosRec n
    | n < 10 = n
    | otherwise = sumaDigitosRec (sumaDigitos n)

simpatico :: Integer -> Bool
simpatico n = sumaDigitosRec n == 1

harshad :: Integer -> Bool
harshad x = mod x (sumaDigitos x) == 0

harshadMultiple :: Integer -> Bool
harshadMultiple n = harshad n && harshad (div n (sumaDigitos n))
--Ejemplo de numero: 6804