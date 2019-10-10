-- 09/10/2019
import Test.QuickCheck


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