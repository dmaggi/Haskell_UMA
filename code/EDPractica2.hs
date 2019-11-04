-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- PRACTICA 2ª (Características de la Programación Funcional)
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería Informática [Informática | del Software | de Computadores].
-- Alumno: MAGGI, DENIS
-- Fecha de entrega:  31 | 10 | 2019
--
-- Ejercicios resueltos de la Relación : 2
--
-------------------------------------------------------------------------------
module Practica2 where

import Test.QuickCheck



-------------------------------------------------------------------------------
-- Ejercicio 4
-------------------------------------------------------------------------------
distintos :: Ord a => [a] -> Bool
distintos [] = True
distintos (x:xs) = (notElem x xs) && distintos xs

-------------------------------------------------------------------------------
-- Ejercicio 11
-------------------------------------------------------------------------------
-- take'
take' :: Int -> [a] -> [a]
take' n xs = [x | (p,x) <- zip [0..n-1] xs ]

-- drop'
drop' :: Int -> [a] -> [a]
drop' n xs = [x | (p,x) <- zip [0.. length xs-1] xs, p>=n]

-------------------------------------------------------------------------------
-- Ejercicio 13
-------------------------------------------------------------------------------
desconocida :: (Ord a) => [a] -> Bool
desconocida xs = and [ x<=y | (x,y) <- zip xs (tail xs) ]
-- Qué hace?
-- Es una función a la cual le pasamos un array de elementos ordenables y nos devuelve
-- True o False si se encuentran ordenados.

-------------------------------------------------------------------------------
-- Ejercicio 14
-------------------------------------------------------------------------------
-- apartados a, b, e y f
-- a)
inserta :: (Ord a) => a -> [a] -> [a]
inserta x xs = (takeWhile (<x) xs) ++ [x] ++ (dropWhile (<x) xs)


-- b)
inserta' :: (Ord a ) => a -> [a] -> [a]
inserta' x [] = [x]
inserta' x (y:ys)
    | x <= y = (x:y:ys)
    | otherwise = y:(inserta x ys)

-- e)

ordena :: (Ord a) => [a] -> [a]
ordena xs = foldr (inserta) [] xs

-- f)  Utiliza para ello la función sorted definida en las transarencias
p_ordena xs=True==>sorted (ordena xs)
sorted :: (Ord a)=>[a]->Bool
sorted []=True
sorted [_]=True
sorted (x:y:xs)=x<=y&& sorted (y:xs)


-------------------------------------------------------------------------------
-- Ejercicio 24
-------------------------------------------------------------------------------
-- binarios ::Integer -> [String]
-- binarios 0 = [""]
-- binarios x | x > 0 = undefined


-------------------------------------------------------------------------------
-- Ejercicio 37
-------------------------------------------------------------------------------

type Izdo = Double
type Dcho = Double
type Epsilon = Double
type Función = Double -> Double
biparticion :: Función -> Izdo -> Dcho -> Epsilon -> Double

biparticion f a b epsilon
  | long < epsilon    = undefined
-- sigue aqui
  where
      long = b - a
-- sigue aqui


-------------------------------------------------------------------------------
-- Lista de ejercicios extra. Ejercicio [lista de pares] 
-------------------------------------------------------------------------------

cotizacion :: [(String, Double)]
cotizacion = [("apple", 116), ("intel", 35), ("google", 824), ("nvidia", 67)]

-- buscarRec
-- buscarC
-- buscarP
-- valorCartera. DIFICIL

-------------------------------------------------------------------------------
-- Lista de ejercicios extra. Ejercicio [mezcla]
-------------------------------------------------------------------------------
-- mezcla

-------------------------------------------------------------------------------
-- Lista de ejercicios extra. Ejercicio [agrupar]
-------------------------------------------------------------------------------
-- agrupar. DIFICIL</div></pre>