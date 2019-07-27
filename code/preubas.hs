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