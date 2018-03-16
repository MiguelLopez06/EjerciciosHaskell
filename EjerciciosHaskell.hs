module Ejercicios where

--PrimerosEjercicios

mult :: Int->Int->Int
mult x y = 	if x == 1 then y
			else if y == 1 then x
			else if x == 0 then 0
			else if y == 0 then 0
			else x + mult x (y-1)
		   
divi :: Int->Int->Int
divi x y = 	if x < y then 0
			else if y == 1 then x
			else if x == 0 then 0
			else if y == 0 then 666
			else 1 + divi (x-y) y

fibo :: Int->Int
fibo x 	=	if x == 1 then 0
			else if x == 2 then 1
			else fibo (x-1) + fibo (x-2)
			
pote :: Int->Int->Int
pote x y =	if x == 0 then 0
			else if x == 1 then 1
			else if y == 0 then 1
			else if y == 1 then x
			else x*pote x (y-1)

sumaDigitos::Int->Int
sumaDigitos n = if n < 10 then n
                else n `mod` 10 + sumaDigitos (n `div` 10)


palindromo::Int->Bool
palindromo n
   | longitud n <= 1 = True
   | primerDigito n == ultimoDigito n = palindromo (removerExtremos n)
   | otherwise = False

mayorDigito::Int->Int
mayorDigito n
   | n < 10 = n
   | n `mod` 10 > mayorDigito (n `div` 10) = n `mod` 10
   | otherwise = mayorDigito (n `div` 10)

longitud::Int->Int
longitud n = if n < 10 then 1
             else 1 + longitud (n `div` 10)

invertir::Int->Int
invertir n = if n < 10 then n
             else ultimoDigito n * 10 ^ longitud (n `div` 10) + invertir (n `div` 10)




--Listas

numeroM :: [Int]->Int 
numeroM [] = 0
numeroM (x:xs) = if x>numeroM(xs) then x 
		 else numeroM(xs)

invertList::[a]->[a]
invertList [] = []
invertList (x:xs) = invertList xs ++ [x]

sumaPares::[Int]->Int
sumaPares [] = 0
sumaPares (x:xs) = sumaLista ([x | x <- (x:xs), esPar(x)])

sumaParesRecursivo::[Int]->Int
sumaParesRecursivo [] = 0
sumaParesRecursivo (b:bs)
   | esPar(b) = b + sumaParesRecursivo (bs)
   | otherwise = sumaParesRecursivo (bs)

cantidadImpares::[Int]->Int
cantidadImpares [] = 0
cantidadImpares (x:xs) = tamLista ([x | x <- (x:xs), esImpar(x)])

cantidadImparesRecursivo::[Int]->Int
cantidadImparesRecursivo [] = 0
cantidadImparesRecursivo (b:bs)
   | esImpar(b) = 1 + cantidadImparesRecursivo (bs)
   | otherwise = cantidadImparesRecursivo (bs)

contieneLista::[Int]->[Int]->Bool
contieneLista x [] = False
contieneLista x (y:ys) = 

contieneListaRecursivo::[Int]->[Int]->Bool
contieneListaRecursivo [] [] = True
contieneListaRecursivo [] (y:ys) = True
contieneListaRecursivo (x:xs) [] = True
contieneListaRecursivo (x:xs) (y:ys) = enLista y (x:xs) && contieneListaRecursivo (x:xs) ys

















 []




