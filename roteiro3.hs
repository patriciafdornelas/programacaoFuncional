-- --1

-- --A)

-- (||) :: Bool->Bool->Bool
-- True || True = True
-- True || False = True
-- False || True = True
-- False || False = False


-- (||) :: Bool->Bool->Bool
-- True || _ = True
-- False || y = y


-- (||) :: Bool -> Bool -> Bool
-- False || False = False
-- _     || _     = True


-- --B)

-- (||) :: Bool->Bool->Bool

-- if True || True then True else if False || False then False else if True || False then True else True


-- (||) :: Bool->Bool->Bool
--    |True || True = True
--    |False || False = False
--    |True || False = True
--    |False || True = True

--2

distancia :: (Float,Float)->(Float,Float)->Float
distancia (x,y) (x1,y1) = sqrt(((y1-y)^2)+((x1-x)^2))

--3

-- 1:[2,3,4] =[1,2,3,4]
-- 'a':['b','c','d'] = "abcd"
-- head [1,2,3] = 1
-- tail [1,2,3] = [2,3]
-- [1,5,2,3]!!1 = 5
-- [1,5,2,3]!!3 = 3
-- elem 2 [1,5,2,3] = True
-- take 2 [1,5,2,3,7] = [1,5]
-- drop 2 [1,5,2,3,7] = [2,3,7]
-- [1,2] ++ [3,4] = [1,2,3,4]
-- [1..10] = [1,2,3,4,5,6,7,8,9,10]
-- [7,6..3] = [7,6,5,4,3]
-- ['b'..'g'] = "bcdefg"
-- take 5 [1,3..] = [1,3,5,7,9]
-- sum [1..10] = 55
-- maximum [1,5,2,3,7] = 7
-- minimum [1,5,2,3,7] = 1

--4

fatorial :: Int -> Int
fatorial n
    |n == 0 = 1
    |otherwise = n * fatorial (n-1)


fatorial1 :: Int->Int
fatorial1 n = n * fatorial1 (n-1)

--5

fibo :: Int->Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-2) + fibo(n-1)

--6

n_tri :: Int->Int
n_tri 0 = 0
n_tri 1 = 1
n_tri n = n + n_tri(n-1)

--7

--A)
fibo2 :: Int -> (Int,Int) 
fibo2 n = (fibo n,fibo (n+1))

--B)

fibo3 :: Int -> Int -> ((Int,Int),(Int,Int))
fibo3 n m = ((fibo n,fibo m),(fibo m, fibo(n+m)))

--C)

fibo4 :: Int -> (Int,Int)
fibo4 n = fibo2 n

--D)

fibo5 :: Int -> ((Int,Int),Int)
fibo5 n = ((fibo4 n),fibo n)

--8

potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 n = 2*potencia2(n-1)

--9

--A)

prodIntervalo :: Int->Int->Int
prodIntervalo m n                   --Erro
    |m == 0 || n == 0 = 0
    |m < n || m == n = m * prodIntervalo(m+1) n
    |otherwise = 1

--B)

fatorial2 :: Int -> Int
fatorial2 0 = 1
fatorial2 n = prodIntervalo 1 n

--11

resto_div :: Float->Float->Float
resto_div m n
    |m < n = m
    |otherwise = resto_div(m-n)n

div_inteira :: Int->Int->Int
div_inteira m n
    |m < n = 0
    |otherwise = 1 + div_inteira (m-n)n

--12

mdc :: Int -> Int -> Int
mdc m n
   |m == 0 = n 
   |m > 0 = mdc(n `mod` m)m

mdc1 :: (Int,Int) -> Int
mdc1 (m,0) = m
mdc1 (m,n) = mdc1 (n, (mod m n))

--13

binomial :: (Int,Int) -> Int
binomial (n,0) = 1
binomial (n,k)
 | k == 0 = 1
 | k == n = 1
 | otherwise = binomial (n-1,k) + binomial (n-1,k-1)
 
binomial1 :: (Int,Int) -> Int
binomial1 (n,0) = 1
binomial1 (n,k) = if (k == n) then 1 else binomial1 (n-1,k) + binomial1 (n-1,k-1)

--14

--a) [5,4,3,2,1] = reverse [1,2,3,4,5] || [5..1]
--b) [a,c,e] = ['a','c'] ++ ['e']
--c) [1,4,7,10,13,16] = [1,4..16]
--d) [(1,1),(-2,5),(-5,9),(-8,13),(-11,17)] = zip [1, (-2)..(-11)] [1,5..17]

--15

--A)

lista1 :: Int -> Int -> [Int]
lista1 a b
  |a==b = [a]
  |a>b = []
  |otherwise = [a+1..b-1]

--B) 

lista2 :: Int -> Int -> [Int]
lista2 a b 
  |a == b || a > b = []
  |even a == True = [a,a+2..b]
  |otherwise = [a+1,a+2..b]
