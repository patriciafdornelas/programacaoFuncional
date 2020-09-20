--Nome: Patrícia Fernandes Dornelas    Matrícula: 11821BCC043



--1

--A)

type Data = (Int,Int,Int)

bissexto :: Int -> Bool
bissexto x = if mod x 4 == 0 && mod x 100 /= 0 then True else False

valida :: Data -> Bool
valida (x,y,z)
   |bissexto z == True && y == 2 && x >= 1 && x <= 29 = resultado
   |bissexto z == False && y == 2 && x >= 1 && x <= 28 = resultado
   |x >= 1 && x <= 30 && y == 4 || y == 6 || y == 9 || y == 11  = resultado
   |x >= 1 && x <= 31 && y == 1 || y == 3 || y == 5 || y == 7 || y == 8 || y == 10 || y == 12 = resultado
   |otherwise = resultado1
   where
     resultado = True
     resultado1 = False


--B)

bissextos :: [Int]-> [Int]
bissextos lista = y
    where
      y = [x |x<-lista,bissexto x]

--C)

type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

precede :: Emprestimo -> Data -> Bool
precede (cod,matri,(d,m,a1),(a,b,c),situ) (x,y,z)
   |c < z = True
   |b < y && c == z = True
   |a < x && b == y && c == z = True
   |otherwise = False

bdEmprestimo::Emprestimos
bdEmprestimo =
    [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
    ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
    ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados [] data1 = []
atrasados (x:xs) data1 = if (precede x data1 == True) then a else b
   where
    a = x:atrasados xs data1
    b = atrasados xs data1

--D)

--D.1)

fibo :: Int->Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-2) + fibo(n-1)

fibo2 :: Int -> (Int,Int) 
fibo2 n = y
   where
     y = (fibo n,fibo (n+1))

--D.2)

fibo3 :: Int -> Int -> ((Int,Int),(Int,Int))
fibo3 n m = y
    where
      y = ((fibo n,fibo m),(fibo m, fibo(n+m)))

--D.3)

fibo4 :: Int -> (Int,Int)
fibo4 n = y
   where
    y = fibo2 n

--D.4)

fibo5 :: Int -> ((Int,Int),Int)
fibo5 n = ((x n),y n)
    where
      x = fibo4
      y = fibo

--E)

prodIntervalo :: Int->Int->Int
prodIntervalo m n                   
    |m == 0 || n == 0 = 0
    |m < n || m == n = m * prodIntervalo(m+1) n
    |otherwise = 1

fatorial2 :: Int -> Int
fatorial2 0 = 1
fatorial2 n = fat
   where
    fat = prodIntervalo 1 n

--2

--A)

valida1 :: Data -> Bool
valida1 (x,y,z) = let 
                   a = True
                   b = False
                  in 
                   if bissexto z == True && y == 2 && x >= 1 && x <= 29 then a
                   else if bissexto z == False && y == 2 && x >= 1 && x <= 28 then a
                   else if x >= 1 && x <= 30 && y == 4 || y == 6 || y == 9 || y == 11  then a
                   else if x >= 1 && x <= 31 && y == 1 || y == 3 || y == 5 || y == 7 || y == 8 || y == 10 || y == 12 then a
                   else b

--B)

bissextos1 :: [Int]-> [Int]
bissextos1 lista = let
                    a = [x | x <- lista, bissexto x]
                   in 
                    a

--C)

atrasados1 :: Emprestimos -> Data -> Emprestimos
atrasados1 [] data1 = []
atrasados1 (x:xs) data1 = let
                           a = precede x data1
                           b = x:atrasados1 xs data1
                           c = atrasados1 xs data1
                          in
                           if (a == True) then b else c

--D)

--D.1)

fibo22 :: Int -> (Int,Int) 
fibo22 n = let 
            a = fibo n
            b = fibo (n+1)
           in 
            (a,b)

--D.2)

fibo33 :: Int -> Int -> ((Int,Int),(Int,Int))
fibo33 n m = let
              a = fibo n
              b = fibo m
              c = fibo(n+m)
             in
              ((a,b),(b, c))

--D.3)

fibo44 :: Int -> (Int,Int)
fibo44 n = let 
            a = fibo2 n
           in 
            a

--D.4)

fibo55 :: Int -> ((Int,Int),Int)
fibo55 n = let 
            a = fibo4 n
            b = fibo n
           in
            ((a),b)

--E)

fatorial22 :: Int -> Int
fatorial22 0 = 1
fatorial22 n = let 
                a = prodIntervalo 1 n
               in
                a

--3
--3.1) (λx. 2*x + 1) 3 = 2*3+1 = 6+1 = 7

--3.2) (λxy. x-y) 5 7 = 5-7 = -2

--3.3) (λyx. x-y) 5 7 = 7-5 = 2

--3.4) (λxy. x-y) (λz. z/2) = (λxy. x-y) z/2 = z/2-z/2 = 0

--3.5) (λxy. x-y) ((λz. z/2)6) 1 = (λxy. x-y) (6/2)1 = (λxy. x-y) 3 1 = 3-1 = 2

--3.6) (λ x. λ y. – x y ) 9 4 = 9-4 = 5

--4

-- (\x -> x + 3) 5 = 8
-- (\x -> \y -> x * y + 5) 3 4 = 17
-- (\(x,y) -> x * y^2) (3,4) = 48
-- (\(x,y,_) -> x * y^2) (3,4,2) = 48
-- (\xs -> zip xs [1,2,3]) [4,5,6] = [(4,1),(5,2),(6,3)]

--5

--A) (λx λy. y)((λz. z)(λz. z))(λw. w) 5 = (\x-> \y-> y)((\z-> z)(\z-> z))(\w-> w) 5 = 5

--B) ((λf. (λx. f(f x))) (λy. (y * y))) 3 = ((λf. (λx. (λy. (y * y))((λy. (y * y)) x))) (λy. (y * y))) 3
--((\f-> (\x-> (\y-> (y * y))((\y-> (y * y)) x))) (\y-> (y * y))) 3 = 81

--C) ((λf. (λx. f(f x)))(λy.(+ y y))) 5 = ((\f-> (\x->f(f x)))(\y->(y+y))) 5 = 20

--D) ((λx. (λy. + x y) 5) ((λy. - y 3) 7)) = ((\x-> (\y->x+y) 5) ((\y->y-3) 7)) = 9

--E) (((λf. (λx. f(f(f x)))) (λy. (y * y))) 2) =  (((\f->(\x->f(f(f x)))) (\y->(y * y))) 2) = 256

--F) (λx. λy. + x ((λx. - x 3) y)) 5 6 =  (\x-> \y->x+((\x->x-3) y)) 5 6 = 8


