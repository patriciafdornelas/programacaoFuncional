
l1=[1..1000]
l2=[1000,999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]

--Ex1

--A)

selecao :: Ord a => [a] -> [a]
selecao = foldr selecao1 []

selecao1 :: (Ord a) => [a] -> [a]
selecao1 [] = []
selecao1 xs = [x] ++ selecao1 (remove x xs)
                    where x = minimo xs


remove :: (Ord a) => a -> [a] -> [a]
remove a [] = []
remove a (x:xs)
   | a == x = xs
   | otherwise = x:(remove a xs)

minimo :: (Ord a) => [a] -> a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
   |x <= (minimo xs ) = x
   |otherwise = minimo xs

--B)

insercao2 :: Ord a => [a] -> [a]
insercao2 = foldr insereOrd []

insereOrd :: (Ord a) => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (y:ys)
   |x <= y = (x:y:ys)
   |otherwise = y:(insereOrd x ys)


--C)

quicksort :: Ord a => [a] -> [a]
quicksort = filter quicksort [] 

quicksortf :: (Ord a) => [a] -> [a]
quicksortf [] = []
quicksortf (s:xs) = quicksortf [x | x <- xs, x < s] ++ [s] ++ quicksortf [x | x <- xs,x >= s]


--2

--Variação 1

bolhapara :: [Integer]->[Integer]
bolhapara [] = []
bolhapara lista = bolhaOrd1 lista 1

bolhaOrd1 :: [Integer] -> Integer ->[Integer]
bolhaOrd1 lista 0 = lista
bolhaOrd1 lista a = n
 where
 (naux,number) = troca1 lista
 n = bolhaOrd1 naux number
    
troca1:: [Integer]->([Integer],Integer)
troca1 [x]= ([x],0)
troca1(x:y:r) 
   | x > y = (x1,n1+1)
   | otherwise = (x2,n2)
                where
                   (xaux,n1) = troca1(x:r)
                   x1 = y:xaux
                   (xaux2,n2) = troca1(y:r)
                   x2 = x:xaux2

--Variação 2

bolhav2 :: [Int] -> [Int]
bolhav2 [] = []
bolhav2 l = bolhaOrdv2 l (length l)
 
bolhaOrdv2 :: [Int] -> Int -> [Int]
bolhaOrdv2 lista 0 = lista
bolhaOrdv2 lista n = bolhaOrdv2 (trocav2 lista (n-1)) (n-1)
 
trocav2 :: [Int] -> Int -> [Int] 
trocav2 [x] _ = [x]
trocav2 lista 0 = lista
trocav2 (x:y:r) n
 | x > y = y:trocav2 (x:r) (n-1)
 | otherwise = x: trocav2 (y:r) (n-1)


--Variação 3


bolhav3::[Int] -> [Int]
bolhav3 [] = []
bolhav3 lista = bolhaOrdv3 lista (length lista) 1
 
bolhaOrdv3 :: [Int]-> Int -> Int ->[Int]
bolhaOrdv3 lista _ 0 = lista
bolhaOrdv3 lista n a = n2
 where 
  (naux,number) = trocav3 lista (n-1)
  n2 = bolhaOrdv3 naux (n-1) number
 
trocav3 :: [Int] -> Int -> ([Int], Int)
trocav3 [x] _ = ([x],0)
trocav3 lista 0 = (lista,0)
trocav3 (x:y:r) n
 | x > y = (x1,n1+1)
 | otherwise = (x2,n2)
 where
  (xaux,n1) = trocav3 (x:r) (n-1)
  x1 = y:xaux
  (x2aux,n2) = trocav3 (y:r) (n-1)
  x2 = x:x2aux


--Variação 1 Com contador


bolha1c::[Int]->([Int],Int)
bolha1c [] = ([],0)
bolha1c l = bolhaOrd1c l 1

bolhaOrd1c :: [Int] -> Int ->([Int],Int)
bolhaOrd1c l 0 = (l,0)
bolhaOrd1c l a = (n,x1 + qtd)
 where
  (naux,number,qtd) = troca1c l
  (n,x1) = bolhaOrd1c naux number

 
troca1c::[Int]->([Int],Int,Int)
troca1c [x]= ([x],0,1)
troca1c(x:y:r) 
 | x > y = (x1,n1+1,n3+1)
 | otherwise = (x2,n2,n4+1)
 where
  (xaux,n1,n3) = troca1c(x:r)
  x1 = y:xaux
  (xaux2,n2,n4) = troca1c(y:r)
  x2 = x:xaux2

--Variação 2 Com contador

bolhav2c :: [Int] -> ([Int],Int)
bolhav2c [] = ([],0)
bolhav2c l = bolhaOrdv2c l (length l)
 
bolhaOrdv2c :: [Int] -> Int -> ([Int],Int)
bolhaOrdv2c lista 0 = (lista,0)
bolhaOrdv2c lista n = (lista,naux + x1)
 where
  (aux,naux) = trocav2c lista (n-1)
  (lista,x1) = bolhaOrdv2c aux (n-1)
                    
 
trocav2c :: [Int] -> Int -> ([Int],Int) 
trocav2c [x] _ = ([x],1)
trocav2c lista 0 = (lista,1)
trocav2c (x:y:r) n
   | x > y = (x1,1+n1)
   | otherwise =(x2,n2+1)
   where
    (xaux,n1) = trocav2c(x:r) (n-1)
    x1 = y:xaux
    (xaux2,n2) = trocav2c(y:r) (n-1)
    x2 = x:xaux2

--Variação 3 Com contador


bolhav3c :: [Int] -> ([Int],Int)
bolhav3c [] = ([],0)
bolhav3c lista = bolhaOrdv3c lista (length lista) 1
 
bolhaOrdv3c :: [Int]-> Int -> Int ->([Int], Int)
bolhaOrdv3c lista _ 0 = (lista,0)
bolhaOrdv3c lista n a = (n2,t1+t)
 where 
  (naux,number,t) = trocav3c lista (n-1)
  (n2,t1) = bolhaOrdv3c naux (n-1) number
                    
 
trocav3c :: [Int] -> Int -> ([Int], Int,Int)
trocav3c [x] _ = ([x],0,1)
trocav3c lista 0 = (lista,0,1)
trocav3c (x:y:r) n
 | x > y = (x1,n1+1,num1+1)
 | otherwise = (x2,n2,num2+1)
 where
  (xaux,n1,num1) = trocav3c (x:r) (n-1)
  x1 = y:xaux
  (x2aux,n2,num2) = trocav3c (y:r) (n-1)
  x2 = x:x2aux


--3

--Variação 1

selecaov1::(Ord a)=>[a]->[a]
selecaov1 [] = []
selecaov1 xs = [x] ++selecaov1 (removev1 x xs)
 where x = minimov1 xs


removev1::(Ord a )=>a->[a]->[a]
removev1 a [] = []
removev1 a (x:xs)
 | a==x = xs
 | otherwise = x:(removev1 a xs)

minimov1::(Ord a)=>[a]->a
minimov1 [] = undefined
minimov1 [x] = x
minimov1 (x:xs)
 | x<=(minimov1 xs) =  x
 | otherwise = minimov1 xs


--Variacao 2

selecao2::[Int]->[Int]
selecao2 [] = []
selecao2 (x:xs) = y:selecao2 n
 where
 (n1,n2,y) = remove_menor xs x
 n = n1++n2
 
remove_menor::[Int] -> Int -> ([Int],[Int],Int)
remove_menor [] x = ([],[],x)
remove_menor (x:xs) n 
 | n < x = (n1,[x]++n2,c)
 | otherwise = ([n]++n3,n4,b)
 where
  (n1,n2,c) = remove_menor xs n
  (n3,n4,b) = remove_menor xs x

--Variação 2 com Contador

selecao2c :: [Int]->([Int],Int)
selecao2c [] = ([],0)
selecao2c (h:t) = (x:teste,aux1+aux2)
 where
  (n1,n2,x,aux1) = remove_menor2c t h
  a = n1++n2
  (teste,aux2) = selecao2c a
 
remove_menor2c :: [Int] -> Int -> ([Int],[Int],Int,Int)
remove_menor2c [] x = ([],[],x,1)
remove_menor2c (h:t) x 
   | x < h = (n1,[h]++n2,c,aux1+1)
   | otherwise = ([x]++n3,n4,b,aux2+1)
   where
    (n1,n2,c,aux1) = remove_menor2c t x
    (n3,n4,b,aux2) = remove_menor2c t h


  --4

  --Variacao 1

quickSort1::(Ord a)=>[a]->[a]
quickSort1 [] =  []
quickSort1 (x:xs) = menores ++ [x] ++ maiores
 where 
 menores = filter (<x) xs
 maiores = filter (>x) xs

--Variação 2

quickSort2::Ord a=>[a]->[a]
quickSort2 [] = []
quickSort2 (x:xs) = quickSort2 menores ++ [x] ++ quickSort2 maiores
 where (menores,maiores) = divide2 x xs

divide2::Ord a =>a->[a]->([a],[a])
divide2 a [] = ([],[])
divide2 a (x:xs)
 | x < a = (x:x1,x2)
 | x >= a = (x1,x:x2)
 where (x1,x2) = divide2 a xs

-- Variacao 3

quickSort3 :: Ord a => [a]->[a]
quickSort3 [] = []
quickSort3 lista = quickSort3 n  ++ [p] ++ quickSort3 n2
 where
 (p,l1) = pivo3 lista
 (n,n2) = divide2 p l1
 
pivo3 :: Ord a => [a]->(a,[a])
pivo3 [x] = (x,[])
pivo3 [x,y] = (x,[y])
pivo3 (x:y:z:t) 
 | x >= y && x >= z = (x,(y:z:t))
 | y >= x && y >= z = (y,(x:z:t))
 | otherwise = (z,(x:y:t))


-- Variação 2 com contador

quicksort2c::Ord a=>[a]->([a],Int)
quicksort2c [] = ([],0)
quicksort2c (x:xs) = (l1 ++ [x] ++ l2,n+n1+n2)
 where
 (l1,n1) =  quicksort2c menores
 (l2,n2) = quicksort2c maiores
 (menores,maiores,n) = divide2c x xs

divide2c::Ord a =>a->[a]->([a],[a],Int)
divide2c a [] = ([],[],0)
divide2c a (h:t)
 | h < a = (h:h1,h2,n+1)
 | h >= a = (h1,h:h2,n+1)
 where 
 (h1,h2,n) = divide2c a t


--Variacao 3 com contador

quickSort3c :: Ord a => [a]->([a],Int)
quickSort3c [] = ([],0)
quickSort3c lista = (lista1,t1+t2+t3+t4)   
 where
 (p,l1,t1) = pivo3c lista
 (n,n2,t2) = divide2c p l1
 (listaAux1 , t3) = quickSort3c n
 (listaAux2,t4) = quickSort3c n2
 lista1 = listaAux1 ++ [p] ++ listaAux2
 
pivo3c :: Ord a => [a]->(a,[a],Int)
pivo3c [x] = (x,[],0)
pivo3c [x,y] = (x,[y],0)
pivo3c (x:y:z:t) 
 | x >= y && x >= z = (x,(y:z:t),2)
 | y >= x && y >= z = (y,(x:z:t),2)
 | otherwise = (z,(x:y:t),2)


--5

--Merge Sort

mergeSort ::Ord a=>[a]->[a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort ys) (mergeSort zs)
 where
 (ys,zs) = divide xs


divide ::Ord a=> [a] -> ([a],[a])
divide [] = ([],[])
divide [x] = ([x],[])
divide (x:y:t)  = ((x:xs),(y:ys))
            where
                (xs,ys) = divide t

merge :: Ord a=> [a]->[a]->[a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys)
 | x < y = x:(merge xs (y:ys))
 | otherwise = y: (merge (x:xs) ys)


-- Merge sort com contador 


mergeSort2 ::Ord a=>[a]->([a],Int)
mergeSort2 [] = ([],0)
mergeSort2 [x] = ([x],1)
mergeSort2 xs = (e,b+d+f)
              where
                (ys,zs) = divide xs
                (a,b) = mergeSort2 ys
                (c,d) = mergeSort2 zs
                (e,f) = merge2 a c


merge2 :: Ord a=> [a]->[a]->([a],Int)
merge2 xs [] = (xs,1)
merge2 [] xs = (xs,1)
merge2 (x:xs) (y:ys)
 | x < y = (a,b+1)
 | otherwise = (c,d+1)
 where
 (a1,b) = merge2 xs (y:ys)
 a = x:a1
 (c1,d) = merge2 (x:xs) ys
 c = y:c1

--Parte B

--6

data Exp a =
 Val a
 | Add (Exp a) (Exp a)
 | Sub (Exp a) (Exp a)
 | Neg (Exp a)
 | Multi (Exp a) (Exp a)
 | Div (Exp a) (Exp a)

avalia::Floating a=>Exp a->a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Neg exp1) = 0 - avalia( exp1)
avalia (Multi exp1 exp2) =  (avalia exp1) * (avalia exp2)
avalia (Div exp1 exp2) = (avalia exp1) / (avalia exp2)


expressao1::Exp Float
expressao1  = Div (Multi (Add (Val 3) (Val 12)) (Sub (Val 15) (Val 5))) (Neg (Neg (Div (Val 10) (Val 2))))

expressao2 :: Exp Float 
expressao2 = Neg (Multi (Sub (Add (Val 8) (Val 6)) (Add (Val 5) (Val 1))) (Add (Val 2) (Div (Val 6) (Val 2))))


--Exercicio 9

--Letra A

data ArvoreBinInt = Nulo | No Int ArvoreBinInt ArvoreBinInt deriving Show


internos::ArvoreBinInt->[Int]
internos Nulo = []
internos (No  x Nulo Nulo) = []
internos (No x esq dir) = [x]++internos(esq)++internos(dir)

--Letra B

somaNos::ArvoreBinInt->Int
somaNos Nulo = 0
somaNos (No x Nulo Nulo) = x
somaNos (No x esq dir) = x+somaNos(esq)+somaNos(dir)


--Letra C

pertence::Int->ArvoreBinInt->Bool
pertence _ Nulo = False
pertence a (No x esq dir)
 | a==x = True
 | otherwise = (pertence a esq) || (pertence a dir) 

--Exercicio 10

data ArvBinEA a = Vazia | Folha a | NoEA (Char, ArvBinEA a, ArvBinEA a) deriving (Show)


ea::ArvBinEA Float
ea = NoEA ('+', NoEA('*', Folha 10, Folha 5), Folha 7)


calculaExpr :: Floating a => ArvBinEA a -> a
calculaExpr Vazia = 0
calculaExpr (Folha a) = a
calculaExpr (NoEA (a,esq,dir)) = realizaAlgebra a (calculaExpr esq) (calculaExpr dir)

realizaAlgebra :: Floating a => Char -> a->a->a
realizaAlgebra a b c 
 | a == '+' = b+c
 | a == '*' = b*c
 | a == '/' = b/c
 | a == '-' = b-c
