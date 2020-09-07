--1

lst1 = [x*2 | x <- [1..10], x*2 >= 12] -- [12,14,16,18,20]
lst2 = [ x | x <- [50..100], mod x 7 == 3] --[52,59,66,73,80,87,94]
lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19] --[10,11,12,14,16,17,18,20]
lst4=[(x,y)| x <- [1..4], y <- [x..5]] --[(1,1),(1,2),(1,3),(1,4),(1,5),(2,2),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5),(4,4),(4,5)]

--2

quadrados :: Int-> Int-> [Int]
quadrados a b = [x*x | x<-[a..b]]

--3

seleciona_impares :: [Int] -> [Int]
seleciona_impares lista = [x | x<-lista,odd x]

--4

tabuada :: Int -> [Int]
tabuada x = [y*x | y<-[1..10]]

--5
bissexto :: Int -> Bool
bissexto x = if mod x 4 == 0 && mod x 100 /= 0 || x == 400 then True else False

bissextos :: [Int]-> [Int]
bissextos lista = [x |x<-lista, bissexto x]

--6

sublistas :: [[Int]] -> [Int]
sublistas lista = [x | xs<-lista, x<-xs]


--7

type Data = (Int, Int, Int)
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
atrasados (x:xs) data1 = if (precede x data1 == True) then x:atrasados xs data1 else atrasados xs data1

--8

npares :: [Int]->Int
npares [] = 0
npares (x:xs)
   |even x == True = 1 + npares xs
   |otherwise = npares xs

--9

produtorio :: [Int] -> Int
produtorio [] = 1
produtorio (x:xs) = x* produtorio xs

--10

comprime :: [[Int]] -> [Int]
comprime [] = []
comprime (x:xs) = x ++ comprime xs

--11

tamanho :: [a]->Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

--12

remove :: Int -> [Int] -> [Int]
remove x [] = []
remove x (y:ys)
    |x == y = remove x ys
    |otherwise = y:(remove x ys)

remDup :: [Int] -> [Int]
remDup [] = []
remDup (x:xs) = x:(remDup (remove x xs))

uniaoNRec :: [Int]-> [Int]-> [Int]
uniaoNRec lista1 lista2 = [x | x <- remDup(lista1++lista2)]


--13

uniaoNRec1 :: [Int]-> [Int]-> [Int]
uniaoNRec1 [] [] = []
uniaoNRec1 [a] [] = [a]
uniaoNRec1 [] [a] = [a]
uniaoNRec1 (x:xs) (y:ys) = remDup(x:xs ++ y:ys)