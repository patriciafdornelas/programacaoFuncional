--Nome integrantes da dupla: Patrícia Fernandes Dornelas e Rui Pablo de Brito Ferreira
--Matrícula : 11821BCC043 e 11721BCC004

--1

triangulo :: Int-> Int-> Int->String
triangulo a b c 
    |a == b && a == c && b == c = "Equilatero"
    |a == 90 || b == 90 || c == 90 = "Retangulo"
    |a > 90 || b > 90 || c > 90 = "Obtuso"
    |a+b == c = "Nao triangulo"
    |otherwise = "Simples"

--2

equacao :: Float-> Float -> Float ->(Float,Float)
equacao a b c 
    |a /= 0 = (((-b)+sqrt((b*b)-4*(a*c)))/(2*a),((-b)-sqrt((b*b)-4*(a*c)))/(2*a))
    |a == 0 = ((-c)/b,a)


--3

type Data = (Int,Int,Int)
passagem :: Float ->Data ->Data ->Float
passagem v (d,m,a) (d1,m1,a1)
    |a-a1 > 2 && a-a1 <= 10 = (40/100)*v
    |a-a1 > 0 && a-a1 <= 2 = (15/100)*v
    |a-a1 <= 70 = (50/100)*v
    |otherwise = v

--4 

--A)
gera1 :: [Int] 
gera1 = [x*x | x<-[4..14], even x == False]

--B)

gera2 :: [(Int,Int)]
gera2 = [(x,x*2)|x<-[1..4], x>1 && x<4]

--C)

gera3 = [x | l1<-[10..15],x<-[1..l1]]

--D)

gera4 = [(x,x+1)| x<-[1..16], even x == False]

--E)

gera5 = [(x+(x+1))| x<-[1..16], even x == False]

--5)

--A)

contaNegM2 :: [Int] -> Int
contaNegM2 lista = length [x | x<-lista, x < 0 && mod (-x) 2 == 0]

--B)

listaNegM2 :: [Int] -> [Int]
listaNegM2 lista = [x | x<-lista, x < 0 && mod (-x) 2 == 0]

--6)

distancia :: (Float,Float)->(Float,Float)->[Float]
distancia (a,a1) (b,b1) = [x | x<-[sqrt((b-a)^2 + (b1-a1)^2)]] 

--7)

fatores :: Int -> [Int]
fatores fat = [x | x<-[1..fat], mod fat x == 0 ]

primos :: Int -> Int -> [Int]
primos a b = [x | x<-[a..b], fatores x == [1,x]]

--8)

mdc::(Int,Int) -> Int
mdc (x,y)
 |y == 0 = x           
 |otherwise = mdc (y, (mod x y))

auxmmc::Int->Int->Int
auxmmc x y =  div (x*y) (mdc (x,y))

mmc::Int->Int->Int->Int
mmc x y z = auxmmc x (auxmmc y z)


--9)

calculaSerie :: Int -> Int -> Float
calculaSerie x 1 = fromIntegral(div 1 x)
calculaSerie x n = if (mod n 2 == 0) then fromIntegral(div n x) + calculaSerie x(n-1) else fromIntegral (div x n) + calculaSerie x (n-1)

--10

auxFizzbuzz :: Int-> String
auxFizzbuzz i
    |mod i 3 == 0 && mod i 5 == 0 = "Fizzbuzz"
    |mod i 3 == 0 = "Fizz"
    |mod i 5 == 0 = "Buzz"
    |otherwise = "No"

fizzbuzz :: Int -> [String]
fizzbuzz y = [auxFizzbuzz x | x <- [1..y]]

--11
auxConta_ocorrencias :: Int-> [Int] ->Int
auxConta_ocorrencias _ [] = 0
auxConta_ocorrencias a  (x:xs) = if (a == x) then 1 + auxConta_ocorrencias a xs else auxConta_ocorrencias a xs

conta_ocorrencias :: Int -> Int -> [Int] -> (Int,Int)
conta_ocorrencias a b lista = (auxConta_ocorrencias a lista, auxConta_ocorrencias b lista)

--12

unica_ocorrencia :: Int-> [Int] -> Bool
unica_ocorrencia a [] = False
unica_ocorrencia a (x:xs)
   |(a == x) = if (elem a xs) then False else True
   |otherwise = unica_ocorrencia a xs

--13

intercala :: [Int] -> [Int] -> [Int]
intercala x [] = x
intercala [] x = x
intercala (x:xs) (y:ys) = x: y:intercala xs ys

--14

type Contato = ([Char], [Char], [Char], [Char])
type Agenda = [Contato]

contato :: Agenda
contato = [("Patricia", "Uma rua ai", "111", "patriciafdornelas@"),
   ("Maria", "Duas ruas ai", "222", "mdemaria@"),
   ("Jose", "Tres ruas ai", "333", "jdejose@")]

recupera_nome ::[Char]-> Agenda -> [Char]
recupera_nome a [] = "Email desconhecido"
recupera_nome a ((n,_,_,em):xs)
   |a == em = n
   |otherwise = recupera_nome a xs

--15

type Pessoa = (String, Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [ ("Rosa",1.66,27,'S'),
    ("João", 1.85, 26, 'C'),
    ("Maria", 1.55, 62, 'S'),
    ("Jose", 1.78, 42, 'C'),
    ("Paulo", 1.93, 25, 'S'),
    ("Clara", 1.70, 33, 'C'),
    ("Bob", 1.45, 21, 'C'),
    ("Rosana", 1.58,39, 'S'),
    ("Daniel", 1.74, 72, 'S'),
    ("Jocileide", 1.69, 18, 'S') ]

--Altura media

alturaMedia :: [Pessoa] -> Float
alturaMediaAux :: [Pessoa] -> Float -> Float -> Float
alturaMediaAux [] n sum = (/) sum n
alturaMediaAux ((a,b,c,d):x) n s = alturaMediaAux x (n+1) (s+b)

alturaMedia li = alturaMediaAux li 0 0

--Idade da pessoa mais nova

menorIdadeAux :: [Pessoa] -> Int -> Int
menorIdadeAux [] i = i
menorIdadeAux ((a,b,c,d):x) i
 | (i > c || i == 0) = menorIdadeAux x c
 | otherwise = menorIdadeAux x i
 
menorIdade :: [Pessoa] -> Int
menorIdade li = menorIdadeAux li 0

--Nome e estado civil da pessoa mais velha  

nomeMaiorIdadeAux :: [Pessoa] -> Int -> String -> String
nomeMaiorIdadeAux [] _ nome = nome
nomeMaiorIdadeAux ((a,b,c,d):x) i nome
 | (i < c || i == 0) = nomeMaiorIdadeAux x c a
 | otherwise = nomeMaiorIdadeAux x i nome

estadoCivilAux :: [Pessoa] -> Int -> Char -> Char
estadoCivilAux [] _ est_civil = est_civil
estadoCivilAux ((a,b,c,d):xs) i est_civil
   |(i < c || i == 0) = estadoCivilAux xs c d
   |otherwise = estadoCivilAux xs i est_civil
 
nomeMaiorIdade :: [Pessoa] -> (String,Char)
nomeMaiorIdade li = (nomeMaiorIdadeAux li 0 "Lista Vazia",estadoCivilAux li 0 'V')

--Todos os dados de cada pessoa com 50 anos ou mais

mais50 :: [Pessoa] -> [Pessoa]
mais50 [] = []
mais50 ((a,b,c,d):x)
 | c >= 50 = (a,b,c,d):(mais50 x)
 | otherwise = mais50 x

--O número de pessoas casadas com idade superior a i (ex: i = 35)

numMaiorI :: Int -> [Pessoa] ->  Int
numMaiorI y [] = 0
numMaiorI y ((a,b,c,d):x)
 | (c >= y && d == 'C') = 1 + numMaiorI y x
 | otherwise = 0 + numMaiorI y x

--16

insere_ord :: Ord a => a -> [a] -> [a]
insere_ord y [] = [y]
insere_ord y (x:xs)
 | y<=x = y : x : xs
 | otherwise = x : insere_ord y xs

 --17

reverte :: Ord a => [a] -> [a]
reverte lista = reverse[x | x<-lista]

--18


remove :: Ord a => a -> [a] -> [a]
remove x [] = []
remove x (y:ys)
    |x == y = remove x ys
    |otherwise = y:(remove x ys)

sem_repetidos :: Ord a => [a] -> [a]
sem_repetidos [] = []
sem_repetidos (x:xs) = x:(sem_repetidos (remove x xs))

--19

disponiveis :: [Int]
disponiveis = [1,2,5,10,20,50,100]

notasTroco :: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco troco = [x:xs | x <- disponiveis, troco >= x,xs <- notasTroco (troco-x) ]

--20

