--1

paridade :: [Int] -> [Bool]
paridade lista =  map even lista

--2

prefixos :: [String] -> [String]
prefixos lista = map (take 3) lista 

--3 

saudacao :: [String] -> [String]
saudacao lista = map ("Oi " ++) lista

--4

-- filtrar :: Enum (a) => [a] -> [a]
-- filtrar z (y:ys) = [x | x <- y:z , filtrar ys]

--5

pares :: [Int] -> [Int]
pares x = filter(even) x

--6

solucoes :: [Int]->[Int]
solucoes l = filter(\l -> 2*l<l+5) l

--7

maior :: [Int]->Int
maior x = foldr1(max)x

--8

menor_min10 :: [Int]->Int
menor_min10 x = foldr(min) 10 x

--9

junta_silabas_plural :: [String] -> String
junta_silabas_plural x = foldr(++) "s" x

--10

lst1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
lst2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
lst3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
lst4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
lst5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
lst6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
lst7 = [1..1000]
lst8 = [1000,999..1]
lst9 = lst1++[0]
lst10 = [0]++lst3
lst11 = lst1++[0]++lst3
lst12 = lst3++[0]++lst1

--Bubblesort
bubblesortCres :: (Ord a) => [a]->[a]
bubblesortCres[]=[]
bubblesortCres lista = bubbleOrd lista (length lista)

bubbleOrd :: (Ord a) => [a] ->Int ->[a]
bubbleOrd lista 0 = lista
bubbleOrd lista n = bubbleOrd (troca lista) (n-1)

troca :: (Ord a) => [a]->[a]
troca[x] = [x]
troca(x:y:zs)
     |x>y  = y : troca (x:zs)
     |otherwise = x : troca (y:zs)

--SelctionSort
menorCres :: (Ord a) => [a]->a
menorCres [] = undefined
menorCres[x] = x
menorCres(x:xs)
   |x <= (menorCres xs) = x
   |otherwise = menorCres xs

removeCres :: (Ord a) => a ->[a] ->[a]
removeCres a [] = []
removeCres a(x:xs)
   |a==x = xs
   |otherwise = x:(removeCres a xs)

selectionsortCres :: (Ord a) => [a] -> [a]
selectionsortCres [] =[]
selectionsortCres xs = [x] ++ selectionsortCres(removeCres x xs)
                             where x = menorCres xs

--Insertionsort
insertionOrdCres :: (Ord a) => a->[a]->[a]
insertionOrdCres x [] = [x]
insertionOrdCres x(y:ys)
         |x <= y = (x:y:ys)
         |otherwise = y : (insertionOrdCres x ys)

insertionsortCres :: (Ord a) => [a] -> [a]
insertionsortCres [] = []
insertionsortCres(x:xs) = insertionOrdCres x (insertionsortCres xs)

--Quicksort 
quicksortCres :: (Ord a) => [a] -> [a]
quicksortCres [] = []
quicksortCres (s:xs) = quicksortCres [x | x <- xs, x<s]++[s]++quicksortCres[x |x <- xs, x>=s]

--11--
--BUBBLESORT--
bubble_sort2 :: (Ord a) => [a] -> ([a], Int)
bubble_sort2 [] = ([], 0)
bubble_sort2 lista = bubble2 (lista, 0) (length lista)

bubble2 :: (Ord a, Num t, Num b, Eq t) => ([a], b) -> t -> ([a], b)
bubble2 (lista, auxcont) 0 = (lista, auxcont)
bubble2 (lista, auxcont) n = bubble2 (auxiliar2 (lista, auxcont)) (n -1)

auxiliar2 :: (Ord a, Num b) => ([a], b) -> ([a], b)
auxiliar2 ([x], cont) = ([x], cont)
auxiliar2 ((x : y : zs), cont) =
  if x > y
    then add (auxiliar2 ((x : zs), cont + 1)) y
    else add (auxiliar2 ((y : zs), cont + 1)) x
  where
    add (lista, auxcont) a = (a : lista, auxcont)

--SELECTIONSORT--
selection_sort2 :: Ord a => [a] -> ([a], Int)
selection_sort2 lista = auxiliarSelection lista 0

auxiliarSelection :: (Ord a) => [a] -> Int -> ([a], Int)
auxiliarSelection [] n = ([], n)
auxiliarSelection (x : xs) n =
  let (least, n_num) = minimo2 (x : xs) n

      remove2 _ [] = []
      remove2 n (h : t) =
        if (n == h)
          then t
          else h : (remove2 n t)

      add (lst, n) y = (y : lst, n)
   in add (auxiliarSelection (remove2 least (x : xs)) n_num) least

minimo2 :: (Ord a) => [a] -> Int -> (a, Int)
minimo2 [] _ = undefined
minimo2 [x] cont = (x, cont)
minimo2 (x : y : xs) cont
  | x > y = minimo2 (y : xs) (cont + 1)
  | otherwise = minimo2 (x : xs) (cont + 1)

--INSERTIONSORT--

insertion_sort2 :: (Ord a) => [a] -> ([a], Int)
insertion_sort2 [] = ([], 0)
insertion_sort2 [x] = ([x], 0)
insertion_sort2 (h : t) =
  let (sorted_tail, n) = insertion_sort2 t

      (lst, n1) = insere_ordenado2 h sorted_tail n
   in (lst, n1)

insere_ordenado2 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insere_ordenado2 x [] n = ([x], n)
insere_ordenado2 x (h : t) n =
  if (x <= h)
    then ((x : h : t), n + 1)
    else add (insere_ordenado2 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)

--QUICKSORT--
auxiliarQuick :: [a] -> Int -> (a -> Bool) -> ([a], Int)
auxiliarQuick [] n _ = ([], n)
auxiliarQuick (x : xs) n cond =
  if (cond x)
    then add (auxiliarQuick xs (n + 1) cond) x
    else auxiliarQuick xs (n + 1) cond
  where
    add (list, n) y = (y : list, n)

quicksort2 :: (Ord a) => [a] -> ([a], Int)
quicksort2 [] = ([], 0)
quicksort2 (piv : xs) =
  let (left, n_L) = auxiliarQuick xs 0 (<= piv)
      (right, n_R) = auxiliarQuick xs 0 (> piv)
      (sorted_L, n1_L) = quicksort2 left
      (sorted_R, n1_R) = quicksort2 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)

--12--
--BUBBLESORT--
bubble_sort3 :: (Ord a) => [a] -> ([a], Int)
bubble_sort3 [] = ([], 0)
bubble_sort3 lista = bubble3 (lista, 0) (length lista)

bubble3 :: (Ord a, Num t, Num b, Eq t) => ([a], b) -> t -> ([a], b)
bubble3 (lista, count) 0 = (lista, count)
bubble3 (lista, count) n = bubble3 (auxiliar3 (lista, count)) (n -1)

auxiliar3 :: (Ord a, Num b) => ([a], b) -> ([a], b)
auxiliar3 ([x], cont) = ([x], cont)
auxiliar3 ((x : y : zs), cont) =
  if x > y
    then add (auxiliar3 ((y : zs), cont + 1)) x
    else add (auxiliar3 ((x : zs), cont + 1)) y
  where
    add (lista, count) a = (a : lista, count)

--SELECTIONSORT--
selection_sort3 :: Ord a => [a] -> ([a], Int)
selection_sort3 lista = auxiliarSelection3 lista 0

auxiliarSelection3 :: (Ord a) => [a] -> Int -> ([a], Int)
auxiliarSelection3 [] n = ([], n)
auxiliarSelection3 (x : xs) n =
  let (least, n_num) = minimo3 (x : xs) n

      remove3 _ [] = []
      remove3 n (h : t) =
        if (n == h)
          then t
          else h : (remove3 n t)

      add (lst, n) y = (y : lst, n)
   in add (auxiliarSelection3 (remove3 least (x : xs)) n_num) least

minimo3 :: (Ord a) => [a] -> Int -> (a, Int)
minimo3 [] _ = undefined
minimo3 [x] cont = (x, cont)
minimo3 (x : y : xs) cont
  | x > y = minimo3 (x : xs) (cont + 1)
  | otherwise = minimo3 (y : xs) (cont + 1)

--INSERTIONSORT--
insertion_sort3 :: (Ord a) => [a] -> ([a], Int)
insertion_sort3 [] = ([], 0)
insertion_sort3 [x] = ([x], 0)
insertion_sort3 (h : t) =
  let (sorted_tail, n) = insertion_sort3 t

      (lst, n1) = insere_ordenado3 h sorted_tail n
   in (lst, n1)

insere_ordenado3 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insere_ordenado3 x [] n = ([x], n)
insere_ordenado3 x (h : t) n =
  if (x >= h)
    then ((x : h : t), n + 1)
    else add (insere_ordenado3 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)

--QUICKSORT--

auxiliarQuick3 :: [a] -> Int -> (a -> Bool) -> ([a], Int)
auxiliarQuick3 [] n _ = ([], n)
auxiliarQuick3 (x : xs) n cond =
  if (cond x)
    then auxiliarQuick3 xs (n + 1) cond
    else add (auxiliarQuick3 xs (n + 1) cond) x
  where
    add (list, n) y = (y : list, n)

quicksort3 :: (Ord a) => [a] -> ([a], Int)
quicksort3 [] = ([], 0)
quicksort3 (piv : xs) =
  let (left, n_L) = auxiliarQuick3 xs 0 (<= piv)
      (right, n_R) = auxiliarQuick3 xs 0 (> piv)
      (sorted_L, n1_L) = quicksort3 left
      (sorted_R, n1_R) = quicksort3 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)
