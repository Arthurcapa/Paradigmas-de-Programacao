module Suguru where

--apos alterar os dois tabuleiros e n no código fonte, use o ghci para carregar o módulo com :l suguru.hs e
--invoque a funcao "solve" (não tem parametros) que a solucao sera impressa na tela

-- O tabuleiro que queremos resolver, zeros representam espaços vazios                                  --ALTERAR VALOR AQUI
tabuleiro :: [Int]
tabuleiro = [0, 3, 0, 2, 0, 3,
             4, 0, 4, 0, 1, 0,
             0, 0, 0, 5, 0, 4,
             3, 5, 0, 0, 0, 0,
             0, 0, 0, 2, 0, 0,
             5, 2, 0, 0, 4, 0]

-- O tabuleiro com as areas definidas atraves de tuplas                                                 --ALTERAR VALOR AQUI
tabuleiroDiv :: [(Int,Int)]
tabuleiroDiv = [(1,0), (1,3), (1,0), (2,2), (2,0), (2,3),
             (3,4), (4,0), (1,4), (5,0), (2,1), (2,0),
             (3,0), (4,0), (4,0), (5,5), (5,0), (5,4),
             (3,3), (3,5), (4,0), (6,0), (6,0), (5,0),
             (7,0), (3,0), (4,0), (6,2), (6,0), (8,0),
             (7,5), (7,2), (7,0), (7,0), (6,4), (8,0)]

--2 3 1 2 4 3
--4 5 4 3 1 5
--2 1 2 5 2 4               <----Solucao para referencia
--3 5 3 1 3 1
--4 1 4 2 5 2
--5 2 3 1 4 1

--n é a dimensao do tabuleiro(tabuleiro tera tamanho nxn)                                               --ALTERAR VALOR AQUI
n :: Int
n = 6

--f é o index do ultimo elemento do tabuleiro
f :: Int
f = (n*n)-1

--matriz com os digitos a serem testados
possibilities :: [Int]
possibilities = [1, 2, 3, 4, 5, 6, 7, 8, 9]

----------------------------------------------------------------------------Funcoes para testar os lados e diagonais em volta do elemento-----------------------------

--testa se o quadrado a esquerda tem um número igual
leftOk :: Int -> [Int] -> Bool
leftOk i s = if (i `mod` n) == 0 then True
  else if s!!(i-1) == 0 then True
    else if s!!i == s!!(i-1) then False
      else True

--testa se o quadrado a direitaa tem um número igual
rightOk :: Int -> [Int] -> Bool
rightOk i s = if ((i+1) `mod` n) == 0 then True
  else if s!!(i+1) == 0 then True
    else if s!!i == s!!(i+1) then False
      else True

--testa se o quadrado acima tem um número igual
upOk :: Int -> [Int] -> Bool
upOk i s = if i < n then True
  else if s!!(i-n) == 0 then True
    else if s!!i == s!!(i-n) then False
      else True

--testa se o quadrado abaixo tem um número igual
downOk :: Int -> [Int] -> Bool
downOk i s = if i >= (n*(n-1)) then True
  else if s!!(i+n) == 0 then True
    else if s!!i == s!!(i+n) then False
      else True

--testa se o quadrado ao nordeste tem um número igual
neOk :: Int -> [Int] -> Bool
neOk i s = if i < n || ((i+1) `mod` n) == 0 then True
  else if s!!(i-n+1) == 0 then True
    else if s!!i == s!!(i-n+1) then False
      else True

--testa se o quadrado ao noroeste tem um número igual
nwOk :: Int -> [Int] -> Bool
nwOk i s = if i < n || (i `mod` n) == 0 then True
  else if s!!(i-n-1) == 0 then True
    else if s!!i == s!!(i-n-1) then False
      else True

--testa se o quadrado ao sudeste tem um número igual
seOk :: Int -> [Int] -> Bool
seOk i s = if i >= (n*(n-1)) || ((i+1) `mod` n) == 0 then True
  else if s!!(i+n+1) == 0 then True
    else if s!!i == s!!(i+n+1) then False
      else True

--testa se o quadrado ao sudoeste tem um número igual
swOk :: Int -> [Int] -> Bool
swOk i s = if i >= (n*(n-1)) || (i `mod` n) == 0 then True
  else if s!!(i+n-1) == 0 then True
    else if s!!i == s!!(i+n-1) then False
      else True

--testa se algum dos quadrados em volta tem um número igual
sidesOk :: Int -> [Int] -> Bool
sidesOk i s = (downOk i s) && (upOk i s) && (leftOk i s) && (rightOk i s) && (neOk i s) && (nwOk i s) && (seOk i s) && (swOk i s)

----------------------------------------------------------------------------Funcoes para testar os números na mesma area do elemento-----------------------------

--testa se duas tuplas são iguais e retorna 1 se sim e 0 se não
isEqual :: (Int, Int) -> (Int, Int) -> Int
isEqual (x, y) (x', y') = if x == x' && y == y' then 1
else 0

--aplica isEqual em todos os elementos de uma matriz
mapF :: (Int, Int) -> [(Int, Int)] -> [Int]
mapF (x', y') s = map (\(x, y) -> isEqual (x', y') (x, y)) s

--testa se tem dois números iguais na área do indice i
areaOk :: Int -> [(Int, Int)] -> Bool
areaOk i s = if (sum (mapF (s!!i) s)) == 2 then False
else True

------------------------------------------------------------------Funcoes para testar se o elemento esta no intervalo de possibilidades para aquela area-----------------------------

--testa se duas tuplas tem o mesmo primeiro elemento
isEqual' :: (Int, Int) -> (Int, Int) -> Int
isEqual' (x, y) (x', y') = if x == x' then 1
else 0

--aplica isEqual' em todos os elementos de uma matriz
mapS :: (Int, Int) -> [(Int, Int)] -> [Int]
mapS (x', y') s = map (\(x, y) -> isEqual' (x', y') (x, y)) s

--retorna quantos quadrados tem a área do indice i
areaSize :: Int -> [(Int, Int)] ->Int
areaSize i s = (sum (mapS (s!!i) s))

--testa se o elemento e é maior do que a quantidade de quadrados da área do indice i
possibilitiesOk :: Int -> Int -> [(Int, Int)] -> Bool
possibilitiesOk e i s = if e < ((areaSize i s) + 1) then True
else False

-------------------------------------------------------------------------------------Funcoes para encontrar a solucao------------------------------------------------

--aplica todos os testes previamente implementados
tester :: Int -> [Int] -> [(Int, Int)] -> Bool
tester i s s' = (sidesOk i s) && (areaOk i s') && (possibilitiesOk (s!!i) i s')

--testa se o tabuleiro foi completamente preenchido
isEnd :: Int -> [Int] -> Bool
isEnd i s = if (i == f) && ((s!!i) /= 0) then True
else False

--encontra o próximo espaço vazio na matriz
nextBlank :: Int -> [Int] -> Int
nextBlank i s | i == f           = f
              | s !! (i + 1) == 0 = i + 1
              | otherwise         = nextBlank (i + 1) s

--retorna um novo tabuleiro com o elemento x inserido no indice i
getTesterMap :: Int -> Int -> [Int] -> [Int]
getTesterMap x i s  = take i s ++ [x] ++ drop (i + 1) s

--retorna um novo tabuleiroDiv com o elemento x inserido no indice i
getTesterMap' :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
getTesterMap' x i s' = take i s' ++ [(fst(s'!!i), x)] ++ drop (i + 1) s'

--aplica a função solver com todos os elementos da matriz x(possibilities)
mapT :: [Int] -> Int -> [Int] -> [(Int, Int)] -> [[Int]]
mapT x i s s' = map (\x -> solver x i s s') x

--funcao recursiva que encontra a solução do suguru
solver :: Int -> Int -> [Int]-> [(Int, Int)] -> [Int]
solver x i s s' = if (tester i (getTesterMap x i s ) (getTesterMap' x i s')) then
  if (isEnd i (getTesterMap x i s)) then (getTesterMap x i s)
  else concat (mapT possibilities (nextBlank i s) (getTesterMap x i s) (getTesterMap' x i s'))
  else []

-------------------------------------------------------------------------------------Funcoes para printar a solucao-----------------------------------------------

--insere um elemento c entre todos os elementos de uma matriz
joinWith :: a -> [a] -> [a]
joinWith _ (x:[])  = [x]
joinWith c (x:xs)  = x : c : joinWith c xs

--altera o resultado obtido para um formato mais legível
pPrint [] = []
pPrint s  = spaceOut s ++ pPrint (drop n s)
  where showS s    = concatMap show s
        space      = ' '
        newline    = "\n"
        spaceOut s = joinWith space (take n (showS s) ++ newline)

--comando para imprimir na tela a solução
solve = putStrLn (pPrint (concat (mapT possibilities 0 tabuleiro tabuleiroDiv)))