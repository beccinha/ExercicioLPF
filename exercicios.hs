menorDeDois:: Int -> Int -> Int
menorDeDois a b |(a <= b) = a
                |True = b

menorDeTres:: Int -> Int -> Int -> Int
menorDeTres a b c |(a <= b) && (b <= c) = a
                  |(b <= a) && (a <= c) = b
                  |True = c


fatorial:: Int -> Int
fatorial 1 = 1
fatorial x = x * fatorial (x-1)

fibonacci:: Int -> Int
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = soma (fibonacci (n-2)) (fibonacci (n-1)) 
                where soma a b = a + b

-- nessa função retona o elemento corresponde a posição que foi especificada
elemento:: [Int] -> Int -> Int
elemento [a] _ = error "especificar a posição que quer retornar"
elemento [] a = error "Lista Vazia"
elemento (a:as) 1 = a 
elemento (a:as) x = elemento as (x-1)

pertence:: [Int] -> Int -> Bool
pertence [] x = False 
pertence (a:as) x | (x==a) = True
                  | True = pertence as x


nro_elementos :: [Int] -> Int
nro_elementos [] = 0
nro_elementos [a] = 1
nro_elementos (a:as) = 1 + nro_elementos as

maior :: [Int] -> Int
maior [] = 0
maior [a] = a 
maior (a:b:as)| (a >= b) = maior (a:as)
              | True = maior (b:as)

conta_ocorrencia:: Int -> [Int] -> Int
conta_ocorrencia _ [] = 0
conta_ocorrencia x (a:as) | (a == x) = 1 + conta_ocorrencia x as
						  | True = conta_ocorrencia x as

unica_ocorrencia:: Int -> [Int] -> Bool
unica_ocorrencia _ [] = error "lista vazia"
unica_ocorrencia x [] = False
unica_ocorrencia x (a:as) | (conta_ocorrencia x (a:as) > 1 || conta_ocorrencia x (a:as) == 0) = False 
                          | otherwise = True

maiores_que:: Int -> [Int] -> [Int]
maiores_que _ [] = []
maiores_que x (a:as) | (a > x) = a : maiores_que x as
                     | otherwise = maiores_que x as


concatena:: [Int] -> [Int] -> [Int]
concatena [] l = l
concatena l [] = l 
concatena (a:as) (b:bs) = a : concatena as (b:bs)  


remover:: Int -> [Int] -> [Int]
remover _ [] = []
remover x (a:as) | (x == a) = as
				 | otherwise = a : remover x as 


remover_ultimo:: [Int] -> [Int]
remover_ultimo [a] = []
remover_ultimo (a:as) = a : remover_ultimo as

remover_repetidos:: [Int] -> [Int]
remover_repetidos [] = []
remover_repetidos (a:as) = a : remover_repetidos (remover_all a as)
                         where
                         	remover_all _ [] = []
                         	remover_all x (h:t) | x == h = remover_all x t
                         						| otherwise = h : remover_all x t


{-maiores:: Int -> [Int] -> [Int]
maiores _ [] = []
maiores 0 l = []
maiores x l@(a:as) = maior l : maiores_que (x-1) l -} 


gera_sequencia:: Int -> [Int]
gera_sequencia 0 = []
gera_sequencia n = (gera_sequencia (n-1)) ++ [n] ++ [(negate n)]


inverte:: [Int] -> [Int]
inverte [] = []
inverte (a:as) = inverte as ++ [a]

divide:: [Int] -> Int -> ([Int],[Int])
divide [] _ = ([],[])
divide [] 0 = ([],[])
divide (a:as) n = ([a] ++ divide (n-1) as),()