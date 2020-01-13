{- |

= Introdução

Nesta tarefa, tivemos que encontrar uma forma de compactar ao máximo as instruções dadas aos bulldozers que 
construíam o mapa. Para atingir esse objetivo, podíamos usar os "comandos": Anda, Sobe, Desce, Repete e Teleporta.

= Objetivos

Como tínhamos pensado em várias estratégias, decidimos colocá-las todas em prática. Para isso, criamos uma função 
que determina qual das estratégias é melhor para cada caso, que usa a tamanhoInstrucoes, que é uma função fornecida
pelos professores. 
A estratégia 1 era a mais simples de todas e consistia na construção de um Repete que juntava instruções iguais
e que estivessem seguidas.
A estratégia 2 consiste em construir a lista de instruções com meio de Repetes do piso que mais vezes aparece e 
depois usar o Teleporta para mudar as peças que não estejam corretamente colocadas. 
A estratégia 3 procurava por padrões verticais. Primeiramente, é feita uma lista de instruções básica, que só traduz 
cada tipo de peça para a respetiva instrução. Depois, divide as instruções numa lista, em que cada elemento é formado 
pelas instruções referentes a peças que se encontram ao mesmo comprimento do inicio. Depois, essas instruções são 
passadas por 3 filtros, um pega apenas nas instruções do tipo anda, outra do tipo sobe e outra do tipo desce, formando-se
assim 3 listas, uma com cada tipo de instrução. Finalmente, cada uma das listas é analisada para ver se existem instruções 
iguais (para serem compactadas) e concatenadas numa só lista de instruções final.
A estratégia 4 consistia em aplicar a estratégia 1 à 3, ou seja, a procurar instruções iguais e seguidas, para construir um
Repete com elas.

= Discussão e Conclusão

Em retrospetiva, acreditamos que fizemos o melhor que conseguimos com os conhecimentos que possuímos. Conseguimos obter
uma percentagem de compressão acima dos 47%, o que nos parece bastante razoável. Na nossa opinião foi uma das nossas tarefas
favoritas, pois permitiu-nos sair um pouco do pré-definido e porque nos obrigou a pensar "fora da caixa".

-}
module Tarefa3_2019li1g059 where

import LI11920
import Tarefa1_2019li1g059
import Tarefa0_2019li1g059
import Data.List

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [(gera 4 20 1),(gera 4 20 2),(gera 4 20 3),(gera 4 20 4),(gera 4 20 5),([[Recta Terra 0, Recta Lama 0,Recta Lama 0,Recta Lama 0]]),([[Recta Terra 0, Recta Lama 0, Recta Lama 0, Recta Lama 0, Rampa Boost 0 2, Recta Lama 2, Rampa Terra 2 0, Recta Lama 0]]),([[Recta Terra 0, Recta Boost 0, Recta Boost 0, Recta Boost 0]]),(gera 1 5 3)]

-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- Escolhe, pelo número mínimo de instruções, a estratégia que vai usar.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
desconstroi :: Mapa -> Instrucoes
desconstroi m | tx == t1 = constroiInstructionSetMapa 0 mapa
              | tx == t2 = construirRepete mapa ++ (analisar mapa (0,0) (b-1))
              | tx == t3 = juntaPistas (transpose (toMatrizInstrucoes mapa (desconstroiMapa 0 mapa)))
              | tx == t4 = toRepete (juntaPistas (transpose (toMatrizInstrucoes mapa (desconstroiMapa 0 mapa))))
    where mapa = map tail m
          (_,b) = dimensaoMatriz m
          t1 = tamanhoInstrucoes (constroiInstructionSetMapa 0 mapa)
          t2 = tamanhoInstrucoes (construirRepete mapa ++ (analisar mapa (0,0) (b-1)))
          t3 = tamanhoInstrucoes (juntaPistas (transpose (toMatrizInstrucoes mapa (desconstroiMapa 0 mapa))))
          t4 = tamanhoInstrucoes (toRepete (juntaPistas (transpose (toMatrizInstrucoes mapa (desconstroiMapa 0 mapa)))))
          tx = minimum [t1,t2,t3,t4]

-- * Funções comuns a todas as estratégias

-- | Gera uma instrução
toInstrucao :: Int -- ^ Número da pista
               -> Peca -- ^ Peça a transformar
               -> Instrucao -- ^ 'Instrução' resultante
toInstrucao npista (Recta piso _) = Anda [npista] piso
toInstrucao npista (Rampa piso h0 hf) | hf > h0 = Sobe [npista] piso (hf - h0)
                                      | hf < h0 = Desce [npista] piso (h0 - hf)


-- * ESTRATÉGIA 1
-- Dadas duas instruções seguidas e iguais, faz o repete que provém delas

-- | Faz as instruções genéricas para uma pista inteira
desconstroiPista :: Int -- ^ Número da pista
                    -> Pista -- ^ Pista a transformar
                    -> Instrucoes -- ^ Lista de 'Instrução' resultante
desconstroiPista _ [] = []
desconstroiPista npista (h:t) = (toInstrucao npista h): desconstroiPista npista t

-- | Faz as instruções genéricas de um mapa, usando a função 'desconstroiPista'
desconstroiMapa :: Int -- ^ Número da pista
                   -> Mapa -- ^ Mapa a transformar
                   -> Instrucoes -- ^ Lista de 'Instrução' resultante
desconstroiMapa _ [] = []
desconstroiMapa npista (h:t) = (desconstroiPista npista h) ++ (desconstroiMapa (npista+1) t)


-- | Faz um MSet com a (Instrução,Int) para facilitar o repete
constroiInstructionSetPista :: Instrucoes -- ^ Lista de 'Instrução'
                               -> [(Instrucao,Int)] -- ^ MSet com Instrução e as vezes que aparece seguida
constroiInstructionSetPista [] = []
constroiInstructionSetPista (h:t) = (h,n):constroiInstructionSetPista r
   where l = takeWhile (== h) (h:t)
         n = length l
         r = dropWhile (== h) (h:t)

-- | Faz um MSet para o mapa inteiro usando o 'constroiInstructionSetPista'
constroiInstructionSetMapa :: Int -- ^ Número de Pista
                              -> Mapa -- ^ Mapa
                              -> Instrucoes -- ^ Lista de 'Instrução' que contém o repete
constroiInstructionSetMapa _ [] = []
constroiInstructionSetMapa npista (h:t) = constroiRepete (constroiInstructionSetPista (desconstroiPista npista h)) ++ (constroiInstructionSetMapa (npista + 1) t)

-- | Converte o MSet em Instrucoes, através do número de vezes que ela aparece
constroiRepete :: [(Instrucao,Int)] -- ^ MSet com Instrução e as vezes que aparece seguida
                  -> Instrucoes -- ^ Lista de 'Instrução' que já contém o 'Repete'
constroiRepete ((a,n):ts) | n > 1 = (Repete n [a]):constroiRepete ts
                          | n == 1 = a:constroiRepete ts
constroiRepete _ = []


-- * ESTRATÉGIA 2
-- Faz um mapa todo com a mesma peça e usa o teleporta para mudar as peças que não são iguais à colocada

-- | Compara qual o tipo de que existem mais Rectas numa Pista
comparar :: (Int,Int,Int,Int) -> Pista -> (Int,Int,Int,Int)
comparar (a,b,c,d) [] = (a,b,c,d)
comparar (a,b,c,d) ((Recta h _):t) = case h of Terra -> comparar (a+1,b,c,d) t
                                               Relva -> comparar (a,b+1,c,d) t
                                               Boost -> comparar (a,b,c+1,d) t
                                               Lama -> comparar (a,b,c,d+1) t
                                               Cola -> comparar (a,b,c,d) t
comparar x ((Rampa _ _ _):t) = comparar x t

-- | Compara qual o tipo de que existem mais Rectas num Mapa
compararM :: (Int,Int,Int,Int) -> Mapa -> (Int,Int,Int,Int)
compararM x [] = x
compararM a (h:t) = compararM (comparar a h) t

-- | Converta o resultado do 'compararM' para um Piso
conversor :: (Int,Int,Int,Int) -> Piso
conversor (a,b,c,d) | e == a = Terra
                    | e == b = Relva
                    | e == c = Boost
                    | e == d = Lama
    where e = maximum [a,b,c,d]

-- | Constrói um repete que faz uma Instrucoes com apenas 2 'Instrucao'
--
-- Contem apenas "Repete ComprimentoDaPista [Anda [0..numeroPistas] PisoQueApareceMaisVezes"
construirRepete :: Mapa -- ^ Mapa
                   -> Instrucoes -- ^ Lista de 'Instrução' apenas com um repete.
construirRepete m = [Repete (y) [Anda [0..(x-1)] piso]]
    where (x,y) = dimensaoMatriz m
          piso = conversor (compararM (0,0,0,0) m)

-- |  Analisa o mapa e teleporta o bulldozer para as 'Peça' diferentes.
--
-- Se a altura da peça for diferente, também o teleporta para lá.
analisar :: Mapa -- ^ Mapa
            -> PosicaoMatriz -- ^ Posição do analisar
            -> Int -- ^ Posição do bulldozer.
            -> Instrucoes -- ^ Lista de 'Instrução' final
analisar m (x,y) k | x > a = []
                   | y > b = analisar m ((x+1),0) d
                   | ror && (piso == pisoMax) && h == 0 = analisar m (x,(y+1)) k
                   | ror && (piso == pisoMax) && h /= 0 = [toInstrucao x peca] ++ analisar m (x,(y+1)) (y+1)
                   | k == y = [toInstrucao x peca] ++ analisar m (x,(y+1)) (y+1)
                   | otherwise = [Teleporta [x] (y-k)] ++ [toInstrucao x peca] ++ analisar m (x,(y+1)) (y+1)
    where (c,d) = dimensaoMatriz m
          (a,b) = ((c-1),(d-1))
          peca = getPeca (x,y) m
          piso = getPiso peca
          ror = getRor peca
          pisoMax = conversor (compararM (0,0,0,0) m)
          h = getAltura peca

-- * Estratégia 3
-- Padrões verticais

-- | Divide as instruções por nr de pista.
toMatrizInstrucoes :: Mapa -- ^ Mapa a desconstruir.
                      -> Instrucoes -- ^ Instruções a compactar.
                      -> [Instrucoes] -- ^ Lista de Instrucoes organizadas por nr de pista.
toMatrizInstrucoes _ [] = []
toMatrizInstrucoes map inst = (fst pista):toMatrizInstrucoes map (snd pista)
  where pista = splitAt (length (head map)) inst

-- | Verifica se existem padrões verticais de cada tipo (anda, sobe ou desce) e compacta instruções que sejam iguais numa instrução do tipo repete.
juntaPistas :: [Instrucoes] -- ^ Lista de instruções a compactar.
                -> Instrucoes -- ^ Instruções a compactar.
juntaPistas [] = []
juntaPistas (h:t) = insPista ++ juntaPistas t
  where insAnda = filterAnda h
        insSobe = filterSobe h
        insDesce = filterDesce h
        juntaAnda = juntaVerticalAnda insAnda
        juntaSobe = juntaVerticalSobe insSobe
        juntaDesce = juntaVerticalDesce insDesce
        insPista = (juntaAnda ++ juntaSobe ++ juntaDesce)

-- | Filtra uma lista de instruções, ficando só com as instruções do tipo anda.
filterAnda :: Instrucoes -- ^ Instruções iniciais.
              -> Instrucoes -- ^ Instruções apenas do tipo Anda.
filterAnda [] = []
filterAnda ((Anda pista piso):t) = (Anda pista piso): (filterAnda t)
filterAnda ((_):t) = filterAnda t

-- | Verifica se duas instruções do tipo anda são totalmente iguais, para depois as juntar com uma instrução do tipo repete.
juntaVerticalAnda :: Instrucoes -- ^ Instruções apenas do tipo Anda.
                     -> Instrucoes -- ^ Junta instruções do tipo Anda.
juntaVerticalAnda [] = []
juntaVerticalAnda [x] = [x]
juntaVerticalAnda ((Anda pista1 piso1):(Anda pista2 piso2):t) | piso1 == piso2 = juntaVerticalAnda ((Anda (pista1 ++ pista2) piso1):t)
                                                              | otherwise = (Anda pista1 piso1): juntaVerticalAnda ((Anda pista2 piso2):t)

-- | Filtra uma lista de instruções, ficando só com as instruções do tipo sobe.
filterSobe :: Instrucoes -- ^ Instruções iniciais.
              -> Instrucoes -- ^ Instruções apenas do tipo Sobe.
filterSobe [] = []
filterSobe ((Sobe pista piso h):t) = (Sobe pista piso h): (filterSobe t)
filterSobe ((_):t) = filterSobe t

-- | Verifica se duas instruções do tipo sobe são totalmente iguais, para depois as juntar com uma instrução do tipo repete.
juntaVerticalSobe :: Instrucoes -- ^ Instruções apenas do tipo Sobe.
                     -> Instrucoes -- ^ Junta instruções do tipo Sobe.
juntaVerticalSobe [] = []
juntaVerticalSobe [x] = [x]
juntaVerticalSobe ((Sobe pista1 piso1 h1):(Sobe pista2 piso2 h2):t) | piso1 == piso2 && h1 == h2 = juntaVerticalSobe ((Sobe (pista1 ++ pista2) piso1 h1):t)
                                                                    | otherwise = (Sobe pista1 piso1 h1): juntaVerticalSobe ((Sobe pista2 piso2 h2):t)

-- | Filtra uma lista de instruções, ficando só com as instruções do tipo desce.
filterDesce :: Instrucoes -- ^ Instruções iniciais.
               -> Instrucoes -- ^ Instruções apenas do tipo Desce.
filterDesce [] = []
filterDesce ((Desce pista piso h):t) = (Desce pista piso h): (filterDesce t)
filterDesce ((_):t) = filterDesce t

-- | Verifica se duas instruções do tipo desce são totalmente iguais, para depois as juntar com uma instrução do tipo repete.
juntaVerticalDesce :: Instrucoes -- ^ Instruções apenas do tipo Desce.
                      -> Instrucoes -- ^ Junta instruções do tipo Desce.
juntaVerticalDesce [] = []
juntaVerticalDesce [x] = [x]
juntaVerticalDesce ((Desce pista1 piso1 h1):(Desce pista2 piso2 h2):t) | piso1 == piso2 && h1 == h2 = juntaVerticalDesce ((Desce (pista1 ++ pista2) piso1 h1):t)
                                                                       | otherwise = (Desce pista1 piso1 h1): juntaVerticalDesce ((Desce pista2 piso2 h2):t)

-- | Compacta instruções iguais numa instrução do tipo repete.
toRepete :: Instrucoes -> Instrucoes
toRepete [] = []
toRepete (h:t) | vezes > 1 = (Repete vezes [peca]):toRepete e
               | otherwise = peca: toRepete e
  where (i,e) = case h of Anda pistas piso -> span (== Anda pistas piso) (h:t)
                          Sobe pistas piso altura -> span (== Sobe pistas piso altura) (h:t)
                          Desce pistas piso altura -> span (== Desce pistas piso altura) (h:t)
        vezes = length i
        peca = head i

-- * Funções auxiliares da Tarefa 3

getPiso :: Peca -> Piso
getPiso (Recta piso _) = piso
getPiso (Rampa piso _ _) = piso

getRor :: Peca -> Bool
getRor (Recta _ _) = True
getRor (Rampa _ _ _) = False

getPeca :: PosicaoMatriz -> Mapa -> Peca
getPeca (x,y) m = encontraPosicaoMatriz (x,y) m

getAltura :: Peca -> Int
getAltura (Recta _ a) = a

-- Esta tarefa utiliza várias estratégias para compactar as instruções, que depois são combinadas e comparadas, para obter a maior percentagem de compactação possível.

-- Estratégia 1 ->

-- Estratégia 2 -> Faz um mapa todo com a mesma peça e usa o teleporta para mudar as peças que não são iguais à colocada

-- Estratégia 3 -> 












