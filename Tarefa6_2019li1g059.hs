{- |

= Introdução

Esta tarefa consistiu na criação de um ro'bot'. 
Esta tarefa foi talvez a mais complicada, no sentido em que nos obrigou a pensar em muitas variáveis e possibilidades 
para o comportamento do bot.

= Objetivos

Nesta tarefa, o nosso objetivo foi construir um bot que conseguisse simular o comportamento humano.
Para cumprir esse objetivo, definimos várias funções para o bot saber o que fazer. A ordem pela qual estas funções 
aparecem na função 'bot' define as prioridades do mesmo. Definimos essas prioridades pela seguinte ordem: 
* Se estiver no começo do mapa, a 1 bloco do fim ou com velocidade igual a 0, ele acelera.
* Se estiver no Ar, ele aproxima a inclinação da inclinação da peça com que se vai intercetar.
* Se estiver numa peça com Cola, o 'bot' move-se para a pista de cima ou para a de baixo.
* Se tiver uma peça com piso Boost no conjunto das peças seguintes ele move-se para lá. 
* Se tiver uma peça com piso Terra no conjunto das peças seguintes ele move-se para lá.
* Se tiver uma peça com piso Relva no conjunto das peças seguintes ele move-se para lá. 
* Se tiver uma peça com piso Lama no conjunto das peças seguintes ele move-se para lá. 
Se o bot não conseguir fazer nenhuma destas coisas, apenas acelera.

= Discussão e conclusão

Pensamos que, apesar de certas limitações que o nosso bot tem, já consegue vencer jogadores humanos. Foi uma das nossas tarefas
favoritas (juntamenta com a Tarefa 3) por nos ter dado "liberdade" e por nos obrigar a pensar "fora da caixa".
Esta tarefa foi um grande desafio, mas pensamos que a concluímos com bastante sucesso.

-}

module Tarefa6_2019li1g059 where

import Tarefa0_2019li1g059
import Tarefa1_2019li1g059
import Tarefa2_2019li1g059
import Tarefa3_2019li1g059
import Tarefa4_2019li1g059
import LI11920
import Data.List

-- * Funções principais da Tarefa 6.           

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot ind est | dist == 0 || dist >= (fromIntegral $ length (head mapa) - 1) = Just Acelera
            | v == 0 = Just Acelera
            | isAr estJ = moveInclinacao mapa jog
            | isChao estJ = fugirCola mapa jog
            | otherwise = Just Acelera
    where mapa = mapaEstado est
          jogadorEst = jogadoresEstado est
          jog = encontraIndiceLista ind jogadorEst
          estJ = estadoJogador jog
          (Jogador _ dist v _ _) = jog

-- ** Funções para quando o bot está no ar

-- *** Função principal

-- | Muda a inclinação de um jogador que está no ar, deixando-o com o minimo possível de inclinação em relação à peça com que se vai intersetar
moveInclinacao :: Mapa            -- ^ Mapa do jogo.
                  -> Jogador      -- ^ O ro'bot'.
                  -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
moveInclinacao mapa jog | intersetam (p0,pf) retaPeca && abs (incP - inc) <= 8 = Nothing
                        | intersetam (p0,pf) retaPeca = inclinacaoCerta inc incP
                        | otherwise = aproximaInclinacao0 inc
        where (Jogador p dist vel _ (Ar h inc g)) = jog
              -- Determinar se se intersetam
              (velX,velY) = parVelocidade inc vel
              vetorVel = Cartesiano velX velY
              vetorG = Cartesiano 0 (-g)
              vetorRes = somaVetores vetorVel vetorG
              p0 = Cartesiano dist h
              pf = somaVetores p0 vetorRes
              sad = fromIntegral (floor (dist))
              retaPeca = ((Cartesiano sad (retornaAltura 0 pecaAtual)),(Cartesiano (sad+1) (retornaAltura 1 pecaAtual)))
              -- Ver onde a inclinacao da peca onde se intersetam
              pecaAtual = encontraPosicaoMatriz (p, fromIntegral (floor dist)) mapa
              incP = retornaInclinacao pecaAtual

-- *** Funções auxiliares

-- | Função auxiliar, que recebe a inclinação do jogador e a inclinação da peça com que se vai intersetar e faz aproxima-as através de uma jogada.
inclinacaoCerta :: Double -- ^ Inclinação do ro'bot'.
                   -> Double -- ^ Inclinação da peça com que se vai intersetar.
                   -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
inclinacaoCerta incJ incP | incJ > incP = Just (Movimenta D)
                          | otherwise = Just (Movimenta E)

-- | Função auxiliar, que recebe a inclinação do jogador e a aproxima do valor 0
aproximaInclinacao0 :: Double -- ^ Inclinação do ro'bot'.
                       -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
aproximaInclinacao0 incJ | incJ >= 15 = Just (Movimenta D)
                         | otherwise = Just (Movimenta E)


-- ** Funções para quando o bot está no ar

-- *** Funções principais

-- | Se a peça atual tiver piso Cola, move-se para cima ou para baixo. Caso não tenha, chama a searchBoost
fugirCola :: Mapa -> Jogador -> Maybe Jogada
fugirCola mapa jog | tipoPecaAtual == Cola && (difPistaCima) >= (-0.2) = Just (Movimenta C)
                   | tipoPecaAtual == Cola && (difPistaBaixo) >= (-0.2) = Just (Movimenta B)
                   | otherwise = searchBoost mapa jog
        where pecaAtual = encontraPosicaoMatriz (p,floor dist) mapa
              tipoPecaAtual = getPiso pecaAtual
              Jogador p dist _ c (Chao _) = jog
              --Diferença para a pistaCima pistaBaixo
              (difPistaCima,difPistaBaixo) = difPistas mapa jog

-- | Procura peças com piso Boost nas peças seguintes do jogador. Caso não existam ou não seja possível mover-se para lá, chama a procTerra.
searchBoost :: Mapa -- ^ Mapa do jogo 
               -> Jogador -- ^ O ro'bot'
               -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
searchBoost mapa jog | boostNaPecaSeguinte && (head mov == Just (Movimenta C)) && (difPistaCima) >= (-0.2) = (head mov)
                     | boostNaPecaSeguinte && (head mov == Just (Movimenta B)) && (difPistaBaixo) >= (-0.2) = (head mov)
                     | boostNaPecaSeguinte = Just Acelera
              --       | not boostNaPecaSeguinte = if getPiso (pecaAnterior) == Boost && c > 0 then Just Dispara else procTerra mapa jog -- Procura por 
                     | otherwise = procTerra mapa jog
    where comp = snd $ dimensaoMatriz mapa -- (pistas,comprimento)
          Jogador p dist _ c (Chao _) = jog
          pecaAtual = encontraPosicaoMatriz (p,floor dist) mapa
          tipoPecaAtual = getPiso pecaAtual
          mov = rotaTipo mapa jog Boost
          boostNaPecaSeguinte = temPisoASeguir Boost colunaTipos
          colunaTipos = listaPisos $ listaPecasSeguintes mapa jog
          pecaAnterior = encontraPosicaoMatriz (p,floor dist - 1) mapa
          --Diferença pista cima 
          (difPistaCima,difPistaBaixo) = difPistas mapa jog

-- | Procura peças com piso Terra nas peças seguintes do jogador. Caso não existam ou não seja possível mover-se para lá, chama a procRelva.
procTerra :: Mapa -- ^ Mapa do jogo 
             -> Jogador -- ^ O ro'bot'
             -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
procTerra mapa jog | checkTerra && (head movTerra == Just (Movimenta C)) && (difPistaCima) >= (-0.2) = head movTerra
                   | checkTerra && (head movTerra == Just (Movimenta B)) && (difPistaBaixo) >= (-0.2) = head movTerra  
                   | checkTerra = Just Acelera                        
                   | otherwise = procRelva mapa jog
  where Jogador p dist _ c (Chao _) = jog
        pecaAtual = encontraPosicaoMatriz (p,floor dist) mapa
        tipoPecaAtual = getPiso pecaAtual
        movTerra = rotaTipo mapa jog Terra
        checkTerra = temPisoASeguir Terra colunaPisos
        colunaPisos = (listaPisos $ listaPecasSeguintes mapa jog)
        --Diferença pista cima 
        (difPistaCima,difPistaBaixo) = difPistas mapa jog


-- | Procura peças com piso Relva nas peças seguintes do jogador. Caso não existam ou não seja possível mover-se para lá, chama a procLama.
procRelva :: Mapa -- ^ Mapa do jogo 
          -> Jogador -- ^ O ro'bot'
          -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
procRelva mapa jog | checkRelva && (head movRelva == Just (Movimenta C)) && (difPistaCima) >= (-0.2) = head movRelva
                   | checkRelva && (head movRelva == Just (Movimenta B)) && (difPistaBaixo) >= (-0.2) = head movRelva
                   | checkRelva = Just Acelera
                   | otherwise = procLama mapa jog
  where Jogador p dist _ c (Chao _) = jog
        movRelva = rotaTipo mapa jog Relva
        checkRelva = temPisoASeguir Relva colunaPisos
        colunaPisos = (listaPisos $ listaPecasSeguintes mapa jog)
        --Diferença pista cima 
        (difPistaCima,difPistaBaixo) = difPistas mapa jog

-- | Procura peças com piso Lama nas peças seguintes do jogador. Caso não existam ou não seja possível mover-se para lá, acelera.
procLama :: Mapa -- ^ Mapa do jogo 
            -> Jogador -- ^ O ro'bot'
            -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
procLama mapa jog | checkLama && (head movLama == Just (Movimenta C)) && (difPistaCima) >= (-0.2) = head movLama
                  | checkLama && (head movLama == Just (Movimenta B)) && (difPistaBaixo) >= (-0.2) = head movLama
                  | otherwise = Just Acelera
  where Jogador p dist _ c (Chao _) = jog
        movLama = rotaTipo mapa jog Lama
        checkLama = temPisoASeguir Lama colunaPisos
        colunaPisos = (listaPisos $ listaPecasSeguintes mapa jog)
        --Diferença pista cima 
        (difPistaCima,difPistaBaixo) = difPistas mapa jog

-- *** Funções auxiliares

-- | Retorna um par com a diferença para a pista de cima e para a pista de baixo, respetivamente
difPistas :: Mapa 
             -> Jogador 
             -> (Double,Double)
difPistas mapa jog = (difPistaCima,difPistaBaixo)
  where Jogador p dist _ c (Chao _) = jog
        pecaAtual = encontraPosicaoMatriz (p,floor dist) mapa
        pecaCima = encontraPosicaoMatriz (p-1,floor dist) mapa
        pecaBaixo = encontraPosicaoMatriz (p+1,floor dist) mapa
        difPistaCima = hpecaatual - hpecacima
        difPistaBaixo = hpecaatual - hpecaBaixo
        hpecaatual = retornaAltura distNaPeca pecaAtual
        hpecacima = retornaAltura distNaPeca pecaCima
        hpecaBaixo = retornaAltura distNaPeca pecaBaixo
        distNaPeca = (dist - fromIntegral (floor dist))

-- | Procura o tipo definido nas peças seguintes e dá a lista de movimentos até lá. Caso estejam na mesma pista, retorna 'Nothing' dentro da lista.
rotaTipo :: Mapa 
            -> Jogador 
            -> Piso 
            -> [Maybe Jogada]
rotaTipo mapa jog piso = mov
  where mapaT = transpose mapa
        Jogador p dist _ c (Chao _) = jog
        listaTipos = listaPisos (listaPecasSeguintes mapa jog)
        posicaoPiso = elemIndices piso listaTipos
        listaRotas = calcularRotas p (posicaoPiso) -- Lista para todas as rotas para os sítios onde há Boosts
        posicaoMenorCaminho = pMenor (map length listaRotas) -- Diz-nos a posição na listaRotas do Boost mais perto
        mov = encontraIndiceLista (posicaoMenorCaminho) listaRotas   -- retorna a lista de movs para chegar ao Boost

-- | Transforma uma lista de peças na lista de pisos dessas peças
listaPisos :: Pista -> [Piso]
listaPisos p = map (getPiso) p

-- | Retorna a lista das peças que se encontram à frente do jogador, i.e., estando o jogador à distância 3.5, retorna as peças nº 5 de todas as pistas.
listaPecasSeguintes :: Mapa -- ^ Mapa do jogo. 
                       -> Jogador -- ^ O ro'bot'.
                       -> Pista -- ^ Lista das peças que se encontram à frente do jogador.
listaPecasSeguintes mapa jog = retorna (floor dist + 1) (transpose mapa)
  where Jogador p dist _ c (Chao _) = jog

-- | Verifica se o Piso dado é elemento da lista de Pisos
temPisoASeguir :: Piso 
                  -> [Piso] 
                  -> Bool
temPisoASeguir p h = elem p h

-- | Dado um mapa, retorna a pista correspondente ao Int dado.
retorna :: Int -- ^ Número da pista 
           -> Mapa -- ^ Mapa do jogo
           -> Pista -- ^ Pista correspondente ao número dado
retorna _ [] = []
retorna a (h:t) | a == 0 = h
                | otherwise = retorna (a-1) t

-- | Dá a lista de rotas para todas as possíveis pistas de destino
calcularRotas :: Int -- ^ Pista atual
                 -> [Int] -- ^ Possíveis pistas de destino
                 -> [[Maybe Jogada]] -- ^ Possíveis listas de movimentos.
calcularRotas _ [] = []
calcularRotas p0 (h:t) = (calcularRota p0 h):(calcularRotas p0 t)

-- | Retorna a rota para a pista de destino.
calcularRota :: Int -- ^ Pista atual
                -> Int -- ^ Pista de destino
                -> [Maybe Jogada] -- ^ Lista de movimentos.
calcularRota p0 pf | p0 == pf = []
                   | p0 < pf = [Just (Movimenta B)] ++ calcularRota (p0+1) pf
                   | p0 > pf  = [Just (Movimenta C)] ++ calcularRota (p0-1) pf

-- | Dada uma lista, retorna a posição do menor elemento.
pMenor :: Ord a => [a] -> Int
pMenor (h:t) = aux5 (h,0) 1 t 
pMenor _ = (-1)

-- | Função auxiliar da pMenor
aux5 :: Ord a => (a,Int) -> Int -> [a] -> Int
aux5 (m,pm) p (h:t) | m > h = aux5 (h,p) (p+1) t
                    | otherwise = aux5 (m,pm) (p+1) t
aux5 (_,pm) _ [] = pm