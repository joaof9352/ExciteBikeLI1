-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g059 where

import LI11920
import Tarefa0_2019li1g059
import Tarefa1_2019li1g059
-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 =  [(0, Movimenta B, Estado (gera 2 6 2) [(Jogador 1 1.4 1.5 4 (Chao True))])
            ,(0, Movimenta B, Estado (gera 2 6 2) [(Jogador 0 1.4 1.5 4 (Chao True))])
            ,(0, Movimenta B, Estado (gera 2 6 2) [(Jogador 0 1.1 1.5 4 (Chao True))])
            ,(0, Movimenta C, Estado (gera 2 6 2) [(Jogador 1 0.5 1.5 4 (Chao True))])
            ,(0, Movimenta B, Estado (gera 2 6 2) [(Jogador 0 0.5 1.5 4 (Chao True))])
            ,(0, Movimenta C, Estado (gera 2 6 2) [(Jogador 1 5.4 1.5 4 (Chao True))])
            ,(0, Movimenta C, Estado (gera 2 6 2) [(Jogador 1 4.4 1.5 4 (Chao True))])
            ,(0, Movimenta C, Estado (gera 2 6 2) [(Jogador 0 5.4 1.5 4 (Chao True))])
            ,(0, Movimenta B, Estado (gera 3 6 2) [(Jogador 1 5.4 1.5 4 (Chao True))])
            ,(0, Movimenta B, Estado (gera 3 6 2) [(Jogador 1 1.5 1.5 4 (Chao True))])
            ,(0, Movimenta C, Estado (gera 3 6 2) [(Jogador 2 4.5 1.5 4 (Chao True))])
            ,(0, Movimenta C, Estado (gera 2 6 2) [(Jogador 1 5.4 1.5 4 (Ar 12 15 0.0))])
            ,(0, Dispara, Estado (gera 2 6 2) [(Jogador 1 1.4 1.5 1 (Chao True))])
            ,(0, Dispara, Estado (gera 2 6 2) [(Jogador 1 1.4 1.5 1 (Ar 4 15 0))])
            ,(0, Dispara, Estado (gera 2 6 2) [(Jogador 1 1.4 1.5 0 (Chao True))])
            ,(0, Dispara, Estado (gera 2 6 2) [(Jogador 1 1.4 1.5 1 (Chao True))])
            ,(0, Dispara, Estado (gera 2 6 2) [(Jogador 1 2.4 1.5 1 (Chao True))])
            ,(0, Dispara, Estado [[Recta Terra 0, Recta Cola 0, Recta Terra 0]] [(Jogador 0 2.2 1.5 5 (Chao True))])
            ,(0, Movimenta E, Estado (gera 2 6 2) [(Jogador 1 0.4 1.5 1 (Ar 3 78 0.0))])
            ,(0, Movimenta D, Estado (gera 2 6 2) [(Jogador 1 0.4 1.5 1 (Ar 3 78 0.0))])
            ,(0, Movimenta E, Estado (gera 2 6 2) [(Jogador 1 0.4 1.5 1 (Ar 3 74 0.0))])
            ,(0, Movimenta E, Estado (gera 2 6 2) [(Jogador 1 0.4 1.5 1 (Chao True))])
            ,(0, Movimenta D, Estado (gera 2 6 2) [(Jogador 1 0.4 1.5 1 (Ar 3 (-76) 0.0))])
            ,(0, Acelera, Estado (gera 2 6 2) [(Jogador 1 0.4 1.5 1 (Ar 3 (-76) 0.0))])
            ,(0, Desacelera, Estado (gera 2 6 2) [(Jogador 1 0.4 1.5 1 (Chao True))])
            ,(0, Acelera, Estado (gera 2 6 2) [(Jogador 1 0.4 1.5 1 (Chao True))])
            ,(0, Movimenta E, Estado (gera 2 6 2) [(Jogador 1 0.4 1.5 1 (Morto 1.0))])
            ]

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada ind b est | b == Dispara = dispara ind est
                 | b == Movimenta E || b == Movimenta D = endireitar ind est b
                 | b == Acelera || b == Desacelera = acelerar ind est b
                 | b == Movimenta C = mudarPistaCima ind est
                 | b == Movimenta B = mudarPistaBaixo ind est

-- | Realiza a "triagem" para o 'Movimenta C'
--
-- Faz a diferença de alturas entre as pistas, verifica se está no chão e se não está na primeira pista e dá essa indicação ao 'moverParaPista' ou ao 'jogadorMorto'.

mudarPistaCima :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
                  -> Estado -- ^ O 'Estado' anterior.
                  -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
mudarPistaCima ind est | chao && podeCima && abs (difPistaCima) <= 0.2 = moverParaPista est ind 3
                       | chao && podeCima && difPistaCima < (-0.2) = jogadorMorto est ind
                       | chao && podeCima && difPistaCima > 0.2 = moverParaPista est ind 4
                       | otherwise = est
        where chao = isChao (estadoJogador jogador)
              jogador = encontraIndiceLista ind (jogadoresEstado est)
              podeCima = movimentoPossivelCima pistaatual
              pistaatual = pistaJogador jogador
              difPistaCima = hpecaatual - hpecacima
              distNaPeca = (dist - fromIntegral (floor dist))
              hpecaatual = retornaAltura distNaPeca (retornaPeca ind est pistaatual)
              hpecacima = retornaAltura distNaPeca (retornaPeca ind est (pistaatual-1))
              dist = distanciaJogador jogador

-- | Realiza a "triagem" para o 'Movimenta B'
--
-- Faz a diferença de alturas entre as pistas, verifica se está no chão e se não está na última pista e dá essa indicação ao 'moverParaPista' ou ao 'jogadorMorto'.
mudarPistaBaixo :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
                   -> Estado -- ^ O 'Estado' anterior.
                   -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
mudarPistaBaixo ind est | chao && podeBaixo && abs (difPistaBaixo) <= 0.2 = moverParaPista est ind 1
                        | chao && podeBaixo && difPistaBaixo < (-0.2) = jogadorMorto est ind
                        | chao && podeBaixo && difPistaBaixo > 0.2 = moverParaPista est ind 2
                        | otherwise = est
        where chao = isChao (estadoJogador jogador)
              jogador = encontraIndiceLista ind (jogadoresEstado est)
              podeBaixo = movimentoPossivelBaixo pistaatual pistamax
              pistaatual = pistaJogador jogador
              pistamax = (fst (dimensaoMatriz (mapaEstado est)) - 1)
              difPistaBaixo = hPecaAtual - hPecaBaixo
              distNaPeca = (dist - fromIntegral (floor dist))
              hPecaAtual = retornaAltura distNaPeca (retornaPeca ind est pistaatual)
              hPecaBaixo = retornaAltura distNaPeca (retornaPeca ind est (pistaatual+1))
              dist = distanciaJogador jogador

-- | Muda o estado dos jogadores conforme o último Int indicar

moverParaPista :: Estado -- ^ O 'Estado' anterior.
                  -> Int -- ^ O identificador do 'Jogador' que efetua a jogada.
                  -> Int -- ^ Identificador da "jogada" que provém da 'mudarPistaBaixo' e da 'mudarPistaCima'
                  -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada
moverParaPista est ind a | a == 1 = est {jogadoresEstado = atualizaIndiceLista ind (jogador {pistaJogador = pistafb}) estjogador}
                         | a == 2 = est {jogadoresEstado = atualizaIndiceLista ind (jogador {pistaJogador = pistafb, estadoJogador = estjogadorAr}) estjogador}
                         | a == 3 = est {jogadoresEstado = atualizaIndiceLista ind (jogador {pistaJogador = pistafc}) estjogador}
                         | a == 4 = est {jogadoresEstado = atualizaIndiceLista ind (jogador {pistaJogador = pistafc, estadoJogador = estjogadorAr}) estjogador}
    where estjogador = jogadoresEstado est
          jogador = encontraIndiceLista ind estjogador
          pistaatual = pistaJogador jogador
          estjogadorAr = Ar hpecaatual inclinacao 0
          hpecaatual = retornaAltura (dist - fromIntegral (floor dist)) (retornaPeca ind est pistaatual)
          dist = distanciaJogador jogador
          inclinacao = retornaInclinacao $ retornaPeca ind est pistaatual
          pistafb = pistaatual + 1
          pistafc = pistaatual - 1

-- | Muda o estado do jogador para Morto 1.0
jogadorMorto :: Estado -- ^ O 'Estado' anterior
                -> Int -- ^ O identificador do 'Jogador'
                -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada
jogadorMorto est ind = est {jogadoresEstado = atualizaIndiceLista ind (jogador {velocidadeJogador = 0.0,estadoJogador = Morto 1.0}) estjogador}
  where estjogador = jogadoresEstado est
        jogador = encontraIndiceLista ind estjogador

-- | Efetua as 'Jogadas' 'Movimenta D' e 'Movimenta E'
endireitar :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
           -> Estado -- ^ O 'Estado' anterior do jogo
           -> Jogada -- ^ A 'Jogada' a efetuar
           -> Estado -- ^ O 'Estado' Resultante
endireitar ind est jogada | isAr ar && inc >= (-75) && jogada == (Movimenta D) = estadoD
                          | isAr ar && inc <= 75 && jogada == (Movimenta E) = estadoE
                          | isAr ar && inc > 75 && jogada == (Movimenta E) = estado90
                          | isAr ar && inc < (-75) && jogada == (Movimenta D) = estado90n
                          | otherwise = est
      where jogador = encontraIndiceLista ind estjogador
            ar = estadoJogador jogador
            inc = getinclinacao ar
            estjogador = jogadoresEstado est
            mudar1 = (jogador {estadoJogador = ar { inclinacaoJogador = (inc-15)}})
            mudar2 = (jogador {estadoJogador = ar { inclinacaoJogador = (inc+15)}})
            estadoD = est {jogadoresEstado = atualizaIndiceLista ind mudar1 estjogador}
            estadoE = est {jogadoresEstado = atualizaIndiceLista ind mudar2 estjogador}
            estado90 = est {jogadoresEstado = atualizaIndiceLista ind (jogador { estadoJogador = ar { inclinacaoJogador = 90}}) estjogador}
            estado90n = est {jogadoresEstado = atualizaIndiceLista ind (jogador { estadoJogador = ar { inclinacaoJogador = -90}}) estjogador}

-- | Efetua as 'Jogadas' 'Acelera' e 'Desacelera'
acelerar :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
         -> Estado -- ^ O 'Estado' anterior do jogo
         -> Jogada -- ^ A 'Jogada' a efetuar
         -> Estado -- ^ O 'Estado' Resultante
acelerar ind est jogada | isChao chao = case jogada of Acelera    -> estadoAcelera
                                                       Desacelera -> estadoDesacelera
                        | otherwise = est
        where jogador = encontraIndiceLista ind estjogador
              chao = estadoJogador jogador
              estjogador = jogadoresEstado est
              aceleraTrue = (jogador {estadoJogador = Chao { aceleraJogador = True}})
              aceleraFalse = (jogador {estadoJogador = Chao { aceleraJogador = False}})
              estadoAcelera = est {jogadoresEstado = atualizaIndiceLista ind aceleraTrue estjogador}
              estadoDesacelera = est {jogadoresEstado = atualizaIndiceLista ind aceleraFalse estjogador}

-- | Efetua a 'Jogada' 'Dispara'
dispara :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
        -> Estado -- ^ O 'Estado' anterior do jogo
        -> Estado -- ^ O 'Estado' Resultante
dispara ind est | dist >= 1 && carga > 0 && isChao chao = estado
                | otherwise = est

    where
          joga = encontraIndiceLista ind estjogador
          (Jogador pista distancia _ carga chao) = joga
          mapa = mapaEstado est
          dist = floor (distancia)
          peca = encontraPosicaoMatriz (pista,(dist-1)) mapa
          estjogador = jogadoresEstado est
          estado = Estado mapaatualizado jogadoratualizado
          mapaatualizado = (atualizaPosicaoMatriz (pista,(dist-1)) (toCola peca) mapa)
          jogadoratualizado = (atualizaIndiceLista ind (joga {colaJogador = carga-1}) estjogador)

-- * Funções auxiliares da Tarefa 2.

-- | Calcula a inclinação de uma determinada peça
retornaInclinacao :: Peca -- ^ Peça da qual queremos obter a Inclinação
                  -> Double -- ^ Inclinação da Peça
retornaInclinacao (Recta _ _) = 0.0
retornaInclinacao (Rampa _ a b) = radianos2graus (atan $ (fromIntegral (b - a)))

-- | Determina se é possível o 'Movimenta B'
movimentoPossivelBaixo :: Int -- ^ Número da pista atual
                       -> Int -- ^ Número de pistas no mapa
                       -> Bool
movimentoPossivelBaixo pistaat pistamax = pistaat < pistamax

-- | Determina se é possível o Movimenta C
movimentoPossivelCima :: Int  -- ^ Número da pista atual
                      -> Bool
movimentoPossivelCima pistaat = pistaat > 0

-- | Calcula a altura a que está o jogador num determinado momento
retornaAltura :: Double -- ^ Distância percorrida pelo jogador na peça
              -> Peca -- ^ Peça em que o jogador se encontra
              -> Double -- ^ Altura a que está o jogador do jogador
retornaAltura _ (Recta _ x) = fromIntegral x
retornaAltura dist (Rampa _ a b) = ((fromIntegral a) + (dist * (fromIntegral $ b-a)))

-- | Retorna a peça na posição atual do 'Jogador'
retornaPeca :: Int -- ^ O identificador do 'Jogador'
            -> Estado -- ^ O 'Estado' atual do jogo
            -> Int -- ^ Número da Pista
            -> Peca -- ^ Peça atual em que se encontra o jogador
retornaPeca ind est pista = encontraPosicaoMatriz (pista,bloco0) mapa
  where mapa = mapaEstado est
        estjogador = jogadoresEstado est
        jogador = encontraIndiceLista ind estjogador
        dist = distanciaJogador jogador
        bloco0 = floor dist

-- | Dada uma peça, transforma o Piso dessa peça em cola
toCola :: Peca -> Peca
toCola (Recta _ b) = Recta Cola b
toCola (Rampa _ b c) = Rampa Cola b c

-- | Verifica se o jogador está no chão
isChao :: EstadoJogador -> Bool
isChao (Chao _) = True
isChao _ = False

-- | Verifica se o jogador está no ar
isAr :: EstadoJogador -> Bool
isAr (Ar _ _ _) = True
isAr _ = False

-- | Retorna a inclinação do Jogador
getinclinacao :: EstadoJogador -> Double
getinclinacao (Ar _ inc _) = inc