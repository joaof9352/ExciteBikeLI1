-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g059 where

import LI11920
import Tarefa0_2019li1g059
import Tarefa1_2019li1g059
import Tarefa2_2019li1g059
import Tarefa3_2019li1g059

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(2,(gera 3 10 1),(Jogador 0 7.2 1 3 (Ar 1 0 0))),
            (2,(gera 3 10 1),(Jogador 2 7.2 1 3 (Ar 1 0 0))),
            (1,(gera 3 10 1),(Jogador 1 1.2 0 3 (Morto 0.80))),
            (0.2,(gera 3 10 1),(Jogador 1 1.2 0 3 (Morto 0.80))),
            (2,(gera 3 10 1),(Jogador 0 7.2 1 3 (Chao True))),
            (2,(gera 3 10 1),(Jogador 0 7.2 1 3 (Ar 1 45 0))),
            (6,(gera 3 10 1),(Jogador 0 1.2 1 3 (Ar 1 90 0))),
            (1,(gera 1 15 1),(Jogador 0 13.5 10 4 (Ar 3 20 10))),
            (0.2,(gera 3 10 1),(Jogador 2 3.5 1.5 3 (Chao True))),
            (0.2,(gera 3 10 1),(Jogador 2 6.5 1.5 3 (Chao True))),
            (0.2,(gera 3 10 1),(Jogador 0 1.5 1.5 3 (Chao False))),
            (1, [[Recta Terra 0, Recta Terra 0, Rampa Terra 0 2, Recta Boost 2]],(Jogador 0 1.9 2.5 3 (Chao False))),
            (0.2,(gera 3 10 1),(Jogador 0 1.5 1.5 3 (Chao False))),
            (1, [[Recta Terra 0, Recta Cola 0]],(Jogador 0 1.2 0.5 3 (Chao False))),
            (1, [[Recta Terra 0, Recta Cola 0]],(Jogador 0 1.2 0.5 3 (Chao True))),
            (1,(gera 1 15 1),(Jogador 0 1.5 0.1 4 (Ar 0.1 20 10)))]

-- * Funções principais da Tarefa 4.

-- *** Variáveis Globais

-- | Retorna a resistência do Ar
resistenciaAr :: Double
resistenciaAr = 0.125

-- | Retorna a aceleração da gravidade
accelGravidade :: Double
accelGravidade = 1

-- ** Função Passo

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- ** Função Acelera

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t m jog | isChao estjog = aceleraChao t m jog
                | isAr estjog = aceleraAr t jog
                | otherwise = jog
    where estjog = estadoJogador jog

-- | Atualiza a velocidade de um 'Jogador' que esteja no Chão
aceleraChao :: Double -- ^ Tempo passado
               -> Mapa -- ^ Mapa
               -> Jogador -- ^ O estado anterior do 'Jogador'
               -> Jogador -- ^ O estado do 'Jogador' após acelerar
aceleraChao t mapa jog = upVel (max 0 (v + (accelMota - (toAtrito peca) * v) * t)) jog
    where accelMota = if (v < 2 && accelJogador) then 1 else 0
          peca = getPeca (pista, floor comp) mapa
          (Jogador pista comp v _ (Chao accelJogador)) = jog

-- | Atualiza a velocidade de um 'Jogador' que esteja no Ar
aceleraAr :: Double -- ^ Tempo passado
             -> Jogador -- ^ O estado anterior do 'Jogador'
             -> Jogador -- ^ O estado do 'Jogador' após acelerar
aceleraAr t jog = upG t (upVel (max 0 (v - (resistenciaAr * v * t))) jog)
    where v = velocidadeJogador jog

-- | Atualiza a velocidade de um determinado 'Jogador' para o valor recebido
upVel :: Double -- ^ Velocidade atualizada
         -> Jogador -- ^ O Estado anterior do 'Jogador'
         -> Jogador -- ^ O novo Estado do 'Jogador'
upVel v jog = jog {velocidadeJogador = v}

-- | Atualiza a gravidade de um determinado 'Jogador'
upG :: Double -- ^ Tempo passado
       -> Jogador -- ^ O estado anterior do 'Jogador'
       -> Jogador -- ^ O estado do 'Jogador' após acelerar
upG t jog = jog {estadoJogador = estnovo}
    where estj = estadoJogador jog
          g = (getGravidade jog) + accelGravidade * t
          estnovo = estj {gravidadeJogador = g}

-- ** Função Move

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move time mapa jog = case est of Morto _ -> (moveMorto time jog)
                                 Chao _ -> (moveChao time mapa jog)
                                 Ar _ _ _ -> (moveAr time mapa jog)
  where est = estadoJogador jog

-- | Altera a posição de 'Jogador', durante um determinado período de tempo, quando o estado inicial deste é Morto.
moveMorto :: Double  -- ^ O tempo decorrido.
          -> Jogador -- ^ O estado anterior do 'Jogador'.
          -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
moveMorto time jog | time < timeout = diminuiTimeout
                   | otherwise = acabaTimeout
  where est = estadoJogador jog -- Estado do Jogador.
        timeout = getTimeOut est -- Timeout inicial do Jogador.
        diminuiTimeout = jog {estadoJogador = Morto {timeoutJogador = (timeout - time)}} -- Timeout final do Jogador.
        acabaTimeout = jog {estadoJogador = Chao False} -- Acaba o timeout do jogador, e passa ao estado Chao False.

-- | Altera a posição de 'Jogador', durante um determinado período de tempo, quando o estado inicial deste é Chão.
moveChao :: Double  -- ^ O tempo decorrido.
         -> Mapa    -- ^ O mapa utilizado.
         -> Jogador -- ^ O estado anterior do 'Jogador'.
         -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
moveChao time mapa jog | distX + dist < (nrpeca + 1) = jog {distanciaJogador = (dist + distX)}
                       | incf >= inc0 = jog {distanciaJogador = (nrpeca + 1)}
                       | otherwise = Jogador npista (nrpeca+1) vel c (Ar h inc0 0)

  where Jogador npista dist vel c (Chao _) = jog
        distX = (time * vel) -- Distância movida pelo jogador durante o tempo decorrido.
        nrpeca = fromIntegral (floor dist) -- Número da peça na pista em que se encontra o jogador.
        pecaAtual = encontraPosicaoMatriz (npista,(floor nrpeca)) mapa -- Peça em que o jogador se encontra.
        pecaNext = encontraPosicaoMatriz (npista,(floor nrpeca) + 1) mapa -- Peça seguinte, na mesma pista, à peça que se encontra o jogador.
        inc0 = retornaInclinacao pecaAtual -- Inclinação da peça em que se encontra o Jogador.
        incf = retornaInclinacao pecaNext -- Inclinação da peça seguinte à que se encontra o jogador.
        h = retornaAltura 0 pecaNext -- Altura inicial da peça seguinte à que se encontra o jogador.

-- | Altera a posição de 'Jogador', durante um determinado período de tempo, quando o estado inicial deste é Ar.
moveAr :: Double  -- ^ O tempo decorrido.
       -> Mapa    -- ^ O mapa utilizado.
       -> Jogador -- ^ O estado anterior do 'Jogador'.
       -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
moveAr t mapa jog | intersetam (p0,pf) retaPeca && abs (incPeca - inc) >= 45 = Jogador p x 0.0 c (Morto 1.0)
                  | intersetam (p0,pf) retaPeca && isReta pecaAtual = Jogador p x (max 0 velX) c (Chao False)
                  | intersetam (p0,pf) retaPeca = Jogador p x (max 0 newVel) c (Chao False)
                  | fromIntegral (floor ((velX*t)+dist)) < posicaoSeguinte = Jogador p (dist + distX) vel c (Ar (altura + distY) inc g) -- tem bug no (velX*t), tem que se somar a dist (!!)
                  | otherwise = Jogador p posicaoSeguinte vel c (Ar (altura + dY) inc g) -- Não é altura + distY, temos de calcular a distY no x em que ele para.

  where (Jogador p dist vel c (Ar altura inc g)) = jog
        (velX,_) = parVelocidade inc vel
        (distX,distY) = posicaoJogador t inc g vel
        vetorPf = Cartesiano distX distY
        p0 = Cartesiano dist altura
        pf = somaVetores p0 vetorPf
        posicaoSeguinte = (fromIntegral (floor dist)) + 1
        retaPeca = pecaToReta (pecaAtual, fromIntegral (floor dist))
        pecaAtual = encontraPosicaoMatriz (p, (floor dist)) mapa
        incPeca = retornaInclinacao pecaAtual
        Cartesiano x _ = intersecao (p0,pf) retaPeca
        newVel = vel * (cos (graus2rad $ inc - incPeca))
        -- Para o ultimocaso
        dX = fromIntegral (floor dist + 1) - dist
        dY = (dX*distY)/distX

-- * Funções Auxiliares da Tarefa 4

-- ** Funções Auxiliares para a função Acelera

-- | Retorna a gravidade do jogador
getGravidade :: Jogador -> Double
getGravidade jog = g
    where Ar _ _ g = estadoJogador jog

-- | Recebe uma peça e dá o atrito conforme o seu piso.
toAtrito :: Peca -> Double
toAtrito peca | piso == Terra = 0.25
              | piso == Relva = 0.75
              | piso == Lama = 1.50
              | piso == Boost = (-0.50)
              | piso == Cola = 3.00
      where piso = getPiso peca

-- ** Funções Auxiliares para a função Move

-- | Dado um jogador no Estado Morto, devolve o timeout que lhe resta.
getTimeOut :: EstadoJogador -> Double
getTimeOut (Morto timeout) = timeout

-- | Dada uma peça, devolve o segmento de Reta correspondente.
pecaToReta :: (Peca,Double) -> Reta
pecaToReta ((Recta _ h),nrpeca) = ((Cartesiano nrpeca (fromIntegral h)),(Cartesiano (nrpeca + 1) (fromIntegral h)))
pecaToReta ((Rampa _ h0 hf),nrpeca) = ((Cartesiano nrpeca (fromIntegral h0)),(Cartesiano (nrpeca + 1) (fromIntegral hf)))

-- | Retorna a distância percorrida em X e a distância percorrida em Y num determinado tempo
posicaoJogador :: Double -- ^ Tempo
                  -> Double -- ^ A inclinação
                  -> Double -- ^ A gravidade
                  -> Double -- ^ A velocidade
                  -> (Double, Double) -- ^ (Distancia percorrida em x, distancia percorrida em y) durante o tempo
posicaoJogador t inc g v = (velX * t,(velY-g)*t)
  where (velX,velY) = parVelocidade inc v

-- | Dada uma velocidade e uma inclinação, retorna a velocidade em X e a velocidade em Y num par ordenado
parVelocidade :: Double -- ^ A inclinação
              -> Double -- ^ A velocidade
              -> (Double, Double) -- ^ (velX,velY) final
parVelocidade inc vel = (vel * (cos (graus2rad inc)),vel * (sin (graus2rad inc)))

-- | Se for reta, retorna True.
isReta :: Peca -> Bool
isReta (Recta _ _) = True
isReta _ = False










