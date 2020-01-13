-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2019li1g059 where

import LI11920
import System.Random
import Tarefa0_2019li1g059

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(1,1,1),(2,3,2),(1,2,1),(3,1,23),(1,4,1),(10,2,7),(0,1,0),(2,5,1),(1,16,198),(3,12,12),(4,15,2),(4,8,12),(1,11,13)]

-- * Funções pré-definidas da Tarefa 1.

geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.

-- | Gera um mapa
gera :: Int -- ^ Número de Pistas
        -> Int -- ^ Comprimento das pistas
        -> Int -- ^ Semente do mapa
        -> Mapa -- ^ Mapa
gera npistas comp seed = toMapa (0,1) mapa (geraAleatorios (2*(comp-1)*npistas) seed)
              where mapa = preMapa npistas comp

-- | Fornece as informações sobre que posição atualizar, com que peça.
toMapa :: PosicaoMatriz -- ^ Posição a atualizar.
          -> Mapa -- ^ Mapa atualizado até à posição anterior.
          -> [Int] -- ^ Inteiros que caracterizam os tipos de pisos e de peças.
          -> Mapa -- ^ Mapa final
toMapa (l,c) mapa (tpiso:ror:resto) | valida = toMapa (l,(c+1)) newmapa resto
                                    | otherwise = toMapa ((l+1),1) mapa (tpiso:ror:resto)
            where valida = ePosicaoMatrizValida (l,c) mapa
                  newmapa = atualizaPecaMapa (l,c) mapa tpiso ror
toMapa _ mapa _ = mapa

-- | Atualiza o mapa, colocando uma dada peça na posição correspondente.
atualizaPecaMapa :: PosicaoMatriz -- ^ Posição a atualizar.
                    -> Mapa -- ^ Mapa atualizado até à posição anterior.
                    -> Int -- ^ Inteiro que caracteriza o tipo de piso.
                    -> Int -- ^ Inteiro que caracteriza o tipo de peça.
                    -> Mapa -- ^ Mapa atualizado.
atualizaPecaMapa (l,c) mapa tpiso ror = atualizaPosicaoMatriz (l,c) peca mapa
            where peca = toPeca (l,c) mapa tpiso ror

-- | Transforma um Par de Inteiros numa Peça.
toPeca :: PosicaoMatriz -- ^ Posição atual.
          -> Mapa -- ^ Mapa atualizado até à posição anterior.
          -> Int -- ^ Inteiro que caracteriza o tipo de piso.
          -> Int -- ^ Inteiro que caracteriza o tipo de peça.
          -> Peca -- ^ Peça resultante.
toPeca (l,c) mapa tpiso ror | ror <= 1 = Rampa piso h0 hfa
                            | ror <= 5 && h0 /= 0 && hfd <= 0 = Rampa piso h0 0
                            | ror <= 5 && h0 /= 0 = Rampa piso h0 hfd
                            | ror <= 5 && h0 == 0 = Recta piso h0
                            | ror <= 9 = Recta piso h0
            where h0 = altura mapa (l,(c-1))          -- Altura inicial da peça atual (Altura final da peça anterior).
                  hfa = (h0+ror+1)                    -- Altura final da peca (rampa ascendente) atual.
                  hfd = (h0-ror+1)                    -- Altura final da peca (rampa descendente) atual.
                  piso = toPiso (l,c) mapa tpiso      -- Piso atual.

-- | Transforma um Inteiro num tipo de Piso.
toPiso :: PosicaoMatriz -- ^ Posição da peça atual.
          -> Mapa -- ^ Mapa atualizado até à posição anterior.
          -> Int -- ^ Inteiro que caracteriza o tipo de piso.
          -> Piso -- ^ Piso formado.
toPiso (l,c) mapa tpiso | tpiso <= 1 = Terra
                        | tpiso <= 3 = Relva
                        | tpiso == 4 = Lama
                        | tpiso == 5 = Boost
                        | tpiso <= 9 = case peca of Recta piso _ -> piso
                                                    Rampa piso _ _  -> piso
                     where peca = encontraPosicaoMatriz (l,(c-1)) mapa -- Peça anterior à peça atual.

-- * Funções auxiliares da Tarefa 1.

-- | Cria um "preMapa" com as dimensões corretas, mas formado apenas por Peças Recta Terra 0.
preMapa :: Int -- ^ Número de pistas do Mapa.
           -> Int -- ^ Comprimento das pistas.
           -> Mapa -- ^ O Mapa resultante.
preMapa npistas comp = replicate npistas (replicate comp (Recta Terra 0))

-- | Indica a altura final de uma peça, numa dada posição.
altura :: Mapa -- ^ Mapa atualizado até à posição anterior.
          -> PosicaoMatriz -- ^ Posição atual.
          -> Int -- ^ Altura final da peça atual.
altura mapa (l,c) = case peca of Recta _ h -> h
                                 Rampa _ _ h  -> h
               where peca = encontraPosicaoMatriz (l,c) mapa


-- Nesta tarefa são construidos os mapas para o jogo. Primeiramente, é feito um "pré-mapa", com as dimensões do mapa pretendido, mas apenas formado por peças
-- do tipo Recta Terra 0. Depois, utilizando as funções da Tarefa 0, cada peça vai sendo substituida pela peça que devia estar no seu lugar. Isto vai ser sendo
-- feito peça a peça, pista a pista.










