module Funciones where

import Prelude as Pre hiding (init)
import Input
import Euterpea hiding (Event)
import Data.List
import Data.Function
import Data.Char as Char
import qualified Data.Map as Map
import Data.Maybe as May
import System.Random

generarRandom longitud = do
    gen <- newStdGen
    let random = take longitud (randomRs (0.0,0.99) gen :: [Float])
    return (random)

producirLetraModelo :: [Map.Map [Char] Int] -> Float -> ([Char] -> [Char])
producirLetraModelo modelo random = producirLetra modelo random

producirLetra :: [Map.Map [Char] Int] -> Float -> [Char] -> [Char]
producirLetra modelo random ant =
    let (listaValores, cant,numNormalizado) =
            (
            Map.toAscList $ (modelo !! 1),
            snd . head $ (Map.toAscList $ (modelo !! 0)),
            sum $ Pre.map (\ (x,y) -> let prob = ((fromIntegral (y) * 0.3) / fromIntegral(cant))
                                      in prob + (buscarDoble x)) listaValores
            )

    in buscarValor (Pre.map (\(x,y) -> let prob = ((fromIntegral (y) * 0.3) / fromIntegral(cant))
                                       in (x,(prob + (buscarDoble x)) / numNormalizado)) listaValores)
            where
                buscarDoble clave =
                    let (valorDoble, valorInd) =
                            (
                            Map.lookup (ant++clave) (modelo !! 2),
                            Map.lookup ant (modelo !! 1)
                            )
                    in  if May.isNothing $ valorDoble then 0.0
                        else (0.7 * (fromIntegral(fromJust $ valorDoble)) /
                                    (fromIntegral(fromJust $ valorInd)))

                buscarValor lista = auxBuscarValor random lista 0.0
                    where
                        auxBuscarValor random ((x,y):xs) acc
                            | random < y + acc  = x
                            | otherwise         = auxBuscarValor random xs (y + acc)

auxGenerarSecuencia (f:fs) ant aux = let valor = f ant in auxGenerarSecuencia fs valor (valor:aux)
auxGenerarSecuencia [] _ aux = aux

generarModelo :: [[Char]] -> [Map.Map [Char] Int]
generarModelo secuencia = [
                            Map.fromList[("", length(secuencia))],
                            Map.fromList(contarPares $ secuencia),
                            Map.fromList(contarPares $ (zipWith (++) (secuencia) (tail $ secuencia)))
                            ]

contarPares :: [[Char]] -> [([Char], Int)]
contarPares xs = let listaOrd = sort $ xs in auxContarPares (tail $ listaOrd) (head $ listaOrd) []
                      where
                            auxContarPares [] _ auxLista    = auxLista
                            auxContarPares xs y []          = auxContarPares xs y [(y,1)]
                            auxContarPares (x:xs) y all@((_,n):auxLista)
                                | x == y      = auxContarPares xs x ((x,n+1):auxLista)
                                | otherwise   = auxContarPares xs x ((x,1):all)

distanciaModeloFijo :: [Map.Map [Char] Int] -> ([Map.Map [Char] Int] -> Float)
distanciaModeloFijo modelo = distanciaModelos modelo

distanciaModelos :: [Map.Map [Char] Int] -> [Map.Map [Char] Int] -> Float
distanciaModelos modeloA modeloB =
    let (distancia0, distancia1, distanciaP) =
            (
            calcularDistancia0,
            calcularDistancia1,
            calcularDistanciaP
            )
        in sqrt(distancia0 + distancia1 + distanciaP)
            where
                calcularDistancia0  = (fromIntegral (snd . head $ Map.toAscList $ (modeloB !! 0))
                                     - fromIntegral (snd . head $ Map.toAscList $ (modeloA !! 0))) ** 2
                calcularDistancia1  = distanciaMapa (modeloA !! 1) (modeloB !! 1)
                calcularDistanciaP  = distanciaMapa (modeloA !! 2) (modeloB !! 2)

distanciaMapa :: Map.Map [Char] Int -> Map.Map [Char] Int -> Float
distanciaMapa mapaA mapaB = fromIntegral $ sum $ Pre.foldl (\acc (clave, valor) -> valor^2 : acc)
                                                    [] (Map.toAscList $ Map.unionWith (-) mapaA mapaB)
