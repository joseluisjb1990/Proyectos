-----------------------------------------------------------------------------
-- |
-- Module      :  Funciones.hs
-- Autor       :  José Luis Jiménez 10-10839
-- 
-- Modulo en el que están todas las funciones usadas en Main.hs
--
-----------------------------------------------------------------------------

module Funciones where

import Prelude as Pre hiding (init)
import Input
import Data.List
import qualified Data.Map as Map
import Data.Maybe as May
import System.Random

{-
    Toma un valor del tipo string que representa un valor del tipo Evento
    y lo convierte al tipo Evento.
-}
stringToEvento :: [Char] -> Evento
stringToEvento xs = auxStringToEvento xs []
    where
        auxStringToEvento (x:xs) aux
            | x == ',' = (read aux :: Int, read xs :: Int)
            |otherwise = auxStringToEvento xs (aux ++ [x])

{- Toma un valor del tipo Evento y lo convierte a un tipo String -}
eventoToString :: Evento -> [Char]
eventoToString (x, y) = (show x) ++ "," ++ (show y)


{-
    Toma un entero y retorna una lista de esa longitud compuesta por números
    random del tipo float en el rango del 0.0 al 0.99
-}
generarRandom :: Int -> IO [Float]
generarRandom longitud = do
    gen <- newStdGen
    let random = take longitud (randomRs (0.0,0.99) gen :: [Float])
    return (random)

{-
    Función que toma un modelo (representado en una lista de Map) y aplica la
    función producirLetra con ese modelo.
-}
producirLetraModelo :: [Map.Map [Char] Int] -> Float -> ([Char] -> [Char])
producirLetraModelo modelo random = producirLetra modelo random

{-
    Función que toma un modelo y un valor del tipo Float y retorna el
    siguiente evento que debería ser usado por la función componer (Main)
    a partir de la distribución de probabilidades generada por el modelo.
-}
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

{-
    Toma una secuencia de funciones parcialmente aplicadas y un valor que
    se usa como argumento para el primer elemento de la secuencia. El valor
    retornado se usa para el segundo elemento de la secuencia y así
    sucesivamente hasta llegar al final de la lista.
-}
auxGenerarSecuencia :: [a -> a] -> a -> [a] -> [a]
auxGenerarSecuencia (f:fs) ant aux = let valor = f ant in auxGenerarSecuencia fs valor (valor:aux)
auxGenerarSecuencia [] _ aux = Pre.reverse aux

{-
    Función que toma una secuencia de string y retorna un modelo a partir
    de la frecuencia de cada elemento y de cada par de elementos.
-}
generarModelo :: [[Char]] -> [Map.Map [Char] Int]
generarModelo secuencia = [
                            Map.fromList[("", length(secuencia))],
                            Map.fromList(contarPares $ secuencia),
                            Map.fromList(contarPares $ (zipWith (++) (secuencia) (tail $ secuencia)))
                            ]

{-
    Función que toma una secuencia de string y retorna una lista de
    tuplas de string y enteros. Cada tupla corresponde a un string de la
    secuencia original y el entero representa la cantidad de veces que
    está ese string en la secuencia.
-}
contarPares :: [[Char]] -> [([Char], Int)]
contarPares xs = let listaOrd = sort $ xs in auxContarPares (tail $ listaOrd) (head $ listaOrd) []
                      where
                            auxContarPares [] _ auxLista    = auxLista
                            auxContarPares xs y []          = auxContarPares xs y [(y,1)]
                            auxContarPares (x:xs) y all@((_,n):auxLista)
                                | x == y      = auxContarPares xs x ((x,n+1):auxLista)
                                | otherwise   = auxContarPares xs x ((x,1):all)

{-
    Función que aplica parcialemente la función distanciaModelos a un modelo
-}
distanciaModeloFijo :: [Map.Map [Char] Int] -> ([Map.Map [Char] Int] -> Float)
distanciaModeloFijo modelo = distanciaModelos modelo

{-
    Función que recibe dos modelos representados commo una lista de Map y
    retorna la distancia entre estos.
-}
distanciaModelos :: [Map.Map [Char] Int] -> [Map.Map [Char] Int] -> Float
distanciaModelos modeloA modeloB =
    let (distancia1, distanciaP) =
            (
            calcularDistancia1,
            calcularDistanciaP
            )
        in sqrt(distancia1 + distanciaP)
            where
                calcularDistancia1  = distanciaMapa (modeloA !! 1) (modeloB !! 1)
                calcularDistanciaP  = distanciaMapa (modeloA !! 2) (modeloB !! 2)

{-
    Función que toma dos diccionarios de string a entero y retorna
    un float que representa la distancia entre los diccionarios usando
    la fórmula explicada en la parte 6 del enunciado del proyecto.
-}
distanciaMapa :: Map.Map [Char] Int -> Map.Map [Char] Int -> Float
distanciaMapa mapaA mapaB = fromIntegral $ Pre.foldl (\acc (clave, valor) -> valor^2 + acc)
                                                    0 (Map.toAscList $ Map.unionWith (-) mapaA mapaB)