module Main where

import Prelude as Pre hiding (init)
import Input
import Euterpea hiding (Event)
import Data.List
import Data.Function
import Data.Char as Char
import qualified Data.Map as Map
import Data.Maybe as May
import System.Random

-- Directorio predeterminado
directorio :: String
directorio = "./xmlTodos/"

-- Longitud de las secuencias musicales generadas
longitud :: Int
longitud = 50

{- Induce un modelo de contexto a partir de la colección musical 
   en el directorio por defecto, genera una secuencia musical 
   nueva a partir de este modelo, la imprime por pantalla y la 
   reproduce.
   -}
componer :: IO ()
componer = componer' directorio

componer' :: String -> IO ()
componer' dir = do
    (seqs, filenames) <- loadMusicXmls dir
    let modelo = let secuencia = Pre.foldr(\x acc -> (Pre.map (eventoToString) x) ++ acc) [] seqs in generarModelo secuencia
    composicion <- generarSecuencia modelo
    putStrLn $ show composicion
    play $ sequenceToMusic composicion

generarSecuencia modelo = do
    listaRandom <- generarRandom
    let listaSalida = Pre.map stringToEvento $ reverse $ auxGenerarSecuencia (Pre.map (\x -> producirLetraModelo modelo x) listaRandom) "" []
    return(listaSalida)

auxGenerarSecuencia (f:fs) ant aux = let valor = f ant in auxGenerarSecuencia fs valor (valor:aux)
auxGenerarSecuencia [] _ aux = aux

producirLetraModelo :: [Map.Map [Char] Int] -> Float -> ([Char] -> [Char])
producirLetraModelo modelo random = producirLetra modelo random

producirLetra :: [Map.Map [Char] Int] -> Float -> [Char] -> [Char]
producirLetra modelo random ant = let (listaValores, cant,numNormalizado) = (Map.toAscList $ (modelo !! 1), snd . head $ (Map.toAscList $ (modelo !! 0)), (sum $ Pre.map (\ (x,y) -> ((fromIntegral (y) * 0.3) / fromIntegral(cant)) + (buscarDoble x)) (listaValores))) in buscarValor (Pre.map (\ (x,y) -> (x,(((fromIntegral (y) * 0.3) / fromIntegral(cant)) + (buscarDoble x)) / numNormalizado)) (listaValores))
                                where
                                    buscarDoble clave = let (valorDoble, valorInd) = (Map.lookup (ant++clave) (modelo !! 2), Map.lookup ant (modelo !! 1)) in if May.isNothing $ valorDoble then 0.0 else (0.7 * (fromIntegral(fromJust $ valorDoble)) / (fromIntegral(fromJust $ valorInd)))
                                    buscarValor lista = auxBuscarValor random lista 0.0
                                        where
                                            auxBuscarValor random ((x,y):xs) acc
                                                | random < y + acc  = x
                                                | otherwise         = auxBuscarValor random xs (y + acc)


generarRandom = do
    gen <- newStdGen
    let random = take longitud (randomRs (0.0,0.99) gen :: [Float])
    return (random)

generarModelo :: [[Char]] -> [Map.Map [Char] Int]
generarModelo secuencia = [Map.fromList[("", length(secuencia))], Map.fromList(contarPares $ secuencia), Map.fromList(contarPares $ (zipWith (++) (secuencia) (tail $ secuencia)))]

contarPares :: [[Char]] -> [([Char], Int)]
contarPares xs = let listaOrd = sort $ xs in auxContarPares (tail $ listaOrd) (head $ listaOrd) []
                      where
                            auxContarPares [] _ auxLista    = auxLista
                            auxContarPares xs y []          = auxContarPares xs y [(y,1)]
                            auxContarPares (x:xs) y all@((_,n):auxLista)
                                | x == y      = auxContarPares xs x ((x,n+1):auxLista)
                                | otherwise   = auxContarPares xs x ((x,1):all)

                                
{- Recupera las diez secuencias más similares a la k-ésima secuencia
   de la colección musical en el directorio por defecto, donde la 
   colección musical ha sido ordenada en orden alfabético por el 
   nombre de archivo. Imprime una lista ordenada de las diez 
   secuencias más similares. En cada fila de la lista se debe indicar 
   el número de la secuencia (relativo al orden alfabético de la 
   colección), el nombre de archivo y la distancia a la consulta.
   -}
buscar :: Int -> IO ()
buscar = buscar' directorio

buscar' :: String -> Int -> IO ()
buscar' dir pos = do
  (seqs, filenames) <- loadMusicXmls dir
  let seqfns_ordenados =  sortBy (\(x0,y0) (x1,y1) -> if y0 < y1 then LT else GT) (Pre.zip seqs filenames)
  
  let secuencia_modelos = foldr (\(secuencia, nombreArch) acc -> (generarModelo $ Pre.map eventoToString secuencia, nombreArch) : acc) [] seqfns_ordenados
  let modeloFijo = distanciaModeloFijo (fst $ (secuencia_modelos !! (pos -1)))
  putStrLn $ secuenciaToString $ take 10 $ sortBy (\(n0,(x0,y0)) (n1,(x1,y1)) -> if x0 < x1 then LT else GT) (Pre.zip [1..] (Pre.map (\ (modelo, nombreArch) -> (modeloFijo modelo, nombreArch)) secuencia_modelos))
    where secuenciaToString secuencia = Pre.foldl (\acc (n,(x0,y0)) -> acc ++ show(n) ++ " " ++ show(y0) ++ " " ++ show(x0) ++ ['\n']) [] secuencia
          
distanciaModeloFijo :: [Map.Map [Char] Int] -> ([Map.Map [Char] Int] -> Float)
distanciaModeloFijo modelo = distanciaModelos modelo

distanciaModelos :: [Map.Map [Char] Int] -> [Map.Map [Char] Int] -> Float
distanciaModelos modeloA modeloB =
    let (distancia0, distancia1, distanciaPares) = (calcularDistancia0, calcularDistancia1, calcularDistanciaPares)
        in sqrt(distancia0 + distancia1 + distanciaPares)
            where
                calcularDistancia0      = (fromIntegral (snd . head $ Map.toAscList $ (modeloB !! 0)) - fromIntegral (snd . head $ Map.toAscList $ (modeloA !! 0))) ** 2
                calcularDistancia1      = distanciaMapa (modeloA !! 1) (modeloB !! 1)
                calcularDistanciaPares  = distanciaMapa (modeloA !! 2) (modeloB !! 2)

distanciaMapa :: Map.Map [Char] Int -> Map.Map [Char] Int -> Float
distanciaMapa mapaA mapaB = fromIntegral $ sum $ Pre.foldl (\acc (clave, valor) -> valor^2 : acc) [] (Map.toAscList $ Map.unionWith (-) mapaA mapaB)

tocar :: Int -> IO ()
tocar n = do
  seqfns <- loadMusicXmls directorio
  let (seqs, filenames) = unzip $ sortBy (compare `on` snd) $ (uncurry zip) seqfns
  if (n > 0) && (n <= length seqs) then
    putStrLn (filenames !! (n-1)) >>
    play (sequenceToMusic (seqs !! (n-1)))
    else
      putStrLn "Indice fuera de rango"
          
eventToNote :: Evento -> Music Note1
eventToNote e = note
  where
  d = (fromIntegral $ snd e) / 16
  p = Euterpea.pitch $ fst e
  note = Prim (Note d (p,[]))
  
sequenceToMusic :: [Evento] -> Music Note1
sequenceToMusic es = line $ Pre.map eventToNote es

main = buscar 59