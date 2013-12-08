module Main where

import Prelude as Pre hiding (init)
import Input
import Funciones
import Euterpea hiding (Event)
import Data.List
import Data.Function
import Data.Char as Char
import qualified Data.Map as Map
import Data.Maybe as May
import System.Random

-- Directorio predeterminado
directorio :: String
directorio = "./xml/"

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
    let modelo =    let secuencia = Pre.foldr(\x acc -> (Pre.map (eventoToString) x) ++ acc) [] seqs
                    in generarModelo secuencia
    composicion <- generarSecuencia modelo
    putStrLn $ show composicion
    play $ sequenceToMusic composicion

generarSecuencia modelo = do
    listaRandom <- generarRandom longitud
    let listaSalida = Pre.map stringToEvento $ reverse $ auxGenerarSecuencia
                        (Pre.map (\x -> producirLetraModelo modelo x) listaRandom) "" []
    return(listaSalida)

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
    let seqfns_ordenados =  sortBy
                            (\(x0,y0) (x1,y1) -> if y0 < y1 then LT else GT)
                            (Pre.zip seqs filenames)
    
    let secuencia_modelos = foldr (\(secuencia, nombreArch) acc ->
                                  (generarModelo $ Pre.map eventoToString secuencia, nombreArch) : acc)
                                  [] seqfns_ordenados
    
    let modeloFijo = distanciaModeloFijo (fst $ (secuencia_modelos !! (pos -1)))
    
    putStrLn $ secuenciaToString $ take 10 $
        sortBy  (\(n0,(x0,y0)) (n1,(x1,y1)) -> if x0 < x1 then LT else GT)
                (Pre.zip [1..] (Pre.map (\ (modelo, nombreArch) ->
                                        (modeloFijo modelo, nombreArch)) secuencia_modelos))
                                    
            where secuenciaToString secuencia =
                    Pre.foldl (\acc (n,(x0,y0)) ->
                        acc ++ show(n) ++ " " ++ show(y0) ++ " " ++ show(x0) ++ ['\n']) [] secuencia

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

main = componer