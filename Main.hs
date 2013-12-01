module Main where

import Prelude as Pre hiding (init)
import Input
import Euterpea hiding (Event)
import Data.List
import Data.Function
import Data.Char as Char
import Data.Map as Map
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
    -- let modelo = ...
    let modelo = let secuencia = Pre.foldr(\x acc -> (Pre.map (convEvento) x) ++ acc) [] seqs in  [Map.fromList[("", length(secuencia))], Map.fromList(contarPares $ secuencia), Map.fromList(contarPares $ (zipWith (++) (secuencia) (tail $ secuencia)))]
--     let composicion
    putStrLn $ show modelo
    putStrLn $ show seqs
--   play $ sequenceToMusic composicion

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
--    -}
-- buscar :: Int -> IO ()
-- buscar = buscar' directorio
--   
-- buscar' :: String -> Int -> IO ()
-- buscar' dir = do
--   seqfns <- loadMusicXmls dir
--   let seqfns_ordenados = unzip $ sortBy (compare `on` snd) $ zip seqfns
--   -- ...

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