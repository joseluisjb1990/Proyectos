module Pixels
( font
, pixelListToPixels
, concatPixels
, messageToPixels
, up
, down
, left
, right
, upsideDown
, backwards
, negative
) where

import Data.Bits
import Data.Char
import Data.List

data Pixel = Pixel { on :: Bool } deriving (Show)

data Pixels = Pixels { color :: G.Color, dots :: [[Pixel]] } deriving (Show)

to_map :: [[Char]] -> M.Map Char Pixels
to_map (x:stringList) = let (width, height) = extract_w_h(x) in M.fromList(buildListOfTuples stringList height)

extract_w_h :: [Char] -> (Int, Int)
extract_w_h string = let list = P.map read (words $ string) in (head list, head $ tail list)

buildListOfTuples :: [[Char]] -> Int -> [(Char,Pixels)]
buildListOfTuples listS height = if not $ P.null listS then
                                        let (h, t) = splitAt (height + 1) listS
                                                in (head $ tail $ head h, listStringToPixels $ tail h) : buildListOfTuples t height
                                 else []

font :: Char -> Pixels
font c = let elem = fontBitmap !! (ord c - 32) in list_to_Pixels elem

list_to_Pixels :: [Int] -> Pixels
list_to_Pixels l = transpose $ map int_to_pixel l

int_to_pixel :: Int -> String
int_to_pixel n = map bit_to_char (take 7 $ iterate sumTupla (n,0))
  where
    sumTupla (n,i) = (n,i+1)
    bit_to_char (n,i) = if testBit n i then '*' else ' '

pixelListToPixels :: [Pixels] -> Pixels
pixelListToPixels p = tail (foldr (\x acc -> ("" : x) ++ acc) [] p)

concatPixels :: [Pixels] -> Pixels
concatPixels = foldl (zipWith (++)) (take 7 (repeat []))

messageToPixels :: String -> Pixels
messageToPixels s = map tail $ foldl (zipWith (\x y -> x ++ " " ++ y)) (take 7 $ repeat []) (map font s)

first_to_last :: [a] -> [a]
first_to_last p = let (h,t) = (head p, tail p) in t ++ [h]

last_to_first :: [a] -> [a]
last_to_first p = let (i,l) = (init p, last p) in l:i

up :: Pixels -> Pixels
up = first_to_last

down :: Pixels -> Pixels
down = last_to_first

left :: Pixels -> Pixels
left = map first_to_last

right :: Pixels -> Pixels
right = map last_to_first

upsideDown :: Pixels -> Pixels
upsideDown = reverse

backwards :: Pixels -> Pixels
backwards = map reverse

negative :: Pixels -> Pixels
negative = map $ map (\x -> if x == '*' then ' ' else '*')
