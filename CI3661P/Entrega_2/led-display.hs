import System.IO
import Data.Char
import Control.Monad
import qualified Data.Map     as M
import qualified Graphics.HGL as G
import Prelude                as P

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

listStringToPixels :: [[Char]] -> Pixels
listStringToPixels listString = Pixels { color= G.White, dots= P.map stringToListOfPixel listString }
                                where
                                        stringToListOfPixel = P.map (\x -> if x == '*' then Pixel { on=True} else Pixel { on=False} )

readFont :: Handle -> IO (M.Map Char Pixels)
readFont handle = do
                    contents <- hGetContents handle
                    return $ to_map(lines $ contents)
main = do
    handle <- openFile "font.txt" ReadMode
    mapFonts <- readFont handle
    mapM_ print (M.toList $ mapFonts)
    hClose handle
