module Main where

import Data.List (transpose, intercalate)
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)


main :: IO ()
--Atrapo los datos del archivo que se ingresa en consola
main = interact (\input -> convert input ++ "\n")

--separo el archivo en partes para poder leerlo
convert :: String -> String
convert = intercalate "," . map (map ocr . transpose . map (chunksOf 3)) . chunksOf 4 . lines


ocr :: [String] -> Char
ocr [" _ ",
     "| |",
     "|_|"] = '0'
     
ocr ["  |",
     "  |"] = '1'
ocr [" _ ",
     " _|",
     "|_ "] = '2'
ocr [" _ ",
     " _|",
     " _|"] = '3'
ocr ["   ",
     "|_|",
     "  |"] = '4'
ocr [" _ ",
     "|_ ",
     " _|"] = '5'
ocr [" _ ",
     "|_ ",
     "|_|"] = '6'
ocr [" _ ",
     "  |",
     "  |"] = '7'
ocr [" _ ",
     "|_|",
     "|_|"] = '8'
ocr [" _ ",
     "|_|",
     " _|"] = '9'
ocr _       = 'N'