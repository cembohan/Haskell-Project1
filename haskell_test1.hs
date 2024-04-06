
import Data.List (intersperse)
import System.IO ()

player1board :: [Integer]
player1board = [4,4,4,4,4,4]
player2board :: [Integer]
player2board = [1,2,4,4,4,4]

player1box :: Integer
player1box = 0
player2box :: Integer
player2box = 0
a :: [Integer]
a = reverse player2board
dsplyBoard = do
    putStr (show player2box ++" | ")
    mapM_ putStr (intersperse " | " (map show a)) >> putStrLn ""    
    putStr "  | "
    mapM_ putStr (intersperse " | " (map show player1board)) >> putStr (" | "++ show player2box)  >> putStrLn ""


main = do
    dsplyBoard