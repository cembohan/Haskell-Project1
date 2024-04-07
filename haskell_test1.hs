
import Data.List (intersperse)
import System.IO ()

player1board :: [Integer] --we initialize both boards
player1board = [4,4,4,4,4,4]
player2board :: [Integer]
player2board = [1,2,4,4,4,4]

player1box :: Integer --initialize boxes that keeps each player's scores
player1box = 0
player2box :: Integer
player2box = 0
dsplyBoard :: [Integer] -> [Integer] -> IO () --this is the function that takes the current state of the game and prints it
--we need to add two more integer inputs to this function so that we can pass scores to be displayed, otherwise works.
dsplyBoard player1board player2board= do
    putStr (show player2box ++" | ")
    mapM_ putStr (intersperse " | " (map show (reverse player2board))) >> putStrLn ""    
    putStr "  | "
    mapM_ putStr (intersperse " | " (map show player1board)) >> putStr (" | "++ show player2box)  >> putStrLn ""


pick :: IO Int --general use formula that reads integer from the user
pick = do
    readLn

updateAtIndex :: Int -> a -> [a] -> [a] --we pass the index to be changed, the new int value its going to take, the initial list and returns the output list
updateAtIndex _ _ [] = []
updateAtIndex index newVal xs
    | index < 0 = error "negative index"
    | index >= length xs = error "index out of bounds"
    | otherwise = beforeIndex ++ (newVal : afterIndex)
    where (beforeIndex, _:afterIndex) = splitAt index xs


main :: IO () --main function, we decide who plays first, then run the function that sees the game through.
main = do
    dsplyBoard player1board player2board
    putStr "Type 1 or 2 to select who plays first: "
    choice <- pick
    putStrLn ("Player "++ show choice ++" goes first")
    playGame player1board

playGame :: [Integer] -> IO() --maybe this can be called if its player1's turn, and we can rename it, p1play, and make a similar one for p2
    --as of now this just takes the input from the user and changes that index to the value 10. we still need to write the actual game itself
playGame currentPlayer1Board = do
    putStrLn "choose a hole: "
    choice <- pick
    let updatedBoard = updateAtIndex (choice-1) 10 currentPlayer1Board
    dsplyBoard updatedBoard player2board
    playGame updatedBoard
    