module Render where
    import System.Console.ANSI
    import DataStructs

    draw :: Char -> Position -> IO ()
    draw c (x, y) = do
        setCursorPosition x y
        putChar c
        setCursorPosition 26 0

    drawList :: Char -> [Position] -> IO ()
    drawList _ [] = do
        setCursorPosition 600 700
        putStr ""
        setCursorPosition 0 0
    drawList c (p:xs) = do
        setCursorPosition (fst p) (snd p)
        putChar c
        drawList c xs
        
    drawScreen :: Screen -> IO ()
    drawScreen scr = do
        drawTitle (titlePos scr) (title scr)
        drawWalls (walls scr)

    drawTitle :: Position -> String -> IO ()
    drawTitle (x, y) str = do
        setCursorPosition x y
        putStr str
    
    drawWalls :: [Position] -> IO ()
    drawWalls [] = do
        setCursorPosition 700 600
        putChar ' '
    drawWalls (x:xs) = do
        setCursorPosition (fst x) (snd x)
        putChar '#'
        drawWalls xs

    clear :: Position -> IO ()
    clear (x, y) = do
        setCursorPosition x y
        putChar ' '
        setCursorPosition 26 0