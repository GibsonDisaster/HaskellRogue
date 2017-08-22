module Misc where
    import Control.Monad
    import System.Random
    import DataStructs

    parseInput :: [Char] -> [Command]
    parseInput chars = takeWhile (/=Quit) (map parseCommand chars)

    parseCommand :: Char -> Command
    parseCommand 'w' = MoveUp
    parseCommand 's' = MoveDown
    parseCommand 'a' = MoveLeft
    parseCommand 'd' = MoveRight
    parseCommand 'q' = Quit
    parseCommand _ = Unknown

    runCommand :: Command -> Position -> Position
    runCommand MoveLeft (x, y) = (x, y-1)
    runCommand MoveRight (x, y) = (x, y+1)
    runCommand MoveUp (x, y) = (x-1, y)
    runCommand MoveDown (x, y) = (x+1, y)
    runCommand _ (x, y) = (x, y)

    isAtPos :: Position -> Position -> Bool
    isAtPos p1 p2 = p1 == p2

    getRandNum :: IO Int
    getRandNum = do 
       let x = (head) <$> (replicateM 1 (randomIO :: IO Int))
       let y = fmap (digits) x
       let z = fmap (head) y
       z >>= return

    digits :: Integral x => x -> [x]
    digits 0 = [0]
    digits x = x `mod` 10 : digits (x `div` 10)

    moveEnemy :: Bool -> Position -> Position
    moveEnemy moved pos = if moved then runCommand MoveDown pos else runCommand MoveUp pos

    -- Cannot use the following functions because of infection by IO Monad

    {- randMovement :: (Eq a, Num a) => [Position] -> a -> [Position]
    randMovement pos rNum
        | length pos == 0 = []
        | rNum == 1 || rNum == 2 = [runCommand MoveUp x] ++ randMovement xs rNum   
        | rNum == 3 || rNum == 4 = [runCommand MoveDown x] ++ randMovement xs rNum 
        | rNum == 5 || rNum == 6 = [runCommand MoveLeft x] ++ randMovement xs rNum 
        | rNum == 7 || rNum == 8 = [runCommand MoveRight x] ++ randMovement xs rNum
        | rNum == 9 = [x] ++ randMovement xs rNum
        | otherwise = []
        where x = head pos
              xs = tail pos -}

    {- randMovement :: (Eq a, Num a) => Position -> a -> Position
    randMovement pos rNum
        | rNum == 1 || rNum == 2 = runCommand MoveUp pos
        | rNum == 3 || rNum == 4 = runCommand MoveDown pos
        | rNum == 5 || rNum == 6 = runCommand MoveLeft pos
        | rNum == 7 || rNum == 8 = runCommand MoveRight pos
        | rNum == 9 = pos
        | otherwise = pos -} 