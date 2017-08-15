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
    digits 0 = []
    digits x = x `mod` 10 : digits (x `div` 10)
