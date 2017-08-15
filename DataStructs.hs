module DataStructs where
    
    type Position = (Int, Int)
    
    data Command =    MoveUp
                    | MoveDown
                    | MoveLeft
                    | MoveRight
                    | Quit
                    | Unknown
                    deriving (Eq, Show)

    data Screen = Screen {
                    title :: String,
                    titlePos :: Position,
                    isPlayable :: Bool,
                    walls :: [Position]
                  } deriving (Eq, Show)

    data GameState = GameState {
                        playerPos :: Position,
                        enemyPos :: [Position],
                        screen :: Screen
                        } deriving (Eq, Show)