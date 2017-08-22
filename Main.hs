module Main where
    import Control.Monad
    import System.Console.ANSI
    import System.IO
    import System.Exit
    import Misc
    import DataStructs
    import Render
    
    initScreen :: IO ()
    initScreen = do
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        hSetEcho stdin False
        hideCursor
        clearScreen

    main :: IO ()
    main = do
        initScreen
        draw '@' (20, 20)
        let e = Enemies {enemyPos=[(10, 4), (4, 10)], movedEnemies=False}
        drawList 'X' (enemyPos e)
        userInput <- getContents
        let gs = GameState {
                            playerPos=(20, 20), 
                            enemies=e,
                            screen=(Screen {
                                            title="Haskell Rogue", 
                                            titlePos=(1, 2), 
                                            isPlayable=True,
                                            walls=((zip [2,2..] [0..50]) ++ [(1, 50)] ++ [(1, 0)] ++ (zip [0,0..] [0..50]))
                                            })
                            }
        foldM_ updateScreen gs (parseInput userInput)
        
    updateScreen :: GameState -> Command -> IO GameState
    updateScreen curGS command = do
        let newPlayerPos = runCommand command (playerPos curGS)
        let newScreen = Screen {title=(title (screen curGS)), titlePos=(titlePos (screen curGS)), isPlayable=True, walls=(walls (screen curGS))}
        let newEnemies = Enemies {enemyPos=map (moveEnemy (movedEnemies (enemies curGS))) (enemyPos (enemies curGS)), movedEnemies=(not (movedEnemies (enemies curGS)))}
        let newGameState = GameState {playerPos=newPlayerPos, enemies=newEnemies, screen=newScreen}
        clear (playerPos curGS)
        clearList (enemyPos (enemies curGS))
        draw '@' newPlayerPos
        drawList 'X' (enemyPos (enemies newGameState))
        setCursorPosition 1 2
        putStr "Haskell Rogue"
        drawScreen newScreen
        if length (filter (isAtPos (playerPos newGameState)) (enemyPos (enemies newGameState))) >= 1 then do 
                                                                  clearScreen
                                                                  die "YOU DIED"
                                                                  else return newGameState