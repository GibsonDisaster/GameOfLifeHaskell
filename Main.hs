module Main where
  import Control.Monad.State
  import Control.Concurrent.Thread.Delay
  import System.IO
  import System.Environment (getArgs)
  import System.Console.ANSI
  import qualified Data.Map as M

  width, height :: Int
  width = 400
  height = 400

  data Cell = Alive | Dead deriving (Show, Eq, Ord)

  type Grid = M.Map (Int, Int) Cell

  -- Just to make drawing easier with ansi terminal
  setCursorPos :: Int -> Int -> IO ()
  setCursorPos = flip setCursorPosition

  -- create initial grid
  createGrid :: Int -> Int -> [(Int, Int)]
  createGrid w h = [(x, y) | x <- [1..w], y <- [1..h]]

  -- draw the grid
  drawGrid :: Grid -> IO ()
  drawGrid g = mapM_ (\((x, y), c) -> do { setCursorPos x y; if c == Alive then putChar '@' else putChar '.'; }) (M.toList g)

  -- get the 8 neighbors of a specific cell in the grid.
  getNeighbors :: Grid -> (Int, Int) -> [Cell]
  getNeighbors g (x, y) = concat $ map (\x -> case x of { Nothing -> []; (Just n) -> [n] }) [n, s, w, e, ne, se, sw, nw]
    where
      n = M.lookup (x, y+1) g
      s = M.lookup (x, y-1) g
      w = M.lookup (x-1, y) g
      e = M.lookup (x+1, y) g
      ne = M.lookup (x+1, y+1) g
      se = M.lookup (x+1, y-1) g
      sw = M.lookup (x-1, y-1) g
      nw = M.lookup (x-1, y+1) g

  -- get length of neighbors for each cell, apply rules, and then return the new cell.
  updateCell :: Grid -> ((Int, Int), Cell) -> ((Int, Int), Cell)
  updateCell g (p@(x, y), c)
    | length ns < 2 && c == Alive = (p, Dead) -- die
    | (length ns == 2 || length ns == 3) && c == Alive = (p, c) --live (no change)
    | length ns > 3 && c == Alive = (p, Dead) --die
    | length ns == 3 && c == Dead = (p, Alive) -- come alive
    | otherwise = (p, c)
      where
        ns = filter (==Alive) $ getNeighbors g p

  -- main control flow of program is done with the State Monad Transformer
  -- to clean up the code and allow IO drawing actions
  simulate :: StateT (Grid, Int) IO ()
  simulate = do
    (g, gen) <- get
    let g' = M.fromList $ map (updateCell g) (M.toList g)
    liftIO $ drawGrid g'
    liftIO $ do
      setCursorPos 20 3
      putStrLn $ "Generation: " ++ show gen
    put (g', gen + 1)
    liftIO $ delay 1000000 -- wait one second
    case (filter (\((x, y), c) -> c == Alive) (M.toList g')) of
      [] -> return ()
      (x:xs) -> simulate

  main :: IO ()
  main = do
    args <- getArgs
    gridFile <- map words <$> lines <$> readFile (head args)
    let cellList = map (\c -> if c == 'x' then Dead else Alive) ((concat . concat) gridFile)
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Conways Game Of Life"
    clearScreen
    let startingGrid = M.fromList $ zip (createGrid (length (head gridFile)) (length gridFile)) cellList
    drawGrid startingGrid
    _ <- getChar
    runStateT simulate (startingGrid, 1)
    showCursor
    setSGR [Reset]
    putStrLn "No more living cells!"