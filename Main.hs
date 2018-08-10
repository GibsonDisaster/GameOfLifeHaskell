module Main where
  import Control.Monad.State
  import Control.Concurrent.Thread.Delay
  import System.IO
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
  createGrid :: Int -> Int -> Grid
  createGrid w h = M.fromList $ [((x, y), Dead) | x <- [1..w], y <- [1..h]]

  -- insert living cells into grid.
  -- TODO:
  -- Make this easier to add cells to
  insertCells :: Grid -> Grid
  insertCells g = M.insert (3, 3) Alive (M.insert (2, 3) Alive (M.insert (4, 3) Alive g))

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
  simulate :: StateT Grid IO ()
  simulate = do
    g <- get
    let g' = M.fromList $ map (updateCell g) (M.toList g)
    liftIO $ drawGrid g'
    put g'
    liftIO $ delay 1000000 -- wait one second
    simulate

  main :: IO ()
  main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Conways Game Of Life"
    clearScreen
    let startingGrid = insertCells (createGrid 5 5)
    drawGrid startingGrid
    runStateT simulate startingGrid
    showCursor
    setSGR [Reset]