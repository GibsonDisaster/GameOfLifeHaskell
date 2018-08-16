module ThreePenny where
  import Control.Concurrent.Thread.Delay
  import System.IO
  import System.Environment (getArgs)
  import System.Console.ANSI
  import qualified Data.Map as M

  import Control.Monad
  import Data.IORef
  import Data.Bifunctor

  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  width, height :: Int
  width = 50
  height = 50

  data Cell = Alive | Dead deriving (Show, Eq, Ord)

  type Grid = M.Map (Int, Int) Cell

  -- create initial grid
  createGrid :: Int -> Int -> Int -> [(Int, Int)]
  createGrid spacing w h = [(x * spacing, y * spacing) | x <- [1..w], y <- [1..h]]

  -- get the 8 neighbors of a specific cell in the grid.
  getNeighbors :: Int -> Grid -> (Int, Int) -> [Cell]
  getNeighbors spacing g (x, y) = concat $ map (\x -> case x of { Nothing -> []; (Just n) -> [n] }) [n, s, w, e, ne, se, sw, nw]
    where
      n = M.lookup (x, y+spacing) g
      s = M.lookup (x, y-spacing) g
      w = M.lookup (x-spacing, y) g
      e = M.lookup (x+spacing, y) g
      ne = M.lookup (x+spacing, y+spacing) g
      se = M.lookup (x+spacing, y-spacing) g
      sw = M.lookup (x-spacing, y-spacing) g
      nw = M.lookup (x-spacing, y+spacing) g

  -- get length of neighbors for each cell, apply rules, and then return the new cell.
  updateCell :: Int -> Grid -> ((Int, Int), Cell) -> ((Int, Int), Cell)
  updateCell spacing g (p@(x, y), c)
    | length ns < 2 && c == Alive = (p, Dead) -- die
    | (length ns == 2 || length ns == 3) && c == Alive = (p, c) --live (no change)
    | length ns > 3 && c == Alive = (p, Dead) --die
    | length ns == 3 && c == Dead = (p, Alive) -- come alive
    | otherwise = (p, c)
      where
        ns = filter (==Alive) $ getNeighbors spacing g p

  setup :: Int -> (Grid, Int) -> Window -> UI ()
  setup spacing start window = do
    return window # set title "Game of Life"

    canvas <- UI.canvas
        # set UI.height 500
        # set UI.width 500
        # set style [("border", "solid black 1px"), ("background", "#000")]

    counter <- UI.span # set text "0"

    timer <- UI.timer # set UI.interval 1000

    getBody window #+ [element canvas, element counter]

    timerBehavior <- accumE (0::Int) $ (\c -> c + 1) <$ UI.tick timer

    st <- liftIO $ newIORef start

    void . onEvent timerBehavior $ \c -> do
      element counter # set text ("Generation: " ++ show c)
      st' <- liftIO $ readIORef st
      let cells = map (\((x, y), c) -> (x, y, if c == Alive then "teal" else "black")) (M.toList (fst st'))
      
      forM_ cells $ \(x, y, color) -> do
        canvas # set' UI.fillStyle (UI.htmlColor color)
        canvas # UI.fillRect ((fromIntegral x :: Double), (fromIntegral y :: Double)) (fromIntegral (spacing * 5) :: Double) (fromIntegral (spacing * 5) :: Double)
      
      let newSt = M.fromList $ map (updateCell spacing (fst st')) (M.toList (fst st'))
      liftIO $ writeIORef st (newSt, (snd st') + 1)
    
    UI.start timer

  main :: IO ()
  main = do
    args <- getArgs
    gridFile <- map words <$> lines <$> readFile (head args)
    let cellList = map (\c -> if c == 'x' then Dead else Alive) ((concat . concat) gridFile)
    let spacing = if length gridFile > maximum (map length gridFile) then 10 * length gridFile else 10 * maximum (map length gridFile)
    let startingGrid = M.fromList $ zip ((createGrid spacing (length (head gridFile)) (length gridFile))) cellList
    startGUI defaultConfig (setup spacing (startingGrid, 1))