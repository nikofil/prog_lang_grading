
data GradDir = N | S | E | W | None deriving (Show, Eq)

gradShow m = putStrLn $ concat (replicate (width m) "+---") ++ "+" ++ gradShowMaze (unzip $ cells m) (width m)

gradShowMaze :: ([Bool], [Bool]) -> Int -> [Char]
gradShowMaze (eastWs, southWs) w
    | eastWs == [] && southWs == [] = ""
    | otherwise = gradShowRow eastWsFront "\n|" "    " "   |"
        ++ gradShowRow southWsFront "\n+" "   +" "---+"
        ++ gradShowMaze (eastWsBack, southWsBack) w
    where (eastWsFront, eastWsBack) = splitAt w eastWs
          (southWsFront, southWsBack) = splitAt w southWs
 
gradShowRow :: [Bool] -> [a] -> [a] -> [a] -> [a]
gradShowRow row start open closed = start ++ concatMap (\x -> if x == True then closed else open) row

gradMkMaze :: Int -> Int -> Maze
gradMkMaze w h = Maze (take (w*h) $ repeat (True, True)) w h

gradUpdateList :: [a] -> Int -> a -> [a]
gradUpdateList l idx e = xs ++ e : ys where (xs, _:ys) = splitAt idx l
 
gradGet :: Maze -> (Int, Int) -> (Bool, Bool)
gradGet m (x, y) = (cells m) !! (y * (width m) + x)
 
gradSet :: Maze -> (Int, Int) -> (Bool, Bool) -> Maze
gradSet m (x, y) v = let idx = y * (width m) + x in
    Maze (gradUpdateList (cells m) (y * (width m) + x) v) (width m) (height m)
 
gradFilterOOB :: Maze -> (Int, Int) -> Bool
gradFilterOOB m (x, y) = x >= 0 && x < (width m) && y >= 0 && y < (height m)
 
gradOppDir :: GradDir -> GradDir
gradOppDir x = case x of N -> S
                         S -> N
                         W -> E
                         E -> W
 
gradDirPos :: (Int, Int) -> GradDir -> (Int, Int)
gradDirPos (x, y) dir = case dir of N -> (x, y-1)
                                    S -> (x, y+1)
                                    W -> (x-1, y)
                                    E -> (x+1, y)
 
gradNeighbors :: Maze -> (Int, Int) -> [((Int, Int), GradDir)]
gradNeighbors m p = GradList.filter ((gradFilterOOB m).fst) $ fmap (\x -> (gradDirPos p x, x)) [N, S, W, E]
 
gradNeighborsAccessible :: Maze -> (Int, Int) -> [(Int, Int)]
gradNeighborsAccessible m p = fmap fst $ GradList.filter (not.unaccessible) $ gradNeighbors m p
    where unaccessible (pos, N) = snd $ gradGet m pos
          unaccessible (pos, S) = snd $ gradGet m p
          unaccessible (pos, E) = fst $ gradGet m p
          unaccessible (pos, W) = fst $ gradGet m pos
 
gradNeighbors' :: Maze -> (Int, Int) -> [(Int, Int)]
gradNeighbors' m p = GradList.filter (gradFilterOOB m) $ fmap (\x -> gradDirPos p x) [S, E]
 
gradDoDfs :: Maze -> Maze
gradDoDfs m = let sx = rand $ width m
                  sy = rand $ height m
                  visited = replicate ((width m) * (height m)) False in
              gradStep m [((sx, sy), None)] visited
 
gradAddPath :: Maze -> (Int, Int) -> GradDir -> Maze
gradAddPath m p d
    | d == E = gradSet m p (False, south)
    | d == S = gradSet m p (east, False)
    where (east, south) = gradGet m p
 
gradAddPaths :: Maze -> ((Int, Int), GradDir) -> Maze
gradAddPaths m (p, d)
    | d == None = m
    | d == E || d == S = gradAddPath m (gradDirPos p $ gradOppDir d) d
    | d == W || d == N = gradAddPath m p $ gradOppDir d
 
gradStep :: Maze -> [((Int, Int), GradDir)] -> [Bool] -> Maze
gradStep m [] _ = m
gradStep m (n:ns) vs
    | isVisited == True = gradStep m ns vs'
    | otherwise = gradStep (gradAddPaths m n) (neigh ++ ns) vs'
    where ((x, y), _) = n
          idx = (y * (width m) + x)
          isVisited = vs !! idx
          neigh = shuffle $ gradNeighbors m $ fst n
          vs' = gradUpdateList vs idx True
 
gradDoKruskal :: Maze -> Maze
gradDoKruskal m = let cells = [(x,y) | y <- [0..height m - 1], x <- [0..width m -1]]
                      cell_sets = [GradSet.fromList [c] | c <- cells]
                      walls = [(c1, c2) | c1 <- cells, c2 <- (gradNeighbors' m c1)]
                      r = shuffle walls in
                  gradKruskal m cell_sets r
 
gradKruskal m cell_sets [] = m
gradKruskal m cell_sets (w:walls) =
    if gradConnected cell_sets w (width m) then gradKruskal m cell_sets walls
    else gradKruskal (gradRemoveWall m w) (gradConnect m cell_sets w) walls
 
gradRemoveWall m ((x1, y1), (x2, y2))
    | x2 - x1 == 1 = gradSet m (x1, y1) (False, south)
    | y2 - y1 == 1 = gradSet m (x1, y1) (east, False)
    where (east, south) = gradGet m (x1, y1)
 
gradConnected cell_sets ((x1, y1), (x2, y2)) w =
    GradSet.member (x1, y1) set2 || GradSet.member (x2, y2) set1
    where set1 = cell_sets !! (y1 * w + x1)
          set2 = cell_sets !! (y2 * w + x2)
 
gradConnect m cell_sets ((x1, y1), (x2, y2)) =
    fmap (\(x, y) -> if elem (x, y) unionCells == True then union
                    else cell_sets !! (y * (width m) + x) ) cells
    where set1 = cell_sets !! (y1 * (width m) + x1)
          set2 = cell_sets !! (y2 * (width m) + x2)
          union = GradSet.union set1 set2
          cells = [(x,y) | y <- [0..height m - 1], x <- [0..width m -1]]
          unionCells = GradSet.toList union
 
gradDfsPath :: Maze -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
gradDfsPath m start end = fromJust $ gradDfsPathHelper (-1, -1) m end start
 
gradDfsPathHelper :: (Int, Int) -> Maze -> (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
gradDfsPathHelper prev m target cur = if cur == target then Just [cur]
                                  else fmap ((:) cur) $ msum $ Nothing : (fmap (gradDfsPathHelper cur m target) $ GradList.delete prev (gradNeighborsAccessible m cur))

mazeCreation = GradTest.TestCase (do
res <- return $ do
    w <- [1..5]
    h <- [1..5]
    ourMaze <- return (gradMkMaze w h)
    theirMaze <- return (makeMaze w h)
    [ GradTest.assertEqual "Maze cells" (cells ourMaze) (cells theirMaze)
     , GradTest.assertEqual "Maze width" w (width theirMaze)
     , GradTest.assertEqual "Maze height" h (height theirMaze) ]
sequence res
return ()
)

tests = GradTest.TestList
    [ GradTest.TestLabel "Empty maze creation" mazeCreation ]

gradMain = GradTest.runTestTT tests
