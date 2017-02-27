
gradMkMaze :: Int -> Int -> Maze
gradMkMaze w h = Maze (take (w*h) $ repeat (True, True)) w h

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
