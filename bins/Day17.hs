module Main (main) where

-- x=253..280, y=-73..-46
xMin :: Int
xMin = 253

xMax :: Int
xMax = 280

yMin :: Int
yMin = -73

yMax :: Int
yMax = -46

findMaxY :: Int
findMaxY =
  maximum
    [ maximum (trajectoryY yVel)
      | yVel <- [0 .. (-yMin)],
        any hitsY (trajectoryY yVel)
    ]

hitsY :: Int -> Bool
hitsY y = y >= yMin && y <= yMax

hitsX :: Int -> Bool
hitsX x = x >= xMin && x <= xMax

hitsXY :: (Int, Int) -> Bool
hitsXY (x, y) = hitsX x && hitsY y

trajectoryY :: Int -> [Int]
trajectoryY = takeWhile (>= yMin) . go 0
  where
    go ypos y =
      let ypos' = ypos + y
       in ypos' : go ypos' (y - 1)

-- trajectoryX may be infinite.
trajectoryX :: Int -> [Int]
trajectoryX = takeWhile (<= xMax) . go 0
  where
    go xpos x =
      let xpos' = xpos + x
          sign
            | x > 0 = -1
            | x < 0 = 1
            | otherwise = 0
       in xpos' : go xpos' (x + sign)

trajectoryXY :: Int -> Int -> [(Int, Int)]
trajectoryXY x y = zip (trajectoryX x) (trajectoryY y)

initialThatHit :: [(Int, Int)]
initialThatHit =
  concat
    [ [ (xVel, yVel)
        | xVel <- [1 .. xMax],
          any hitsXY (trajectoryXY xVel yVel)
      ]
      | yVel <- [yMin .. (-yMin)],
        any hitsY (trajectoryY yVel)
    ]

main :: IO ()
main = do
  putStrLn ("Part 1: " ++ show findMaxY)
  putStrLn ("Part 2: " ++ show (length initialThatHit))