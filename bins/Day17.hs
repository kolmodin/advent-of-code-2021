{-# LANGUAGE TupleSections #-}

module Main (main) where

-- x=253..280, y=-73..-46
xMin, xMax, yMin, yMax :: Int
(xMin, xMax) = (253, 280)
(yMin, yMax) = (-73, -46)

findMaxY :: Int
findMaxY =
  maximum
    [ maximum ypath
      | yVel <- [0 .. (-yMin)],
        let ypath = trajectoryY yVel,
        any hitsY ypath
    ]

hitsY :: Int -> Bool
hitsY y = y >= yMin && y <= yMax

hitsX :: Int -> Bool
hitsX x = x >= xMin && x <= xMax

hitsXY :: (Int, Int) -> Bool
hitsXY (x, y) = hitsX x && hitsY y

trajectoryY :: Int -> [Int]
trajectoryY = takeWhile (>= yMin) . map fst . iterate go . (0,)
  where
    go (y, vy) = (y + vy, vy - 1)

-- trajectoryX may be infinite.
trajectoryX :: Int -> [Int]
trajectoryX = takeWhile (<= xMax) . map fst . iterate go . (0,)
  where
    go (x, vx) = (x+vx, vx - signum vx)

trajectoryXY :: Int -> Int -> [(Int, Int)]
trajectoryXY x y = zip (trajectoryX x) (trajectoryY y)

initialThatHit :: [(Int, Int)]
initialThatHit =
  concat
    [ [ (vx, vy)
        | vx <- [1 .. xMax],
          any hitsXY (trajectoryXY vx vy)
      ]
      | vy <- [yMin .. (-yMin)],
        any hitsY (trajectoryY vy)
    ]

main :: IO ()
main = do
  putStrLn ("Part 1: " ++ show findMaxY)
  putStrLn ("Part 2: " ++ show (length initialThatHit))