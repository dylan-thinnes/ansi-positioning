{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
import Data.List (intercalate, permutations, nub, inits, transpose)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Monad (guard)
import System.Environment (getArgs)

-- colors
ansi :: String -> [Int] -> String
ansi mode xs = "\ESC[" ++ intercalate ";" (map show xs) ++ mode

color :: Bool -> (Int, Int, Int) -> String -> String
color bg (r, g, b) text = ansi "m" [if bg then 48 else 38, 2, r, g, b] ++ text ++ ansi "m" [0]

tileS :: (Int, Int, Int) -> String -> String
tileS = color True

tile :: (Int, Int, Int) -> String
tile rgb = tileS rgb " "

fromBlack :: (Int, Int, Int) -> Double -> (Int, Int, Int)
fromBlack = linear (0,0,0)

grey, red :: Double -> (Int, Int, Int)
grey = fromBlack (256,256,256)
red = fromBlack (256,0,0)

linear :: (Int, Int, Int) -> (Int, Int, Int) -> Double -> (Int, Int, Int)
linear (r1, g1, b1) (r2, g2, b2) pct = (blend r1 r2, blend g1 g2, blend b1 b2)
  where
    blend start end
      = max 0 $ min 255
      $ start + round (pct * fromIntegral (end - start))

toPos :: Int -> Int -> String
toPos x y = ansi "H" [y,x]

putAt :: Int -> Int -> String -> String
putAt x y msg = ansi "s" [] ++ toPos x y ++ msg ++ ansi "u" []

-- conway
type Field a = [[a]]
type Cell = (Int, Int, Int)

fieldMap :: (a -> b) -> Field a -> Field b
fieldMap = map . map

collectNeighbours :: Field a -> Field (a, [a])
collectNeighbours inp = zipWith zip inp (neighs inp)
  where
    neighs = transpose . map conv1D . transpose . map (conv1D . map pure)

    conv1D :: [[a]] -> [[a]]
    conv1D xs = zipWith3 (\a b c -> a <> b <> c) xs (tail xs ++ [head xs]) (last xs : init xs)

render :: Field String -> String
render = unlines . map concat

renderInPlace :: Field Cell -> String
renderInPlace field
  = concat
  $ concat
  $ zipWith (\y row -> zipWith (f y) [0..] row) [0..] field
  where
    f :: Int -> Int -> Cell -> String
    f y x cell = putAt x (y + 3) $ tile cell

glider :: Field Int
glider = [[0,1,0],[0,0,1],[1,1,1]]

gliders :: Int -> Int -> Field Int
gliders x y =
  let g1 = extend 5 5 0 glider
      catRep n = concat . replicate n
  in
  catRep y $ map (catRep x) g1

extend :: Int -> Int -> a -> Field a -> Field a
extend xt yt def inp =
  let xa = length $ head inp
      ya = length inp
  in
  map (\row -> row ++ replicate (xt - xa) def) inp ++ replicate (yt - ya) (replicate xt def)

example1 :: Int -> Int -> Field Cell
example1 xm ym = (map . map) f [[(x, y) | x <- [0..xm-1]] | y <- [0..ym-1]]
  where
    f :: (Int, Int) -> Cell
    f (x, y) =
      let r = (xm - x) * 255 `div` xm
          g = (ym - y) * 255 `div` ym
          b = (xm * ym - x * y) `mod` (xm * ym) * 255 `div` (xm * ym)
      in
      (r,g,b)

baseRule :: (Int, [Int]) -> Int
baseRule (live, neighs)
  | (live, sum neighs) `elem` [(1, 3), (1, 4), (0, 3)]
  = 1
  | otherwise
  = 0

colorRule :: (Cell, [Cell]) -> Cell
colorRule (self, neighs)
  | isLive self && neighCount neighs `elem` [3,4]
  = self
  | not (isLive self) && neighCount neighs == 3
  = onCell (`div` 3) $ foldr1 (overCell (+)) neighs
  | otherwise
  = (0,0,0)

colorRule2 :: (Cell, [Cell]) -> Cell
colorRule2 (self, neighs)
  | (isLive self, neighCount neighs) `elem` [(True,3),(True,4),(False,3)]
  = onCell (`div` neighCount neighs) $ foldr1 (overCell (+)) neighs
  | otherwise
  = (0,0,0)

isLive :: Cell -> Bool
isLive (0,0,0) = False
isLive _ = True

neighCount :: [Cell] -> Int
neighCount = length . filter isLive

overField :: (a -> a -> b) -> Field a -> Field a -> Field b
overField = zipWith . zipWith

onCell :: (Int -> Int) -> Cell -> Cell
onCell f (r, g, b) = (f r, f g, f b)

overCell :: (Int -> Int -> Int) -> Cell -> Cell -> Cell
overCell f (r0, g0, b0) (r1, g1, b1) =
  (r0 `f` r1, g0 `f` g1, b0 `f` b1)

pureCell :: Int -> Cell
pureCell a = (a,a,a)

step :: Field Cell -> Field Cell
step = fieldMap colorRule . collectNeighbours
start1, start2, start3 :: Field Cell
start1 =
  overField (overCell (+))
    (extend 60 60 (0,0,0)
      (overField (overCell (*))
        (fieldMap pureCell (gliders 3 3))
        (example1 15 15)))
    (reverse $ map reverse
      (extend 60 60 (0,0,0) $
        let y = (255,255,0)
            o = (0,0,0)
        in
        [[y,o,o,y,y,y,y,o,y,y,y,y,o,y,y,y,y,y,o,y]
        ,[y,y,o,y,o,y,o,y,o,o,y,y,o,o,y,o,o,y,o,y]
        ,[o,o,y,y,y,y,o,y,o,y,o,o,o,y,o,y,y,y,o,o]
        ,[y,o,y,o,y,y,y,y,y,o,y,y,o,y,o,y,o,o,o,o]
        ,[o,o,y,y,y,y,y,y,o,o,y,y,y,o,y,y,y,o,o,o]
        ,[y,o,o,o,y,y,y,o,o,o,y,o,o,o,o,o,y,y,y,y]
        ,[y,y,y,o,o,y,o,o,o,y,y,o,y,y,y,y,y,y,o,y]
        ,[y,y,y,y,y,o,o,y,y,o,o,o,y,y,y,o,o,y,y,y]
        ,[y,y,y,o,y,y,y,y,y,y,y,o,o,o,o,o,o,o,y,y]
        ,[o,o,y,o,o,o,y,y,o,y,y,o,y,o,y,o,o,o,o,y]
        ,[o,o,o,y,o,y,y,y,y,o,o,y,o,o,y,y,o,o,y,o]
        ,[y,o,y,o,y,y,o,y,y,o,o,o,o,y,y,y,y,o,o,y]
        ,[y,o,o,y,o,y,o,o,o,o,o,y,y,o,y,o,o,o,o,y]
        ,[y,o,y,y,y,y,o,o,o,y,y,o,o,y,o,y,o,o,o,o]
        ,[y,y,o,o,y,o,y,y,y,o,o,o,y,o,y,y,y,o,o,o]
        ,[o,y,o,o,o,o,y,y,o,y,o,y,y,y,y,y,o,y,y,y]
        ,[y,y,o,y,y,y,o,y,y,y,o,o,o,y,y,y,o,y,o,y]
        ,[y,o,y,y,y,y,y,y,o,o,o,o,o,y,o,o,o,y,o,y]
        ,[o,o,o,y,y,y,o,y,o,o,o,y,y,o,y,y,o,o,y,o]
        ,[y,y,y,y,y,o,o,o,o,y,y,o,y,o,o,y,o,y,o,y]
        ]
      ))
start2 =
  let r = (255,0,0)
      g = (0,0,255)
      o = (0,0,0)
  in
  [[r,g,o,o,o,o,g,g,o,o,o,o,g,g,g,g,o,g,g,o]
  ,[r,r,g,o,o,o,g,g,g,o,g,o,g,o,o,g,o,o,o,o]
  ,[o,r,o,o,g,g,g,g,o,g,g,o,g,o,o,g,g,g,o,o]
  ,[r,o,r,o,o,g,o,o,g,g,o,o,o,o,g,g,g,g,o,g]
  ,[r,r,r,o,r,o,o,o,g,o,g,o,g,o,g,o,g,g,g,g]
  ,[o,o,r,o,r,o,o,g,o,o,g,g,o,o,g,g,o,g,g,o]
  ,[o,o,r,r,o,o,r,o,o,o,g,o,o,o,o,g,g,o,g,o]
  ,[o,o,r,r,r,o,o,r,g,o,o,g,g,o,o,g,g,o,g,o]
  ,[r,r,r,r,o,o,r,r,o,g,g,o,g,o,o,g,o,g,o,g]
  ,[r,r,r,o,r,r,r,r,o,o,g,o,g,o,o,o,o,o,g,g]
  ,[r,o,o,r,o,r,r,r,r,o,o,o,o,g,o,g,g,g,g,g]
  ,[r,r,o,o,r,o,o,o,r,o,r,r,g,g,g,g,g,o,g,g]
  ,[r,r,r,r,o,o,o,o,r,o,r,o,o,o,g,o,o,g,o,g]
  ,[r,r,o,r,o,o,o,o,r,r,r,o,r,o,o,g,o,g,o,o]
  ,[r,r,r,o,r,r,r,o,r,r,r,o,o,r,r,o,o,g,g,g]
  ,[r,r,r,r,r,r,o,r,o,o,o,o,r,o,r,o,o,g,g,o]
  ,[r,r,o,r,r,o,r,o,r,r,o,r,r,o,o,o,o,o,o,o]
  ,[r,r,r,o,r,o,r,r,o,o,r,o,o,o,r,o,o,o,o,g]
  ,[o,r,o,r,o,r,o,r,r,o,o,o,o,o,o,o,o,o,o,g]
  ,[r,r,o,r,r,r,o,o,r,r,r,o,o,r,r,o,o,o,r,o]
  ]
start3 =
  let r = (255,0,0)
      g = (0,0,255)
      o = (0,0,0)
  in
  [[o,o,g,g,o,g,g,o,g,o,o,g,o,o,o,g,o,g,g,o,o,g,o,g,g,g,o,o,g,g,o,o,o,o,g,o,g,g,o,o]
  ,[o,r,g,o,o,o,o,g,g,g,o,g,g,g,g,o,g,g,g,o,g,g,g,o,o,g,o,g,g,g,o,g,g,g,g,o,o,o,g,g]
  ,[o,o,r,g,g,g,g,o,o,o,g,g,o,g,g,g,o,g,o,g,o,o,o,g,g,o,o,g,o,o,o,g,g,o,g,o,o,g,o,g]
  ,[o,o,r,r,g,o,o,g,o,o,g,o,g,g,g,g,g,o,o,g,g,g,o,o,o,g,g,o,o,o,o,o,o,o,o,g,o,o,g,o]
  ,[o,r,o,o,o,g,g,g,g,g,o,g,g,g,o,o,o,g,o,g,g,g,o,g,o,o,o,g,g,g,g,o,o,o,g,g,o,g,g,g]
  ,[r,o,o,r,o,r,o,o,g,g,o,o,g,g,o,o,o,g,g,o,g,g,o,g,o,g,g,g,g,g,o,g,g,g,o,o,o,g,o,g]
  ,[o,r,o,r,r,o,r,o,o,o,o,g,g,o,g,o,g,g,g,o,o,o,g,o,o,g,o,g,o,o,o,g,o,g,o,o,o,g,o,g]
  ,[r,o,o,r,o,o,r,r,o,g,o,g,g,g,o,o,o,g,g,g,o,g,g,g,g,o,o,o,o,o,o,o,o,o,g,g,g,g,o,o]
  ,[o,o,r,o,o,r,o,o,o,g,g,g,o,g,g,g,o,g,o,o,o,o,g,g,o,g,g,o,g,o,o,o,o,o,g,g,g,g,o,o]
  ,[o,o,r,r,r,o,o,o,o,o,o,g,g,g,g,o,g,g,g,g,o,o,o,g,g,g,g,o,o,g,g,g,o,o,g,g,o,o,o,o]
  ,[o,o,o,o,o,r,r,r,r,r,o,o,g,o,o,o,g,o,g,g,o,g,o,o,o,o,g,g,g,g,o,g,g,o,o,g,o,o,o,g]
  ,[o,r,r,r,r,o,r,o,r,o,r,r,o,o,g,g,o,g,o,o,g,g,o,g,o,g,g,o,o,o,o,g,o,g,o,g,o,g,o,g]
  ,[o,o,o,o,o,r,r,o,o,o,r,o,o,g,g,o,o,o,g,o,g,g,g,g,o,g,g,g,o,g,g,o,o,g,g,g,o,g,g,g]
  ,[r,o,o,r,o,r,o,o,r,r,r,o,r,r,o,g,g,o,o,g,g,o,o,o,g,o,g,o,o,o,g,o,o,o,o,g,g,o,g,o]
  ,[o,r,r,o,o,r,r,r,o,r,r,o,r,r,o,g,o,o,g,g,g,g,o,g,g,o,o,o,g,o,o,o,o,o,o,o,o,g,g,g]
  ,[r,o,o,r,r,o,o,o,r,r,r,r,o,o,o,o,g,g,o,g,o,g,g,o,g,g,o,o,g,g,g,o,o,o,o,o,o,g,g,g]
  ,[r,o,r,r,o,o,r,r,o,o,r,r,r,o,o,r,o,o,o,o,g,o,g,o,g,o,o,g,o,o,g,g,o,g,o,g,g,g,g,o]
  ,[o,r,r,r,r,o,o,o,r,r,o,r,o,r,r,r,r,o,g,o,g,o,o,g,g,o,g,g,o,g,g,o,o,g,g,g,g,o,o,g]
  ,[o,r,o,o,r,o,r,r,o,o,r,r,r,o,r,r,r,r,r,o,g,o,o,g,g,g,o,o,o,g,g,g,o,o,o,g,g,g,g,o]
  ,[o,r,r,o,r,o,r,o,r,r,r,o,r,r,o,o,o,r,r,o,o,o,g,o,o,o,g,g,o,o,o,o,g,g,o,o,o,g,o,g]
  ,[o,r,o,r,o,o,o,r,r,o,o,r,r,r,o,r,r,r,r,r,o,o,g,g,g,o,o,g,g,g,g,o,g,o,g,o,g,o,g,g]
  ,[o,r,r,r,r,o,r,r,o,o,o,r,o,r,r,o,r,o,o,o,o,r,g,o,o,o,o,g,o,g,g,g,g,g,o,o,g,o,g,g]
  ,[o,o,r,r,r,r,o,o,o,r,r,o,r,r,r,r,r,o,r,r,o,r,o,g,g,o,o,g,g,g,o,o,g,g,g,g,o,o,o,g]
  ,[r,o,r,r,r,o,r,r,o,r,r,o,r,r,o,o,o,o,r,o,r,o,o,o,o,o,g,g,o,g,g,o,o,g,g,o,g,g,o,o]
  ,[o,r,r,r,r,r,r,o,r,o,r,r,r,r,o,r,o,o,r,o,o,o,r,o,o,g,o,g,g,g,g,g,g,g,o,o,o,g,o,o]
  ,[r,o,o,r,r,r,r,r,r,o,o,o,o,o,r,o,o,r,o,o,o,r,o,o,r,r,o,g,o,g,g,o,o,o,g,g,g,g,g,g]
  ,[o,o,o,o,o,o,r,r,r,r,o,r,o,r,o,o,r,o,o,o,r,r,o,o,o,o,o,g,g,o,o,o,g,g,o,g,g,g,g,o]
  ,[r,o,r,o,o,o,o,o,r,o,r,o,r,o,o,r,r,r,o,r,r,o,o,o,o,o,r,r,g,g,o,g,g,o,g,g,o,g,o,o]
  ,[r,o,r,r,o,o,r,o,o,o,r,o,r,o,r,o,o,r,r,o,r,r,o,r,r,o,r,r,o,g,o,o,g,o,g,o,g,g,g,g]
  ,[r,o,r,r,r,r,o,r,o,o,r,r,r,r,r,r,r,o,o,o,o,o,o,r,o,o,o,r,r,o,o,o,g,g,o,g,o,o,o,o]
  ,[r,r,o,r,o,r,r,r,r,r,r,o,r,r,o,o,r,o,r,r,r,r,r,o,r,r,o,o,o,o,r,o,o,g,o,o,o,o,g,g]
  ,[r,o,o,r,o,o,r,o,r,r,o,r,o,r,o,o,r,r,r,r,r,r,o,o,o,o,o,o,o,o,r,r,o,o,g,o,o,g,o,o]
  ,[r,o,r,o,r,r,r,r,r,o,o,r,r,o,o,r,r,r,r,o,o,r,o,r,r,o,r,o,o,r,o,r,o,g,g,o,o,o,g,g]
  ,[r,r,o,o,o,r,r,r,o,o,r,o,r,r,o,r,o,r,o,o,r,r,r,o,o,o,r,r,r,o,r,r,o,o,o,o,o,o,g,g]
  ,[r,o,o,o,o,o,o,o,o,r,r,r,o,o,o,o,o,r,r,r,o,r,o,r,o,o,o,r,o,r,o,r,o,o,r,o,o,g,g,g]
  ,[r,r,r,o,r,r,o,o,o,r,o,r,r,r,o,r,r,o,o,r,r,r,r,r,o,r,r,r,r,o,r,o,r,o,r,o,o,o,g,o]
  ,[r,o,o,o,r,o,r,o,o,r,o,o,r,o,r,o,o,r,o,r,r,o,o,r,r,r,o,o,r,r,o,o,r,o,o,o,o,g,o,g]
  ,[o,r,r,o,o,o,o,r,o,o,o,o,o,r,r,r,o,o,o,r,r,o,o,o,r,o,o,o,o,o,r,r,o,o,r,o,r,r,g,o]
  ,[o,o,r,o,r,r,o,o,r,r,o,o,o,o,o,o,r,o,o,o,r,o,o,r,o,o,o,r,o,r,r,r,o,r,o,o,o,o,r,o]
  ,[r,r,o,r,o,o,r,o,o,r,r,o,o,r,r,r,o,o,r,o,o,r,o,o,r,o,o,o,o,o,r,o,r,o,o,r,r,o,r,o]
  ]


renderInPlace' :: Field (Bool, Cell) -> String
renderInPlace' field
  = concat
  $ concat
  $ zipWith (\y row -> zipWith (f y) [0..] row) [0..] field
  where
    f :: Int -> Int -> (Bool, Cell) -> String
    f y x (b, cell)
      | b = putAt x (y + 3) $ tile cell
      | otherwise = ""

main = do
  let fields = iterate step start3
      checkTile t0 t1 = (t0 == t1, t1)
      diffed = zipWith (overField checkTile) fields (tail fields)
  mapM_
    (putStr . renderInPlace')
    (fieldMap (True,) (head fields) : diffed)
  getLine

--main = mapM f $ iterate step start2
--  where
--    f field = do
--      putStrLn (render (fieldMap tile field))
--      getLine
