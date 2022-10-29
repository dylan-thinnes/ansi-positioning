{-# LANGUAGE RecordWildCards #-}
import Data.List (intercalate, permutations, nub, inits)
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

-- conway
combine = zipWith . zipWith
rot n xs = take (length xs) $ drop (length xs + n) (cycle xs)
rot2 x y = rot y . map (rot x)
neighbours field =
  foldr1 (combine (+))
    [ rot2 x y field
    | x <- [-1,0,1]
    , y <- [-1,0,1]
    , x /= 0 || y /= 0
    ]
living = map (map (fromEnum . (== 1)))
step field = combine f field (neighbours (living field))
  where f _ 3 = 1
        f 0 _ = 0
        f 1 2 = 1
        f n _ = n + 1

-- print
pretty :: [[Integer]] -> String
pretty = unlines . (map . concatMap) (\n -> tile $ fromBlack (256,0,128) $ sqrt $ 1 / fromIntegral n)

play :: [[Integer]] -> IO ()
play field = do
  putStrLn "\ESC[2J"
  putStrLn $ pretty field
  _ <- getLine
  play $ step field

-- fields
glider x y = extendY y $ extendX x <$> [[0,1,0],[0,0,1],[1,1,1]]

extendX n row = row ++ replicate (n - length row) 0
extendY n rows = rows ++ replicate (n - length rows) (replicate (length $ head rows) 0)

fromFile path = do
  src <- lines <$> readFile path
  let parsed = (map . map) (fromIntegral . fromEnum . (/= ' ')) src
  let extended = map (extendX (maximum $ map length parsed)) parsed
  pure extended

--rgb = [(r,g,b) | let x = [0,128,255], r <- x, g <- x, b <- x, r /= g || g /= b || b /= r]
rgb = let toTriple [r,g,b] = (r,g,b) in map toTriple (permutations [255,128,0]) ++ map toTriple (nub $ permutations [255,255,0])
blends = [linear rgb1 rgb2 | rgb1 <- rgb, rgb2 <- rgb, rgb1 /= rgb2]
tileLine blender = concatMap tile $ blender <$> [0,0.01..1]
--main = putStrLn $ unlines $ tileLine <$> blends
blend2D t u u' v = let x = [0,0.01..1] in zipWith (\s e -> linear s e <$> x) (linear t u <$> x) (linear u' v <$> x)
--main = putStrLn $ unlines $ map (concatMap tile) $ blend2D (255,255,255) (255,0,255) (0,0,255) (0,255,0)

toPos :: Int -> Int -> String
toPos x y = ansi "H" [x,y]
putAt :: Int -> Int -> String -> String
putAt x y msg = ansi "s" [] ++ toPos x y ++ msg ++ ansi "u" []

puts =
  [(10,10,(255,128,0))
  ,(10,12,(255,0,128))
  ]
--main = putStrLn $ concatMap (\(x, y, c) -> putAt x y $ tile c) puts

--data LangtonState = LangtonState
--  { antPos :: (Int, Int)
--  , antDir :: (Int, Int)
--  , field :: M.Map (Int, Int) Bool
--  }
--
--stepLangton :: LangtonState -> (String, LangtonState)
--stepLangton LangtonState {..} =
--  let posColor = fromMaybe False $ M.lookup antPos field
--      newField = M.insert antPos (not posColor) field
--      newAntDir
--        | posColor  = (negate $ snd antDir, fst antDir)
--        | otherwise = (snd antDir, negate $ fst antDir)
--      newAntPos@(newY, newX) =
--        let (dy, dx) = newAntDir
--            (y, x) = antPos
--        in
--        (y + dy, x + dx)
--      ansi = toPos 0 0 ++ putAt newX newY (tile $ if not posColor then (255,0,0) else (0,0,255))
--  in
--  (ansi, LangtonState newAntPos newAntDir newField)
--
--main = f 0 (LangtonState (40,40) (1,0) M.empty)
--  where
--    f n state = do
--      let (msg, state') = stepLangton state
--      putStr msg
--      if n < 11000 then f (n + 1) state' else pure ()

data Rotation = R | L deriving (Show, Eq, Ord)
data LangtonState = LangtonState
  { antPos :: (Int, Int)
  , antDir :: (Int, Int)
  , antStates :: [((Int, Int, Int), Rotation)]
  , field :: M.Map (Int, Int) Int
  }

stepLangton :: LangtonState -> (String, LangtonState)
stepLangton LangtonState {..} =
  let posColor = fromMaybe 0 $ M.lookup antPos field
      newColor = succ posColor `mod` length antStates
      newField = M.insert antPos newColor field
      newAntDir@(dy, dx)
        | snd (antStates !! posColor) == R = (negate $ snd antDir, fst antDir)
        | otherwise = (snd antDir, negate $ fst antDir)
      (y, x) = antPos
      newAntPos@(newY, newX) = (y + dy, x + dx)
      ansi = toPos 0 0 ++ putAt x y (tile $ fst $ antStates !! newColor) -- ++ putAt newX newY (tileS (fst $ antStates !! newColor) "X")
  in
  (ansi, LangtonState newAntPos newAntDir antStates newField)

--llrr = [((255,0,0), L), ((0,0,255), L), ((0,255,0), R), ((0,128,255), R)]
--rlr = [((255,0,0), R), ((0,0,255), L), ((0,255,0), R)]
--rl = [((255,0,0),R), ((0,0,255),L)]

colors = do
  options <- inits [0, 255,128,64,32,16,8,4,2]
  r <- options
  g <- options
  b <- options
  guard $ r /= g || g /= b || b /= r
  pure (r, g, b)

mkSpec = zip colors . map (\c -> if c `elem` "Ll" then L else R)

llrr = mkSpec "llrr"
rlr = mkSpec "rlr"
rl = mkSpec "rl"
lrrrrrllr = mkSpec "lrrrrrllr"
llrrrlrlrllr = mkSpec "llrrrlrlrllr"
rrlllrlllrrr = mkSpec "rrlllrlllrrr"

--main = f 0 (LangtonState (240,120) (1,0) rrlllrlllrrr M.empty)
main = do
  [nRaw, specRaw] <- getArgs
  let n = read nRaw
  let spec = mkSpec specRaw
  let f i state = do
        -- getLine
        let (msg, state') = stepLangton state
        putStr msg
        if i > n
           then getLine >> f 0 state'
           else f (i + 1) state'
  f 0 (LangtonState (100,100) (1,0) spec M.empty)

