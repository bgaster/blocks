{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PackageImports #-}
module Blocks where

import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Applicative
import Control.Monad
import Control.Lens ((+~), (^.), contains)
import Data.Foldable (foldMap, traverse_)
import Data.Vinyl
import Data.Set (Set)
import Data.Vinyl.Universe ((:::), SField(..))
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil.Camera2D
import Graphics.Rendering.OpenGL
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.VinylGL
import Data.Vector.Storable (fromList)
import Linear (V1(..), V2(..), _x, M33)
import System.FilePath ((</>))
import FRP.Elerea.Simple
import Data.List (transpose)
import Data.Maybe
import Control.Concurrent (threadDelay)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import Data.Bool.Extras
import System.Random
import Graphics.Renderer.FontAtlas
import Data.Char
import Window

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
toDigits :: Int -> [Int]
toDigits x = let d = x `div` 10
                 m = x `mod` 10
             in if d == 0
                then [m]
                else toDigits d ++ [m]

--------------------------------------------------------------------------------
-- Board
--------------------------------------------------------------------------------

type BlockID = Int

noBlockID = 7 :: BlockID
blockIDI  = 6 :: BlockID
blockIDO  = 5 :: BlockID
blockIDS  = 4 :: BlockID
blockIDZ  = 3 :: BlockID
blockIDT  = 2 :: BlockID
blockIDJ  = 1 :: BlockID
blockIDL  = 0 :: BlockID

chooseBlock :: IO BlockID
chooseBlock = do
  bid <- getStdRandom (randomR (0, 6))
  return bid

-- Board starts noBlockID for cell

type Row = [BlockID]

type Board = [Row]

printBoard :: Board -> IO ()
printBoard = mapM_ (\row -> printRow row >> putStr "\n")
 where printRow = mapM_ (\v -> putStr (show v) >> putStr " ")

emptyRow :: Row
emptyRow = replicate 10 noBlockID

emptyBoard ::Board
emptyBoard = replicate 20 emptyRow

applyBlock :: Board -> Block -> Board
applyBlock = error ""

rowFull :: Row -> Bool
rowFull = all isBlock

isBlock :: BlockID -> Bool
isBlock = not . noBlock

noBlock :: BlockID -> Bool
noBlock = (noBlockID==)

-- Check for full rows and removes them, appending an empty line at the front
-- Returns updated board and number of lines deleted
updateRows :: Board -> (Board, Int)
updateRows board = (replicate n emptyRow ++ b, n)
  where (b, n) = foldr (\row (board, n) -> if rowFull row
                                           then (board, n+1)
                                           else (row : board, n) ) ([], 0) board

-- Adopt the OriginalNintendo Scoring System
-- Number of Lines:   1                 2                   3                   4
--                   40 * (level + 1)   100 * (level + 1)   300 * (level + 1)   1200 * (level + 1)
-- 
-- Returns the number of points given the number of rows and level
calPoints :: Int -> Int -> Int
calPoints lines level = ([40, 100, 300, 1200] !! (lines-1)) * (level + 1)

levelUp :: Int -> Int
levelUp linesCompleted = bool (bool (1 + ((linesCompleted - 1) `div` 10)) 10 (linesCompleted > 90))
                              1 (linesCompleted == 0)

type Block = [[BlockID]]

{-
 As we know the size of each of our blocks we should really define them using dependently
 typed matrixes and then well typed transforms. This would lead to a bit more work on
 apply to board functions as blocks has different sizes.

 can come back to look at this later...
-}

--------------

pivit :: Pos
pivit = (2,1)

blockI0 = [ [noBlockID, noBlockID, noBlockID, noBlockID],
            [blockIDI, blockIDI, blockIDI, blockIDI],
            [noBlockID, noBlockID, noBlockID, noBlockID],
           [noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockI1 = [ [noBlockID, noBlockID, blockIDI, noBlockID],
             [noBlockID, noBlockID, blockIDI, noBlockID],
             [noBlockID, noBlockID, blockIDI, noBlockID],
             [noBlockID, noBlockID, blockIDI, noBlockID] ] :: Block

blockJ0 = [ [ noBlockID, noBlockID, noBlockID, noBlockID],
            [ noBlockID, blockIDJ, blockIDJ, blockIDJ],
            [ noBlockID, noBlockID, noBlockID, blockIDJ],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockJ1 = [ [ noBlockID, noBlockID, blockIDJ, blockIDJ],
            [ noBlockID, noBlockID, blockIDJ, noBlockID],
            [ noBlockID, noBlockID, blockIDJ, noBlockID],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockJ2 = [ [ noBlockID, blockIDJ, noBlockID, noBlockID],
            [ noBlockID, blockIDJ, blockIDJ, blockIDJ],
            [ noBlockID, noBlockID, noBlockID, noBlockID],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockJ3 = [ [ noBlockID, noBlockID, blockIDJ, noBlockID],
            [ noBlockID, noBlockID, blockIDJ, noBlockID],
            [ noBlockID, blockIDJ, blockIDJ, noBlockID],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockL0 = [ [ noBlockID, noBlockID, noBlockID, noBlockID],
            [ noBlockID, blockIDL, blockIDL, blockIDL],
            [ noBlockID, blockIDL, noBlockID, noBlockID],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockL1 = [ [ noBlockID, noBlockID, blockIDL, noBlockID],
            [ noBlockID, noBlockID, blockIDL, noBlockID],
            [ noBlockID, noBlockID, blockIDL, blockIDL],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockL2 = [ [ noBlockID, noBlockID, noBlockID, blockIDL],
               [ noBlockID, blockIDL, blockIDL, blockIDL],
               [ noBlockID, noBlockID, noBlockID, noBlockID],
               [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockL3 = [ [ noBlockID, blockIDL, blockIDL, noBlockID],
            [ noBlockID, noBlockID, blockIDL, noBlockID],
            [ noBlockID, noBlockID, blockIDL, noBlockID],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockO0 = [ [ noBlockID, noBlockID, noBlockID, noBlockID],
            [ noBlockID, blockIDO, blockIDO, noBlockID],
            [ noBlockID, blockIDO, blockIDO, noBlockID],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockS0 = [ [ noBlockID, noBlockID, noBlockID, noBlockID],
            [ noBlockID, noBlockID, blockIDS, blockIDS],
            [ noBlockID, blockIDS,  blockIDS, noBlockID],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockS1 = [ [ noBlockID, noBlockID, blockIDS, noBlockID],
            [ noBlockID, noBlockID, blockIDS, blockIDS],
            [ noBlockID, noBlockID,  noBlockID, blockIDS],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockT0 = [ [ noBlockID, noBlockID, noBlockID, noBlockID],
            [ noBlockID, blockIDT, blockIDT, blockIDT],
            [ noBlockID, noBlockID, blockIDT, noBlockID],

            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockT1 = [ [ noBlockID, noBlockID, blockIDT, noBlockID],
            [ noBlockID, noBlockID, blockIDT, blockIDT],
            [ noBlockID, noBlockID, blockIDT, noBlockID],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockT2 = [ [ noBlockID, noBlockID, blockIDT, noBlockID],
            [ noBlockID, blockIDT, blockIDT, blockIDT],
            [ noBlockID, noBlockID, noBlockID, noBlockID],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockT3 = [ [ noBlockID, noBlockID, blockIDT,  noBlockID],
            [ noBlockID, blockIDT,  blockIDT,  noBlockID],
            [ noBlockID, noBlockID, blockIDT,  noBlockID],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block            

blockZ0 = [ [ noBlockID, noBlockID, noBlockID, noBlockID],
            [ noBlockID, blockIDZ, blockIDZ, noBlockID ],
            [ noBlockID, noBlockID,  blockIDZ, blockIDZ ],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

blockZ1 = [ [ noBlockID, noBlockID, noBlockID, blockIDZ],
            [ noBlockID, noBlockID, blockIDZ, blockIDZ ],
            [ noBlockID, noBlockID,  blockIDZ, noBlockID ],
            [ noBlockID, noBlockID, noBlockID, noBlockID] ] :: Block

-- next two functions calulate the number of rows and columns that are left, right, 
leftRight :: Block -> (Int, Int)
leftRight b = (minimum $ map (length . takeWhile noBlock . take 2) b,
        minimum $ map (length . dropWhile (not . noBlock) . drop 3) b)
       

upDown :: Block -> (Int, Int)
upDown = (\b -> (length $ takeWhile (==True) b,
                length $ dropWhile (==False) $ dropWhile (==True) b)) . map (all noBlock)

isBlockOnBoard :: Pos -> Block -> Bool
isBlockOnBoard (x,y) b = let (l,r)   = leftRight b
                             (u,d)   = upDown b
                             (px,py) = pivit
                         in (y - (1 - u) >= 0) && (y + (2 - d) <= 19) &&
                            (x - (2 - l) >= 0) && (x + (1 - r) <= 9)

-- Precompute all blocks and their corresponding rotations
blocks :: [ [Block] ]
blocks = [ [ blockL0, blockL1, blockL2, blockL3 ],
           [ blockJ0, blockJ1, blockJ2, blockJ3 ],
           [ blockT0, blockT1, blockT2, blockT3 ],
           [ blockZ0, blockZ1, blockZ0, blockZ1 ],
           [ blockS0, blockS1, blockS0, blockS1 ],
           [ blockO0, blockO0, blockO0, blockO0 ],
           [ blockI0, blockI1, blockI0, blockI1 ] ]

rotatedBlock :: BlockID -> Int -> Block
rotatedBlock blockID rotation = (blocks !! blockID) !! rotation

-- place a block on an empty board at position (x,y)
-- if the block cannot be placed at the specifed position fail
placeBlockEmptyBoard' p@(x,y) id row =
  let (l,r) = leftRight block
      block = rotatedBlock id row
      (u,d) = upDown block
      b     = replicate ((y - 1) + u) emptyRow
      a     = replicate ((19 - y - 2) + d) emptyRow
      bs  = map (\row -> replicate ((x - 2) + l) noBlockID ++
                         take ((4-l) - r) (drop l row) ++
                         replicate ((9 - x - 1) + r) noBlockID) (drop u (take (4 - d) block))
  in if isBlockOnBoard p block
     then Just (b ++ bs ++ a)
     else Nothing

data Update = UPlace | UReplace
  deriving (Eq, Show)

-- combine two boards 
-- the idea is that we have board with single (i.e. current) block placed
-- and then we simply try and map that board over the current playing board
-- returns a merged board if the current piece can be placed correclty, otherwise
-- fails to construct a board
-- we actually have two modes, place and replace
overlayBoard :: Update -> Maybe Board -> Maybe Board -> Maybe Board
overlayBoard replace (Just b) (Just b') = zipWithM (zipWithM (f replace)) b b'
  where 
        f UPlace v v' = if noBlock v && not (noBlock v')
                        then Just v'
                        else if isBlock v'
                             then Nothing
                             else Just v
                                 
        f UReplace v v' = if isBlock v'
                          then Just noBlockID
                          else Just v

overlayBoard _       _        _         = Nothing

data Move = MoveL | MoveR | MoveD
   deriving (Show, Eq)
            
type Pos = (Int, Int)

inBoard :: Pos -> Bool
inBoard (x,y) = (0 <= x && x < 10) && (0 <= y && y < 20)

initialPosition :: BlockID -> Pos
initialPosition id = (5,0)

iterationDelay :: Int -> Double
iterationDelay level = (11.0 - (fromIntegral level)) * 0.05

block     = SField :: SField ("block"    ::: BlockID) -- current block
nblock    = SField :: SField ("nblock"   ::: BlockID) -- next block
rotation  = SField :: SField ("rotation" ::: Int)     -- current rotation
pos       = SField :: SField ("position" ::: Pos)     -- current position
board     = SField :: SField ("board"    ::: Board)   -- current board
score     = SField :: SField ("score"    ::: Int)     -- current score
nlines    = SField :: SField ("nlines"   ::: Int)     -- number of complete lines
idelay    = SField :: SField ("idelay"   ::: Double)  -- delay between drop
ticks     = SField :: SField ("ticks"    ::: Double)  -- ticks since start
frames    = SField :: SField ("frames"   ::: Double)  -- number of frames processed

type World' = ["block" ::: BlockID,
               "nblock" ::: BlockID,
               "rotation" ::: Int,
               "position" ::: Pos,
               "board" ::: Board,
               "score" ::: Int,
               "nlines" ::: Int,
               "idelay" ::: Double,
               "ticks" ::: Double,
               "frames" ::: Double]

type World = PlainFieldRec World' 

mkWorld :: BlockID ->
           BlockID ->
           Int ->
           Pos ->
           Board ->
           Int ->
           Int ->
           Double ->
           Double ->
           Double ->
           World
mkWorld id nid r p b s l d t f =
  block =: id <+> nblock =: nid <+> rotation =: r <+> pos =: p <+>
  board =: b <+> score =: s <+> nlines =: l <+>
  idelay =: d <+> ticks =: t <+> frames =: f

initialWorld :: BlockID -> BlockID -> World
initialWorld bid nbid = mkWorld bid nbid 0 (initialPosition bid)
                           (fromJust $ overlayBoard UPlace (Just emptyBoard) $
                            placeBlockEmptyBoard' (initialPosition bid) bid 0) 0 0
                            (iterationDelay 1) 0.0 0.0

--------------------------------------------------------------------------------
-- Graphics Stuff
--------------------------------------------------------------------------------

type Point2D = V2 GLfloat
type UV      = Point2D

type VPos  = "vertexCoord" ::: Point2D
type Tex  = "texCoord"    ::: Point2D

vpos :: SField VPos
vpos = SField

tex :: SField Tex
tex = SField

tex_width  = (1 / 8) :: GLfloat
tex_height = 64 :: GLfloat

calcTex offset =
  [[V2 (offset * tex_width) 1, V2 (offset') 1, V2 (offset * tex_width) 0],
   [V2 (offset * tex_width) 0, V2 (offset') 0, V2 (offset') 1]]
  where
    offset' = tex_width + offset * tex_width

noBlockTex = calcTex 7 :: [[UV]]
blockTexI  = calcTex 6 :: [[UV]]
blockTexO  = calcTex 5 :: [[UV]]
blockTexS  = calcTex 4 :: [[UV]]
blockTexZ  = calcTex 3 :: [[UV]]
blockTexT  = calcTex 2 :: [[UV]]
blockTexJ  = calcTex 1 :: [[UV]]
blockTexL  = calcTex 0 :: [[UV]]

blockTex = [ blockTexI, blockTexO, blockTexS, blockTexZ,
             blockTexT, blockTexJ, blockTexL, noBlockTex]

-- this should move to a library as it can be part of the 2D engine
square :: GLfloat -> GLfloat -> [[Point2D]]
square x y = [[V2 (x * cell_width) (y * cell_height + cell_height),
               V2 (x * cell_width + cell_width) (y * cell_height + cell_height),
               V2 (x * cell_width) (y * cell_height)],
              
              [V2 (x * cell_width) (y * cell_height),
               V2 (x * cell_width + cell_width) (y * cell_height),
               V2 (x * cell_width + cell_width) (y * cell_height + cell_height)
               ]
              ]
   where
     cell_width :: GLfloat
     cell_width = 1.0 / 16

     cell_height :: GLfloat
     cell_height = 1.0 / 20


createScore :: Int -> [Char]
createScore s = let digits = take 5 $ toDigits s
                in (take (5 - (length digits)) ['0','0','0','0','0']) ++
                   (map (\c -> toEnum $ c + 48) digits)

-- As we only want to print the minimum number of digits we
-- generate verts least digit first and then we simply draw the required
-- number of triangles. Score limited to 5 digits!!
scoreText :: GLfloat  ->  
             GLfloat  ->
             CharInfo ->
             [Char] ->
             IO (BufferedVertices [VPos,Tex])
scoreText x y offsets digits = bufferVertices $ scoreText' x y offsets digits 

scoreText' :: GLfloat  ->  
              GLfloat  ->
              CharInfo ->
              [Char] ->
              [PlainFieldRec [VPos,Tex]]
scoreText' x y offsets (d1:d2:d3:d4:d5:_) = vt
  where (o1,o1',h1) = charToOffsetWidthHeight offsets d1
        (o2,o2',h2) = charToOffsetWidthHeight offsets d2
        (o3,o3',h3) = charToOffsetWidthHeight offsets d3
        (o4,o4',h4) = charToOffsetWidthHeight offsets d4
        (o5,o5',h5) = charToOffsetWidthHeight offsets d5
        vt :: [PlainFieldRec [VPos,Tex]]
        vt = concat $ concat ps 
        f (sq, t) = zipWith (zipWith (\pt uv -> vpos =: pt <+> tex =: uv)) sq t
        
        ps = map f [
          (square x y, [[V2 o1 0, V2 o1' 0, V2 o1 h1],
                        [V2 o1 h1, V2 o1' h1, V2 o1' 0]]),
          (square (x+1) y, [[V2 o2 0, V2 o2' 0, V2 o2 h2],
                           [V2 o2 h1, V2 o2' h1, V2 o2' 0]]),
          (square (x+2) y, [[V2 o3 0, V2 o3' 0, V2 o3 h3],
                           [V2 o3 h1, V2 o3' h1, V2 o3' 0]]),
          (square (x+3) y, [[V2 o4 0, V2 o4' 0, V2 o4 h4],
                           [V2 o4 h1, V2 o4' h4, V2 o4' 0]]),
          (square (x+4) y, [[V2 o5 0, V2 o5' 0, V2 o5 h5],
                           [V2 o5 h5, V2 o5' h5, V2 o5' 0]]) ]

-- Generate the triangles for the board, this is done just once
-- If we used a SOA VOA, then we could upload this once and never again
graphicsBoard :: [[Point2D]]
graphicsBoard = concat $ concat $ b
  where b = map (\y -> map (\x -> square x y) [0..9])
                [19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0]

initialGraphicsBoard :: Board -> [PlainFieldRec [VPos,Tex]]
initialGraphicsBoard =
  concat . zipWith (zipWith (\pt uv -> vpos =: pt <+> tex =: uv)) graphicsBoard . boardToTex

--boardToTex :: Board -> [[ [[UV]] ]]
boardToTex = concat . map (foldr (\a b -> (blockTex!!a) ++ b) [])

boardVerts :: Board -> IO (BufferedVertices [VPos,Tex])
boardVerts = bufferVertices . initialGraphicsBoard 

type GLInfo = PlainFieldRec '["cam" ::: M33 GLfloat]

loadTextures :: [FilePath] -> IO [TextureObject]
loadTextures = fmap (either error id . sequence) . mapM aux
  where aux f = do img <- readTexture ("resources" </> f)
                   traverse_ (const texFilter) img
                   return img
        texFilter = do textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
                       texture2DWrap $= (Repeated, ClampToEdge)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False

isKeyPressed :: TQueue EventKey -> IO (Bool, Bool, Bool, Bool, Bool)
isKeyPressed kq = do
  k <- atomically $ tryReadTQueue kq
  maybe (return (False, False, False, False, False))
        (\(EventKey win k scancode ks mk) ->
          if isPress ks
          then case k of
               Key'Left   -> return (True, False, False, False, False)
               Key'Right  -> return (False, True, False, False, False)
               Key'Up     -> return (False, False, True, False, False)
               Key'Down   -> return (False, False, False, True, False)
               Key'Q      -> return (False, False, False, False, True)
               Key'Escape -> return (False, False, False, False, True)
               _          -> return (False, False, False, False, False)
          else return (False, False, False, False, False)) k

calPos :: Bool -> Bool -> Pos -> Pos
calPos l r (x,y) = let x' = bool x (x-1) l
                    in (bool x' (x'+1) r, y)

calRot :: Int -> Bool -> Int
calRot r = bool r ((r + 1) `mod` 4)

{- Game logic is pretty simple:

   1. Check if quit, if so then clean up and exit

   2. If game is not game over
      1. Check if Left, Right, or Rotate key's pressed, if so
           See if movement is valid (include move down with gravity) and update board if so
           Otherwise, check to see if can move down and update board if so
           Otherwise, introduce new piece
      2. Check if any rows have been completed and remove and shuffle down, adding new rows at top, if necessary
      3. Generate updated verts and texture and upload to GL
   3. Renderer frame (this is not done in play function)
   4. Continue (loop) to next frame

-}

-- add drop key support
           
-- return nothing if QUIT (and for the moment gameover)
play :: BufferedVertices [VPos, Tex] -> UI -> World -> IO (Maybe World)
play verts ui world = do
  (l,r,u,d, q) <- isKeyPressed (keys ui) --isKeyPressed' ui

  let d = (world ^. rLens idelay) - (timeStep ui) 
      yadd = bool 0 1 (d <= 0)

  if q -- QUIT
  then return Nothing
  else do let (x,y)   = world ^. rLens pos
              (x',y') = calPos l r (x,y)
              y''     = y'+ yadd
              rot     = world ^. rLens rotation
              rot'    = calRot rot u
              bid     = world ^. rLens block
              nbid    = world ^. rLens nblock
                    
              -- try and place updated piece (including any user movement)
              b       = overlayBoard UReplace (Just $ world ^. rLens board) $
                                                        placeBlockEmptyBoard' (x, y) bid rot

              pb      = placeBlockEmptyBoard' (x',y'') bid rot'
              ub      = overlayBoard UPlace b pb

              nd      = bool d (iterationDelay (levelUp (world ^. rLens nlines))) (d <= 0)
          if isJust ub -- can piece be placed, including user movement
          then do reloadVertices verts (fromList $ initialGraphicsBoard $ fromJust ub)
                  return $ Just $ mkWorld bid nbid rot' (x',y'') (fromJust ub) 
                                               (world ^. rLens score) (world ^. rLens nlines) 
                                               nd
                                               (world ^. rLens ticks) ((world ^. rLens frames) + 1)
          else do let pb' = placeBlockEmptyBoard' (x, y + yadd) bid rot
                      ub' = overlayBoard UPlace b pb'
                  if isJust ub'
                  then do reloadVertices verts (fromList $ initialGraphicsBoard $ fromJust ub')
                          return $ Just $ mkWorld bid nbid rot' (x, y + yadd) (fromJust ub') 
                                               (world ^. rLens score) (world ^. rLens nlines) 
                                               nd
                                               (world ^. rLens ticks) ((world ^. rLens frames) + 1)
                  else do let (upb, nls) = updateRows $ world ^. rLens board
                              s = world ^. rLens score
                              nl = (world ^. rLens nlines) + nls
                          s' <- if nls > 0 
                                then do let l = levelUp nl
                                            ss = (s + calPoints nls l)
                                        return ss
                                else return s
                          nbid' <- chooseBlock
                          let (nx,ny) = initialPosition nbid
                              npb = placeBlockEmptyBoard' (nx,ny) nbid 0
                              nb  = overlayBoard UPlace (Just $ upb) npb
                          if isJust nb
                          then do reloadVertices verts (fromList $ initialGraphicsBoard $ fromJust nb)
                                  return $ Just $ mkWorld nbid nbid' 0 (nx,ny) (fromJust nb) 
                                                      s' nl
                                                      nd
                                                      (world ^. rLens ticks) ((world ^. rLens frames) + 1)

                          else return Nothing -- Gameover                

renderer :: World -> IO (GLInfo -> World -> UI -> IO (Maybe World))
renderer iworld = do
   ts <- simpleShaderProgram ("shaders"</>"text.vert") ("shaders"</>"text.frag")
   s  <- simpleShaderProgram ("shaders"</>"piece.vert") ("shaders"</>"piece.frag")
   
   [blocks] <- loadTextures ["blocks.png"]
   putStrLn "Loaded shaders"
   setUniforms s (texSampler =: 0)
   nbid <- chooseBlock
   verts <- boardVerts (iworld ^. rLens board)
   indices <- bufferIndices [0..(2 * 10 * 20 * 3)]
   vao <- makeVAO $ do
     enableVertices' s verts
     bindVertices verts
     bindBuffer ElementArrayBuffer $= Just indices
     
   (chars, offsets)  <- createAtlas ("resources"</>"ArcadeClassic.ttf") 48 1   
   setUniforms ts (texSampler =: 1)
   tverts   <- scoreText 11 18 offsets $ createScore 0
   tindices <- bufferIndices [0 .. 2 * 3 * 5]
   tvao <- makeVAO $ do
     enableVertices' ts tverts
     bindVertices tverts
     bindBuffer ElementArrayBuffer $= Just tindices

   return $ \i world ui -> do

     w <- play verts ui world

     if isJust w
     then do 
             currentProgram $= Just (program s)
             setUniforms s i
             withVAO vao . withTextures2D [blocks] $ drawIndexedTris (2 * 10 * 20)
             currentProgram $= Just (program ts)
             setUniforms ts i

             -- TODO: we should really only upload new score verts when it changes
             --       this needs to be moved into play
             reloadVertices tverts
                            (fromList $ scoreText' 11 18 offsets $ createScore $ 
                                        ((fromJust w) ^. rLens score))
             withVAO tvao . withTextures2D [chars] $ drawIndexedTris (2*5)
             return w
     else return w
             
   where
     texSampler = SField :: SField ("tex" ::: GLint)

loop :: IO UI -> World -> IO ()
loop tick world = do
  clearColor $= Color4 0.00 0.1 0.1 1
  r <- Blocks.renderer world
  go camera2D world r
  where go :: Camera GLfloat -> World -> (GLInfo -> World -> UI -> IO (Maybe World)) -> IO ()
        go c world draw = do
          ui <- tick
          clear [ColorBuffer, DepthBuffer]
          let mCam  = camMatrix c
              info  = SField =: mCam
              cells = [0,0,0] 
          world' <- draw info world ui
          if isNothing world'
          then return ()
          else do --let world'' = (ticks `rPut` (((fromJust world') ^. rLens ticks) + timeStep ui) ) (fromJust world')
                  --fps = (world'' ^. rLens frames) / ((world'' ^. rLens ticks))
                  --print ("FPS: " ++ show fps)
                  swapBuffers (window ui) >> go c (fromJust world') draw

main :: IO ()
main = do
  let width  = 580
      height = 960
  tick      <- initGL "ABC or Another Blocks Clone" width height
  bid       <- chooseBlock
  nbid      <- chooseBlock  
  loop tick $ initialWorld bid nbid
  return ()
