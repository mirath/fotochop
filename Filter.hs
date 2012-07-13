module Filter (Filter) where

import Data.Array as Arr
import Data.Foldable as F
import GHC.Float
import Data.Ix
import Data.Int
import Data.Sequence as Seq
import Data.Word
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Monad (fmap)

type Footprint = Seq.Seq (Int,Int)

type Col = (Word8,Word8,Word8,Word8)

type Pos = (Int,Int)

type Neighborhood = Seq.Seq (Pos,Col)

type Img = Array (Int,Int) Col

perPixelFunction :: (Pos,Col) -> (Pos,Col)
perPixelFunction (p,c) = (p,c)
  
combiner :: Col -> (Pos,Col) -> Col
combiner ca (p,cb) = sumColors ca cb

combinerBaseColor :: Col
combinerBaseColor = (0,0,0,0)

postprocesser :: Neighborhood -> Col -> Img -> Col
postprocesser n c _ =
  funColor (div) c (toWord8 ne,toWord8 ne,toWord8 ne,toWord8 ne)
    where ne = Seq.length n

mapNeighborhood :: ((Pos,Col) -> (Pos,Col)) ->
                  Footprint -> Img -> Pos -> Neighborhood
mapNeighborhood f fp img pos = fmap f neigh
  where neigh = neighborhood fp pos img

combine :: (Col -> (Pos,Col) -> Col) ->
           Col -> Neighborhood -> Col
combine f cb neigh = F.foldl f cb neigh
           
postprocessing :: (Neighborhood -> Col -> Img -> Col) ->
                  Neighborhood -> Col -> Img -> Col
postprocessing f neigh cf img = f neigh cf img

class Filter f where  
  filterPixel :: f -> Img -> Pos -> Col
                 
  filterImage :: f -> Img -> Img

neighborhood :: Footprint -> Pos -> Img -> Neighborhood
neighborhood fp offset img = F.foldl addToPixels Seq.empty fp
  where wrap = (wrapExtend (bounds img)).(sumTuples offset)
        addToPixels s p = s |> (p, ((!) img (wrap p)))

sumTuples :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumTuples (a,b) (c,d) = (a+c,b+d)

sumColors :: Col -> Col -> Col
sumColors  = funColor (+)

funColor :: (Word8 -> Word8 -> Word8) -> Col -> Col -> Col
funColor f (a,b,c,d) (a',b',c',d') =
  (cc (f a a') , cc (f b b') , cc (f c c') , cc (f d d'))
  where cc = clamp8 0 255
        
wrapExtend :: ((Int,Int),(Int,Int)) -> (Int,Int) -> (Int,Int)
wrapExtend ((lx,ly),(hx,hy)) (x,y) = (clampX x, clampY y)
  where clampX = clamp lx hx
        clampY = clamp ly hy

clamp :: Int -> Int -> Int -> Int
clamp low high n = min high $ max low n

clamp8 :: Word8 -> Word8 -> Word8 -> Word8
clamp8 low high n = min high $ max low n

toWord8 :: Int -> Word8
toWord8 a = fromIntegral a

-- Test functions
squareFootprint :: Int -> Int -> Footprint
squareFootprint w h = Seq.fromList [(x,y)| x <- [-h..h] , y <- [-w..w]]
  where w2 = w `div` 2
        h2 = h `div` 2

circularFootprint :: Float -> Footprint
circularFootprint r = Seq.fromList
                      [(ceiling x,ceiling y)|
                       x <- [-r2..r2] , y <- [-r2..r2] ,
                       d (x,y) <= r ]
  where d = distF (0.0,0.0)
        r2 = r/2.0
        

testImage :: Int -> Int -> Array (Int,Int) Col
testImage w h = array ((0,0),(h-1,w-1)) (sampleImg1Bindings w h)

sampleImg1Bindings :: Int -> Int -> [((Int,Int),Col)]
sampleImg1Bindings w h =
  [((x,y) , (c (x,y), 0, c (x,y), 255)) |
   x<-[0..(h-1)] , y<-[0..(w-1)]]
  where c = dist (0,0)

dist :: (Int,Int) -> (Int,Int) -> Word8
dist (x,y) (x',y') = floor $ sqrt $ int2Float $ (x-x')^2 + (y-y')^2

distF :: (Float,Float) -> (Float,Float) -> Float
distF (x,y) (x',y') = sqrt $ (x-x')**2 + (y-y')**2

-- Image drawing functions

showImage :: Array (Int,Int) (Word8,Word8,Word8) ->
             Array (Int,Int) (Word8,Word8,Word8) -> 
             IO ()
showImage img1 img2  =
  let (_ , (w1,h1)) = bounds img1
      (_ , (w2,h2)) = bounds img2
      winWidth = w1+w2+2
      winHeight = max (h1+1) (h2+1)
      offset1@(off1x,off1y) = ((fromIntegral (-winWidth `div` 2) :: Int),fromIntegral (-winHeight `div` 2) :: Int)
      offset2               = (off1x+w1+1,off1y+h1+1) in
  do
    (progname,_) <- getArgsAndInitialize
    initialDisplayMode $= [Graphics.UI.GLUT.DoubleBuffered]
    createWindow "Fotochop 0.1"
    windowSize $= Size (fromIntegral winWidth :: Int32) (fromIntegral winHeight :: Int32)
    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer]
    displayCallback $= (drawImages img1 img2 (0,0) (winWidth,winHeight))
    mainLoop

drawPoint' :: (Int,Int) -> (Int,Int) -> ((Int,Int),(Word8,Word8,Word8)) -> IO ()
drawPoint' winSize imgSize (pos,color) = drawPoint winSize imgSize color pos

drawPoint :: (Int,Int) -> (Int,Int) -> (Word8,Word8,Word8) -> (Int,Int) -> IO ()
drawPoint (winWidth,winHeight) (imgWidth,imgHeight) (r,g,b) pos@(x,y) = do
  color $ (Color4 r g b 255 :: Color4 Word8)
  vertex $ Vertex3 xcoor ycoor 0.0
    where iw2 = (int2Float imgWidth)
          ih2 = (int2Float imgHeight)/2.0
          xcoor = ((int2Float x)/iw2 - 1.0)
          ycoor = ((int2Float y)/ih2 - 1.0)

drawImages :: Array (Int,Int) (Word8,Word8,Word8) -> 
              Array (Int,Int) (Word8,Word8,Word8) -> 
              (Int,Int) -> (Int,Int) ->  IO ()
drawImages img1 img2 offset1 winSize@(winWidth,winHeight) = 
  let (_ , (w1,h1)) = bounds img1
      (_ , (w2,h2)) = bounds img2
      imgSize1 = (w1,h1) 
      imgSize2 = (w2,h2)
      offset2 = ((fst offset1)+w1,0)
      fstImg = renderPrimitive Points $ F.mapM_ ((drawPoint' winSize imgSize1).(addOffset offset1)) (assocs img1)
      sndImg = renderPrimitive Points $ F.mapM_ ((drawPoint' winSize imgSize2).(addOffset offset2)) (assocs img2) in
  do
    windowSize $= Size (fromIntegral winWidth :: Int32) (fromIntegral winHeight :: Int32)
    clear [ColorBuffer]
    fstImg
    sndImg
    flush
    swapBuffers
                    
addOffset :: (Int,Int) -> ((Int,Int),(Word8,Word8,Word8)) ->
             ((Int,Int),(Word8,Word8,Word8))
addOffset (offx,offy) ((x,y),color) = ((x+offx,y+offy),color)
