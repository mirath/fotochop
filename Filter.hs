module Filter (
  --Types
  Filter,
  Footprint,
  Col,
  Pos,
  Neighborhood,
  Img,
  
  --filters
  defaultFilter,
  noRed,
  
  --filtering functions
  filterPixel,
  filterImage,
  applyCustomFilter,
  
  --utilities
  wrapExtend,
  sumTuplesI,
  sumColorsF,
  funColorsF,
  clampI,
  clampF,
  clamp8,
  color8ToColorF,
  colorFToColor8,
  squareFootprint,
  circularFootprint) where

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

-- Type aliases for commodity
type Footprint = Seq.Seq (Int,Int)

type Col = (Word8,Word8,Word8,Word8)

type ColF = (Float,Float,Float,Float)

type Pos = (Int,Int)

type Neighborhood = Seq.Seq (Pos,ColF)

type Img = Array (Int,Int) Col

--Filter type and default filter
data Filter = Defined  {perPixelFunction :: (Pos,ColF) -> (Pos,ColF),
                        combiner :: ColF -> (Pos,ColF) -> ColF,
                        combinerBaseColor :: ColF,
                        postprocesser :: Neighborhood -> ColF -> Img -> ColF,
                        footprint :: Footprint
                       }
              | Custom { customFilter :: Neighborhood -> Img -> ColF,
                         footprint :: Footprint}
                
defaultFilter :: Filter
defaultFilter = Defined { perPixelFunction = perPixelFunctionDefault,
                          combiner = combinerDefault,
                          combinerBaseColor = combinerBaseColorDefault,
                          postprocesser = postprocesserDefault,
                          footprint = circularFootprint 3
                        }
                          
noRed :: Filter
noRed = defaultFilter {postprocesser = (\ _ (r,g,b,a) _ -> (0,g,b,a)),
                       footprint = (squareFootprint 0 0)}

perPixelFunctionDefault :: (Pos,ColF) -> (Pos,ColF)
perPixelFunctionDefault (p,c) = (p,c)
  
combinerDefault :: ColF -> (Pos,ColF) -> ColF
combinerDefault ca (p,cb) = sumColorsF ca cb
 
combinerBaseColorDefault :: ColF
combinerBaseColorDefault = (0.0,0.0,0.0,0.0)
 
postprocesserDefault :: Neighborhood -> ColF -> Img -> ColF
postprocesserDefault n c _ =
  funColorsF (/) c (int2Float ne,int2Float ne,int2Float ne,int2Float ne)
    where ne = Seq.length n

-- Filtering functions to be exportated
filterPixel :: Filter -> Img -> Pos -> (Pos,Col)
filterPixel (Defined ppf comb combC postp fp) img pos =
  (pos, colorFToColor8 (postprocessing postp neigh prefinalC img))
    where neigh = neighborhood fp pos img
          prefinalC = (combine comb combC) $ (mapNeighborhood ppf neigh)
filterPixel (Custom f fp) img pos =
  (pos,colorFToColor8 (f neigh img))
    where neigh = neighborhood fp pos img
          
filterImage :: Filter -> Img -> Img
filterImage filt img = img//newpixels
  where newpixels = map (filterPixel filt img) (indices img)

-- Pixel proccesing functions
applyCustomFilter :: (Neighborhood -> Img -> ColF) -> Neighborhood -> Img -> ColF
applyCustomFilter f neigh img = f neigh img

mapNeighborhood :: ((Pos,ColF) -> (Pos,ColF)) -> Neighborhood -> Neighborhood
mapNeighborhood f neigh = fmap f neigh

combine :: (ColF -> (Pos,ColF) -> ColF) ->
           ColF -> Neighborhood -> ColF
combine f cb neigh = F.foldl f cb neigh
           
postprocessing :: (Neighborhood -> ColF -> Img -> ColF) ->
                  Neighborhood -> ColF -> Img -> ColF
postprocessing f neigh cf img = f neigh cf img

neighborhood :: Footprint -> Pos -> Img -> Neighborhood
neighborhood fp offset img = F.foldl addToPixels Seq.empty fp
  where wrap = (wrapExtend (bounds img)).(sumTuplesI offset)
        addToPixels s p = s |> (p, color8ToColorF ((!) img (wrap p)))

--Utility functions
wrapExtend :: ((Int,Int),(Int,Int)) -> (Int,Int) -> (Int,Int)
wrapExtend ((lx,ly),(hx,hy)) (x,y) = (clampX x, clampY y)
  where clampX = clampI lx hx
        clampY = clampI ly hy

sumTuplesI :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumTuplesI (a,b) (c,d) = (a+c,b+d)

funColorsF :: (Float -> Float -> Float) -> ColF -> ColF -> ColF
funColorsF f (a,b,c,d) (a',b',c',d') = (f a a' , f b b' , f c c' , f d d')
        
sumColorsF :: ColF -> ColF -> ColF
sumColorsF  = funColorsF (+)

clampI :: Int -> Int -> Int -> Int
clampI low high n = min high $ max low n

clampF :: Float -> Float -> Float -> Float
clampF low high n = min high $ max low n

clamp8 :: Word8 -> Word8 -> Word8 -> Word8
clamp8 low high n = min high $ max low n

int2Word8 :: Int -> Word8
int2Word8 a = fromIntegral $ clampI 0 255 a

float2Word8 :: Float -> Word8
float2Word8 a = int2Word8 $ float2Int $ clampF 0.0 255.0 a

color8ToColorF :: Col -> ColF
color8ToColorF (a,b,c,d) = (w8ToF a,w8ToF b,w8ToF c,w8ToF d)
  where w8ToF n = int2Float (fromIntegral n :: Int)

colorFToColor8 :: ColF -> Col
colorFToColor8 (a,b,c,d) = (fTow8 a,fTow8 b,fTow8 c,fTow8 d)
  where fTow8 n = fromIntegral (float2Int n) :: Word8

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

-- Test functions        
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

showImage :: Array (Int,Int) Col ->
             Array (Int,Int) Col -> 
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

drawPoint' :: (Int,Int) -> (Int,Int) -> ((Int,Int),Col) -> IO ()
drawPoint' winSize imgSize (pos,color) = drawPoint winSize imgSize color pos

drawPoint :: (Int,Int) -> (Int,Int) -> Col -> (Int,Int) -> IO ()
drawPoint (winWidth,winHeight) (imgWidth,imgHeight) (r,g,b,a) pos@(x,y) = do
  color $ (Color4 r g b a :: Color4 Word8)
  vertex $ Vertex3 xcoor ycoor 0.0
    where iw2 = (int2Float imgWidth)
          ih2 = (int2Float imgHeight)/2.0
          xcoor = ((int2Float x)/iw2 - 1.0)
          ycoor = ((int2Float y)/ih2 - 1.0)

drawImages :: Array (Int,Int) Col -> 
              Array (Int,Int) Col -> 
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
                    
addOffset :: (Int,Int) -> ((Int,Int),Col) ->
             ((Int,Int),Col)
addOffset (offx,offy) ((x,y),color) = ((x+offx,y+offy),color)
