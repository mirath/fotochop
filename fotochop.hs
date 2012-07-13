import Data.Array
import Data.Foldable as F
import Data.Ix
import Data.Word
import Data.Int
import GHC.Float
import Graphics.HGL as HGL

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Concurrent
  
-- main functions
main :: IO ()
main = do
  return ()

mainInit :: IO (Array (Int,Int) (Word8,Word8,Word8))
mainInit = return $ array ((0,0),(0,0)) [((0,0),(255,255,255))]

mainFilter :: IO (Array (Int,Int) (Word8,Word8,Word8))
mainFilter = return $ array ((0,0),(0,0)) [((0,0),(255,255,255))]

mainWriteFile :: String -> IO ()
mainWriteFile filename = return ()

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
      offset2 = ((fst offset1)+w1,0) in
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
  
                                      
-- Test functions

testImage :: Int -> Int -> Array (Int,Int) (Word8,Word8,Word8)
testImage w h = array ((0,0),(h-1,w-1)) (sampleImg1Bindings w h)

sampleImg1Bindings :: Int -> Int -> [((Int,Int),(Word8,Word8,Word8))]
sampleImg1Bindings w h =
  [((x,y) , (c (x,y), 0, c (x,y))) |
   x<-[0..(h-1)] , y<-[0..(w-1)]]
  where c = dist (0,0)

dist :: (Int,Int) -> (Int,Int) -> Word8
dist (x,y) (x',y') = floor $ sqrt $ int2Float $ (x-x')^2 + (y-y')^2