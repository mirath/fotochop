import Data.Array
import Data.Ix
import Data.Word
import GHC.Float
import Graphics.HGL as HGL






class Filter f where
  filterPixel :: (Filter f) =>
                 f -> Array (Int,Int) (Word8,Word8,Word8) -> (Int,Int) -> (Word8,Word8,Word8)
  filterImage :: (Filter f) =>
                 f -> Array (Int,Int) (Word8,Word8,Word8) -> Array (Int,Int) (Word8,Word8,Word8)
  
-- type Image = (IArray a (Word8,Word8,Word8), Ix i) => a i (Word8,Word8,Word8)

bla::(Eq a,Num a) => a
bla = 5

main :: IO ()
main = do
  return ()


mainInit :: IO (Array (Int,Int) (Word8,Word8,Word8))
mainInit = return $ array ((0,0),(0,0)) [((0,0),(255,255,255))]

mainFilter :: IO (Array (Int,Int) (Word8,Word8,Word8))
mainFilter = return $ array ((0,0),(0,0)) [((0,0),(255,255,255))]

mainWriteFile :: String -> IO ()
mainWriteFile filename = return ()

showImage :: Array (Int,Int) (Word8,Word8,Word8) -> Array (Int,Int) (Word8,Word8,Word8) -> IO ()
showImage img1 img2  =
  let (_ , (w1,h1)) = bounds img1
      (_ , (w2,h2)) = bounds img2 in
  do
    HGL.runGraphics (
      HGL.withWindow_ "Main Window" (w1+w2+2, max (h1+1) (h2+1)) $
      (\w -> do
          HGL.drawInWindow w $ drawPoint (240,240,240) (50,50)
          HGL.getKey w)
      )

drawPoint :: (Word8,Word8,Word8) -> (Int,Int) -> Graphic
drawPoint (r,g,b) pos@(x,y) = withColor (Red) $ HGL.line pos (x+10,y+10)

drawPoint' :: ((Int,Int),(Word8,Word8,Word8)) -> Graphic
drawPoint' (pos,color) = drawPoint color pos

drawImage :: Array (Int,Int) (Word8,Word8,Word8) -> (Int,Int) -> [Graphic]
drawImage img offset = 
  map (drawPoint'.(addOffset offset)) (assocs img) 
    where (_ , (w1,h1)) = bounds img
          
addOffset :: (Int,Int) -> ((Int,Int),(Word8,Word8,Word8)) ->
             ((Int,Int),(Word8,Word8,Word8))
addOffset (offx,offy) ((x,y),color) = ((x+offx,y+offy),(color))
  
testImage :: Int -> Int -> Array (Int,Int) (Word8,Word8,Word8)
testImage w h = array ((0,0),(h-1,w-1)) (sampleImg1Bindings w h)

sampleImg1Bindings :: Int -> Int -> [((Int,Int),(Word8,Word8,Word8))]
sampleImg1Bindings w h =
  [((x,y) , (c (x,y), c (x,y), c (x,y))) |
   x<-[0..(h-1)] , y<-[0..(w-1)] , ch<-[0,1,2]]
  where c = dist (0,0)

dist :: (Int,Int) -> (Int,Int) -> Word8
dist (x,y) (x',y') = floor $ sqrt $ int2Float $ (x-x')^2 + (y-y')^2