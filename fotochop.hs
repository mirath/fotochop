{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs
import System.Environment
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
import Control.Concurrent.MVar
import Control.Monad

import DevILWrapper
import Filter (noRed,defaultFilter,filterImage,Filter)

-- Comand line data type
data ImageCmd = Image { ifile :: String, ofile :: String }
              deriving (Show, Data, Typeable)

image = Image {
  ifile = def
          &= help "The image to be loaded and filtered"
          &= typ "INPUT_IMAGE_FILE",
  ofile = def
          &= help "The image to be written in disc after filtering."
          &= typ "OUTPUT_IMAGE_FILE"
  } &= summary ("FotoChop v0.1, (c) 2012 Victor De Ponte & Germán Jaber, "++
        "(c) 2012 Universidad Simon Bolivar.")

-- main functions
main :: IO ()
main = do
  --cmdArgs sample
  [imgFile] <- getArgs
  ilInit
  originalPicture <- readImage' imgFile
  --mutablePicture <- newMVar $ filterImage defaultFilter originalPicture
  mutablePicture <- newMVar originalPicture
  forkOS $ showImage originalPicture mutablePicture
  mainIOLoop mutablePicture

prompt :: IO ()
prompt = putStr "\n>> "

mainIOLoop :: MVar Img -> IO ()
mainIOLoop img = do
  prompt
  cmdline <- getLine
  case (head cmdline) of
    'b' -> mainFilter [defaultFilter] img
    'r' -> mainFilter [noRed] img
    'w' -> mainWriteFile "new.jpg" img
    otherwise -> return ()
  putStr "\n"
  --(cmd,args) <- return
  putStrLn cmdline
  mainIOLoop img

mainInit :: IO Img
mainInit = return $ array ((0,0),(0,0)) [((0,0),(255,255,255,255))]

mainFilter :: [Filter] -> MVar Img -> IO ()
mainFilter filts img = 
  do
    image <- takeMVar img
    newimg <- return $ F.foldl' (flip filterImage) image filts
    putMVar img newimg
    return ()

mainWriteFile :: String -> MVar Img -> IO ()
mainWriteFile filename img = 
  do
    image <- takeMVar img
    writeImage' filename image
    putMVar img image
    return ()

-- Image drawing functions

showImage :: Img -> MVar Img -> IO ()
showImage img1 img2  =
  let (_ , (w1,h1)) = bounds img1 in
  do
    image2 <- takeMVar img2
    (_ , (w2,h2)) <- return $ bounds image2
    putMVar img2 image2
    winWidth <- return $ h1+h2+2
    winHeight <- return $ max (w1+1) (w2+1)
    (progname,_) <- getArgsAndInitialize
    initialDisplayMode $= [Graphics.UI.GLUT.DoubleBuffered]
    createWindow "Fotochop v0.1"
    windowSize $=
      Size (fromIntegral winWidth :: Int32) (fromIntegral winHeight :: Int32)
    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer]
    displayCallback $= (drawImages img1 img2 (0,0) (winWidth,winHeight))
    mainLoop

drawPoint' :: (Int,Int) -> (Int,Int) ->
              ((Int,Int),RGBAPixel) -> IO ()
drawPoint' winSize imgSize (pos,color) = drawPoint winSize imgSize color pos

drawPoint :: (Int,Int) -> (Int,Int) -> RGBAPixel -> (Int,Int) -> IO ()
drawPoint (winWidth,winHeight) (imgWidth,imgHeight) (r,g,b,a) pos@(x,y) =
  do
    color $ (Color4 r g b a :: Color4 Word8)
    vertex $ Vertex3 xcoor ycoor 0.0
      where iw2 = (int2Float imgWidth)/2.0
            ih2 = (int2Float imgHeight)
            xcoor = ((int2Float y)/ih2 - 1.0)
            ycoor = ((int2Float x)/iw2 - 1.0)

{- TO DO: resize window when peeking in the MVar -}
drawImages :: Img ->
              MVar Img ->
              (Int,Int) -> (Int,Int) ->  IO ()
drawImages img1 img2 offset1 winSize@(winWidth,winHeight) =
  let (_ , (w1,h1)) = bounds img1
      imgSize1 = (w1,h1)
      offset2 = (0,(fst offset1)+h1)
      --The offset is in the X direction, but the #$%& coordinate system of
      --DevIL forces me to put it in the Y component
      fstImg = renderPrimitive Points $
               F.mapM_ ((drawPoint' winSize imgSize1).(addOffset offset1))
               (assocs img1) in
  do
    image2 <- takeMVar img2
    (_ , (w2,h2)) <- return $ bounds image2
    imgSize2 <- return $ (w2,h2)
    windowSize $= Size (fromIntegral winWidth :: Int32)
      (fromIntegral winHeight :: Int32)
    clear [ColorBuffer]
    fstImg
    renderPrimitive Points $
      F.mapM_ ((drawPoint' winSize imgSize2).(addOffset offset2))
      (assocs image2)
    putMVar img2 image2
    flush
    swapBuffers

addOffset :: (Int,Int) -> ((Int,Int),RGBAPixel) ->
             ((Int,Int),RGBAPixel)
addOffset (offx,offy) ((x,y),color) = ((x+offx,y+offy),color)


-- Test functions

testImage :: Int -> Int -> Img
testImage w h = array ((0,0),(h-1,w-1)) (sampleImg1Bindings w h)

sampleImg1Bindings :: Int -> Int -> [((Int,Int),RGBAPixel)]
sampleImg1Bindings w h =
  [((x,y) , (c (x,y), 0, c (x,y),255)) |
   x<-[0..(h-1)] , y<-[0..(w-1)]]
  where c = dist (0,0)

dist :: (Int,Int) -> (Int,Int) -> Word8
dist (x,y) (x',y') = floor $ sqrt $ int2Float $ (x-x')^2 + (y-y')^2