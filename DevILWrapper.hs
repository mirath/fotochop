{- |
   Module     : DevILWrapper
   Copyright  : (c) 2012 Universidad Sim&#243;n Bol&#237;var,
                (c) 2012 Victor De Ponte &
                (c) 2012 German Jaber
   License    : BSD3

   Maintainer : Victor De Ponte <victor.dpo@gmail.com> &
                German Jaber <germanjaber@gmail.com>
   Stability  : stable
   Portability: GHC/Hugs

Wrapper library for the /Codec.Image.DevIL/ module. Provides wrapper functions
which return Arrays of the form:

> Array (Int,Int) (Word8,Word8,Word8,Word8)

where '(/Int/,/Int/)' represent the index of a 2D image, and
'(/Word8/,/Word8/,/Word8/,/Word8/)' are the RGBA channels of the loaded image;
instead of the UArrays of the form:

> UArray (Int, Int, Int) Word8

returned by the functions of /Codec.Image.DevIL/, where '(/Int/, /Int/, /Int/)'
represents the (x,y,channel) indexes of the loaded image, and the /Word8/ is the
value of the specified index.

This representation results less efficient in time and space performance, but
results easier to deal with.

-}

module DevILWrapper (
  -- * Convenience Type Aliases
  Img(..), Index(..), RGBAPixel(..), Range(..),
  -- * Wrapper Functions
  readImage',  -- :: FilePath -> IO Img
  -- writeImage'  -- :: FilePath -> Img -> IO ()
  ) where

import Data.Sequence as S
import Data.Foldable as F
import Data.Map as M
import Codec.Image.DevIL
import Data.Array.Base as A
import Data.Array.Unboxed as AU
import System.Environment

type Image = UArray (Int, Int, Int) Word8
type Img = Array Index RGBAPixel
type Index = (Int,Int)
type RGBAPixel = (Word8,Word8,Word8,Word8)
type Range = (Index,Index)

setVal :: RGBAPixel -> Int -> Word8 -> RGBAPixel
setVal (r,g,b,a) channel val =
  case channel of
    0 -> (val,g,b,a)
    1 -> (r,val,b,a)
    2 -> (r,g,val,a)
    3 -> (r,g,b,val)

cushy :: Image -> Img
cushy readImg =
  array newBounds $ newMap
    where
      readMap = A.assocs readImg
      init = ( ( (0,0) , (0,0,0,0) ), S.empty)
      f :: ((Index,RGBAPixel), Seq (Index,RGBAPixel))
           -> ((Int,Int,Int),Word8)
           -> ((Index,RGBAPixel), Seq (Index,RGBAPixel))
      f (( (x,y) , pixel ), seq) ((x1,y1,chan), val) =
        case chan of
          3 -> (newPixel, seq |> newPixel)
          _ -> (newPixel, seq)
        where
          newPixel = ((x1,y1), setVal pixel chan val)
      newMap = F.toList $ snd $ foldl' f init readMap
      newBounds = case (bounds readImg) of
        ((lx,ly,_),(ux,uy,_)) -> ((lx,ly),(ux,uy))

readImage' :: FilePath -> IO Img
readImage' fp = do
  image <- readImage fp
  return $ cushy image

-- writeImage' :: FilePath -> Img -> IO ()
-- writeImage' fp img = writeImage fp $ unCushy img

main = do
  [imageName] <- getArgs
  ilInit
  putStr $ "Leyendo la imagen...\n\tImagen: "++imageName++"\n"
  loadedImg <- readImage imageName
  putStr "Imagen cargada!!!\n"
  putStr "Representando la imagen...\n"
  putStr "Imagen representada!!!\n"
  print $ cushy loadedImg
  putStr "Escribiendo la imagen...\n"
  writeImage  ("new"++imageName) loadedImg
  putStr $ "Imagen escrita!!!\n\tImagen: "++imageName++"\n\n"
  print $ show loadedImg