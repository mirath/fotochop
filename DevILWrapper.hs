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
  writeImage'  -- :: FilePath -> Img -> IO ()
  ) where

import Data.Sequence as S
import Data.Foldable as F
import Data.Map as M
import Codec.Image.DevIL
import Data.Array.Base as A
import Data.Array.Unboxed as AU
import System.Environment

type Image = UArray (Int, Int, Int) Word8
{- EXPORTED TYPE ALIASES -}
type Img = Array Index RGBAPixel
type Index = (Int,Int)
type RGBAPixel = (Word8,Word8,Word8,Word8)
type Range = (Index,Index)
{- END OF EXPORTED TYPE ALIASES -}

setVal :: RGBAPixel -> Int -> Word8 -> RGBAPixel
setVal (r,g,b,a) channel val =
  case channel of
    0 -> (val,g,b,a)
    1 -> (r,val,b,a)
    2 -> (r,g,val,a)
    3 -> (r,g,b,val)

cushy :: Image -> Img
cushy readImg = array newBounds newMap
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

unCushy :: Img -> Image
unCushy image = AU.array newBounds newMap
  where
    original = A.assocs image
    newMap = F.concatMap f original
    f :: (Index, RGBAPixel) -> [((Int, Int, Int), Word8)]
    f ((x,y),(r,g,b,a)) = [((x,y,0),r),((x,y,1),g),((x,y,2),b),((x,y,3),a)]
    newBounds = case (bounds image) of
      ((lx,ly),(ux,uy)) -> ((lx,ly,0),(ux,uy,3))

{- EXPORTED FUNCTIONS -}

-- | Reads an image into an RGBA array. Indices are (row,column), and the value
--   is a tuple of the (r,g,b,a) channels.
readImage' :: FilePath  -- ^ path to the image to read.
              -> IO Img -- ^ IO action with the read image as its side-effect.
readImage' fp = do
  image <- readImage fp
  return $ cushy image

-- | Writes an RGBA array to a file. Indices are (row,column), and the value
--   is a tuple of the (r,g,b,a) channels.
writeImage' :: FilePath -- ^ path to the image to write.
               -> Img   -- ^ Array representation of the image to be written.
               -> IO () -- ^ void IO action.
writeImage' fp img = writeImage fp $ unCushy img

{- END OF EXPORTED FUNCTIONS -}

main = do
  [imageName] <- getArgs
  ilInit
  putStr $ "Leyendo la imagen...\n\tImagen: "++imageName++"\n"
  loadedImg <- readImage imageName
  putStr "Imagen cargada!!!\n"
  print $ show loadedImg
  putStr "Representando la imagen...\n"
  putStr "Imagen representada!!!\n"
  print $ cushy loadedImg
  putStr "Devolviendo la representacion\n"
  putStr "Representación devuelta!!!\n"
  print $ unCushy $ cushy loadedImg
  putStr "Escribiendo la imagen...\n"
  writeImage  ("new"++imageName) $ unCushy $ cushy loadedImg
  putStr $ "Imagen escrita!!!\n\tImagen: "++imageName++"\n\n"