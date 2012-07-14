{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
import System.Exit
import System.IO
import System.Console.CmdArgs
import System.Environment

import Data.Sequence as S
import Data.Foldable as F
import Data.Array
import Data.Word
import Data.Int
import Data.Ix

import GHC.Float
import Graphics.HGL as HGL
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Error as ME
import Control.Monad

import Text.ParserCombinators.Parsec as P

import DevILWrapper
import Filter (noRed,defaultFilter,gaussianFilter,medianFilter,
               contourFilter1,contourFilter2,sharpeningFilter1,
               sharpeningFilter2,filterImage,blackAndWhiteFilter,
               canny,Filter)

{- Error reporting data type -}
data CLInputError = FlagNeeded String
                  | FileNotReadable String
                  | FileNotWritable String
                  | FileNotSeekable String
                  | UnknownError
                  deriving (Show)

instance ME.Error CLInputError  where
  noMsg = error "Error desconocido"
  strMsg = error

type ErrorIO = ErrorT CLInputError IO

{- Comand line data type & functions -}
data FotochOpts = FotochOpts { ifile :: String, ofile :: String }
              deriving (Show, Data, Typeable)

_PROGRAM_NAME = "fotochop"
_PROGRAM_VERSION = "0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "A simple haskell comand-line image filtering application."
_COPYRIGHT = "(c) 2012 Victor De Ponte & Germán Jaber, (c) 2012 "++
             "Universidad Simon Bolivar."
_DETAILS = [("Visit the repo at https://github.com/mirath/fotochop "++
            "to get the source code.")]

getOpts :: IO FotochOpts
getOpts = cmdArgs $ fotochOpts
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME
    &= summary (_PROGRAM_INFO ++ "\n" ++ _COPYRIGHT ++ "\n")
    &= details _DETAILS


fotochOpts = FotochOpts {
  ifile = def
          &= help "The image to be loaded and filtered"
          &= typ "INPUT_IMAGE_FILE",
  ofile = def
          &= help "The image to be written in disc after filtering."
          &= typ "OUTPUT_IMAGE_FILE"
  }

handleOpts :: FotochOpts -> ErrorIO (String,String)
handleOpts opts@FotochOpts{..} = do
  case (Prelude.null ifile) of
    True -> do
      lift $ withArgs ["--help"] getOpts
      throwError $
        FlagNeeded "--ifile is blank!\nYou must provide an input image file."
    False -> do
      ihandle <- lift $ openFile ifile ReadMode
      isRead <- lift $ hIsReadable ihandle
      case (isRead) of
        True -> do
          isSeek <- lift $ hIsSeekable ihandle
          case (isSeek) of
            True -> case (Prelude.null ofile) of
              False -> do
                ohandle <- lift $ openFile ofile WriteMode
                isWrite <- lift $ hIsWritable ohandle
                case (isWrite) of
                  True -> return (ifile,ofile)
                  False -> throwError $
                           FileNotWritable $ "Output file: "++ofile
              True -> return (ifile,ofile)
            False -> throwError $ FileNotSeekable $ "Input file: "++ifile
        False -> throwError $ FileNotReadable $ "Input file: "++ifile


-- main functions
main :: IO ()
main = do
  args <- getArgs
  opts <- ((if Prelude.null args then (withArgs ["--help"]) else id) getOpts)
  archivos <- runErrorT $ handleOpts opts
  case archivos of
    Left err -> putStrLn (show err)
    Right (inf,outf) -> do
      ilInit
      originalPicture <- readImage' inf
      mutablePicture <- newMVar originalPicture
      forkOS $ showImage originalPicture mutablePicture
      putStrLn _PROGRAM_INFO
      putStrLn (_COPYRIGHT ++ "\n\n")
      mainIOLoop mutablePicture outf

prompt :: IO ()
prompt = putStrLn ">> "

mainIOLoop :: MVar Img -> FilePath -> IO ()
mainIOLoop img outf = forever $ do
  prompt
  cmdline <- getLine
  case (parse command "parse error" cmdline) of
    Left err -> print err
    Right (Filter Default) -> do
      putStrLn "Applying default filter..."
      mainFilter [defaultFilter] img
    Right (Filter Gaussian) -> do
      putStrLn "Applying gaussian filter..."
      mainFilter [gaussianFilter 5] img
    Right (Filter Median) -> do
      putStrLn "Applying median filter..."
      mainFilter [medianFilter 5] img
    Right (Filter Contour1) -> do
      putStrLn "Applying contour1 filter..."
      mainFilter [contourFilter1] img
    Right (Filter Contour2) -> do
      putStrLn "Applying contour2 filter..."
      mainFilter [contourFilter2] img
    Right (Filter Sharpening1) -> do
      putStrLn "Applying sharpening1 filter..."
      mainFilter [sharpeningFilter1] img
    Right (Filter Sharpening2) -> do
      putStrLn "Applying sharpening1 filter..."
      mainFilter [sharpeningFilter2] img
    Right (Filter BlackAndWhite) -> do
      putStrLn "Applying black and white filter..."
      mainFilter [blackAndWhiteFilter] img
    Right (Filter Canny) -> do
      putStrLn "Applying canny filter..."
      mainCanny img
    Right (Filter NoRed) -> do
      putStrLn "Applying no-red filter..."
      mainFilter [noRed] img
    Right (Filter NoGreen) -> do
      putStrLn "Applying no-green filter..."
      mainFilter [noGreen] img
    Right (Filter NoBlue) -> do
      putStrLn "Applying no-blue filter..."
      mainFilter [noBlue] img
    Right (Filter AllRed) -> do
      putStrLn "Applying all-red filter..."
      mainFilter [allRed] img
    Right (Filter AllGreen) -> do
      putStrLn "Applying all-green filter..."
      mainFilter [allGreen] img
    Right (Filter AllBlue) -> do
      putStrLn "Applying all-blue filter..."
      mainFilter [allBlue] img
    Right (Filter (Weighted m)) -> do
      putStrLn "Applying weighted filter..."
      mainFilter [allGreen] img
    Right (Filter (Composition s)) -> do
      putStrLn "Applying composition of filter..."
      mainFilter [allGreen] img
    Right Write -> do
      case (Prelude.null outf) of
        True -> putStrLn "Output file not provided."
        False -> do
          image <- takeMVar img
          writeImage' outf image
          putMVar img image
    Right Quit -> do
      putStrLn "exiting application..."
      exitSuccess
  mainIOLoop img outf

mainInit :: IO Img
mainInit = return $ array ((0,0),(0,0)) [((0,0),(255,255,255,255))]

mainFilter :: [Filter] -> MVar Img -> IO ()
mainFilter filts img =
  do
    image <- takeMVar img
    newimg <- return $ F.foldl' (flip filterImage) image filts
    putMVar img newimg
    return ()

mainCanny :: MVar Img -> IO ()
mainCanny img =
  do
    image <- takeMVar img
    newimg <- return $ canny image
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

{- TO DO: resize window when peeking in the MVar -}
drawImages :: Img ->
              MVar Img ->
              (Int,Int) -> (Int,Int) ->  IO ()
drawImages img1 img2 offset1 winSize@(winWidth,winHeight) =
  let (_ , (w1,h1)) = bounds img1
      imgSize1 = (w1,h1)
      offset2 = (0,(fst offset1)+h1) in
      --This offset is seen like it is in the X direction,
      --but the #$%& coordinate system of DevIL forces me
      --to code it in the Y component
  do
    image2 <- takeMVar img2
    (_ , (w2,h2)) <- return $ bounds image2
    imgSize2 <- return $ (w2,h2)
    windowSize $= Size (fromIntegral winWidth :: Int32)
      (fromIntegral winHeight :: Int32)
    clear [ColorBuffer]
    drawImage offset1 img1
    drawImage offset2 image2
    putMVar img2 image2
    flush
    swapBuffers

drawImage :: (Int,Int) -> Img -> IO()
drawImage offset img = renderPrimitive Points drawing
  where  (_ , (w,h)) = bounds img
         imgSize = (w,h)
         drawing = F.mapM_
                  ((drawPoint' imgSize).(addOffset offset))
                  (assocs img)

drawPoint' :: (Int,Int) -> ((Int,Int),RGBAPixel) -> IO ()
drawPoint' imgSize (pos,color) = drawPoint imgSize color pos

drawPoint :: (Int,Int) -> RGBAPixel -> (Int,Int) -> IO ()
drawPoint (imgWidth,imgHeight) (r,g,b,a) pos@(x,y) =
  do
    color $ (Color4 r g b a :: Color4 Word8)
    vertex $ Vertex3 xcoor ycoor 0.0
      where iw2 = (int2Float imgWidth)/2.0
            ih2 = (int2Float imgHeight)
            xcoor = ((int2Float y)/ih2 - 1.0)
            ycoor = ((int2Float x)/iw2 - 1.0)

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

-- CLI Parser

data Command = Filter FilterAlgorithm
             | Write
             | Quit
             deriving (Show)

data FilterAlgorithm = Default
                     | Gaussian
                     | Median
                     | Contour1
                     | Contour2
                     | Sharpening1
                     | Sharpening2
                     | BlackAndWhite
                     | Canny
                     | NoRed | NoGreen | NoBlue
                     | AllRed | AllGreen | AllBlue
                     | Weighted [((Int,Int), (Word8,Word8,Word8,Word8))]
                     | Composition (Seq FilterAlgorithm)
                     deriving (Show)

int :: Parser Int
int = do
  pe <- many digit
  return $ read (pe)

word8 :: Parser Word8
word8 = do
  pe <- many digit
  return $ read (pe)

pos :: Parser (Int,Int)
pos = do
  P.char '('
  spaces
  x <- int
  spaces
  P.char ','
  spaces
  y <- int
  spaces
  P.char ')'
  return (x,y)

colorParser :: Parser (Word8,Word8,Word8,Word8)
colorParser = do
  P.char '('
  spaces
  r <- word8
  spaces
  P.char ','
  spaces
  g <- word8
  spaces
  P.char ','
  spaces
  b <- word8
  spaces
  P.char ','
  spaces
  a <- word8
  spaces
  P.char ')'
  return (r,g,b,a)

tuple :: Parser ((Int,Int),(Word8,Word8,Word8,Word8))
tuple = do
  P.char '('
  spaces
  p <- pos
  spaces
  P.char ','
  spaces
  c <- colorParser
  spaces
  P.char ')'
  return (p,c)

list :: Parser [((Int,Int),(Word8,Word8,Word8,Word8))]
list = do
  P.char '['
  spaces
  ls <- many tuple
  spaces
  P.char ']'
  return ls

defaultFilterParser :: Parser FilterAlgorithm
defaultFilterParser = do
  string "Default"
  return Default

gaussian :: Parser FilterAlgorithm
gaussian = do
  string "Gaussian"
  return Gaussian

median :: Parser FilterAlgorithm
median = do
  string "Median"
  return Median

contour1 :: Parser FilterAlgorithm
contour1 = do
  string "Contour1"
  return Contour1

contour2 :: Parser FilterAlgorithm
contour2 = do
  string "Contour2"
  return Contour2

sharpening1 :: Parser FilterAlgorithm
sharpening1 = do
  string "Sharpening1"
  return Sharpening1

sharpening2 :: Parser FilterAlgorithm
sharpening2 = do
  string "Sharpening2"
  return Sharpening2

blackAndWhite :: Parser FilterAlgorithm
blackAndWhite = do
  string "BlackAndWhite"
  return BlackAndWhite

cannyParser :: Parser FilterAlgorithm
cannyParser = do
  string "Canny"
  return Canny

nored :: Parser FilterAlgorithm
nored = do
  string "NoRed"
  return NoRed

nogreen :: Parser FilterAlgorithm
nogreen = do
  string "NoGreen"
  return NoGreen

noblue :: Parser FilterAlgorithm
noblue = do
  string "NoBlue"
  return NoBlue

allred :: Parser FilterAlgorithm
allred = do
  string "AllRed"
  return AllRed

allgreen :: Parser FilterAlgorithm
allgreen = do
  string "AllGreen"
  return AllGreen

allblue :: Parser FilterAlgorithm
allblue = do
  string "AllBlue"
  return AllBlue

weighted :: Parser FilterAlgorithm
weighted = do
  string "Weighted"
  spaces
  l <- list
  return $ Weighted l

composition :: Parser FilterAlgorithm
composition = do
  filters <- filterRec `sepBy` (string "<>")
  return $ Composition $ S.fromList filters

filterRec :: Parser FilterAlgorithm
filterRec = try defaultFilterParser
            <|> try gaussian
            <|> try median
            <|> try contour1
            <|> try contour2
            <|> try sharpening1
            <|> try sharpening2
            <|> try blackAndWhite
            <|> try cannyParser
            <|> try nored
            <|> try nogreen
            <|> try noblue
            <|> try allred
            <|> try allgreen
            <|> try allblue
            <|> try weighted
            <|> composition

cmdfilter :: Parser Command
cmdfilter = do
  spaces
  string "filter"
  spaces
  f <- filterRec
  return $ Filter f

cmdwrite :: Parser Command
cmdwrite = do
  spaces
  string "write"
  spaces
  return Write

cmdquit :: Parser Command
cmdquit = do
  spaces
  string "quit"
  spaces
  return Quit

command :: Parser Command
command = try cmdfilter
          <|> try cmdwrite
          <|> cmdquit
