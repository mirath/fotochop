module Filter (Filter) where

class Filter f where
  filterPixel :: (Filter f) =>
                 f -> Array (Int,Int) (Word8,Word8,Word8) ->
                 (Int,Int) -> (Word8,Word8,Word8)
  filterImage :: (Filter f) =>
                 f -> Array (Int,Int) (Word8,Word8,Word8) ->
                 Array (Int,Int) (Word8,Word8,Word8)
