module Lib
    ( someFunc
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Int
import Data.Binary

low = encode $(minBound :: Int16) :: B.ByteString
high = encode $( maxBound :: Int16) :: B.ByteString

tone1992 =  B.concat $ replicate 2 low ++ replicate 2 high
tone1336 =  B.concat $ replicate 3 low ++ replicate 3 high
sec1empty = B.concat $ replicate 8000 (encode (0::Int)) :: B.ByteString

someFunc :: IO ()
someFunc = do
  B.putStr $ B.concat $ repeat $ foldr1 B.append [tone1992,tone1992, tone1336,tone1336,tone1336,tone1336, tone1336]

--  concat $  repeat (replicate 10 low ++ replicate 10 high)