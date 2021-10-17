module Lib
  ( someFunc,
  )
where

import Data.Binary
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.ByteString.Lazy as B
import Data.Fixed
import Data.Int
import Data.Tuple.Extra

-- format = BigEndian

rate = 8000

low = encode $(minBound :: Int16) :: B.ByteString

high = encode $(maxBound :: Int16) :: B.ByteString

alleMeineEntchen = [(C, 3, 300),
                    (D, 3, 300),
                    (E, 3, 300),
                    (F, 3, 300),
                    (G, 3, 600),
                    (G, 3, 600),

                    (A, 3, 300),
                    (A, 3, 300),
                    (A, 3, 300),
                    (A, 3, 300),
                    (G, 3, 1200),

                    (A, 3, 300),
                    (A, 3, 300),
                    (A, 3, 300),
                    (A, 3, 300),
                    (G, 3, 1200),

                    (F, 3, 300),
                    (F, 3, 300),
                    (F, 3, 300),
                    (F, 3, 300),
                    (E, 3, 600),
                    (E, 3, 600),

                    (G, 3, 300),
                    (G, 3, 300),
                    (G, 3, 300),
                    (G, 3, 300),

                    (C, 3, 1200)]

someFunc :: IO ()
someFunc = do
  B.putStr $ funcToByteString $ foldr1 (++) $ (++) (pause 20) <$> ( uncurry3 playNote <$> alleMeineEntchen)

playNote :: Note -> Int -> Int -> [Double]
playNote note octave ms = take (msToSamples ms) $ genWave (freqOfNthNote note octave) squareF

msToSamples :: Int -> Int
msToSamples ms = round (fromIntegral ms/1000 * rate)

pause :: Int -> [Double]
pause ms = replicate (msToSamples ms) 0

data Note = C | CUp | D | DUp | E | F | FUp | G | GUp | A | AUp | B deriving (Enum)

getKey :: Note -> Int -> Int
getKey note octave = fromEnum note + 4 + (12 * (octave - 1))

freqOfNthNote :: Note -> Int -> Double
freqOfNthNote note octave = freqOfNthNoteD n
  where
    n = fromIntegral $ getKey note octave

freqOfNthNoteD :: Double -> Double
freqOfNthNoteD n = 2 ** ((n - 49) / 12) * 440

funcToByteString :: [Double] -> B.ByteString
funcToByteString vs = B.concat (d2bs <$> vs)

genWave :: Double -> (Double -> Double) -> [Double]
genWave hertz waveF =  (\s -> waveF (s * (1 / rate) * hertz)) <$> [0 ..]

squareF :: Double -> Double
squareF x = if x `mod'` 1 < 0.5 then -1 else 1

sinF :: Double -> Double
sinF x = sin (x * 2 * pi)

d2bs :: Double -> B.ByteString
d2bs d = encode (round $ d * fromIntegral (maxBound :: Int16) :: Int16)

concatSounds :: [B.ByteString] -> B.ByteString
concatSounds = B.concat

repeatInfinite :: B.ByteString -> B.ByteString
repeatInfinite bs = B.concat $ repeat bs
