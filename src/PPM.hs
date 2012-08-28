{- a very simple module to save image into PPM file -}
module PPM (savePPM)
    where

import Data
import Data.Word
import qualified Data.ByteString.Lazy as BIN

quant8 :: Double -> Word8
quant8 x = round $ x * 255

cquant8 :: Color -> [Word8]
cquant8 (r,g,b) = [quant8 r, quant8 g, quant8 b]

stringToBin :: String -> BIN.ByteString
stringToBin = BIN.pack . map (fromIntegral . fromEnum)

header :: Int -> Int -> BIN.ByteString
header width height = stringToBin $ "P6\n" ++ show width ++ " " ++ show height ++ " 255\n"

body :: [Color] -> BIN.ByteString
body = BIN.pack . concatMap (cquant8 . clips)

makePPM :: Int -> Int -> [Color] -> BIN.ByteString
makePPM width height = BIN.append (header width height) . body

savePPM :: Int -> Int -> FilePath -> [Color] -> IO ()
savePPM width height f = BIN.writeFile f . makePPM width height
