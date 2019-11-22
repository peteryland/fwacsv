module Lib
    ( processFiles
    ) where

import GHC.Word(Word32)
import GHC.Int(Int64)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Binary.Get(runGet, getWord32le, getFloatle, Get)
import Control.Monad(replicateM, forM_)
import Data.Time(NominalDiffTime)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import Data.Time.Format(formatTime, defaultTimeLocale)
import Data.List(intercalate)

showTime :: NominalDiffTime -> String
showTime t = formatTime defaultTimeLocale "%s,%Y-%m-%d %H:%M:%S" $ posixSecondsToUTCTime t

show' :: Float -> String
show' f = if f > 10000000 then "NaN" else show f

show'' :: NominalDiffTime -> [Float] -> String
show'' t fs = showTime t ++ "," ++ (intercalate "," $ map show' fs)

showN :: Int -> Int -> NominalDiffTime -> [(Word32, Float)] -> String
showN _ _ _ [] = ""
showN _ 0 _ _  = ""
showN n m t xs = show'' t (map snd $ take n xs) ++ "\n" ++ showN n (m - 1) (t + 10) (drop n xs)

-- showN n m t xs = if map fst xs == 0:(replicate n 0x42fa0000 ++ replicate n 0xc2480000)
                   -- then ""
                   -- else show'' t (map snd $ take n xs) ++ "\n" ++ showN n (m - 1) (t + 10) (drop n xs)

getData :: Int -> Get a -> BS.ByteString -> [a]
getData l f s = runGet (replicateM (fromIntegral (BS.length s) `div` l) f) s

getName :: BS.ByteString -> String
getName = BS.unpack . BS.takeWhile (/= '\0') . BS.drop 4

chunksOf :: Int -> Int64 -> BS.ByteString -> ([BS.ByteString], BS.ByteString)
chunksOf 0 _ xs = ([], xs)
chunksOf n l xs = let (x, y) = BS.splitAt l xs
                      (xs', rest) = chunksOf (n - 1) l y
                  in (x:xs', rest)

processFiles :: [FilePath] -> IO ()
processFiles args = do
  forM_ args (\arg -> do
    s <- BS.readFile arg
    let xs = zip (getData 4 getWord32le s) (getData 4 getFloatle s)
    let timestamp = fromIntegral $ fst $ xs !! 1 -- timestamp
    let n = fromIntegral $ fst $ xs !! 3 -- number of sensors
    let m = fromIntegral $ fst $ xs !! 4 -- number of readings
    let s' = BS.drop 750 s -- drop the file header
    let (sensor_headers, s'') = chunksOf n 528 s'
    putStrLn $ "Timestamp,Time," ++ (intercalate "," $ map getName sensor_headers)
    let xs' = zip (getData 4 getWord32le s'') (getData 4 getFloatle s'')
    putStrLn $ showN n m timestamp xs'
    )
