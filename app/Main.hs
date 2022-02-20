{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Data.Binary           (encodeFile)
import           Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as B
import           Data.Massiv.Array
import           Data.Massiv.Array.IO
import qualified Data.Vector.Unboxed   as V
import           Graphics.STL
import           HTile
import           Options.Applicative hiding (header)
import           Linear (V4(..),V3(..))
import           System.FilePath
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Control.Monad
import           Data.Int (Int16)
import           Data.Word (Word16)

data Opts = Opts { infile :: String
                 , outfile :: String
                 , hex :: Bool
                 , _scale :: Float
                 , _pitch :: Float
                 }
main :: IO ()
main = do
  let optsInfo = info (opts <**> helper) briefDesc
  args <- execParser optsInfo
  heights <- if takeExtension (infile args) == ".hgt"
            then do
               bs <- decodeFile (infile args) :: IO HGT
               let hs = V.fromList . fmap (fromIntegral :: Int16 -> Word16) . fmap abs $ elevations bs
                   dims = if V.length hs == 3601^2 then 3601 else 3601
               -- pure $ makeArrayLinear Seq (Sz (dims :. dims)) (\i -> hs V.! i)
               pure $ makeArray Seq (Sz (2000 :. 2000)) (\(i :. j) -> hs V.! (i*3601+j))


            -- Automatically convert colour images to greyscale using the D65
            -- illuminant standard. If the image is already greyscale this
            -- transformation is the identity (this is not documented in
            -- massiv-io/color, but it is the obvious behaviour and it is what
            -- happens)
            else fmap raw . delay <$> (readImageAuto (infile args) :: IO (Image S (Y D65) Word16))
  
  -- heights <- extractM zeroIndex (Sz2 1200 1200) heights'
  let located = computeAs U $ locate $ heights
      (Sz2 x y) = size located

      f = recip $ _scale args
      p = _pitch args
  
      x' = x - 1 -- n vertices in a row are connected by n-1 segments
      y' = y - 1
      -- num_facets = facets from the: up and down edges + left and
      -- right edges + top and bottom faces
      squareFacets = 2 * (2 * x') + 2 * (2 * y') + 2 * (2 * (x' * y'))

      hexFacets = let n = (min x y) `div` 2 in 12 * (n-2) * (n-1)

      sides = mkEdges f p located
      surface = dropWindow $ mkTop located
      top = toList . scaleArray f p $ surface
      bottom = toList . scaleArray f p $ mkBottom surface

      locatedRh = computeAs B . locateRh . resampleToHex . computeSource @U . fmap fromIntegral $ heights

      -- tris = if hex args 
      --        then V.fromListN hexFacets $ mkHex locatedRh 
      --        else V.fromListN squareFacets . flattenTuples $ (top++bottom++sides)

      tris = if hex args 
             then V.fromList $ mkHex f p locatedRh 
             else V.fromList . flattenTuples $ (top++bottom++sides)

      r = STL { header = pack "Made with hTile"
              -- , numTriangles = if hex args then fromIntegral hexFacets else fromIntegral squareFacets
              , numTriangles = fromIntegral $ V.length tris
              , normals = V.map (\ (V4 n _ _ _) -> n) tris
              , triangles = V.map (\ (V4 _ a b c) -> V3 a b c) tris
              }
  -- print top
  encodeFile (outfile args) r


opts :: Parser Opts
opts = Opts
      <$> strOption
          ( long "input"
         <> short 'i'
         <> metavar "SOURCE"
         <> help "Source image" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "TARGET"
         <> help "STL destination" )
      <*> switch
          ( long "hex"
         <> short 'h'
         <> help "Build hexagonal prisms" )
      <*> option auto
          ( long "scale"
         <> short 's'
         <> showDefault
         <> value 100
         <> help "Scale factor from geotiff values to STL units" )
      <*> option auto
          ( long "pitch"
         <> short 'p'
         <> showDefault
         <> value 30
         <> help "Pixel pitch of geotiff measured in geotiff values, e.g. SRTM data -> 30 m/pixel" )

data HGT = HGT { elevations :: [Int16] }
instance Binary HGT where
  put (HGT es) = do
    sequence_ $ putInt16be <$> es
  get = do
    HGT <$> replicateM (3601^2) getInt16be

-- getInt16be :: Get Int16
-- getInt16be = do a <- getWord16be
--                 return $ fromIntegral a
-- putInt16be :: Int16 -> Put
-- putInt16be i = putWord16be ((fromIntegral i) :: Word16)                
