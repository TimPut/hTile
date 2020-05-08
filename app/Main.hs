{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Data.Binary           (encodeFile)
import           Data.ByteString.Char8 (pack)
import           Data.Massiv.Array
import           Data.Massiv.Array.IO
import qualified Data.Vector.Unboxed   as V
import           Graphics.STL
import           HTile
import           Options.Applicative hiding (header)

data Opts = Opts { infile :: String
                 , outfile :: String
                 , hex :: Bool
                 }
main :: IO ()
main = do
  let optsInfo = info (opts <**> helper) briefDesc
  args <- execParser optsInfo
  -- TODO: proper cli
  -- [inFile, outFile] <- getArgs :: IO [String]

  -- Automatically convert colour images to greyscale using the D65
  -- illuminant standard. If the image is already greyscale this
  -- transformation is the identity (this is not documented in
  -- massiv-io/color, but it is the obvious behaviour and it is what
  -- happens)
  file <- readImageAuto (infile args) :: IO (Image S (Y D65) Word16)
  let file' = delay file
      located = computeSource @U . locate . fmap raw $ file'
      (Sz2 x y) = size located

      x' = x - 1 -- n vertices in a row are connected by n-1 segments
      y' = y - 1
      -- num_facets = facets from the: up and down edges + left and
      -- right edges + top and bottom faces
      squareFacets = 2 * (2 * x') + 2 * (2 * y') + 2 * (2 * (x' * y'))

      hexFacets = let n = (min x y) `div` 2 in 12 * (n-2) * (n-1)

      sides = mkEdges located
      surface = dropWindow $ mkTop located
      top = toList . scaleArray $ surface
      bottom = toList . scaleArray $ mkBottom surface

      locatedRh = computeAs B . locateRh . resampleToHex . computeSource @U . fmap fromIntegral . fmap raw $ file'

      tris = if hex args 
             then V.fromListN hexFacets $ mkHex locatedRh 
             else V.fromListN squareFacets . flattenTuples $ (top++bottom++sides)

      r = STL { header = pack "Made with hTile"
              , numFacets = if hex args then fromIntegral hexFacets else fromIntegral squareFacets
              , triangles = tris
              }
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
 




