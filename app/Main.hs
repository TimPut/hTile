{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Data.Binary           (encodeFile)
import           Data.ByteString.Char8 (pack)
import           Data.Massiv.Array
import           Data.Massiv.Array.IO
import qualified Data.Vector.Unboxed   as V
import           Graphics.STL
import           HTile
import           System.Environment    (getArgs)

main :: IO ()
main = do
  -- TODO: proper cli
  [inFile, outFile] <- getArgs :: IO [String]

  -- Automatically convert colour images to greyscale using the D65
  -- illuminant standard. If the image is already greyscale this
  -- transformation is the identity (this is not documented in
  -- massiv-io/color, but it is the obvious behaviour and it is what
  -- happens)
  file <- readImageAuto inFile :: IO (Image S (Y D65) Word16)
  let file' = delay file
      located = computeSource @U . locate . fmap raw $ file'
      (Sz2 x y) = size located

      x' = x - 1 -- n vertices in a row are connected by n-1 segments
      y' = y - 1
      -- num_facets = facets from the: up and down edges + left and
      -- right edges + top and bottom faces
      facets = 2 * (2 * x') + 2 * (2 * y') + 2 * (2 * (x' * y'))

      sides = mkEdges located
      surface = dropWindow $ mkTop located
      top = toList . scaleArray $ surface
      bottom = toList . scaleArray $ mkBottom surface

      tris = V.fromListN facets . flattenTuples $ (top++bottom++sides)
      r = STL { header = pack "Made with hTile"
              , numFacets = fromIntegral facets
              , triangles = tris
              }
  encodeFile outFile r
