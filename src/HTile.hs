{- |
Copyright: (c) 2020 tim put
SPDX-License-Identifier: GPL-3.0-only
Maintainer: tim put <timput@gmail.com>

Convert geotiffs and other images to STLs for printing or machining.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE PartialTypeSignatures     #-}

module HTile where

import           Data.Binary          hiding (get)
import           Data.Massiv.Array
import           Data.Massiv.Array.Unsafe
import           Data.Massiv.Array.IO
-- import           Graphics.STL         hiding (Triangle)
import           Linear               hiding (project)
import           Prelude              hiding (map)
import           Hexagonal

type Triangle = V4 (V3 Float)

flattenTuples :: [(a,a)] -> [a]
flattenTuples []         = []
flattenTuples ((a,b):xs) = a:b: flattenTuples xs

-- The source files I've used are ~30m/px in the horizontal with pixel
-- values in meters. If we scale down 1:100, then load the resulting
-- STL in a slicer which assumes mm scale units, then we get a
-- 1:100000 scale result, which is a good ballpark scale factor.
scale :: Float -> Float -> Triangle -> Triangle
scale factor pitch (V4 n a b c) = rebuildNormal $ V4 n (s a) (s b) (s c)
  where s (V3 x y z) = let factor' = pitch*factor
                       in V3 (x*factor') (y*factor') (z*factor)

scaleArray :: (Source r i (V4 (V3 Float), V4 (V3 Float))) =>
         Float -> Float -> 
         Array r i (V4 (V3 Float), V4 (V3 Float)) ->
         Array D i (V4 (V3 Float), V4 (V3 Float))
scaleArray f p = map (\(t1,t2) -> (scale f p t1, scale f p t2))

-- Extract raw pixel data from colour space and pixel wrappers
raw :: (ColorModel cs e, Components cs e ~ Word16) => Pixel cs e -> Word16
raw = toComponents . pixelColor

rebuildNormal :: Triangle -> Triangle
rebuildNormal (V4 _ a b c) = let t = c - a
                                 u = b - a
                             in V4 (unsafeNormalize (t `cross` u)) a b c

-- Annotate pixels with their locations as a function of their indices
locate :: (Source r Ix2 Word16) => Array r Ix2 Word16 -> Array D Ix2 (V3 Float)
locate = imap (\ (x :. y) z -> V3 (s x) (s y) (s' z))
               where
                 s :: Int -> Float
                 s = fromIntegral
                 s' :: Word16 -> Float
                 s' = fromIntegral

-- triangulate on the up diagonal |/|
triangulation :: Stencil Ix2 (V3 Float) (Triangle, Triangle)
-- triangulation :: _
triangulation = makeUnsafeStencil (Sz (2 :. 2)) (0 :. 0) $ \ _ get ->
               let tl = get (0 :. 0)
                   tr = get (1 :. 0)
                   bl = get (0 :. 1)
                   br = get (1 :. 1)
               in (V4 0 tl bl tr, V4 0 tr bl br)
{-# INLINE triangulation #-}

-- Takes array of vertices to an array of pairs of triangles, each
-- pair being the triangles which result from the square of vertices
-- located by the top-left vertex. Note that the resulting array is
-- smaller than the input by one in each direction.
mkTop :: Array U Ix2 (V3 Float) -> Array DW Ix2 (Triangle, Triangle)
mkTop = applyStencil noPadding triangulation


frontSideStencil :: Stencil Ix1 (V3 Float) (Triangle, Triangle)
frontSideStencil = makeUnsafeStencil (Sz 2) 0 $ \ _ get ->
               let tl = get (0)
                   tr = get (1)
                   bl = projectOnXy tl
                   br = projectOnXy tr
               in (V4 0 tl bl tr, V4 0 tr bl br)
{-# INLINE frontSideStencil #-}

backSideStencil :: Stencil Ix1 (V3 Float) (Triangle, Triangle)
backSideStencil = makeUnsafeStencil (Sz 2) 0 $ \ _ get ->
               let tl = get (0)
                   tr = get (1)
                   bl = projectOnXy tl
                   br = projectOnXy tr 
               in (V4 0 tl tr bl, V4 0 tr br bl)
{-# INLINE backSideStencil #-}

mkFrontSides :: Manifest r Ix1 (V3 Float) =>
             Array r Ix1 (V3 Float) ->
             Array DW Ix1 (Triangle, Triangle)
mkFrontSides = applyStencil noPadding frontSideStencil

mkBackSides :: Manifest r Ix1 (V3 Float) =>
            Array r Ix1 (V3 Float) ->
            Array DW Ix1 (Triangle, Triangle)
mkBackSides = applyStencil noPadding backSideStencil

mkBottom :: (Source r i (V4 (V3 Float), V4 (V3 Float))) =>
         Array r i (V4 (V3 Float), V4 (V3 Float)) ->
         Array D i (V4 (V3 Float), V4 (V3 Float))
mkBottom = map (\(t1,t2) -> (projectTriangle t1, projectTriangle t2))

-- projectTriangle flips the triangle orientation (swaps c and b) to
-- produce the correct normals.
projectTriangle :: Triangle -> Triangle
projectTriangle (V4 _ a b c) = V4 0 (projectOnXy a) (projectOnXy c) (projectOnXy b)

projectOnXy :: V3 Float -> V3 Float
projectOnXy (V3 x y _) = V3 x y 0

mkEdges :: ( Manifest (R r) Ix1 (V3 Float)
          , OuterSlice r Ix2 (V3 Float)
          , (InnerSlice r Ix2 (V3 Float))) =>
          Float -> Float ->
            Array r Ix2 (V3 Float) ->
          [(Triangle,Triangle)]
mkEdges a p arr =
  let (Sz2 x y) = size arr
      f = toList . scaleArray a p . dropWindow
      upEdge = f $ mkFrontSides (arr !> 0)
      downEdge  = f $ mkBackSides (arr !> (x-1))
      leftEdge  = f $ mkFrontSides (arr <! 0)
      rightEdge  = f $ mkBackSides (arr <! (y-1))
  in upEdge ++ downEdge ++ leftEdge ++ rightEdge

unsafeNormalize :: V3 Float -> V3 Float
unsafeNormalize v = fmap (/ sqrt l) v
  where l = quadrance v

-- TODO rewrite hex utilites for better interface with massiv, less
-- wrapping and unwrapping of Linear.V2
resampleToHex
  :: (Floating e, RealFrac e, Manifest r1 Ix2 e,
      Construct r2 Ix2 e) =>
     Array r1 Ix2 e -> Array r2 Ix2 e
resampleToHex arr = makeArray Par (Sz (hx :. hy)) indexer
  where
    indexer = \(i :. j) -> bilinearStoRh (\(V2 x y) -> arr ! (x :. y)) (V2 i j)
    (Sz2 sx sy) = size arr
    V2 hx hy = fmap (\a -> a - 1) . fmap (floor :: Float -> Int) $ sToRh (V2 sx sy)

-- Annotate row offset hexagonal grid pixels with their Cartesian
-- coordinates as a function of their indices.
locateRh :: Array D Ix2 Float -> Array D Ix2 (V3 Float)
locateRh = imap (\ (x :. y) z -> let V2 x' y' = rhToS (V2 x y) in V3 x' y' z)

mkTopRh
  :: (Manifest r Ix2 e, Num e, Extract r Ix2 e,
      Source (R r) Ix2 e) =>
     Array r Ix2 e -> Array D Ix2 (V4 e, V4 e)
mkTopRh arr = imap mkTriangles (extract' (0 :. 0) (Sz (x-1 :. y-1)) arr)
  where
    Sz2 x y = size arr
    mkTriangles (i :. j) v =
        let
            tl =  v -- arr ! (i :. j)
            tr = arr ! (i+ 1 :. j)
            bl = arr ! (i :. j + 1)
            br = arr ! (i + 1 :. j + 1)
        in if even j
           then (V4 0 tl bl tr, V4 0 tr bl br)
           else (V4 0 tl bl br, V4 0 tl br tr)

-- TODO this bound is conservative in some cases
maxHex :: Sz2 -> (Int, V2 Int)
maxHex (Sz2 x y) = let l = ((min x y) `div` 2) - 1 in (l, V2 ((l `div` 2) + 1) 0)

mkHexEdges
  :: (Manifest r Ix2 (V3 Float), Integral a2) =>
     Array r Ix2 (V3 Float)
     -> a2 -> V2 Int -> [(V4 (V3 Float), V4 (V3 Float))]
mkHexEdges arr l offset =
  let 
      vs = fmap (\(V2 i j) -> index' arr (i :. j)) $ hexagonIndicesRh l offset
  in  mkSideTris vs
  where
    mkSideTris (tl:tr:hs) = let bl = projectOnXy tl
                                br = projectOnXy tr
                            in (V4 0 tl bl tr, V4 0 tr bl br):mkSideTris (tr:hs)
    mkSideTris _ = []

mkHex
  :: (Manifest r Ix2 (V3 Float), Extract r Ix2 (V3 Float),
      Source (R r) Ix2 (V3 Float)) =>
    Float -> Float ->
     Array r Ix2 (V3 Float) -> [Triangle]
mkHex f p arr = hTop ++ hBottom ++ hSides
    where
      (l, offset) = maxHex $ size arr
      hSurface = filter (\(V4 _ a b c) -> vertexInHex a && vertexInHex b && vertexInHex c) . flattenTuples . toList . mkTopRh $ arr
      hTop = fmap (scale f p) $ hSurface
      hBottom = fmap (scale f p) . fmap projectTriangle $ hSurface
      hSides = fmap (scale f p) . flattenTuples $ mkHexEdges arr l offset
      epsilon = 0.5
      shift (V2 x y) =  V2 (x - epsilon / 2) (y - epsilon / sqrt 3)
      vertexInHex (V3 x y _) = pointInHex (fromIntegral l + epsilon ) (shift . fmap fromIntegral $ offset) (V2 x y)
