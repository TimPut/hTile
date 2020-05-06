{- |
Copyright: (c) 2020 tim put
SPDX-License-Identifier: GPL-3.0-only
Maintainer: tim put <timpu@gmail.com>

Convert geotiffs and other images to STLs for printing or machining.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module HTile where

import           Data.Binary          hiding (get)
import           Data.Massiv.Array
import           Data.Massiv.Array.IO hiding (V3)
import           Graphics.STL
import           Linear               hiding (project)
import           Prelude              hiding (map)

flattenTuples :: [(a,a)] -> [a]
flattenTuples []         = []
flattenTuples ((a,b):xs) = a:b: flattenTuples xs

-- The source files I've used are ~30m/px in the horizontal with pixel
-- values in meters. If we scale down 1:100, then load the resulting
-- STL in a slicer which assumes mm scale units, then we get a
-- 1:100000 scale result, which is a good ballpark scale factor.
scale :: Triangle -> Triangle
scale (V4 n a b c) = rebuildNormal $ V4 n (s a) (s b) (s c)
  where s (V3 x y z) = let m = 100 in V3 (x * (30/m)) (y*(30/m)) (z/m)

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
triangulation = makeStencilDef 0 (Sz (2 :. 2)) (0 :. 0) $ \ get ->
               let tl = get (0 :. 0)
                   tr = get (1 :. 0)
                   bl = get (0 :. 1)
                   br = get (1 :. 1)
               in (,) <$> (V4 <$> 0 <*> tl <*> bl <*> tr) <*> (V4 <$> 0 <*> tr <*> bl <*> br)
{-# INLINE triangulation #-}

-- Takes array of vertices to an array of pairs of triangles, each
-- pair being the triangles which result from the square of vertices
-- located by the top-left vertex. Note that the resulting array is
-- smaller than the input by one in each direction.
mkTop :: Array U Ix2 (V3 Float) -> Array DW Ix2 (Triangle, Triangle)
mkTop = applyStencil noPadding triangulation


frontSideStencil :: Stencil Ix1 (V3 Float) (Triangle, Triangle)
frontSideStencil = makeStencilDef 0 (Sz 2) (0) $ \ get ->
               let tl = get (0)
                   tr = get (1)
                   bl = projectOnXy <$> tl
                   br = projectOnXy <$> tr
               in (,) <$> (V4 <$> 0 <*> tl <*> bl <*> tr) <*> (V4 <$> 0 <*> tr <*> bl <*> br)
{-# INLINE frontSideStencil #-}

backSideStencil :: Stencil Ix1 (V3 Float) (Triangle, Triangle)
backSideStencil = makeStencilDef 0 (Sz 2) (0) $ \ get ->
               let tl = get (0)
                   tr = get (1)
                   bl = projectOnXy <$> tl
                   br = projectOnXy <$> tr
               in (,) <$> (V4 <$> 0 <*> tl <*> tr <*> bl) <*> (V4 <$> 0 <*> tr <*> br <*> bl)
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
            Array r Ix2 (V3 Float) ->
          [(Triangle,Triangle)]
mkEdges arr =
  let (Sz2 x y) = size arr
      f = toList . dropWindow
      upEdge = f $ mkFrontSides (arr !> 0)
      downEdge  = f $ mkBackSides (arr !> (x-1))
      leftEdge  = f $ mkFrontSides (arr <! 0)
      rightEdge  = f $ mkBackSides (arr <! (y-1))
  in upEdge ++ downEdge ++ leftEdge ++ rightEdge

unsafeNormalize :: V3 Float -> V3 Float
unsafeNormalize v = fmap (/sqrt l) v
  where l = quadrance v
