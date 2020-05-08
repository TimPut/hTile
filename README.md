# hTile

[![Hackage](https://img.shields.io/hackage/v/hTile.svg?logo=haskell)](https://hackage.haskell.org/package/hTile)
[![GPL-3.0-only license](https://img.shields.io/badge/license-GPL--3.0--only-blue.svg)](LICENSE)

Convert geotiffs and other images to STLs for printing or machining.

![](imgs/sample.jpeg?raw=true)<sup>[1](#fstl)</sup>
![](imgs/hfstl.jpeg?raw=true)<sup>[1](#fstl)</sup>

# Usage
If you are looking for topographic data to use with this, I recommend
getting it from https://www.eorc.jaxa.jp/ALOS/en/aw3d30/.

```
hTile -i inputFile.tif -o outputFile.stl 
```
```
hTile -i inputFile.tif -o outputFile.stl --hex
```

The hex output option also resamples (bilinear) the input data on a
regular hexagonal grid. This produces more aesthetically pleasing
surfaces in the STL, since the surfaces are composed primarily of
nearly equilateral triangles.

# Known Bugs
1) Breaks on tifs with nonstandard metadata. This is a problem upstream
   with Juicy-Pixels. The workaround is to preprocess the images by
   stripping the meta data with
   ```
   mogrify -strip inputFile.tif
   ```
# TODO
1) [x] Currently processes entire rectangular area of the source image into
   STL, we would like to eventually process hexagonal sub-regions.
2) [ ] Currently processes whole image into a single STL, we would like to
   tile multiple STLs out of a single source image.
3) [ ] ~~Currently assumes monochrome tif source file. Need to add format
   checking, and compatibility with other formats.~~ Relatively
   untested, but should work now.


<a name="fstl">1</a>: preview rendered with https://www.mattkeeter.com/projects/fstl/
