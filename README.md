# hTile

[![Hackage](https://img.shields.io/hackage/v/hTile.svg?logo=haskell)](https://hackage.haskell.org/package/hTile)
[![GPL-3.0-only license](https://img.shields.io/badge/license-GPL--3.0--only-blue.svg)](LICENSE)

Convert geotiffs and other images to STLs for printing or machining.

![](imgs/sample.jpeg?raw=true)<sup>[1](#fstl)</sup>
![](imgs/hfstl.jpeg?raw=true)<sup>[1](#fstl)</sup>

# Getting Data
If you are looking for topographic data to use with this, I recommend
getting it from https://e4ftl01.cr.usgs.gov/MEASURES/NASADEM_HGT.001/2000.02.11/ <s>https://www.eorc.jaxa.jp/ALOS/en/aw3d30/</s>

# Usage
```
> hTile --help
Usage: hTile (-i|--input SOURCE) (-o|--output TARGET) [-h|--hex]
             [-s|--scale ARG] [-p|--pitch ARG]

Available options:
  -i,--input SOURCE        Source image
  -o,--output TARGET       STL destination
  -h,--hex                 Build hexagonal prisms
  -s,--scale ARG           Scale factor from geotiff values to STL units
                           (default: 100.0)
  -p,--pitch ARG           Pixel pitch of geotiff measured in geotiff values,
                           e.g. SRTM data -> 30 m/pixel (default: 30.0)
  -h,--help                Show this help text
```

The hex output option also performs a bilinear resampling of the input
data on a regular hexagonal grid. This produces more aesthetically
pleasing surfaces in the STL, since the surfaces are composed primarily
of nearly equilateral triangles.

# Known Bugs
1) Breaks on tif files with nonstandard metadata which includes many geotiffs.
   This is a problem upstream with Juicy-Pixels. The workaround is to
   preprocess the images by stripping the meta data with
   ```
   mogrify -strip inputFile.tif
   ```
<a name="fstl">1</a>: preview rendered with https://www.mattkeeter.com/projects/fstl/
