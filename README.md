# Multi-scale Information Extraction in R

### Abstract

The study of the earth’s surface from remote sensing data has dramatically changed our ability to monitor and assess environmental parameters on a multitude of scales and spatial resolutions. The study of forest products and their measurement cannot be limited to fixed discrete units as these are dynamic system that needs a continuous variable mapping methodology to gain a useful insight into their ecological processes. In this study, we generate relationship between our field observations of leaf area index and crown closure with the multi-spectral information of Landsat 5 TM imagery, wetness, greenness, and brightness layers. These information layers are modelled to generate a continuous-variable map for leaf area index and crown closure for the entire study area. In the end, we demonstrate of multi-scale information extraction through the generation of two new composite map products by using these layers are base layers.

*Keywords: Multi-scale information extraction, Landsat 5 Thematic Mapper*

## Raster Image Processing with R

Set working directory and load in libraries. Load libraries tiff, raster, rgdal for raster processing. 

```{r Load }
setwd('D:/CLASS_NOTES/WIN2018/GEOG633/LAB02/')
# Load required libraries
library('tiff')
library(corrplot)
library('rgdal')
# For vector work
library(sf)
# For metadata/attributes- vectors or rasters
library(raster)
require(foreign)
```
It is best to use 'raster' package when working with geo-referenced TIFF files. However, sometimes it is must faster to use the 'tiff' library when doing raster calculation. For such cases, use raster package to read and store spatial information such as cooridinate reference system (crs) and spatial extent

```{r}
# Get metadata from georeferenced landcover TIFF image.
# ----------------------------------------------------
landcover <- raster('TIFF/CLASSIFIED_UNSUPERVISED.tif')
study_extent <- landcover@extent
study_coord  <- landcover@crs
# Plot raster
raster::plot(landcover)
```
Here, we just read and stored the two parameters that were important for us: spatial extent and coordinate system. Now, we load the landcovr image we want to work with. 

```{r}
# Read in landcover image
landcover  <- readTIFF('TIFF/LAB01_LANDCOVER_FINAL_CORRECTED.tif', indexed = TRUE,  as.is=TRUE, info = TRUE)
attributes(landcover)
# Read in Water-masked image
water_mask <- readTIFF('TIFF/LAB01_WATER_MASK.tif',        indexed = FALSE, as.is=TRUE, info = FALSE, convert = TRUE)
attributes(water_mask)
# Assign water areas in Landcover as 'Water' class (Class ID = 30)
ls <- landcover
ls[water_mask == 0 ] <- as.integer(30)
# Clear memory of water mask image
rm(water_mask)
```

Here, two images were loaded: landcover and water_mask. The landcover image is the resulting image from PCI Geomatica after Isodata unsupervised classification. The water_mask is a binary image showing the area of water presence. Both of these images are of same size (pixelwise) and we assign a classId of '30' to water. 
