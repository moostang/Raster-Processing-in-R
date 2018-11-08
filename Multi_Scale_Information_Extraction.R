# Multi-scale Information Extraction
# ----------------------------------
# http://www.datacarpentry.org/R-spatial-raster-vector-lesson/06-vector-open-shapefile-in-r/
# set working directory to the directory location on your computer where
# you downloaded and unzipped the data files for the tutorial
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

# Get metadata from georeferenced landcover TIFF image.
# ----------------------------------------------------
landcover <- raster('TIFF/CLASSIFIED_UNSUPERVISED.tif')
study_extent <- landcover@extent
study_coord  <- landcover@crs


# Make Contingency Table
# ----------------------
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

# Assign x-y pixel-coordinates for Lead Area Index from database file
# -------------------------------------------------------------------
# Read DBF file
require(foreign)
df <- read.dbf('DATA/lai.dbf')
lai <- df  # backup

# Mask the LAI data to the extents of the study area, i.e. landcover@extent
x0 <- study_extent[1]
xn <- study_extent[2]
y0 <- study_extent[4]
yn <- study_extent[3]

# Create two new columns to store pixel locations corresponding to coordinates of LAI.
i <- 1
j <- 1

# Get total number of rows of df table
n <- dim(df)[1]

# Compute pixel locations---row and columns---for total number of rows
for(i in 1:n){
  in_x <-df[i,2] 
  out_x <- ceiling((in_x - x0)/30)
  in_y <-df[i,3] 
  out_y <- ceiling((y0 - in_y)/30) 
  df[i,5] <- out_x
  df[i,6] <- out_y
}

# Assign header names for the new two columns
names(df)[5] <- 'x_pixel'
names(df)[6] <- 'y_pixel'

# Read in the additional rasters for greenness, wetness, brightness, and the DEM as ordinary tiff
greenness  <- readTIFF('TIFF/LAB01_GREENNESS.tif', indexed = TRUE,  as.is=TRUE, info = TRUE)
attributes(greenness)
wetness  <- readTIFF('TIFF/LAB01_WETNESS.tif', indexed = TRUE,  as.is=TRUE, info = TRUE)
attributes(wetness)
brightness  <- readTIFF('TIFF/LAB01_BRIGHTNESS.tif', indexed = TRUE,  as.is=TRUE, info = TRUE)
attributes(brightness)
elevation  <- readTIFF('TIFF/LAB01_DEM.tif', indexed = TRUE,  as.is=TRUE, info = TRUE)
attributes(elevation)

# Read individual Landsat bands as oridinary tiff
TM1  <- readTIFF('TIFF/LAB01_TM1.tif', indexed = TRUE,  as.is=TRUE, info = TRUE)
attributes(TM1)
TM2  <- readTIFF('TIFF/LAB01_TM2.tif', indexed = TRUE,  as.is=TRUE, info = TRUE)
attributes(TM2)
TM3  <- readTIFF('TIFF/LAB01_TM3.tif', indexed = TRUE,  as.is=TRUE, info = TRUE)
attributes(TM3)
TM4  <- readTIFF('TIFF/LAB01_TM4.tif', indexed = TRUE,  as.is=TRUE, info = TRUE)
attributes(TM4)
TM5  <- readTIFF('TIFF/LAB01_TM5.tif', indexed = TRUE,  as.is=TRUE, info = TRUE)
attributes(TM5)
TM7  <- readTIFF('TIFF/LAB01_TM7.tif', indexed = TRUE,  as.is=TRUE, info = TRUE)
attributes(TM7)

# Read aspect and slope files
ASPECT  <- raster('TIFF/LAB02_ASPECT.tif')
ASPECT
SLOPE  <- raster('TIFF/LAB02_SLOPE.tif')
SLOPE

# Calculate NDVI Residual Severity Ratio (RSR)
NDVI <- ((TM4 - TM3)/(TM4 + TM3))*(1 - (TM5 - min(TM5))/(max(TM5) - min(TM5)) ) 
RSR <- (TM4/TM3)*( (max(TM5)-TM5)/(max(TM5) - min(TM5)) )

# Expand df table by including parameter value at those pixel locations. 
for(i in 1:n){
  x        <- df$x_pixel[i]
  y        <- df$y_pixel[i]
  df[i, 7] <- greenness[ y,x]
  df[i, 8] <- wetness[   y,x]
  df[i, 9] <- brightness[y,x]
  df[i,10] <- elevation[ y,x]
  df[i,11] <- TM1[       y,x]
  df[i,12] <- TM2[       y,x]
  df[i,13] <- TM3[       y,x]
  df[i,14] <- TM4[       y,x]
  df[i,15] <- TM5[       y,x]
  df[i,16] <- TM7[       y,x]
  df[i,17] <- landcover[ y,x]
  df[i,18] <- SLOPE[     y,x]
  df[i,19] <- ASPECT[    y,x]
  df[i,20] <- NDVI[      y,x]
}
df$Slope <- as.numeric(df$Slope)
df$Aspect <- as.numeric(df$Aspect)

# Rename coloumn header
names(df)[7:20] <- c("Greenness", "Wetness", "Brightness", "Elevation", "TM1", "TM2", "TM3", "TM4", "TM5", "TM7", 
                     "Landcover", "Slope", "Aspect", "NDVI")
names(df)

# Assignment of numeric values to landcover classes
landcover_class <- c("Coniferous Forest", "Shadows", "Mixed Forest", "Broadleaf Forest", "Shrub", "Herbaceous", 
                     "Barren Land", "Snow", "Water")
landcover_num   <- c(16, 17, 18, 19, 20, 21,  23 , 25, 30)

# Make new column for writing Landcover classes
for(i in 1:n){
  for(j in 1:length(landcover_num)){
    if(df$Landcover[i] == landcover_num[j]){
      df[i,21] <- landcover_class[j]
    }
  }
}
names(df)[21] <- "Class"
names(df)
#

# Include RSR values in the df table
for(i in 1:n){
  x <- df$x_pixel[i]
  y <- df$y_pixel[i]
  df[i,22] <- RSR[ y,x]
}
names(df)[22] <- "RSR"


# Output df table as csv file
# ---------------------------
write.csv(df, 'LAI.csv')

# Calculate correlation matrix
correlation_P <- cor(df[c(4,7:17,20)], method="pearson")
# Output results
write.csv(correlation_P, "correlation_P_LAI.csv")
# Plot correlation matrix
correlation_P #or just print the entire dataframe
corrplot(correlation_P, type = "upper", order = "hclust", method = "ellipse",
         tl.cex = 0.7, tl.col = "black", tl.srt = 45)
#

# -----------------------------------------------------
# Stepwise Regression
# -----------------------------------------------------
# Perform a stepwise regression with NDVI, Greenness, Wetness, and Brightness to determine the best 
# model that characterizes the LAI information. 
library(MASS)
fit <- lm(df$LAI~df$NDVI + df$Greenness + df$Wetness + df$Brightness,data=df)
step <- stepAIC(fit, direction="both")
step$anova # display results



Step0 <- lm(df$LAI~ df$Wetness + df$Greenness + df$Brightness)
summary(Step0)

Step1 <- lm(df$LAI~ df$Greenness + df$Brightness)
summary(Step1)

Step2 <- lm(df$LAI~ df$Greenness + df$Wetness)
summary(Step2)

Step3 <- lm(df$LAI~ df$Brightness + df$Wetness)
summary(Step3)

Step4 <- lm(df$LAI~ df$Wetness)
summary(Step4)

Step5 <- lm(df$LAI~ df$Brightness)
summary(Step5)

Step6 <- lm(df$LAI~ df$Greenness)
summary(Step6)

Step7 <- lm(df$LAI~ df$NDVI)
summary(Step7)

Step8 <- lm(df$LAI~ df$RSR)
summary(Step8)

AIC(Step0)
AIC(Step1)
AIC(Step2)
AIC(Step3)
AIC(Step4)
AIC(Step5)
AIC(Step6)
AIC(Step7)
AIC(Step8)

step(lm(df$LAI~df$NDVI + df$Greenness + df$Wetness + df$Brightness, data=df), direction="both")


lai_map <- Step1$coefficients[1] + Step1$coefficients[2]*greenness + Step1$coefficients[3]*brightness
lai_map <- raster(lai_map)
lai_map@extent <- study_extent
lai_map@crs <- study_coord
#plot(lai_map)
#writeRaster(lai_map, 'lai_map_no_mask.tif', overwrite=TRUE)
# STOP AND CHECK FOR STRANGE VALUES IN FORESTED AREA
#
#lc <- raster('TIFF/CLASSIFIED_UNSUPERVISED.tif')
#mask_all <- lc
##mask_all[mask_all > 0] <- 0
#mask_all[lc == 16] <- 1
#mask_all[lc == 18] <- 1
#mask_all[lc == 19] <- 1
#mask_all[lc == 20] <- 1
#mask_all[lc == 21] <- 1

water_mask <- raster("TIFF/LAB01_WATER_MASK.tif")
lai_map[water_mask == 1] <- 0
set.seed(1)
#lai_map <- lai_map*mask_all
####################
# Make sure that there are not many values below 0 
# in Forested Region. Other areas can have other values.
####################
lai_map[lai_map <= 0] <- 0
lai_map <- ratify(lai_map)
rm(water_mask, lc)
writeRaster(lai_map, 'TIFF/FINAL_LAI.tif', datatype='INT1U', overwrite=TRUE)
plot(lai_map)

