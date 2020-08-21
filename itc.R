rm(list=ls())
library(raster)
library(caret)
library(mapview)
library(sf)
library(CAST)
library(plainview) 
library(rgdal)
library(glcm)
library(RStoolbox)
library(itcSegment)

#data_root <- "/Users/michael/GEO/DataScience/acacia_namibia/data/"
args <- commandArgs(trailingOnly = TRUE)
## LOAD RASTER DATA
#sat_img <- raster(paste(data_root, "acacia_sat_img.tif", sep=""),band=1)
sat_img <- raster(args[1],band=1)
##proj4string(sat_img)
##spplot(sat_img)

##imagery - An object of class raster on which to perform the segmentation. The image should be projected
##epsg - The EPSG code of the reference system of the image.
##searchWinSize - Size (in pixels) of the moving window used to the detect the local maxima. It should be an odd number larger than 3.
##TRESHSeed - Growing threshold 1. It should be between 0 and 1.
##TRESHCrown - Growing threshold 2. It should be between 0 and 1.
##DIST - Maximum value of the crown diameter of a detected tree (in meters).
##th - Digital number value below which a pixel cannot be a local maxima.
##ischm TRUE if the imagery is a Canopy Height Model (CHM). Default: FALSE.

#Looks OK but inverse
#seg <- itcIMG(sat_img,epsg=32733,search = 15, TRESHSeed =  0.50, TRESHCrown = 0.70, DIST = 10, th=1, ischm = FALSE)
seg <- itcIMG(sat_img,epsg=32733,search = 9, TRESHSeed =  0.5, TRESHCrown = 0.5, DIST = 7, th=5, ischm = FALSE)#seg2 <- itcIMG(sat_img,epsg=32733)
spplot(seg)
writeOGR(seg, paste(args[2], sep = ""), "segmentation", driver = "ESRI Shapefile")
