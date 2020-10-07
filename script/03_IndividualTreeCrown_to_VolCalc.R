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
library(foreign)
#Check empty grids
args <- commandArgs(trailingOnly = TRUE)

segmentation <- function(image, output_raw, output_final)
                {
                args <- commandArgs(trailingOnly = TRUE)
                sat_img <- raster(image,band=1)
                proj4string(sat_img)
                ##imagery - An object of class raster on which to perform the segmentation. The image should be projected
                ##epsg - The EPSG code of the reference system of the image.
                ##searchWinSize - Size (in pixels) of the moving window used to the detect the local maxima. It should be an odd number larger than 3.
                ##TRESHSeed - Growing threshold 1. It should be between 0 and 1.
                ##TRESHCrown - Growing threshold 2. It should be between 0 and 1.
                ##DIST - Maximum value of the crown diameter of a detected tree (in meters).
                ##th - Digital number value below which a pixel cannot be a local maxima.
                ##ischm TRUE if the imagery is a Canopy Height Model (CHM). Default: FALSE.
                
                seg <- itcIMG(sat_img,epsg=32733,search = 9, TRESHSeed =  0.5, TRESHCrown = 0.5, DIST = 7, th=5, ischm = FALSE)
                writeOGR(seg, output_raw, "segmentation", driver = "ESRI Shapefile")
                
                gridx <- readOGR(output_raw)
                #mapview(gridx,map.types = "Esri.WorldImagery")
                ##gridx$CR_m <- gridx$length(shortest_line(centroid($geometry),boundary($geometry)))
                
                gridx$CR_m <- sqrt(gridx$CA_m2 / 3.1416)
                gridx$CR_dia <-gridx$CR_m*2
                gridx$CR_sqrd <-gridx$CR_dia^2
                ###Ashraf to replace new vol formula
                
                gridx$vol <- (-0.0263+0.0024)*gridx$CR_sqrd
                st_write(st_as_sf(gridx),output_final)
                
                }

t <- segmentation("/Users/michael/GEO/DataScience/acacia_namibia/sample_data/out_trees_only.tif", 
                  "/Users/michael/GEO/DataScience/acacia_namibia/sample_data/acacia_poly_raw.shp",
                  "/Users/michael/GEO/DataScience/acacia_namibia/sample_data/acacia_poly_vol_FINAL.shp")


#Below is a work-in progress. DO NOT DELETE! Keep in commented. - MM
#segmentation()
#file_list <- file('/Users/michael/GEO/DataScience/acacia_namibia/input_tif.txt')
##file_list <- file(args[1])
#flist <- readLines(file_list, warn=FALSE)
##print(flist)
##vector_list <- file(args[2])
#vector_list <- file('/Users/michael/GEO/DataScience/acacia_namibia/out_shp.txt')
#vlist <- readLines(vector_list, warn=FALSE)
##print(vlist)
#print('Running ITC Segmentation. Please wait...')
#for(i in flist)
#  for(j in vlist)
#{segmentation(i, j)}

#a = mapply(c,flist, vlist, SIMPLIFY = FALSE)

#for(i in a)
#  segmentation(i)
#
## merge all shapefile
#for(j in vlist)
#  print(j)
#  #union(j)

# Crown volume computation
#vol_calc <- function(shapefile, out_shapefile){
  #Compute volume for merged file
  #gridx <- read_sf(union)
  #gridx = union
  
#  gridx <- readOGR(shapefile)
  #mapview(gridx,map.types = "Esri.WorldImagery")
  ##gridx$CR_m <- gridx$length(shortest_line(centroid($geometry),boundary($geometry)))
  
#  gridx$CR_m <- 0.55
#  gridx$CR_dia <-gridx$CR_m*2
#  gridx$CR_sqrd <-gridx$CR_dia^2
  ###Ashraf to replace new vol formula
  
#  gridx$vol <- (-0.0263+0.0024)*gridx$CR_sqrd
#  st_write(st_as_sf(gridx),out_shapefile)
  
  ##mapview(gridx,map.types = "Esri.WorldImagery")
#}