rm(list=ls())
library(maptools)
library(raster)
library(sf)
library(mapview)
library(ggplot2)
library(rgdal)
library(RStoolbox)
library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)
library(parallel)
#install.packages("evaluate")
#install.packages("snow")

## Activate Parallel Processing
detectCores()
# Detect the number of available cores and create cluster
cl <- parallel::makeCluster(detectCores())

# Activate cluster for foreach library
doParallel::registerDoParallel(cl)

###----------------RECLASSIFICATION SCRIPT---------------------###
## This will take the output of the ML_SpatialPrediction_auto.R script

args <- commandArgs(trailingOnly = TRUE)

pred_image <- args[1] #The predicted image from Step 1 (ML Acacia Prediction)
orig_image <- args[2] # The Original Raw Image
data_root <- args[3] #Path to data folder where you want to save results
out_poly <- args[4] #output polygon of acacia for clipping with extension
out_rast_rcl <- args[5] #output raster of the reclassification (0-1) with extension
out_rast_f <- args[6] #output clipped raster name with extension
out_grid <- args[7] #output grid shapefile with extension
classif <- raster(pred_image)


acacia_campX <-classif
acacia_campX[!acacia_campX==1]<-NA
##mapview(acacia_campX,map.types = "Esri.WorldImagery")#,maxpixels =  63201546)

acc <-  writeRaster(acacia_campX, paste(data_root,out_rast_rcl, sep = ""),overwrite=TRUE) 
#acc_read <- raster(acc)
acc_read <- raster(paste(data_root,out_rast_rcl, sep = ""))

acc_rpj <- projectRaster(acc_read, crs=CRS('+init=EPSG:32733'))
#temp_v <- rasterToPolygons(acc_read, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)

beginCluster(n=10)
temp_v <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(acc_read), 
                                         as_points = FALSE, merge = TRUE))
endCluster()
v_save <- writeOGR(temp_v, paste(data_root,out_poly, sep = ""), "segmentation", driver = "ESRI Shapefile")
v = v_save

###----------------CLIPPING SCRIPT---------------------###

## Example SpatialPolygonsDataFrame
v_tf <- read_sf(paste(data_root,out_poly, sep = ""))

## This is the 3 band raster ORIGINAL from DG (mosaic AOI)
#r <- stack(orig_image)
r <- stack(orig_image)

## crop and mask
beginCluster(n=10)
c <- crop(r, v_tf)
m <- mask(c,v_tf)
##mapview(m,map.types = "Esri.WorldImagery")

# Save raster
writeRaster(m, paste(data_root,out_rast_f, sep = ""),overwrite=TRUE)
endCluster()
###----------------GRIDDING SCRIPT---------------------###
e <- extent(c)
p <- as(e, 'SpatialPolygons')
proj4string(p) = CRS("+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs")
print(p)
#s <- shapefile(p, "/Users/michael/GEO/DataScience/acacia_namibia/sample_data/sample_extent.shp", overwrite=TRUE)
shp <- st_as_sf(p)
print(shp)
dim = 3
grid <- shp %>% 
  st_make_grid(n = c(dim, dim), what = "polygons") %>% # grid of poly
  st_intersection(shp)  

##mapview(grid,map.types = "Esri.WorldImagery")
st_write(grid, paste(data_root,out_grid, sep = ""))
