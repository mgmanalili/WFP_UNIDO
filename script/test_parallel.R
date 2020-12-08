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
library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)
library(parallel)

sat_img <- raster('/Users/michael/GEO/DataScience/acacia/data/for_itc_small.tif')

#Define how many cores you want to use
UseCores <- detectCores() -1
#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)  


foreach(i=1:length(sat_img)) %dopar% {
  library(raster)
  
  seg <- itcIMG(sat_img,epsg=32733,search = 9, TRESHSeed =  0.5, TRESHCrown = 0.5, DIST = 7, th=5, ischm = FALSE)
  
  v_save <- writeOGR(seg, '/Users/michael/GEO/DataScience/acacia/data/test_par_novol.shp', "segmentation", driver = "ESRI Shapefile")
  
  v = v_save
}
