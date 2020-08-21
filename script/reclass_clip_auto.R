library(maptools)  ## For wrld_simpl
library(raster)
library(sf)
library(mapview)

## This will take the output of the ML_SpatialPrediction_auto.R script
classif <- raster("/Users/michael/GEO/DataScience/acacia_namibia/data/pred_camp4.tif")

acacia_campX <-classif
acacia_campX[!acacia_campX==1]<-NA
mapview(acacia_campX,map.types = "Esri.WorldImagery")#,maxpixels =  63201546)

acc <- writeRaster(acacia_campX, "/Users/michael/Downloads/acacia_class_only_campX.tif",overwrite=TRUE) 
acc_read <- raster(acc)

#v <- rasterToPolygons(acc_read, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=True)

## Example SpatialPolygonsDataFrame
v <- read_sf("/Users/michael/Downloads/ext.shp")

## This is the 3 band raster original from DG (mosaic AOI)
r <- stack("/Users/michael/GEO/DataScience/acacia_namibia/data/Camp4_utm.tif")

## crop and mask
crop_mask <- mask(r, v)

## Check that it worked
plot(crop_mask)
mapview(crop_mask,map.types = "Esri.WorldImagery")

# Save raster
writeRaster(crop_mask, "/Users/michael/Downloads/TreesOnly_CampX.tif",overwrite=TRUE)
