rm(list=ls())
library(maptools)
library(raster)
library(sf)
library(mapview)
library(ggplot2)

###----------------RECLASSIFICATION SCRIPT---------------------###
## This will take the output of the ML_SpatialPrediction_auto.R script
classif <- raster("/Users/michael/GEO/DataScience/acacia_namibia/data/pred_camp4.tif")

acacia_campX <-classif
acacia_campX[!acacia_campX==1]<-NA
mapview(acacia_campX,map.types = "Esri.WorldImagery")#,maxpixels =  63201546)

acc <- writeRaster(acacia_campX, "/Users/michael/Downloads/acacia_class_only__binary_campX.tif",overwrite=TRUE) 
acc_read <- raster(acc)

#temp_v <- rasterToPolygons(acc_read, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=True)
#v_save <- writeOGR(temp_v, "/Users/michael/Downloads/trees_only_campx_poly.shp", "segmentation", driver = "ESRI Shapefile")
v = v_save
###----------------CLIPPING SCRIPT---------------------###

## Example SpatialPolygonsDataFrame
#v <- read_sf(v_save)
v <- read_sf("/Users/michael/Downloads/ext.shp")

## This is the 3 band raster ORIGINAL from DG (mosaic AOI)
r <- stack("/Users/michael/GEO/DataScience/acacia_namibia/data/Camp4_utm.tif")

## crop and mask
crop_mask <- mask(r, v)

## Check that it worked
mapview(crop_mask,map.types = "Esri.WorldImagery")

# Save raster
writeRaster(crop_mask, "/Users/michael/Downloads/TreesOnly_CampX.tif",overwrite=TRUE)

###----------------GRIDDING SCRIPT---------------------###
# load some spatial data. Administrative Boundary
e <- extent(r)
p <- as(e, 'SpatialPolygons')
crs(p) <- proj4string(classif)
s <- shapefile(p, '/Users/michael/Downloads/extent_CampX.shp', overwrite=TRUE)
shp <- st_as_sf(s)
mapview(shp,map.types = "Esri.WorldImagery")
grid <- shp %>% 
  st_make_grid(n = c(250, 250), what = "polygons") %>% # grid of poly
  st_intersection(shp)  


ggplot() + 
geom_sf(data = shp) + 
geom_sf(data = grid)

# write the result to a text file containing locations or path to the image/shp result

#Do zonal and check if not null
