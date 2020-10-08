rm(list=ls())
library(maptools)
library(raster)
library(sf)
library(mapview)
library(ggplot2)
library(rgdal)
library(RStoolbox)

###----------------RECLASSIFICATION SCRIPT---------------------###
## This will take the output of the ML_SpatialPrediction_auto.R script
classif <- raster("/Users/michael/GEO/DataScience/acacia_namibia/sample_data/out_ml_pred.tif")

acacia_campX <-classif
acacia_campX[!acacia_campX==1]<-NA
mapview(acacia_campX,map.types = "Esri.WorldImagery")#,maxpixels =  63201546)

acc <- writeRaster(acacia_campX, "/Users/michael/GEO/DataScience/acacia_namibia/sample_data/out_rcl_bin.tif",overwrite=TRUE) 
#acc_read <- raster(acc)
acc_read <- raster("/Users/michael/GEO/DataScience/acacia_namibia/sample_data/out_rcl_bin.tif")

acc_rpj <- projectRaster(acc_read, crs=CRS('+init=EPSG:32733'))
temp_v <- rasterToPolygons(acc_read, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
v_save <- writeOGR(temp_v, "/Users/michael/GEO/DataScience/acacia_namibia/sample_data/out_rcl_bin_poly.shp", "segmentation", driver = "ESRI Shapefile")
v = v_save
###----------------CLIPPING SCRIPT---------------------###

## Example SpatialPolygonsDataFrame
v <- read_sf("/Users/michael/GEO/DataScience/acacia_namibia/sample_data/out_rcl_bin_poly.shp")
v_tf <- st_transform(v, crs = 32733)
## This is the 3 band raster ORIGINAL from DG (mosaic AOI)
r <- stack("/Users/michael/GEO/DataScience/acacia_namibia/data/Camp4_utm.tif")

## crop and mask
c <- crop(r, v_tf)
m <- mask(c,v_tf)
mapview(m,map.types = "Esri.WorldImagery")

# Save raster
writeRaster(m, "/Users/michael/GEO/DataScience/acacia_namibia/sample_data/out_clip_raster.tif",overwrite=TRUE)

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

mapview(grid,map.types = "Esri.WorldImagery")
st_write(grid, "/Users/michael/GEO/DataScience/acacia_namibia/sample_data/out_grid.shp")
