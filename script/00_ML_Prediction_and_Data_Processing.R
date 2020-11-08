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
library(maptools)
library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)
library(parallel)

options(warn=-1)

## Activate Parallel Processing
detectCores()
# Detect the number of available cores and create cluster
cl <- parallel::makeCluster(detectCores())

# Activate cluster for foreach library
doParallel::registerDoParallel(cl)

#------
#image <- args[1] #Path to the raw satellite/drone image (e.g. path/to/data/my_raster.tif)
#name_rast <- args[2] #name of input raster without extension (e.g. my_raster)
#roi <- args[3] #Path to the training polygon (e.g. path/to/data/roi.shp)
#data_root <- args[4] #Path to data folder where you want to save results
#out <- args[5] # Output ML prediction raster with extension (e.g. pred_raster.tif)
#out_poly <- args[6] #output polygon of acacia for clipping with extension
#out_rast_rcl <- args[7] #output raster of the reclassification (0-1) with extension
#out_rast_f <- args[8] #output clipped raster name with extension (itc_clipped.tif)
#arg2_out_raw <- args[9] # Output ITC segmentation shapefile in raw form (no volume computation)
#arg3_result <- args[10] # Output ITC segmentation shapefile with volume computation
#------

args <- commandArgs(trailingOnly = TRUE)
params <- args[1] 
#params <- "/Users/michael/GEO/DataScience/acacia/params.txt"
con <- file(params, open='r')
#con <- file(params, open='r')
image <- readLines(con)[[1]]
image = image
con <- file(params, open='r')
name_rast <- readLines(con)[[2]]
name_rast = name_rast
con <- file(params, open='r')
roi <- readLines(con)[[3]]
roi = roi
con <- file(params, open='r')
data_root <- readLines(con)[[4]]
data_root = data_root
con <- file(params, open='r')
out <- readLines(con)[[5]]
out = out
con <- file(params, open='r')
out_poly <- readLines(con)[[6]]
out_poly = out_poly
con <- file(params, open='r')
out_rast_rcl <- readLines(con)[[7]]
out_rast_rcl = out_rast_rcl
con <- file(params, open='r')
out_rast_f <- readLines(con)[[8]]
out_rast_f = out_rast_f
## Uncomment once the itcsegment parallel is fixed
#con <- file(params, open='r')
#arg2_out_raw <- readLines(con)[[9]]
#arg2_out_raw = arg2_out_raw
#con <- file(params, open='r')
#arg3_result <- readLines(con)[[10]]
#arg3_result = arg3_result

##----------------------------START OF SCRIPT----------------------------##
#The predicted image from Step 1 (ML Acacia Prediction)
orig_image = image # The Original Raw Image
sat_img <- raster(image)
trainSites <- read_sf(roi)

out = paste(data_root,out, sep = "")

beginCluster(n=10)
## LOAD RASTER DATA
rglcm <- glcm(sat_img,window = c(9,9), 
              shift = c(1,1),
              statistics = c("variance", "homogeneity","dissimilarity","entropy"))
glcm_brick <- stack(rglcm)
## PCA Operation
sat_img_s <- stack(sat_img)
rand_sam <- sampleRandom(sat_img_s, 10000) # sample 5000 random grid cells
pca <- prcomp(rand_sam, scale=TRUE, retx=FALSE) 
pca_brick <- predict(sat_img_s, pca, index=1:3) # create new rasters based on PCA predictions
#nlayers(rglcm)

## MACHINE LEARNING MODEL
predStack <- stack(rglcm,pca_brick,sat_img) #Add DEM - Resample and clip in QGIS
nlayers(predStack)
names(predStack)
names(predStack) <- c('glcm_variance', 'glcm_homogeneity', 'glcm_dissimilarity',
                      'glcm_entropy','layer.1','layer.2','layer.3',name_rast)
names(predStack)
#spplot(scale(predStack))

trainSites <- st_transform(trainSites,crs=projection(predStack))

extr <- extract(predStack, trainSites, df=TRUE,na.rm=TRUE)
#extr <- merge(x = extr, y = trainSites, by = "id")
extr <- merge(extr, trainSites, by.x="ID", by.y="id",all.y=TRUE)

#head(extr)

set.seed(100)
trainids <- createDataPartition(extr$ID,list=FALSE,p=0.15)
trainDat <- extr[trainids,]

predictors <- c('glcm_variance', 'glcm_homogeneity', 'glcm_dissimilarity',
                'glcm_entropy','layer.1','layer.2','layer.3',name_rast)

response <- "type"

ctrl <- trainControl(method="cv", 
                     number =10, 
                     savePredictions = TRUE)

# train the model
set.seed(100)
model <- train(trainDat[,predictors],
               trainDat[,response],
               method="rf",
               metric="Kappa",
               trControl=ctrl,
               importance=TRUE,
               ntree=75)
print(model)

# get all cross-validated predictions:
cvPredictions <- model$pred[model$pred$mtry==model$bestTune$mtry,]
# calculate Kappa etc:
confusionMatrix(cvPredictions$pred,cvPredictions$obs)$overall

# do prediction:
prediction <- predict(predStack,model)

#assign some colors that are easy to interpret visually (optional only if running inside R)
#cols_df <- data.frame("Type_en"=c("AC","BU","DV","PO", "RD","SO","SV","SH","GR")

v <- read_sf(roi)
x <- unique(v[["type"]])
cols_df <- data.frame("Type_en"=x)

#plot prediction:
#spplot(prediction,col.regions=as.character(cols_df$col))

#spfolds <- read_sf(paste(data_root,"spfolds.shp", sep = ""))
#mapview(spfolds,map.types = "Esri.WorldImagery")+
#  mapview(trainSites)

set.seed(100)
folds <- CreateSpacetimeFolds(trainDat, spacevar="ID", k=10)

ctrl_sp <- trainControl(method="cv",
                        savePredictions = TRUE,
                        index=folds$index,
                        indexOut=folds$indexOut)

set.seed(100)
model_spatialCV <- train(trainDat[,predictors],
                         trainDat[,response],
                         method="rf",
                         metric="Kappa",
                         trControl=ctrl_sp,
                         tuneGrid=data.frame("mtry"=model$bestTune$mtry),
                         importance=TRUE,
                         ntree=75)

cvPredictions <- model_spatialCV$pred[model_spatialCV$pred$mtry==model_spatialCV$bestTune$mtry,]
confusionMatrix(cvPredictions$pred,cvPredictions$obs)$overall

prediction_sp <- predict(predStack,model_spatialCV)
#spplot(prediction_sp,col.regions=as.character(cols_df$col))
#plot(varImp(model))

ffsmodel_spatial <- ffs(trainDat[,predictors],
                        trainDat[,response],
                        method="rf",
                        metric="Kappa",
                        tuneGrid=data.frame("mtry"=model$bestTune$mtry),
                        trControl = ctrl_sp,
                        ntree=75) 

#plot_ffs(ffsmodel_spatial)
#plot_ffs(ffsmodel_spatial, plotType="selected")
#print(ffsmodel_spatial)

prediction_ffs <- predict(predStack,ffsmodel_spatial)
#spplot(prediction_ffs,col.regions=as.character(cols_df$col))
cvPredictions <- ffsmodel_spatial$pred[ffsmodel_spatial$pred$mtry==ffsmodel_spatial$bestTune$mtry,]
confusionMatrix(cvPredictions$pred,cvPredictions$obs)$overall
writeRaster(prediction_ffs, out,overwrite=TRUE)
endCluster()
##

###----------------RECLASSIFICATION SCRIPT---------------------###
## This will take the output of the ML_SpatialPrediction_auto.R script

classif <- raster(out)

acacia_campX <-classif
acacia_campX[!acacia_campX==1]<-NA

acc <-  writeRaster(acacia_campX, paste(data_root,out_rast_rcl, sep = ""),overwrite=TRUE) 
#acc_read <- raster(acc)
acc_read <- raster(paste(data_root,out_rast_rcl, sep = ""))

acc_rpj <- projectRaster(acc_read, crs=CRS('+init=EPSG:32733'))


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
endCluster()
# Save raster
writeRaster(m, paste(data_root,out_rast_f, sep = ""),overwrite=TRUE)

print("Script Finished Processing...")
###----------------GRIDDING SCRIPT---------------------###
#e <- extent(c)
#p <- as(e, 'SpatialPolygons')
#proj4string(p) = CRS("+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs")
#print(p)
#shp <- st_as_sf(p)
#print(shp)
#dim = 3
#grid <- shp %>% 
#  st_make_grid(n = c(dim, dim), what = "polygons") %>% # grid of poly
#  st_intersection(shp)  
#st_write(grid, paste(data_root,out_grid, sep = ""))

###----------------SEGMENTATION---------------------###
#sat_img <- raster(out_rast_f)
#seg <- itcIMG(sat_img,epsg=32733,search = 9, TRESHSeed =  0.5, TRESHCrown = 0.5, DIST = 7, th=5, ischm = FALSE)
#clusterR(seg)
#writeOGR(seg, paste(data_root,arg2_out_raw, sep = ""), "segmentation", driver = "ESRI Shapefile")

#gridx <- readOGR(output_raw)
#gridx$CR_m <- sqrt(gridx$CA_m2 / 3.1416)
#gridx$CR_dia <-gridx$CR_m*2
#gridx$CR_sqrd <-gridx$CR_dia^2

###Ashraf to replace new vol formula
#gridx$vol <- (-0.0263+0.0024)*gridx$CR_sqrd
#st_write(st_as_sf(gridx),paste(data_root,arg3_result, sep = ""))
##-----------------END OF SCRIPT--------------------##

