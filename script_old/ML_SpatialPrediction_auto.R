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

args <- commandArgs(trailingOnly = TRUE)

spatial_pred <- function(sat_image, trainSites, out_tif)
  {
  ## LOAD RASTER DATA
  sat_image = args[1]
  
  trainSites <- read_sf(args[2])
  
  
  rglcm <- glcm(sat_img,window = c(9,9), 
                shift = c(1,1),
                statistics = c("variance", "homogeneity","dissimilarity","entropy"))
  glcm_brick <- stack(rglcm)
  ## PCA Operation
  rand_sam <- sampleRandom(sat_img_s, 10000) # sample 5000 random grid cells
  pca <- prcomp(rand_sam, scale=TRUE, retx=FALSE) 
  pca_brick <- predict(sat_img_s, pca, index=1:3) # create new rasters based on PCA predictions
  nlayers(rglcm)
  
  ## MACHINE LEARNING MODEL
  predStack <- stack(rglcm,pca_brick,sat_img_s) #Add DEM - Resample and clip in QGIS
  nlayers(predStack)
  names(predStack)
  names(predStack) <- c('variance', 'homogeneity', 'dissimilarity', 'entropy','pc1','pc2','pc3','red', 'green', 'blue')
  names(predStack)
  spplot(scale(predStack))
  
  trainSites <- st_transform(trainSites,crs=projection(predStack))
  
  extr <- extract(predStack, trainSites, df=TRUE)
  #extr <- merge(x = extr, y = trainSites, by = "id")
  extr <- merge(extr, trainSites, by.x="ID", by.y="id",all.y=TRUE)
  
  head(extr)
  
  set.seed(100)
  trainids <- createDataPartition(extr$ID,list=FALSE,p=0.15)
  trainDat <- extr[trainids,]
  
  predictors <- c('variance', 'homogeneity', 'dissimilarity', 'entropy','pc1','pc2','pc3','red', 'green', 'blue')
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
  
  #assign some colors that are easy to interpret visually:
  #cols_df <- data.frame("Type_en"=c("AC","BU","DV","PO", "RD","SO","SV","SH","GR"),
  #                      "col"=c("red", "white", "orange","green","black","blue","pink", "yellow", "violet"))
  cols_df <- data.frame("Type_en"=c("AC","BU","SO"),
                        "col"=c("green", "white", "orange"))
  
  
  #plot prediction:
  spplot(prediction,col.regions=as.character(cols_df$col))
  
  spfolds <- read_sf(paste(data_root,"spfolds.shp", sep = ""))
  mapview(spfolds,map.types = "Esri.WorldImagery")+
    mapview(trainSites)
  
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
  spplot(prediction_sp,col.regions=as.character(cols_df$col))
  
  plot(varImp(model))
  
  ffsmodel_spatial <- ffs(trainDat[,predictors],
                          trainDat[,response],
                          method="rf",
                          metric="Kappa",
                          tuneGrid=data.frame("mtry"=model$bestTune$mtry),
                          trControl = ctrl_sp,
                          ntree=75) 
  
  plot_ffs(ffsmodel_spatial)
  
  plot_ffs(ffsmodel_spatial, plotType="selected")
  
  prediction_ffs <- predict(predStack,ffsmodel_spatial)
  spplot(prediction_ffs,col.regions=as.character(cols_df$col))
  
  cvPredictions <- ffsmodel_spatial$pred[ffsmodel_spatial$pred$mtry==ffsmodel_spatial$bestTune$mtry,]
  confusionMatrix(cvPredictions$pred,cvPredictions$obs)$overall
  out_tif = args[3]
  writeRaster(prediction, out_tif)}

spatial_pred()



