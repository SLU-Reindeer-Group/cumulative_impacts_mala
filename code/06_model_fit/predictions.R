library(tidyverse)

# spatial prediction
install.load::install_load("raster", "rasterVis", "terra", "stars")

# vindval data
load(file = "data/01_GPS/05_annotated_data/data_annotated_calving_ready_2h_rsf.RData")
load(file = "data/01_GPS/05_annotated_data/data_annotated_summer_ready_2h_rsf.RData")
load(file = "data/01_GPS/05_annotated_data/data_annotated_autumn_ready_2h_rsf.RData")


# load maps
maps <- terra::rast("maps/for_prediction/predictors.tif")

# terra::plot(maps[[1:16]])
# terra::plot(maps[[17:32]])

maps_df <- as.data.frame(maps, xy = TRUE, cell = TRUE, na.rm = FALSE)
str(maps_df)


# load models

# calving
# model comparison
load("output/rsf_calving_multi_zoi_table.rda")
# compare models
tab_models
# Re-run best model
form <- tab_models$definition[1]
best_fit_calving <- glm(form,
                        data = data_ready_calving_rsf_2h, #weights = w_obs,
                        family = binomial(link = "logit"))
best_fit_calving$formula

# summer
# model comparison
load("output/rsf_summer_multi_zoi_table.rda")
# compare models
tab_models
# Re-run best model
form <- tab_models$definition[1]
best_fit_summer <- glm(form,
                        data = data_ready_summer_rsf_2h, #weights = w_obs,
                        family = binomial(link = "logit"))
best_fit_summer$formula

# autumn
# model comparison
load("output/rsf_autumn_multi_zoi_table.rda")
# compare models
tab_models
# Re-run best model
form <- tab_models$definition[1]
best_fit_autumn <- glm(form,
                        data = data_ready_autumn_rsf_2h, #weights = w_obs,
                        family = binomial(link = "logit"))
best_fit_autumn$formula


# Predictions

# calving before --------------------------------

# set sos and eos to match phase
maps_before_df <- maps_df %>% 
  rename(eos=eos_before, sos=sos_before)
# predict
pred_calving_before_construction <- terra::predict(best_fit_calving, maps_before_df, type = "response")
# create raster
map_pred_calving_before_construction <- maps[[2]]
# fill raster with predicted values
terra::values(map_pred_calving_before_construction) <- pred_calving_before_construction

names(map_pred_calving_before_construction) <- "calving_before_construction"
terra::plot(map_pred_calving_before_construction)
terra::writeRaster(map_pred_calving_before_construction, filename = "output/map_prediction_calving_before_construction.tif", overwrite=T)

rm(pred_calving_before_construction)
rm(map_pred_calving_before_construction)

# calving during/construction --------------------------------

# set sos and eos to match phase
maps_during_df <- maps_df %>% 
  rename(eos=eos_during, sos=sos_during)
# predict
pred_calving_construction <- terra::predict(best_fit_calving, maps_during_df, type = "response")
# create raster
map_pred_calving_construction <- maps[[2]]
# fill raster with predicted values
terra::values(map_pred_calving_construction) <- pred_calving_construction

names(map_pred_calving_construction) <- "calving_construction"
terra::plot(map_pred_calving_construction)
terra::writeRaster(map_pred_calving_construction, filename = "output/map_prediction_calving_construction.tif", overwrite=T)

rm(pred_calving_construction)
rm(map_pred_calving_construction)


# calving operation/after --------------------------------

# set sos and eos to match phase
maps_after_df <- maps_df %>% 
  rename(eos=eos_after, sos=sos_after)
# predict
pred_calving_operation <- terra::predict(best_fit_calving, maps_after_df, type = "response")
# create raster
map_pred_calving_operation <- maps[[2]]
# fill raster with predicted values
terra::values(map_pred_calving_operation) <- pred_calving_operation

names(map_pred_calving_operation) <- "calving_operation"
terra::plot(map_pred_calving_operation)
terra::writeRaster(map_pred_calving_operation, filename = "output/map_prediction_calving_operation.tif", overwrite=T)

rm(pred_calving_operation)
rm(map_pred_calving_operation)

#-------------------
#############################

# summer before --------------------------------

# set sos and eos to match phase
maps_before_df <- maps_df %>% 
  rename(eos=eos_before, sos=sos_before)
# predict
pred_summer_before_construction <- terra::predict(best_fit_summer, maps_before_df, type = "response")
# create raster
map_pred_summer_before_construction <- maps[[2]]
# fill raster with predicted values
terra::values(map_pred_summer_before_construction) <- pred_summer_before_construction

names(map_pred_summer_before_construction) <- "summer_before_construction"
terra::plot(map_pred_summer_before_construction)
terra::writeRaster(map_pred_summer_before_construction, filename = "output/map_prediction_summer_before_construction.tif", overwrite=T)

rm(pred_summer_before_construction)
rm(map_pred_summer_before_construction)

# summer during/construction --------------------------------

# set sos and eos to match phase
maps_during_df <- maps_df %>% 
  rename(eos=eos_during, sos=sos_during)
# predict
pred_summer_construction <- terra::predict(best_fit_summer, maps_during_df, type = "response")
# create raster
map_pred_summer_construction <- maps[[2]]
# fill raster with predicted values
terra::values(map_pred_summer_construction) <- pred_summer_construction

names(map_pred_summer_construction) <- "summer_construction"
terra::plot(map_pred_summer_construction)
terra::writeRaster(map_pred_summer_construction, filename = "output/map_prediction_summer_construction.tif", overwrite=T)

rm(pred_summer_construction)
rm(map_pred_summer_construction)


# summer operation/after --------------------------------

# set sos and eos to match phase
maps_after_df <- maps_df %>% 
  rename(eos=eos_after, sos=sos_after)
# predict
pred_summer_operation <- terra::predict(best_fit_summer, maps_after_df, type = "response")
# create raster
map_pred_summer_operation <- maps[[2]]
# fill raster with predicted values
terra::values(map_pred_summer_operation) <- pred_summer_operation

names(map_pred_summer_operation) <- "summer_operation"
terra::plot(map_pred_summer_operation)
terra::writeRaster(map_pred_summer_operation, filename = "output/map_prediction_summer_operation.tif", overwrite=T)

rm(pred_summer_operation)
rm(map_pred_summer_operation)

#-------------------
#############################

# autumn before --------------------------------

# set sos and eos to match phase
maps_before_df <- maps_df %>% 
  rename(eos=eos_before, sos=sos_before)
# predict
pred_autumn_before_construction <- terra::predict(best_fit_autumn, maps_before_df, type = "response")
# create raster
map_pred_autumn_before_construction <- maps[[2]]
# fill raster with predicted values
terra::values(map_pred_autumn_before_construction) <- pred_autumn_before_construction

names(map_pred_autumn_before_construction) <- "autumn_before_construction"
terra::plot(map_pred_autumn_before_construction)
terra::writeRaster(map_pred_autumn_before_construction, filename = "output/map_prediction_autumn_before_construction.tif", overwrite=T)

rm(pred_autumn_before_construction)
rm(map_pred_autumn_before_construction)

# autumn during/construction --------------------------------

# set sos and eos to match phase
maps_during_df <- maps_df %>% 
  rename(eos=eos_during, sos=sos_during)
# predict
pred_autumn_construction <- terra::predict(best_fit_autumn, maps_during_df, type = "response")
# create raster
map_pred_autumn_construction <- maps[[2]]
# fill raster with predicted values
terra::values(map_pred_autumn_construction) <- pred_autumn_construction

names(map_pred_autumn_construction) <- "autumn_construction"
terra::plot(map_pred_autumn_construction)
terra::writeRaster(map_pred_autumn_construction, filename = "output/map_prediction_autumn_construction.tif", overwrite=T)

rm(pred_autumn_construction)
rm(map_pred_autumn_construction)


# autumn operation/after --------------------------------

# set sos and eos to match phase
maps_after_df <- maps_df %>% 
  rename(eos=eos_after, sos=sos_after)
# predict
pred_autumn_operation <- terra::predict(best_fit_autumn, maps_after_df, type = "response")
# create raster
map_pred_autumn_operation <- maps[[2]]
# fill raster with predicted values
terra::values(map_pred_autumn_operation) <- pred_autumn_operation

names(map_pred_autumn_operation) <- "autumn_operation"
terra::plot(map_pred_autumn_operation)
terra::writeRaster(map_pred_autumn_operation, filename = "output/map_prediction_autumn_operation.tif", overwrite=T)

rm(pred_autumn_operation)
rm(map_pred_autumn_operation)


#############################
# plot
#############################

calving <- c("output/map_prediction_calving_before_construction.tif",
            "output/map_prediction_calving_construction.tif",
            "output/map_prediction_calving_operation.tif")

pred_calving <- terra::rast(calving)

names(pred_calving)

pred_calving$calving_before_construction_construction <- pred_calving$calving_construction - pred_calving$calving_before_construction
pred_calving$calving_construction_after <- pred_calving$calving_operation - pred_calving$calving_construction
pred_calving$calving_before_after <- pred_calving$calving_operation - pred_calving$calving_before_construction

names(pred_calving) <- c("Calving Before construction", "During construction", "After construction",
                        "Difference Before construction - During construction",
                        "Difference During construction - After construction",
                        "Difference Before construction - After construction")


png(filename="output/prediction_maps_calving.png", width = 30, height = 11, units = "cm", res = 300)
plot(pred_calving)  
dev.off()


#------------------------

summer <- c("output/map_prediction_summer_before_construction.tif",
            "output/map_prediction_summer_construction.tif",
            "output/map_prediction_summer_operation.tif")

pred_summer <- terra::rast(summer)

names(pred_summer)

pred_summer$summer_before_construction_construction <- pred_summer$summer_construction - pred_summer$summer_before_construction
pred_summer$summer_construction_after <- pred_summer$summer_operation - pred_summer$summer_construction
pred_summer$summer_before_after <- pred_summer$summer_operation - pred_summer$summer_before_construction

names(pred_summer) <- c("Summer Before construction", "During construction", "After construction",
                       "Difference Before construction - During construction",
                       "Difference During construction - After construction",
                       "Difference Before construction - After construction")

png(filename="output/prediction_maps_summer.png", width = 30, height = 11, units = "cm", res = 300)
plot(pred_summer)  
dev.off() 

#------------------------

autumn <- c("output/map_prediction_autumn_before_construction.tif",
              "output/map_prediction_autumn_construction.tif",
              "output/map_prediction_autumn_operation.tif")

pred_autumn <- terra::rast(autumn)

names(pred_autumn)

pred_autumn$autumn_before_construction_construction <- pred_autumn$autumn_construction - pred_autumn$autumn_before_construction
pred_autumn$autumn_construction_after <- pred_autumn$autumn_operation - pred_autumn$autumn_construction
pred_autumn$autumn_before_after <- pred_autumn$autumn_operation - pred_autumn$autumn_before_construction

names(pred_autumn) <- c("Autumn Before construction", "During construction", "After construction",
                        "Difference Before construction - During construction",
                        "Difference During construction - After construction",
                        "Difference Before construction - After construction")

png(filename="output/prediction_maps_autumn.png", width = 30, height = 11, units = "cm", res = 300)
plot(pred_autumn)  
dev.off()


