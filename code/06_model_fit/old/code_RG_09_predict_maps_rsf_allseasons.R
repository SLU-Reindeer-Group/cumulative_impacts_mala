#' ---
#' title: 'Maps from regional (2nd order) habitat selection analysis of Swedish reindeer: Malå sameby'
#' author: Bernardo Niebuhr
#' abstract: ""
#' ---

# Load packages
if(!require(install.load)) install.packages('install.load'); library(install.load)

# --------------- label=setup, warning=FALSE, message=FALSE
# Set up 

# Clean everything before beginning
rm(list = ls())

# Load packages
install.load::install_load("tidyverse")
install.load::install_load("survival")
install.load::install_load("amt", "terra", "raster", "rasterVis")

#-----------
#' # Load annotated and clean movement data

# fix rate = 12h
load(file = "output/data_annotated_autumn_ready_2h_rsf.RData")
data_ready_autumn_rsf_2h

# change a few variables
data_ready_autumn_rsf_2h <- data_ready_autumn_rsf_2h %>% 
  dplyr::mutate(landcover_cc = forcats::fct_relevel(landcover_cc, "clear cuts", after = 0))
dist_cutoff <- 1.5e4
wfs <- c("Jokkmokksliden", "Storliden", "Ytterberg", "Amliden")
data_ready_autumn_rsf_2h <- data_ready_autumn_rsf_2h %>% 
  dplyr::mutate(viewshed_aml15km = ifelse(wind_dist_aml >  dist_cutoff, 0, viewshed_amliden_mala_binary),
                cos_aspect = cos(aspect),
                wind_dist_comb = pmin(wind_dist_jsy, wind_dist_aml),
                dist_wf_closest = apply(cbind(wind_dist_jokk, wind_dist_stor, wind_dist_ytte, wind_dist_aml), 1, which.min),
                wf_name = factor(dist_wf_closest, levels = 1:4, labels = wfs))
#-----------
#' # Load maps

# vector of wind turbines
# turbines <- sf::st_read("maps/wind_turbines_Malå.shp")

# read maps

# dem
dem <- raster::raster("maps/dem_10m.tif")
names(dem) <- "dem"
plot(dem)

# slope
slope <- raster::raster("maps/dem_slope.tif")
names(slope) <- "slope"
plot(slope)

# aspect
aspect <- raster::raster("maps/dem_aspect.tif")
cos_aspect <- cos(aspect)
names(cos_aspect) <- "cos_aspect"

# tpi 150m
tpi_s150m <- raster::raster("maps/tpi_s150m.tif")
tpi_cat_s150m <- tpi_s150m
SD <- sd(tpi_s150m[], na.rm = T)
tpi_vals <- case_when(
  tpi_s150m[] <= -SD[1] ~ 1,
  tpi_s150m[] > -SD[1] & tpi_s150m[] <= -SD[1]/2  ~ 2,
  tpi_s150m[] > -SD[1]/2 & tpi_s150m[] <= 0  ~ 3,
  tpi_s150m[] > 0 & tpi_s150m[] <= SD[1]/2  ~ 4,
  tpi_s150m[] > SD[1]/2 & tpi_s150m[] <= SD[1]  ~ 5,
  tpi_s150m[] > SD[1] ~ 6
)
values(tpi_cat_s150m) <- tpi_vals
# tpi_cat_s150m[] <- tpi_vals
tpi_levels <- c("valley", "lower slope", "flat area", "medium slope", "upper slope", "ridge")
levels(tpi_cat_s150m) <- tpi_levels
plot(tpi_cat_s150m)
is.factor(tpi_cat_s150m)
names(tpi_cat_s150m) <- "tpi_cat_s150m"

# tpi 250m
tpi_s250m <- raster::raster("maps/tpi_s250m.tif")
tpi_cat_s250m <- tpi_s250m
SD <- sd(tpi_s250m[], na.rm = T)
tpi_vals <- case_when(
  tpi_s250m[] <= -SD[1] ~ 1,
  tpi_s250m[] > -SD[1] & tpi_s250m[] <= -SD[1]/2  ~ 2,
  tpi_s250m[] > -SD[1]/2 & tpi_s250m[] <= 0  ~ 3,
  tpi_s250m[] > 0 & tpi_s250m[] <= SD[1]/2  ~ 4,
  tpi_s250m[] > SD[1]/2 & tpi_s250m[] <= SD[1]  ~ 5,
  tpi_s250m[] > SD[1] ~ 6
)
values(tpi_cat_s250m) <- tpi_vals
# tpi_cat_s250m[] <- tpi_vals
tpi_levels <- c("valley", "lower slope", "flat area", "medium slope", "upper slope", "ridge")
levels(tpi_cat_s250m) <- tpi_levels
plot(tpi_cat_s250m)
is.factor(tpi_cat_s250m)
names(tpi_cat_s250m) <- "tpi_cat_s250m"

# tpi 510m
tpi_s510m <- raster::raster("maps/tpi_s510m.tif")
tpi_cat_s510m <- tpi_s510m
SD <- sd(tpi_s510m[], na.rm = T)
tpi_vals <- case_when(
  tpi_s510m[] <= -SD[1] ~ 1,
  tpi_s510m[] > -SD[1] & tpi_s510m[] <= -SD[1]/2  ~ 2,
  tpi_s510m[] > -SD[1]/2 & tpi_s510m[] <= 0  ~ 3,
  tpi_s510m[] > 0 & tpi_s510m[] <= SD[1]/2  ~ 4,
  tpi_s510m[] > SD[1]/2 & tpi_s510m[] <= SD[1]  ~ 5,
  tpi_s510m[] > SD[1] ~ 6
)
values(tpi_cat_s510m) <- tpi_vals
# tpi_cat_s510m[] <- tpi_vals
tpi_levels <- c("valley", "lower slope", "flat area", "medium slope", "upper slope", "ridge")
levels(tpi_cat_s510m) <- tpi_levels
plot(tpi_cat_s510m)
is.factor(tpi_cat_s510m)
names(tpi_cat_s510m) <- "tpi_cat_s510m"

# phase
phase <- dem
phase <- !is.na(phase)
phase[] <- ifelse(phase[] == 1, 1, NA)
levels(phase) <- "Operation"
plot(phase)
is.factor(phase)
names(phase) <- "phase"

# land cover (before clear cuts)
landcover <- raster::raster("maps/landcover_nmd_mod.tif")
lc_class <- read.csv("data/nmd_classes_eng_reclassified.csv", header = F, 
                     col.names = c("code", "class"), stringsAsFactors = F) %>% 
  dplyr::arrange(code)
levels(landcover) <- lc_class$class
landcover
plot(landcover)
names(landcover) <- "landcover"
# landcover[!is.na(landcover)][1:10]

# clear cuts
clear_cuts <- raster::raster("maps/clear_cuts_2020.tif")
plot(clear_cuts)

# wind distance
wind_dist <- raster::raster("maps/wind_dist.tif")
plot(wind_dist)

# wind distance jokk
wind_dist_jokk <- raster::raster("maps/wind_dist_jokk.tif")
plot(wind_dist_jokk)

# wind distance stor
wind_dist_stor <- raster::raster("maps/wind_dist_stor.tif")
plot(wind_dist_stor)

# wind distance åm
wind_dist_aml <- raster::raster("maps/wind_dist_aml.tif")
plot(wind_dist_aml)

# wind distance ytter
wind_dist_ytte <- raster::raster("maps/wind_dist_ytte.tif")
plot(wind_dist_ytte)

# wind distance jokk - stor - ytter
wind_dist_jsy <- raster::raster("maps/wind_dist_jsy.tif")
plot(wind_dist_jsy)

# wind distance combination jsy + aml
wind_dist_comb <- wind_dist_jsy
values(wind_dist_comb) <- pmin(values(wind_dist_jsy), values(wind_dist_aml))
plot(wind_dist_comb)
names(wind_dist_comb) <- "wind_dist_comb"

# wf name
wf_name <- wind_dist_jsy
vals <- apply(cbind(values(wind_dist_jokk), values(wind_dist_stor), values(wind_dist_ytte), values(wind_dist_aml)), 1, which.min)
vals2 <- unlist(vals)
values(wf_name)[!is.na(values(wf_name))] <- vals2
plot(wf_name)
wfs <- c("jokkmokksliden", "storliden", "ytteberg", "amliden")
# levels(wf_name) <- wfs
names(wf_name) <- "wf_name"

# house dist
house_dist <- raster::raster("maps/house_dist.tif")
plot(house_dist)

# large road  dist
public_road_dist <- raster::raster("maps/public_road_dist.tif")
plot(public_road_dist)

# pl dist
pl_dist <- raster::raster("maps/pl_dist.tif")
plot(pl_dist)

# mining dist
mining_dist <- raster::raster("maps/mining_dist.tif")
plot(mining_dist)

# small roads
priv_road_dist <- raster::raster("maps/priv_road_dist.tif")
plot(priv_road_dist)

# trails dist
trail_dist <- raster::raster("maps/trail_dist.tif")
plot(trail_dist)

# # wolf
# wolf <- raster::raster("maps/wolf_2016.tif")
# names(wolf) <- "wolf"
# plot(wolf)

# wolverine
wolverine <- raster::raster("maps/wolverine_2016.tif")
names(wolverine) <- "wolverine"
plot(wolverine)

# # viewshed
# viewshed <- raster::raster("maps/viewshed_mittadalen_binary.tif")
# plot(viewshed)
# names(viewshed) <- "viewshed"

#----------------------
#' # Model for phase, BA design, all individuals

#--------
# # data
# data_ready <- data_ready %>% 
#   dplyr::mutate(phase = forcats::fct_recode(phase, "Before construction" = "before",
#                                             "Construction" = "during",
#                                             "Operation" = "after"))
# 
table(data_ready$phase)

#--------
# model for autumn

# fit
formula_autumn <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  log(pl_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_dist+1) +
  log(trail_dist+1) + log(house_dist+1) +
  landcover_cc*phase*log(wind_dist_comb+1) + wf_name
formula_autumn2 <- update(formula_autumn, ~ . - cos_aspect - log(house_dist + 1))

fit2.3 <- glm(update(formula_autumn2, ~ . + tpi_cat_s510m), 
              data = data_ready_autumn_rsf_2h, family = binomial)

best_fit_autumn <- fit2.3



# select maps
summary(best_fit_autumn)

#--------
# maps 

maps_after <- raster::stack(phase, dem, slope, tpi_cat_s510m, landcover, clear_cuts,
                            public_road_dist, priv_road_dist, pl_dist, trail_dist,
                            mining_dist, wind_dist_comb, wf_name)
best_fit_autumn$formula
maps_after

maps_after_df <- as.data.frame(maps_after)
colnames(maps_after_df)

maps_after_df$tpi_cat_s510m <- factor(maps_after_df$tpi_cat_s510m, levels = 1:6, 
                                      labels = tpi_levels)
maps_after_df$phase <- factor(3, levels = 1:3, 
                              labels = c("Before construction", "Construction", "Operation"))
wfs <- c("Jokkmokksliden", "Storliden", "Ytterberg", "Amliden")
maps_after_df$wf_name <- factor(maps_after_df$wf_name, levels = 1:4, labels = wfs)

# landcover and clear cuts
year <- 2016
lc_class
maps_after_df$landcover_cc <- ifelse(maps_after_df$clear_cuts_2020 > year, 118, maps_after_df$landcover)
maps_after_df$landcover_cc = factor(maps_after_df$landcover_cc,
                                    levels = lc_class$code,
                                    labels = lc_class$class) %>% as.character()
maps_after_df$landcover_cc <- case_when(
  maps_after_df$landcover_cc == "pine forests" | maps_after_df$landcover_cc == "other coniferous forests" ~ "coniferous forests",
  maps_after_df$landcover_cc == "open wetlands" | maps_after_df$landcover_cc == "heath" ~ "mires",
  maps_after_df$landcover_cc == "water" ~ NA_character_,
  TRUE ~ maps_after_df$landcover_cc
) %>% as.factor()

maps_after_df$landcover_cc = forcats::fct_collapse(maps_after_df$landcover_cc,
                                                   "open lands" = c("arable lands", "tundra/rocks", "other open lands")) %>% 
  forcats::fct_relevel("clear cuts", after = 0)

# other phases
maps_before_df <- maps_after_df %>% 
  dplyr::mutate(phase = factor(1, levels = 1:3, 
                               labels = c("Before construction", "Construction", "Operation")))

maps_during_df <- maps_after_df %>% 
  dplyr::mutate(phase = factor(2, levels = 1:3, 
                               labels = c("Before construction", "Construction", "Operation")))

# save.image("output/maps_loaded_to_predict.RData")

#--------
# predict

# before
predb <- predict(best_fit_autumn, maps_before_df, type = "response")

maps_before_df1 <- maps_before_df %>% slice(1:(nrow(maps_before_df)/2))
predb1_autumn <- predict(best_fit_autumn, maps_before_df1, type = "response")
maps_before_df2 <- maps_before_df %>% slice((nrow(maps_before_df)/2+1):nrow(maps_before_df))
predb2_autumn <- predict(best_fit_autumn, maps_before_df2, type = "response")

prediction_phase_before_autumn <- dem
prediction_phase_before_autumn[] <- c(predb1_autumn, predb2_autumn)
names(prediction_phase_before_autumn) <- "Before construction"
plot(prediction_phase_before_autumn)

# during
# predd_autumn <- predict(best_fit_autumn, maps_during_df, type = "response")

maps_during_df1 <- maps_during_df %>% slice(1:(nrow(maps_during_df)/2))
predd1_autumn <- predict(best_fit_autumn, maps_during_df1, type = "response")
maps_during_df2 <- maps_during_df %>% slice((nrow(maps_during_df)/2+1):nrow(maps_during_df))
predd2_autumn <- predict(best_fit_autumn, maps_during_df2, type = "response")

prediction_phase_during_autumn <- dem
prediction_phase_during_autumn[] <- c(predd1_autumn, predd2_autumn)
names(prediction_phase_during_autumn) <- "Construction"
plot(prediction_phase_during_autumn)

# after
# preda <- predict(best_fit_phase, maps_after_df, type = "response")
rm(maps_during_df1, maps_during_df2)

maps_after_df1 <- maps_after_df %>% slice(1:(nrow(maps_after_df)/2))
preda1_autumn <- predict(best_fit_autumn, maps_after_df1, type = "response")
maps_after_df2 <- maps_after_df %>% slice((nrow(maps_after_df)/2+1):nrow(maps_after_df))
preda2_autumn <- predict(best_fit_autumn, maps_after_df2, type = "response")

prediction_phase_after_autumn <- dem
prediction_phase_after_autumn[] <- c(preda1_autumn, preda2_autumn)
names(prediction_phase_after_autumn) <- "Operation"
plot(prediction_phase_after_autumn)

# plot
# png("results/prediction_map_rsf_phase.png", width = 20, height = 20, units = "cm", res = 300)
# plot(c(prediction_phase_before, prediction_phase_during, prediction_phase_after))
# dev.off()

# png("results/prediction_map_rsf_phase.png", width = 20, height = 20, units = "cm", res = 300)
# par(mfrow = c(2,2))
# plot(prediction_phase_before)
# plot(as(turbines[1], "Spatial"), add = T, pch = 4, cex = 0.5)
# plot(prediction_phase_during)
# plot(as(turbines[1], "Spatial"), add = T, pch = 4, cex = 0.5)
# plot(prediction_phase_after)
# plot(as(turbines[1], "Spatial"), add = T, pch = 4, cex = 0.5)
# dev.off()

names_phase <- c("Before construction", "Construction", "Operation")
png("results/prediction_map_rsf_phase_autumn.png", width = 30, height = 11, units = "cm", res = 300)
plt <- raster::stack(prediction_phase_before_autumn, prediction_phase_during_autumn, 
                     prediction_phase_after_autumn) %>% 
  rasterVis::levelplot(par.settings = rasterTheme(region = rev(terrain.colors(n = 100))),
                       names.attr = names_phase, layout = c(3,1))
plt + latticeExtra::layer(sp.points(as(turbines[1], "Spatial"), 
                                    col = "black", cex = 0.4, pch = 4))
dev.off()

# save maps
terra::writeRaster(prediction_phase_before_autumn, "results/prediction_maps/prediction_rsf_phase_before_autumn.tif", 
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)
terra::writeRaster(prediction_phase_during_autumn, "results/prediction_maps/prediction_rsf_phase_during_autumn.tif", 
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)
terra::writeRaster(prediction_phase_after_autumn, "results/prediction_maps/prediction_rsf_phase_after_autumn.tif", 
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


#import maps autumn 
prediction_phase_before_autumn <- raster::raster("results/prediction_maps/prediction_rsf_phase_before_autumn.tif")
prediction_phase_during_autumn <- raster::raster("results/prediction_maps/prediction_rsf_phase_during_autumn.tif")
prediction_phase_after_autumn <- raster::raster("results/prediction_maps/prediction_rsf_phase_after_autumn.tif")

# difference autumn ####
after_minus_before <- prediction_phase_after_autumn - prediction_phase_before_autumn
names(after_minus_before) <- "Operation minus Before construction"
during_minus_before <- prediction_phase_during_autumn - prediction_phase_before_autumn
names(during_minus_before) <- "During construction minus Before construction"

png("results/prediction_map_rsf_autumn_diff.png", width = 20, height = 20, units = "cm", res = 300)
plot(raster::stack(prediction_phase_before_autumn, during_minus_before, after_minus_before))
dev.off()

png("results/prediction_map_rsf_autumn_diff2.png", width = 25, height = 10, units = "cm", res = 300)
rasternames <- c("During - Before construction", "Operation - Before construction")
rasterVis::levelplot(raster::stack(during_minus_before, after_minus_before),
                     names.attr = rasternames, layout = c(2,1))
dev.off()

#----------------------
#' # Model for phase all individuals - calving

#-----------
# data
load(file = "output/data_annotated_calving_ready_2h_rsf.RData")
data_ready_calving_rsf_2h

# limited viewshed for amliden
dist_cutoff <- 1.5e4
wfs <- c("Jokkmokksliden", "Storliden", "Ytterberg", "Amliden")
data_ready_calving_rsf_2h <- data_ready_calving_rsf_2h %>% 
  dplyr::mutate(viewshed_aml15km = ifelse(wind_dist_aml >  dist_cutoff, 0, viewshed_amliden_mala_binary),
                cos_aspect = cos(aspect),
                wind_dist_comb = pmin(wind_dist_jsy, wind_dist_aml),
                dist_wf_closest = apply(cbind(wind_dist_jokk, wind_dist_stor, wind_dist_ytte, wind_dist_aml), 1, which.min),
                wf_name = factor(dist_wf_closest, levels = 1:4, labels = wfs))

#-----------
# model

# fit
formula_calving <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  log(pl_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_dist+1) +
  log(trail_dist+1) + log(house_dist+1) +
  landcover_cc*phase*log(wind_dist_comb+1)+ wf_name

fit2 <- glm(update(formula_calving, ~ . + tpi_cat_s150m),
            data = data_ready_calving_rsf_2h, family = binomial)

best_fit_calving <- fit2

# select maps
summary(best_fit_calving)

#--------
# maps 

maps_after <- raster::stack(phase, dem, slope, cos_aspect, tpi_cat_s150m, 
                            landcover, clear_cuts,
                            public_road_dist, priv_road_dist, pl_dist, trail_dist,
                            house_dist, mining_dist, wind_dist_comb, wf_name)
best_fit_calving$formula
maps_after

maps_after_df <- as.data.frame(maps_after)
colnames(maps_after_df)

maps_after_df$tpi_cat_s150m <- factor(maps_after_df$tpi_cat_s150m, levels = 1:6, 
                                      labels = tpi_levels)
maps_after_df$phase <- factor(3, levels = 1:3, 
                              labels = c("Before construction", "Construction", "Operation"))
wfs <- c("Jokkmokksliden", "Storliden", "Ytterberg", "Amliden")
maps_after_df$wf_name <- factor(maps_after_df$wf_name, levels = 1:4, labels = wfs)

# landcover and clear cuts
year <- 2016
lc_class
maps_after_df$landcover_cc <- ifelse(maps_after_df$clear_cuts_2020 > year, 118, maps_after_df$landcover)
maps_after_df$landcover_cc = factor(maps_after_df$landcover_cc,
                                    levels = lc_class$code,
                                    labels = lc_class$class) %>% as.character()
maps_after_df$landcover_cc <- case_when(
  maps_after_df$landcover_cc == "pine forests" | maps_after_df$landcover_cc == "other coniferous forests" ~ "coniferous forests",
  maps_after_df$landcover_cc == "open wetlands" | maps_after_df$landcover_cc == "heath" ~ "mires",
  maps_after_df$landcover_cc == "water" ~ NA_character_,
  TRUE ~ maps_after_df$landcover_cc
) %>% as.factor()

maps_after_df$landcover_cc = forcats::fct_collapse(maps_after_df$landcover_cc,
                                                   "open lands" = c("arable lands", "tundra/rocks", "other open lands")) %>% 
  forcats::fct_relevel("clear cuts", after = 0)

# other phases
maps_before_df <- maps_after_df %>% 
  dplyr::mutate(phase = factor(1, levels = 1:3, 
                               labels = c("Before construction", "Construction", "Operation")))

maps_during_df <- maps_after_df %>% 
  dplyr::mutate(phase = factor(2, levels = 1:3, 
                               labels = c("Before construction", "Construction", "Operation")))

# save.image("output/maps_loaded_to_predict_calving.RData")

#--------
# predict

# before
# predb <- predict(best_fit_calving, maps_before_df, type = "response")

maps_before_df1 <- maps_before_df %>% slice(1:(nrow(maps_before_df)/2))
predb1_calving <- predict(best_fit_calving, maps_before_df1, type = "response")
maps_before_df2 <- maps_before_df %>% slice((nrow(maps_before_df)/2+1):nrow(maps_before_df))
predb2_calving <- predict(best_fit_calving, maps_before_df2, type = "response")

prediction_phase_before_calving <- dem
prediction_phase_before_calving[] <- c(predb1_calving, predb2_calving)
names(prediction_phase_before_calving) <- "Before construction"
plot(prediction_phase_before_calving)

# during
# predd_calving <- predict(best_fit_calving, maps_during_df, type = "response")
rm(list = grep("predb", ls(), value = T))

maps_during_df1 <- maps_during_df %>% slice(1:(nrow(maps_during_df)/3))
predd1_calving <- predict(best_fit_calving, maps_during_df1, type = "response")
maps_during_df2 <- maps_during_df[(nrow(maps_during_df)/3+1):(2*nrow(maps_during_df)/3),]
predd2_calving <- predict(best_fit_calving, maps_during_df2, type = "response")
maps_during_df3 <- maps_during_df[(2*nrow(maps_during_df)/3+1):nrow(maps_during_df),]
predd3_calving <- predict(best_fit_calving, maps_during_df3, type = "response")

prediction_phase_during_calving <- dem
prediction_phase_during_calving[] <- c(predd1_calving, predd2_calving, predd3_calving, NA)
names(prediction_phase_during_calving) <- "Construction"
plot(prediction_phase_during_calving)

# after
# preda <- predict(best_fit_phase, maps_after_df, type = "response")
rm(list = grep("predd", ls(), value = T))
rm(list = grep("maps_before", ls(), value = T))

maps_after_df1 <- maps_after_df %>% slice(1:(nrow(maps_after_df)/3))
preda1_calving <- predict(best_fit_calving, maps_after_df1, type = "response")
maps_after_df2 <- maps_after_df[(nrow(maps_after_df)/3+1):(2*nrow(maps_after_df)/3),]
preda2_calving <- predict(best_fit_calving, maps_after_df2, type = "response")
maps_after_df3 <- maps_after_df[(2*nrow(maps_after_df)/3+1):nrow(maps_after_df),]
preda3_calving <- predict(best_fit_calving, maps_after_df3, type = "response")


prediction_phase_after_calving <- dem
prediction_phase_after_calving[] <- c(preda1_calving, preda2_calving, preda3_calving, NA)
names(prediction_phase_after_calving) <- "Operation"
plot(prediction_phase_after_calving)

# plot
# png("results/prediction_map_rsf_phase.png", width = 20, height = 20, units = "cm", res = 300)
# plot(c(prediction_phase_before, prediction_phase_during, prediction_phase_after))
# dev.off()

# png("results/prediction_map_rsf_phase.png", width = 20, height = 20, units = "cm", res = 300)
# par(mfrow = c(2,2))
# plot(prediction_phase_before)
# plot(as(turbines[1], "Spatial"), add = T, pch = 4, cex = 0.5)
# plot(prediction_phase_during)
# plot(as(turbines[1], "Spatial"), add = T, pch = 4, cex = 0.5)
# plot(prediction_phase_after)
# plot(as(turbines[1], "Spatial"), add = T, pch = 4, cex = 0.5)
# dev.off()

names_phase <- c("Before construction", "Construction", "Operation")
png("results/prediction_map_rsf_phase_calving.png", width = 30, height = 11, units = "cm", res = 300)
plt <- raster::stack(prediction_phase_before_calving, prediction_phase_during_calving, 
                     prediction_phase_after_calving) %>% 
  rasterVis::levelplot(par.settings = rasterTheme(region = rev(terrain.colors(n = 100))),
                       names.attr = names_phase, layout = c(3,1))
plt + latticeExtra::layer(sp.points(as(turbines[1], "Spatial"), 
                                    col = "black", cex = 0.4, pch = 4))
dev.off()

# save maps
terra::writeRaster(prediction_phase_before_calving, "results/prediction_maps/prediction_rsf_phase_before_calving.tif", 
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)
terra::writeRaster(prediction_phase_during_calving, "results/prediction_maps/prediction_rsf_phase_during_calving.tif", 
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)
terra::writeRaster(prediction_phase_after_calving, "results/prediction_maps/prediction_rsf_phase_after_calving.tif", 
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


#import maps calving 
prediction_phase_before_calving <- raster::raster("results/prediction_maps/prediction_rsf_phase_before_calving.tif")
prediction_phase_during_calving <- raster::raster("results/prediction_maps/prediction_rsf_phase_during_calving.tif")
prediction_phase_after_calving <- raster::raster("results/prediction_maps/prediction_rsf_phase_after_calving.tif")

# difference calving ####
after_minus_before <- prediction_phase_after_calving - prediction_phase_before_calving
names(after_minus_before) <- "Operation minus Before construction"
during_minus_before <- prediction_phase_during_calving - prediction_phase_before_calving
names(during_minus_before) <- "During construction minus Before construction"

png("results/prediction_map_rsf_calving_diff.png", width = 20, height = 20, units = "cm", res = 300)
plot(raster::stack(prediction_phase_before_calving, during_minus_before, after_minus_before))
dev.off()

png("results/prediction_map_rsf_calving_diff2.png", width = 25, height = 10, units = "cm", res = 300)
rasternames <- c("During - Before construction", "Operation - Before construction")
rasterVis::levelplot(raster::stack(during_minus_before, after_minus_before),
                     names.attr = rasternames, layout = c(2,1))
dev.off()

#plot swedish maps  ####
#FIX THIS WITH NEW MODEL WITHOUT INTERACTION LICHEN*LANDCOVER!!!

names_weather <- c("God vinter", "Medelsvår vinter", "Svår vinter")
png("results_swe/prediction_map_rsf_weather.png", width = 30, height = 11, units = "cm", res = 300)
plt <- raster::stack(prediction_weather_good, prediction_weather_medium, prediction_weather_bad) %>% 
  rasterVis::levelplot(par.settings = rasterTheme(region = rev(terrain.colors(n = 100))),
                       names.attr = names_weather, layout = c(3,1))
plt + latticeExtra::layer(sp.points(as(turbines[1], "Spatial"), 
                                    col = "black", cex = 0.4, pch = 4))
dev.off()

# difference weather swe
bad_minus_good <- prediction_weather_bad - prediction_weather_good
names(bad_minus_good) <- "Skillnad mellan svår och god vinter"
medium_minus_good <- prediction_weather_medium - prediction_weather_good
names(medium_minus_good) <- "Skillnad mellan medelsvår och god vinter"

png("results_swe/prediction_map_rsf_weather_diff.png", width = 20, height = 20, units = "cm", res = 300)
plot(raster::stack(prediction_weather_good, medium_minus_good, bad_minus_good))
dev.off()

png("results_swe/prediction_map_rsf_weather_diff2.png", width = 25, height = 10, units = "cm", res = 300)
rasternames <- c("Medelsvår - god vinter", "Svår vinter - god vinter")
rasterVis::levelplot(raster::stack(medium_minus_good, bad_minus_good),
                     names.attr = rasternames, layout = c(2,1))
dev.off()


#----------------------
#' # Model for phase all individuals - summer

#-----------
# data
load(file = "output/data_annotated_summer_ready_2h_rsf.RData")
data_ready_summer_rsf_2h

# limited viewshed for amliden
dist_cutoff <- 1.5e4
wfs <- c("Jokkmokksliden", "Storliden", "Ytterberg", "Amliden")
data_ready_summer_rsf_2h <- data_ready_summer_rsf_2h %>% 
  dplyr::mutate(viewshed_aml15km = ifelse(wind_dist_aml >  dist_cutoff, 0, viewshed_amliden_mala_binary),
                cos_aspect = cos(aspect),
                wind_dist_comb = pmin(wind_dist_jsy, wind_dist_aml),
                dist_wf_closest = apply(cbind(wind_dist_jokk, wind_dist_stor, wind_dist_ytte, wind_dist_aml), 1, which.min),
                wf_name = factor(dist_wf_closest, levels = 1:4, labels = wfs))

#-----------
# model

# fit
formula_summer <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  log(pl_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_dist+1) +
  log(trail_dist+1) + log(house_dist+1) +
  landcover_cc*phase*log(wind_dist_comb+1)+ wf_name

formula_summer2 <- update(formula_summer, ~ . - cos_aspect)

fit2 <- glm(update(formula_summer2, ~ . + tpi_cat_s150m),
            data = data_ready_summer_rsf_2h, family = binomial)

best_fit_summer <- fit2

# select maps
summary(best_fit_summer)

#--------
# maps 

maps_after <- raster::stack(phase, dem, slope, tpi_cat_s150m, 
                            landcover, clear_cuts,
                            public_road_dist, priv_road_dist, pl_dist, trail_dist,
                            house_dist, mining_dist, wind_dist_comb, wf_name)
best_fit_summer$formula
maps_after

maps_after_df <- as.data.frame(maps_after)
colnames(maps_after_df)

maps_after_df$tpi_cat_s150m <- factor(maps_after_df$tpi_cat_s150m, levels = 1:6, 
                                      labels = tpi_levels)
maps_after_df$phase <- factor(3, levels = 1:3, 
                              labels = c("Before construction", "Construction", "Operation"))
wfs <- c("Jokkmokksliden", "Storliden", "Ytterberg", "Amliden")
maps_after_df$wf_name <- factor(maps_after_df$wf_name, levels = 1:4, labels = wfs)

# landcover and clear cuts
year <- 2016
lc_class
maps_after_df$landcover_cc <- ifelse(maps_after_df$clear_cuts_2020 > year, 118, maps_after_df$landcover)
maps_after_df$landcover_cc = factor(maps_after_df$landcover_cc,
                                    levels = lc_class$code,
                                    labels = lc_class$class) %>% as.character()
maps_after_df$landcover_cc <- case_when(
  maps_after_df$landcover_cc == "pine forests" | maps_after_df$landcover_cc == "other coniferous forests" ~ "coniferous forests",
  maps_after_df$landcover_cc == "open wetlands" | maps_after_df$landcover_cc == "heath" ~ "mires",
  maps_after_df$landcover_cc == "water" ~ NA_character_,
  TRUE ~ maps_after_df$landcover_cc
) %>% as.factor()

maps_after_df$landcover_cc = forcats::fct_collapse(maps_after_df$landcover_cc,
                                                   "open lands" = c("arable lands", "tundra/rocks", "other open lands")) %>% 
  forcats::fct_relevel("clear cuts", after = 0)

# other phases
maps_before_df <- maps_after_df %>% 
  dplyr::mutate(phase = factor(1, levels = 1:3, 
                               labels = c("Before construction", "Construction", "Operation")))

maps_during_df <- maps_after_df %>% 
  dplyr::mutate(phase = factor(2, levels = 1:3, 
                               labels = c("Before construction", "Construction", "Operation")))

# save.image("output/maps_loaded_to_predict_summer.RData")

#--------
# predict
rm(list = grep("minus", ls(), value = T))
rm(list = grep("autumn", ls(), value = T))

# before
# predb <- predict(best_fit_summer, maps_before_df, type = "response")

pred_before_summer <- predict_large(best_fit_summer, maps_before_df, n.parts = 3, type = "response")

# maps_before_df1 <- maps_before_df[1:(nrow(maps_before_df)/2)),]
# predb1_summer <- predict(best_fit_summer, maps_before_df1, type = "response")
# maps_before_df2 <- maps_before_df %>% slice((nrow(maps_before_df)/2+1):nrow(maps_before_df))
# predb2_summer <- predict(best_fit_summer, maps_before_df2, type = "response")

prediction_phase_before_summer <- dem
prediction_phase_before_summer[] <- pred_before_summer
names(prediction_phase_before_summer) <- "Before construction"
plot(prediction_phase_before_summer)

# during
# predd_summer <- predict(best_fit_summer, maps_during_df, type = "response")
rm(list = grep("predb", ls(), value = T))

pred_during_summer <- predict_large(best_fit_summer, maps_during_df, n.parts = 3, type = "response")

# maps_during_df1 <- maps_during_df %>% slice(1:(nrow(maps_during_df)/3))
# predd1_summer <- predict(best_fit_summer, maps_during_df1, type = "response")
# maps_during_df2 <- maps_during_df[(nrow(maps_during_df)/3+1):(2*nrow(maps_during_df)/3),]
# predd2_summer <- predict(best_fit_summer, maps_during_df2, type = "response")
# maps_during_df3 <- maps_during_df[(2*nrow(maps_during_df)/3+1):nrow(maps_during_df),]
# predd3_summer <- predict(best_fit_summer, maps_during_df3, type = "response")

prediction_phase_during_summer <- dem
prediction_phase_during_summer[] <- pred_during_summer
names(prediction_phase_during_summer) <- "Construction"
plot(prediction_phase_during_summer)

# after
# preda <- predict(best_fit_phase, maps_after_df, type = "response")
rm(pred_during_summer)

pred_after_summer <- predict_large(best_fit_summer, maps_after_df, n.parts = 3, type = "response")

# maps_after_df1 <- maps_after_df %>% slice(1:(nrow(maps_after_df)/3))
# preda1_summer <- predict(best_fit_summer, maps_after_df1, type = "response")
# maps_after_df2 <- maps_after_df[(nrow(maps_after_df)/3+1):(2*nrow(maps_after_df)/3),]
# preda2_summer <- predict(best_fit_summer, maps_after_df2, type = "response")
# maps_after_df3 <- maps_after_df[(2*nrow(maps_after_df)/3+1):nrow(maps_after_df),]
# preda3_summer <- predict(best_fit_summer, maps_after_df3, type = "response")


prediction_phase_after_summer <- dem
prediction_phase_after_summer[] <- pred_after_summer
names(prediction_phase_after_summer) <- "Operation"
plot(prediction_phase_after_summer)

#plot
names_phase <- c("Before construction", "Construction", "Operation")
png("results/prediction_map_rsf_phase_summer.png", width = 30, height = 11, units = "cm", res = 300)
plt <- raster::stack(prediction_phase_before_summer, prediction_phase_during_summer, 
                     prediction_phase_after_summer) %>% 
  rasterVis::levelplot(par.settings = rasterTheme(region = rev(terrain.colors(n = 100))),
                       names.attr = names_phase, layout = c(3,1))
plt + latticeExtra::layer(sp.points(as(turbines[1], "Spatial"), 
                                    col = "black", cex = 0.4, pch = 4))
dev.off()

# save maps
terra::writeRaster(prediction_phase_before_summer, "results/prediction_maps/prediction_rsf_phase_before_summer.tif", 
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)
terra::writeRaster(prediction_phase_during_summer, "results/prediction_maps/prediction_rsf_phase_during_summer.tif", 
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)
terra::writeRaster(prediction_phase_after_summer, "results/prediction_maps/prediction_rsf_phase_after_summer.tif", 
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


#import maps summer 
prediction_phase_before_summer <- raster::raster("results/prediction_maps/prediction_rsf_phase_before_summer.tif")
prediction_phase_during_summer <- raster::raster("results/prediction_maps/prediction_rsf_phase_during_summer.tif")
prediction_phase_after_summer <- raster::raster("results/prediction_maps/prediction_rsf_phase_after_summer.tif")

# difference summer ####
after_minus_before <- prediction_phase_after_summer - prediction_phase_before_summer
names(after_minus_before) <- "Operation minus Before construction"
during_minus_before <- prediction_phase_during_summer - prediction_phase_before_summer
names(during_minus_before) <- "During construction minus Before construction"

png("results/prediction_map_rsf_summer_diff.png", width = 20, height = 20, units = "cm", res = 300)
plot(raster::stack(prediction_phase_before_summer, during_minus_before, after_minus_before))
dev.off()

png("results/prediction_map_rsf_summer_diff2.png", width = 25, height = 10, units = "cm", res = 300)
rasternames <- c("During - Before construction", "Operation - Before construction")
rasterVis::levelplot(raster::stack(during_minus_before, after_minus_before),
                     names.attr = rasternames, layout = c(2,1))
dev.off()




#----------------------
## Model for predators all individuals #####

#----------------
# data
data_pred <- data_ready %>% 
  dplyr::filter(winter >= 2015 & winter <= 2018) %>% 
  dplyr::mutate(wolf_std = scale(wolf)/2,
                wolverine_std = scale(wolverine)/2)

#----------------
# model 

# fit
fit_pred_2.3 <- with(data_pred,
                     glm(case_ ~ landcover_cc +
                           scale(wolf) + scale(wolverine) +
                           scale(slope) + 
                           tpi_cat_s510m +
                           scale(dem) + I(scale(dem)^2) + 
                           scale(dem):scale(wolf) +
                           scale(dem)*scale(wolverine)*landcover_cc +
                           scale(lichen) + scale(lichen)*scale(wolverine)*landcover_cc +
                           scale(lichen):scale(wolf) +
                           log(wind_dist+1) + 
                           scale(wolverine):log(wind_dist+1) +
                           log(house_dist+1) + log(public_road_dist+1) +
                           log(public_road_dist+1):scale(wolf), family = binomial))

best_fit_pred <- fit_pred_2.3

#----------------
# select maps
summary(best_fit_pred)
best_fit_pred$call

maps_wolf <- raster::stack(dem, slope, tpi_cat_s510m, lichen, landcover, clear_cuts,
                           wind_dist, wolf, wolverine, house_dist, public_road_dist)
maps_wolf

maps_wolf_df <- as.data.frame(maps_wolf)

maps_wolf_df$tpi_cat_s510m <- factor(maps_wolf_df$tpi_cat_s510m, levels = 1:6, 
                                     labels = tpi_levels)

# landcover and clear cuts
year <- 2016
lc_class
maps_wolf_df$landcover_cc <- ifelse(maps_wolf_df$clear_cuts_2020 > year, 118, maps_wolf_df$landcover)
maps_wolf_df$landcover_cc = factor(maps_wolf_df$landcover_cc,
                                   levels = lc_class$code,
                                   labels = lc_class$class) %>% as.character()
maps_wolf_df$landcover_cc <- case_when(
  maps_wolf_df$landcover_cc == "pine forests" | maps_wolf_df$landcover_cc == "other coniferous forests" ~ "coniferous forests",
  maps_wolf_df$landcover_cc == "open wetlands" | maps_wolf_df$landcover_cc == "heath" ~ "mires",
  maps_wolf_df$landcover_cc == "water" ~ NA_character_,
  TRUE ~ maps_wolf_df$landcover_cc
) %>% as.factor()

maps_wolf_df$landcover_cc = forcats::fct_collapse(maps_wolf_df$landcover_cc,
                                                  "open lands" = c("arable lands", "tundra/rocks", "other open lands")) %>% 
  forcats::fct_relevel("clear cuts", after = 0)

#----------------
# predict

# 2016
predw <- predict(best_fit_pred, maps_wolf_df, type = "response")

prediction_pred_high <- dem
prediction_pred_high[] <- predw
names(prediction_pred_high) <- "2016, high predation"
plot(prediction_pred_high)

# for each year
maps_pred_year <- list()

for(year in 2015:2018) {
  print(year)
  # read maps
  wolf <- terra::rast(paste0("maps/wolf_", year, ".tif"))
  names(wolf) <- "wolf"
  
  wolverine <- terra::rast(paste0("maps/wolverine_", year, ".tif"))
  names(wolverine) <- "wolverine"
  
  # replace predator data on general dataframe
  preds_year <- as.data.frame(c(wolf, wolverine))
  
  maps_wolf_df_change <- maps_wolf_df %>% 
    dplyr::select(-c("wolf", "wolverine")) %>% 
    dplyr::bind_cols(preds_year)
  
  # update landcover and clear cuts
  lc_class
  maps_wolf_df_change$landcover_cc <- ifelse(maps_wolf_df_change$clear_cuts_2020 > year, 118, maps_wolf_df_change$landcover)
  maps_wolf_df_change$landcover_cc = factor(maps_wolf_df_change$landcover_cc,
                                            levels = lc_class$code,
                                            labels = lc_class$class) %>% as.character()
  maps_wolf_df_change$landcover_cc <- case_when(
    maps_wolf_df_change$landcover_cc == "pine forests" | maps_wolf_df_change$landcover_cc == "other coniferous forests" ~ "coniferous forests",
    maps_wolf_df_change$landcover_cc == "open wetlands" | maps_wolf_df_change$landcover_cc == "heath" ~ "mires",
    maps_wolf_df_change$landcover_cc == "water" ~ NA_character_,
    TRUE ~ maps_wolf_df_change$landcover_cc
  ) %>% as.factor()
  
  maps_wolf_df_change$landcover_cc = forcats::fct_collapse(maps_wolf_df_change$landcover_cc,
                                                           "open lands" = c("arable lands", "tundra/rocks", "other open lands")) %>% 
    forcats::fct_relevel("clear cuts", after = 0)
  
  # predict
  predw <- predict(best_fit_pred, maps_wolf_df_change, type = "response")
  prediction_pred <- dem
  prediction_pred[] <- predw
  names(prediction_pred) <- year
  plot(prediction_pred)
  
  # save prediction
  maps_pred_year <- c(maps_pred_year, prediction_pred)
}

rast1 <- maps_pred_year[[1]]
rast2 <- maps_pred_year[[2]]
rast3 <- maps_pred_year[[3]]
rast4 <- maps_pred_year[[4]]

maps_pred_year_rast <- raster::stack(rast1, rast2, rast3, rast4)
names(maps_pred_year_rast) <- paste0("year:", as.character(2015:2018))

# prediction maps
# png("results/prediction_map_rsf_pred.png", width = 28, height = 20, units = "cm", res = 300)
# par(mfrow = c(3,2))
# years <- 2015:2019
# for(i in 1:dim(maps_pred_year_rast)[3]) {
#   raster::plot(maps_pred_year_rast[[i]], main = years[i])
#   plot(as(turbines[1], "Spatial"), add = T, pch = 4, cex = 0.5)
# }
# dev.off()
# par(mfrow = c(1,1))

png("results/prediction_map_rsf_pred.png", width = 18, height = 18, units = "cm", res = 300)
plt <- rasterVis::levelplot(maps_pred_year_rast, 
                            par.settings = rasterTheme(region = rev(terrain.colors(n = 100))),
                            names.attr = c(as.character(2015:2018)))
plt + latticeExtra::layer(sp.points(as(turbines[1], "Spatial"), 
                                    col = "black", cex = 0.4, pch = 4))
dev.off()

# predators maps
# png("results/wolves_years.png", width = 28, height = 20, units = "cm", res = 300)
# par(mfrow = c(3,2))
# years <- 2015:2019
# for(i in 1:length(years)) {
#   raster::plot(raster(paste0("maps/wolf_", years[i], ".tif")), main = years[i])
#   plot(as(turbines[1], "Spatial"), add = T, pch = 4, cex = 0.5)
# }
# dev.off()
# 
# png("results/wolverine_years.png", width = 18, height = 25, units = "cm", res = 300)
# par(mfrow = c(3,2))
# years <- 2015:2019
# for(i in 1:length(years)) {
#   raster::plot(raster(paste0("maps/wolverine_", years[i], ".tif")), main = years[i])
#   plot(as(turbines[1], "Spatial"), add = T, pch = 4, cex = 0.5)
# }
# dev.off()

# export
# save maps
years <- 2015:2018
for(i in 1:length(years)) {
  terra::writeRaster(maps_pred_year_rast[[i]], paste0("results/prediction_maps/prediction_rsf_predators_", years[i],".tif"), 
                     gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)  
}

# wolves
wolves <- list.files("maps", pattern = "wolf", full.names = T) %>% 
  grep(pattern = "15|16|17|18", value = T) %>% 
  grep(pattern = ".tif", value = T) %>% 
  raster::stack()

png("results/wolves_years.png", width = 18, height = 18, units = "cm", res = 300)
plt <- rasterVis::levelplot(wolves, 
                            par.settings = rasterTheme(region = rev(terrain.colors(n = 100))),
                            names.attr = c(as.character(2015:2018)))
plt + latticeExtra::layer(sp.points(as(turbines[1], "Spatial"), 
                                    col = "black", cex = 0.4, pch = 4))
dev.off()

# wolverines
wolverines <- list.files("maps", pattern = "wolverine", full.names = T) %>% 
  grep(pattern = "15|16|17|18", value = T) %>% 
  grep(pattern = ".tif", value = T) %>% 
  raster::stack()

png("results/wolverines_years.png", width = 18, height = 18, units = "cm", res = 300)
plt <- rasterVis::levelplot(wolverines, 
                            par.settings = rasterTheme(region = rev(terrain.colors(n = 100))),
                            names.attr = c(as.character(2015:2019)))
plt + latticeExtra::layer(sp.points(as(turbines[1], "Spatial"), 
                                    col = "black", cex = 0.4, pch = 4))
dev.off()


# predictions and predator maps in the same panel

# normalize predator density between 0 and 1
max_wolf <- as.data.frame(wolves) %>% 
  max(na.rm = T)
wolves <- wolves/max_wolf

max_wolverine <- as.data.frame(wolverines) %>% 
  max(na.rm = T)
wolverines <- wolverines/max_wolverine

predictions_predators <- raster::stack(rast1, wolves[[1]], wolverines[[1]],
                                       rast2, wolves[[2]], wolverines[[2]],
                                       rast3, wolves[[3]], wolverines[[3]],
                                       rast4, wolves[[4]], wolverines[[4]])

names_rast <- lapply(2015:2018, function(x) paste(c("Reindeer use", "Wolf", "Wolverine"), x)) %>% 
  unlist()
png("results/prediction_predators.png", width = 20, height = 24, units = "cm", res = 300)
plt <- rasterVis::levelplot(predictions_predators, 
                            par.settings = rasterTheme(region = rev(terrain.colors(n = 100))),
                            names.attr = names_rast)
plt + latticeExtra::layer(sp.points(as(turbines[1], "Spatial"), 
                                    col = "black", cex = 0.4, pch = 4))
dev.off()
