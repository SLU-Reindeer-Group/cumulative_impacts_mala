#' ---
#' title: 'Habitat selection analysis of Swedish reindeer during calving at regional scale: RSF at Mala sameby'
#' author: Bernardo Niebuhr
#' abstract: ""
#' ---

# --------------- label=load_packages, warning=FALSE, message=FALSE, echo=FALSE

# setwd("~/Mounts/P-Prosjekter/41203800_oneimpact/05_papers/12_cumulative_impacts_mala/")

# Load packages
if(!require(install.load)) install.packages('install.load'); library(install.load)
install.load::install_load('ezknitr', 'knitr')

# --------------- label=setup, warning=FALSE, message=FALSE
# Set up 

# Clean everything before beginning
rm(list = ls())

# Load packages
install.load::install_load("tidyverse", "ggridges", "GGally")
install.load::install_load("broom", "bbmle", "ggeffects")
install.load::install_load("glmnet")

#library(devtools)
#devtools::install_github("phuais/multifitR")

#-----------
#' # Calving analysis
#' 
#' ## Load annotated and clean movement data

# vindval data
load(file = "data/01_GPS/05_annotated_data/data_annotated_calving_ready_2h_rsf.RData")

#-----------------------
# Fit

# weights - not used
data_ready_calving_rsf_2h$w_obs <- ifelse(data_ready_calving_rsf_2h$case_, 1, 5000)

summary(data_ready_calving_rsf_2h$wind_turbines_dist_comb)
#hist(data_ready_calving_rsf_2h$wind_turbines_dist_comb+1, breaks = 100, xlim = c(0, 10000))

#-------
# GLM, no threshold regression

# scale(ndvi) [time integrated ndvi] was included in all models, but was removed afterwards.
# we noticed the variable did not represent variation in NDVI properly, but was basically
# representing avoidance of forested areas.

base_formula <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(sos) + 
  log(power_lines_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + 
  log(trails_dist+1) + 
  log(house_dist+1) + log(mining_Krist_dist+1) +
  wf_name + landcover_cc*phase*log(wind_turbines_dist_comb+1)# + 
  #log(mining_Krist_dist+1):log(wind_turbines_dist_comb+1)
# Adding an interaction wind*mining explodes the VIF and creates issues with fitting

fit1 <- glm(update(base_formula, ~ . + tpi_cat_s150m), 
            data = data_ready_calving_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))

fit1$formula
summary(fit1)
car::vif(fit1)

# backward deletion - do not do it now
# remove cos_aspect
base_formula2 <- base_formula
#base_formula2 <- update(base_formula, ~ . - cos_aspect)
fit2 <- glm(update(base_formula2, ~ . + tpi_cat_s150m),
            data = data_ready_calving_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))

fit2$formula
summary(fit2)
car::vif(fit2)

# compare TPI
fit2.2 <- glm(update(base_formula2, ~ . + tpi_cat_s250m), 
            data = data_ready_calving_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))

fit2.3 <- glm(update(base_formula2, ~ . + tpi_cat_s510m), 
              data = data_ready_calving_rsf_2h, #weights = w_obs,
              family = binomial(link = "logit"))

# compare
(AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, weights = T, base = T))

# compare viewshed and landcover_cc
# formula_calving3 <- update(base_formula2, ~ . + tpi_cat_s150m - 
#                              landcover_cc*phase*log(wind_dist_jsy+1) +
#                              viewshed_lc*phase*log(wind_dist_jsy+1))
base_formula3 <- update(base_formula2, ~ . + tpi_cat_s150m -
                             landcover_cc*phase*log(wind_turbines_dist_comb+1) +
                             viewshed_lc*phase*log(wind_turbines_dist_comb+1))
base_formula3.2 <- update(base_formula2, ~ . + tpi_cat_s250m -
                             landcover_cc*phase*log(wind_turbines_dist_comb+1) +
                             viewshed_lc*phase*log(wind_turbines_dist_comb+1))
base_formula3.3 <- update(base_formula2, ~ . + tpi_cat_s510m -
                             landcover_cc*phase*log(wind_turbines_dist_comb+1) +
                             viewshed_lc*phase*log(wind_turbines_dist_comb+1))

fit3 <- glm(base_formula3,
            data = data_ready_calving_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))
fit3.2 <- glm(base_formula3.2,
            data = data_ready_calving_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))
fit3.3 <- glm(base_formula3.3,
            data = data_ready_calving_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))

car::vif(fit3)
summary(fit3)

# compare
(AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, 
                                fit3, fit3.2, fit3.3, weights = T, base = T))

# best model
best_fit <- fit3
summary(best_fit)

#-------
# GLM, no threshold regression, using ZoI for wind power and mining

# base formula
base_formula <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(sos) + 
  log(power_lines_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + 
  log(trails_dist+1) + 
  log(house_dist+1) + scale(mining_Krist_nearestXXX) +
  wf_name + landcover_cc*phase*scale(wind_turbines_ZZZ)

# values for ZOI radii and metrics, TPI scales, and type of land cover variable
scals <- c(100, 250, 500, 1000, 2500, 5000, 10000)
zois <- c("nearest", "cumulative")
wind_scals <- paste0(rep(zois, each = length(scals)), rep(scals, length(zois)))
tpi_scals <- c("tpi_cat_s150m", "tpi_cat_s250m", "tpi_cat_s510m")
lc_var <- c("landcover_cc", "viewshed_lc")

# n models = 7*2*14*3*2 = 1176

# loop over models
fits <- tibble::tibble(mining = numeric(), wind = character(), 
                       tpi = character(), lc = character(), 
                       aic = numeric(), definition = character())

counter <- 1
for(i in scals) {
  for(j in wind_scals) {
  for(tt in tpi_scals) {
  for(lc in lc_var) {
    updt <- paste0("~ . - scale(mining_Krist_nearestXXX) - ",
                   "landcover_cc*phase*scale(wind_turbines_ZZZ) + ", 
                   tt, " + ",
                   paste0("scale(mining_Krist_nearest", i, ") + "),
                   paste0(lc, "*phase*scale(wind_turbines_", j, ")"))
    form <- update(base_formula, updt)
    
    print(paste0("mines = ", i, "; wind = ", j, "; tpi = ", tt, 
                 "; lc = ", lc, " up and running..."))
    # fit1 <- glm(form, 
    #             data = data_ready_calving_rsf_2h, #weights = w_obs,
    #             family = binomial(link = "logit"))
    fits[counter,1] <- i
    fits[counter,2] <- j
    fits[counter,3] <- tt
    fits[counter,4] <- lc
    # fits[counter,5] <- AIC(fit1)
    fits[counter,6] <- paste(as.character(fit1$formula)[c(2,1,3)], collapse = " ")
    
    counter <- counter+1
  }}}
}

# save(fits, file = "output/calving_multi_zoi_table.rda")
load("output/rsf_calving_multi_zoi_table.rda")
fits

# compare models
(tab_models <- fits |>
  dplyr::mutate(mining = as.character(mining)) |>
  dplyr::bind_rows(
    tibble::tibble(mining = "log", wind = "log", 
                   tpi = "tpi_cat_s150m",
                   lc = "viewshed_lc",
                   aic = AIC(best_fit),
                   definition = paste(as.character(best_fit$formula)[c(2,1,3)], collapse = " "))) |>
  dplyr::mutate(dAIC = abs(aic - min(aic))) |>
  dplyr::arrange(dAIC))

tab_models |>
  readr::write_csv(file = "output/01_traditional_fit/rsf_calving_AIC_table_complete.csv")

# save(tab_models, file = "output/rsf_calving_multi_zoi_table.rda")

#---------------
# Re-run best model
form <- tab_models$definition[1]
best_fit <- glm(form, 
                data = data_ready_calving_rsf_2h, #weights = w_obs,
                family = binomial(link = "logit"))

#---------------
# More info, summary, and predictions on the best model
broom::glance(best_fit)
broom::tidy(best_fit) |> print(n = 100)

m1_plot <- broom::tidy(best_fit) |> 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0))) |> 
  ggplot(aes(x = term, y = estimate, color = significant)) +
  geom_point(show.legend = F) +
  # geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
  #                 position = position_dodge(width = 0.7), size = 0.8,
  #                 show.legend = F) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Relative Selection Strength", x = "") +
  theme_light() + 
  coord_flip()
m1_plot
ggsave("rsf_calving_coefs_log.png", plot = m1_plot, path = "output/01_traditional_fit/", 
       width = 20, height = 20, units = "cm", dpi = 300)

model_call <- sapply(attr(AICvals_phase, "row.names"), function(x) paste(as.character(formula(eval(as.symbol(x))))[c(2,1,3)], collapse = " "))
AICvals_phase |> 
  dplyr::as_tibble(rownames = "model") |>
  dplyr::mutate(definition = unname(model_call)) |>
  readr::write_csv(file = "output/01_traditional_fit/rsf_calving_AIC_table_log.csv")

summary(best_fit)
coefs <- broom::tidy(best_fit) |> 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0)),
                p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
coefs |> 
  mutate_if(is.numeric, round, digits = 3) |> 
  readr::write_csv(file = "output/01_traditional_fit/rsf_calving_coef_table_log.csv")

# Model validation
# AUC
library(pROC)
roc_best_fit <- pROC::roc(data_ready_calving_rsf_2h$case_, fitted(best_fit))
pROC::auc(roc_best_fit)

# CV
library(boot)
cv.glm()

#-------------------------------
# predictions

cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))

# averaged over all conditions, only slope
pr_slp <- ggpredict(best_fit, terms = "slope",
                   typical = "median") 
plot(pr_slp, lim = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey")

# only wind dist, averaged over all land cover types
# pr_wind_general <- ggpredict(best_fit, terms = c("wind_turbines_dist_comb", "phase"),
#                             typical = "median")
# pr_wind_general$x <- pr_wind_general$x/1000
# g_wind_general <- pr_wind_general |> 
#   plot(limits = c(0,1), show.title = F, show.x.title = F, col = cols) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to wind turbines (km)",
#        y = "Predicted probability of use",
#        colour = "Phase") +
#   xlim(0, 10)
# ggsave(filename = "results/calving_wind_phase_general.png", plot = g_wind_general, 
#        width = 15, height = 12, units = "cm", dpi = 300)

# wind dist, phase, and land cover, for ytteberg
pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_turbines_dist_comb", "phase", "viewshed_lc"),
                               typical = "median", condition = c(wf_name = "ytteberg"))
#, condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
g_log_wind <- pr_log_wind_phase |> 
  plot(show.title = F, col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to wind turbines (m)",
       y = "Predicted probability of use",
       colour = "Phase") +
  #ylim(0, 1) +
  xlim(0, 10)
ggsave(filename = "output/01_traditional_fit/calving_wind_phase_view_ytte.png", plot = g_log_wind, 
       width = 20, height = 10, units = "cm", dpi = 300)

# wind dist, phase, and land cover, for places closer to each of the WFs
wfs <- levels(data_ready_calving_rsf_2h$wf_name)
for(i in wfs) {
  pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_turbines_dist_comb", "phase", "viewshed_lc"),
                                 typical = "median", condition = c(wf_name = i))
  #, condition = c(landcover_cc = "coniferous forests"))
  pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
  g_log_wind <- pr_log_wind_phase |> 
    plot(show.title = F, limits = c(0,1), colors = cols) +
    geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
    labs(x = "Distance to wind turbines (km)",
         y = "Predicted probability of use",
         colour = "Phase") +
    xlim(0, 10)
  ggsave(filename = paste0("output/01_traditional_fit/rsf_calving_wind_phase_view_", i, ".png"), plot = g_log_wind, 
         width = 20, height = 8, units = "cm", dpi = 300)
}
# pr_wind_phase <- ggpredict(fit9_nolog, terms = c("wind_dist", "phase", "landcover"))
# g_wind <- pr_wind_phase |> 
#   plot(show.title = F, show.x.title = F) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to wind turbines (km)",
#        y = "Predicted probability of use") +
#   xlim(0, 10)
# ggsave(filename = "results/calving_wind_phase_lc_no_log.png", plot = g_wind, 
#        width = 20, height = 20, units = "cm", dpi = 300)

# mining
pr_mining <- ggpredict(best_fit, terms = c("mining_Krist_dist"))
pr_mining$x <- pr_mining$x/1000
g_mining <- pr_mining |> 
  plot(show.title = F, show.x.title = F,
       #limits = c(0,1), 
       colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to mining (km)",
       y = "Predicted probability of use",
       color = "Phase") +
  xlim(0, 20)
ggsave(filename = "rsf_calving_mining.png", plot = g_mining, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# roads
pr_public_road <- ggpredict(best_fit, terms = c("public_road_dist"))
pr_public_road$x <- pr_public_road$x/1000
g_road <- pr_public_road |>
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to large roads (km)",
       y = "Predicted probability of use",
       colour = "Phase") +
  xlim(0, 3)
ggsave(filename = "rsf_calving_roads.png", plot = g_road,
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# error with NAs
pr_priv_road <- ggpredict(best_fit, terms = c("priv_road_dist"))
pr_priv_road$x <- pr_priv_road$x/1000
g_prov_road <- pr_priv_road |> 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to small roads (km)",
       y = "Predicted probability of use",
       colour = "Phase") +
  xlim(0, 1)
ggsave(filename = "rsf_calving_private_roads.png", plot = g_prov_road, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

#tab <- do.call(cbind.data.frame, attr(pr_priv_road, "constant.values"))
#df <- data.frame(priv_road_dist = pr_priv_road$x*1000, tab)
#df <- data.frame(priv_road_dist = 1, tab)
#pred <- predict(best_fit, df, type = "link")
#exp(log(df$priv_road_dist+1)*-0.037344)

# power lines
pr_pl <- ggpredict(best_fit, terms = c("power_lines_dist"))
pr_pl$x <- pr_pl$x/1000
g_pl <- pr_pl |> 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to power lines (km)",
       y = "Predicted probability of use") +
  xlim(0, 10)
ggsave(filename = "rsf_calving_pl.png", plot = g_pl, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# trails
pr_trail <- ggpredict(best_fit, terms = c("trails_dist"))
pr_trail$x <- pr_trail$x/1000
g_trail <- pr_trail |> 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to trails (km)",
       y = "Predicted probability of use") +
  xlim(0, 5)
ggsave(filename = "rsf_calving_trails.png", plot = g_trail,
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# houses
pr_house <- ggpredict(best_fit, terms = c("house_dist"))
pr_house$x <- pr_house$x/1000
g_house <- pr_house |> 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to houses (km)",
       y = "Predicted probability of use") +
  xlim(0, 5)
ggsave(filename = "rsf_calving_houses.png", plot = g_trail, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# elevation
pr_elevation <- ggpredict(best_fit, terms = c("dem"))
g_elevation <- pr_elevation |> 
  plot(show.title = F, show.x.title = F,
       #limits = c(0,1), 
       colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Elevation (m)",
       y = "Predicted probability of use",
       color = "Phase") + 
  xlim(0, NA)
ggsave(filename = "rsf_calving_elevation.png", plot = g_elevation, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# tpi
pr_tpi <- ggpredict(best_fit, terms = c("tpi_cat_s150m"))
g_tpi <- pr_tpi |> 
  plot(show.title = F, show.x.title = F,
       #limits = c(0,1), 
       colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Terrain position index category (150m)",
       y = "Predicted probability of use",
       color = "Phase") 
ggsave(filename = "rsf_calving_tpi.png", plot = g_tpi, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

#-------
# spatial prediction
install.load::install_load("raster", "rasterVis", "terra", "stars")

# vindval data
load(file = "data/01_GPS/05_annotated_data/data_annotated_calving_ready_2h_rsf.RData")

# model comparison
load("output/rsf_calving_multi_zoi_table.rda")

# compare models
tab_models
#---------------
# Re-run best model
form <- tab_models$definition[1]
best_fit <- glm(form, 
                data = data_ready_calving_rsf_2h, #weights = w_obs,
                family = binomial(link = "logit"))

#-----------
# Load maps
maps_predictors <- terra::rast("maps/for_prediction/predictors_after.tif")
# maps_predictors <- maps_after
maps_predictors[["dem"]] <- dem
maps_predictors[["mining_Krist_dist"]] <- mining_dist
maps_predictors[["wind_turbines_dist_comb"]] <- wind_dist_comb
vars <- c("public_road_dist", "priv_road_dist", "power_lines_dist", 
          "trails_dist", "house_dist")
maps_predictors[[vars]] <- c(public_road_dist, priv_road_dist, pl_dist, trail_dist, house_dist)

best_fit$formula
names(maps_after)

#----
# after construction
maps_after <- maps_predictors
names(maps_after)[which(names(maps_after) == "sos_after")] <- "sos"
terra::plot(maps_after[[1:16]])
terra::plot(maps_after[[17:32]])

maps_after_df <- as.data.frame(maps_after, xy = TRUE, cell = TRUE, na.rm = FALSE)
str(maps_after_df)

pred_calv_after <- terra::predict(best_fit, maps_after_df, type = "response")

#maps_after_pred <- as.data.frame(terra::rast(dem), xy = TRUE, cell = TRUE, na.rm = FALSE)
map_after_pred <- terra::rast(dem)
terra::values(map_after_pred) <- pred_calv_after
names(map_after_pred) <- "Operation"
terra::plot(map_after_pred)
rasterVis::levelplot(map_after_pred)
terra::writeRaster(map_after_pred, filename = "output/map_prediction_calving_after.tif")

#prediction_calving_after <- terra::predict(best_fit, maps_after, type = "response")

#---------------
# other phases

# Before
maps_before <- maps_predictors
names(maps_before)[which(names(maps_before) == "sos_before")] <- "sos"
terra::plot(maps_before[[1:16]])
terra::plot(maps_before[[17:32]])

maps_before_df <- as.data.frame(maps_before, xy = TRUE, cell = TRUE, na.rm = FALSE)
maps_before_df <- maps_before_df |> 
  dplyr::mutate(phase = factor(1, levels = 1:3, 
                               labels = levels(maps_before_df$phase)))
# phase = "Before construction"

str(maps_before_df)

pred_calv_before <- terra::predict(best_fit, maps_before_df, type = "response")


# saveRDS(maps_before_df, file = "maps/for_prediction/maps_before_df.rds")

# During


#saveRDS(maps_during_df, file = "maps/for_prediction/maps_during_df.rds")

#--------
# predict


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
                     prediction_phase_after_calving) |> 
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
prediction_phase_before_calving <- terra::rast("results/prediction_maps/prediction_rsf_phase_before_calving.tif")
prediction_phase_during_calving <- terra::rast("results/prediction_maps/prediction_rsf_phase_during_calving.tif")
prediction_phase_after_calving <- terra::rast("results/prediction_maps/prediction_rsf_phase_after_calving.tif")

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
plt <- raster::stack(prediction_weather_good, prediction_weather_medium, prediction_weather_bad) |> 
  rasterVis::levelplot(par.settings = rasterTheme(region = rev(terrain.colors(n = 100))),
                       names.attr = names_weather, layout = c(3,1))
plt + latticeExtra::layer(sp.points(as(turbines[1], "Spatial"), 
                                    col = "black", cex = 0.4, pch = 4))
dev.off()


