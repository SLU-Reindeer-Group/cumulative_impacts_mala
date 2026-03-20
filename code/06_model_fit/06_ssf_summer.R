#' ---
#' title: 'Step selection analysis of Swedish reindeer during summer at local scale: iSSF at Mala sameby'
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
install.load::install_load("survival", "amt")

#library(devtools)
#devtools::install_github("phuais/multifitR")


###################################################
# TO DO:
# - annotate crossings
# - think about how to evaluate before vs during vs after (given the limitation of the clogit fit)
# - how do we set and update the step length distributions? and compute log_rss
# 

#-----------
#' # summer analysis
#' 
#' ## Load annotated and clean movement data

# vindval data
load(file = "data/01_GPS/05_annotated_data/data_annotated_summer_ready_2h_ssf_cross.RData")
# load(file = "data/01_GPS/05_annotated_data/data_annotated_summer_ready_2h_ssf.RData")

#------------
#- Check correlations


#-----------------------
# Fit

# subsample for tests
# data_ready_summer_ssf_2h |> 
#   dplyr::filter(case_ == TRUE) |>  
#   dplyr::slice_sample(prop = 0.2) |> 
#   pull(step_id_)

# weights - not used
data_ready_summer_ssf_2h$w_obs <- ifelse(data_ready_summer_ssf_2h$case_, 1, 5000)
# step_id
data_ready_summer_ssf_2h <- data_ready_summer_ssf_2h |> 
  dplyr::mutate(step_id_ = paste0(id, "_", step_id_),
                log_sl = log(sl_+1),
                cos_ta = cos(ta_),
                landcover_cc_grouped_end = relevel(landcover_cc_grouped_end, ref = "deciduous forests"),
                landcover_cc_grouped_start = relevel(landcover_cc_grouped_start, ref = "deciduous forests"),
                tpi_cat_s150m_end <- as.factor(tpi_cat_s150m_end),
                wind_turbines_cumulative100_start_construction_scaled = scale(wind_turbines_cumulative100_start_construction),
                wind_turbines_cumulative250_start_construction_scaled = scale(wind_turbines_cumulative250_start_construction),
                wind_turbines_cumulative500_start_construction_scaled = scale(wind_turbines_cumulative500_start_construction),
                wind_turbines_cumulative1000_start_construction_scaled = scale(wind_turbines_cumulative1000_start_construction),
                wind_turbines_cumulative2500_start_construction_scaled = scale(wind_turbines_cumulative2500_start_construction),
                wind_turbines_cumulative5000_start_construction_scaled = scale(wind_turbines_cumulative5000_start_construction),
                wind_turbines_cumulative10000_start_construction_scaled = scale(wind_turbines_cumulative10000_start_construction),
                wind_turbines_cumulative100_start_operation_scaled = scale(wind_turbines_cumulative100_start_operation),
                wind_turbines_cumulative250_start_operation_scaled = scale(wind_turbines_cumulative250_start_operation),
                wind_turbines_cumulative500_start_operation_scaled = scale(wind_turbines_cumulative500_start_operation),
                wind_turbines_cumulative1000_start_operation_scaled = scale(wind_turbines_cumulative1000_start_operation),
                wind_turbines_cumulative2500_start_operation_scaled = scale(wind_turbines_cumulative2500_start_operation),
                wind_turbines_cumulative5000_start_operation_scaled = scale(wind_turbines_cumulative5000_start_operation),
                wind_turbines_cumulative10000_start_operation_scaled = scale(wind_turbines_cumulative10000_start_operation),
                wind_turbines_nearest100_start_construction_scaled = scale(wind_turbines_nearest100_start_construction),
                wind_turbines_nearest250_start_construction_scaled = scale(wind_turbines_nearest250_start_construction),
                wind_turbines_nearest500_start_construction_scaled = scale(wind_turbines_nearest500_start_construction),
                wind_turbines_nearest1000_start_construction_scaled = scale(wind_turbines_nearest1000_start_construction),
                wind_turbines_nearest2500_start_construction_scaled = scale(wind_turbines_nearest2500_start_construction),
                wind_turbines_nearest5000_start_construction_scaled = scale(wind_turbines_nearest5000_start_construction),
                wind_turbines_nearest10000_start_construction_scaled = scale(wind_turbines_nearest10000_start_construction),
                wind_turbines_nearest100_start_operation_scaled = scale(wind_turbines_nearest100_start_operation),
                wind_turbines_nearest250_start_operation_scaled = scale(wind_turbines_nearest250_start_operation),
                wind_turbines_nearest500_start_operation_scaled = scale(wind_turbines_nearest500_start_operation),
                wind_turbines_nearest1000_start_operation_scaled = scale(wind_turbines_nearest1000_start_operation),
                wind_turbines_nearest2500_start_operation_scaled = scale(wind_turbines_nearest2500_start_operation),
                wind_turbines_nearest5000_start_operation_scaled = scale(wind_turbines_nearest5000_start_operation),
                wind_turbines_nearest10000_start_operation_scaled = scale(wind_turbines_nearest10000_start_operation),
                dem_end_scaled = scale(dem_end),
                slope_end_scaled = scale(slope_end),
                sos_end_scaled = scale(sos_end),
                log_power_lines_dist_end = log(power_lines_dist_end+1), 
                log_priv_road_dist_end = log(priv_road_dist_end+1), 
                log_public_road_dist_end = log(public_road_dist_end+1),
                log_house_dist_end = log(house_dist_end+1),
                log_mining_Krist_dist_end = log(mining_Krist_dist_end+1),
                mining_Krist_nearest100_end_scaled = scale(mining_Krist_nearest100_end),
                mining_Krist_nearest250_end_scaled = scale(mining_Krist_nearest250_end),
                mining_Krist_nearest500_end_scaled = scale(mining_Krist_nearest500_end),
                mining_Krist_nearest1000_end_scaled = scale(mining_Krist_nearest1000_end),
                mining_Krist_nearest2500_end_scaled = scale(mining_Krist_nearest2500_end),
                mining_Krist_nearest5000_end_scaled = scale(mining_Krist_nearest5000_end),
                mining_Krist_nearest10000_end_scaled = scale(mining_Krist_nearest10000_end),
                wind_turbines_cumulative100_end_construction_scaled = scale(wind_turbines_cumulative100_end_construction),
                wind_turbines_cumulative250_end_construction_scaled = scale(wind_turbines_cumulative250_end_construction),
                wind_turbines_cumulative500_end_construction_scaled = scale(wind_turbines_cumulative500_end_construction),
                wind_turbines_cumulative1000_end_construction_scaled = scale(wind_turbines_cumulative1000_end_construction),
                wind_turbines_cumulative2500_end_construction_scaled = scale(wind_turbines_cumulative2500_end_construction),
                wind_turbines_cumulative5000_end_construction_scaled = scale(wind_turbines_cumulative5000_end_construction),
                wind_turbines_cumulative10000_end_construction_scaled = scale(wind_turbines_cumulative10000_end_construction),
                wind_turbines_cumulative100_end_operation_scaled = scale(wind_turbines_cumulative100_end_operation),
                wind_turbines_cumulative250_end_operation_scaled = scale(wind_turbines_cumulative250_end_operation),
                wind_turbines_cumulative500_end_operation_scaled = scale(wind_turbines_cumulative500_end_operation),
                wind_turbines_cumulative1000_end_operation_scaled = scale(wind_turbines_cumulative1000_end_operation),
                wind_turbines_cumulative2500_end_operation_scaled = scale(wind_turbines_cumulative2500_end_operation),
                wind_turbines_cumulative5000_end_operation_scaled = scale(wind_turbines_cumulative5000_end_operation),
                wind_turbines_cumulative10000_end_operation_scaled = scale(wind_turbines_cumulative10000_end_operation),
                wind_turbines_nearest100_end_construction_scaled = scale(wind_turbines_nearest100_end_construction),
                wind_turbines_nearest250_end_construction_scaled = scale(wind_turbines_nearest250_end_construction),
                wind_turbines_nearest500_end_construction_scaled = scale(wind_turbines_nearest500_end_construction),
                wind_turbines_nearest1000_end_construction_scaled = scale(wind_turbines_nearest1000_end_construction),
                wind_turbines_nearest2500_end_construction_scaled = scale(wind_turbines_nearest2500_end_construction),
                wind_turbines_nearest5000_end_construction_scaled = scale(wind_turbines_nearest5000_end_construction),
                wind_turbines_nearest10000_end_construction_scaled = scale(wind_turbines_nearest10000_end_construction),
                wind_turbines_nearest100_end_operation_scaled = scale(wind_turbines_nearest100_end_operation),
                wind_turbines_nearest250_end_operation_scaled = scale(wind_turbines_nearest250_end_operation),
                wind_turbines_nearest500_end_operation_scaled = scale(wind_turbines_nearest500_end_operation),
                wind_turbines_nearest1000_end_operation_scaled = scale(wind_turbines_nearest1000_end_operation),
                wind_turbines_nearest2500_end_operation_scaled = scale(wind_turbines_nearest2500_end_operation),
                wind_turbines_nearest5000_end_operation_scaled = scale(wind_turbines_nearest5000_end_operation),
                wind_turbines_nearest10000_end_operation_scaled = scale(wind_turbines_nearest10000_end_operation),
                wind_turbines_nearest1000_cross_construction = ifelse(wind_turbines_nearest_exp_decay1000_cross_construction > 0.05, 1, 0),
                wind_turbines_nearest1000_cross_operation = ifelse(wind_turbines_nearest_exp_decay1000_cross_operation > 0.05, 1, 0),
                wind_turbines_nearest500_cross_construction = ifelse(wind_turbines_nearest_exp_decay500_cross_construction > 0.05, 1, 0),
                wind_turbines_nearest500_cross_operation = ifelse(wind_turbines_nearest_exp_decay500_cross_operation > 0.05, 1, 0),
                public_road_nearest100_cross = ifelse(public_road_nearest_exp_decay100_cross > 0.05, 1, 0)
  )
data_ready_summer_ssf_2h$landcover_cc_grouped_end
data_ready_summer_ssf_2h$tpi_cat_s150m_end <- factor(data_ready_summer_ssf_2h$tpi_cat_s150m_end)
data_ready_summer_ssf_2h$tpi_cat_s250m_end <- factor(data_ready_summer_ssf_2h$tpi_cat_s250m_end)
data_ready_summer_ssf_2h$tpi_cat_s510m_end <- factor(data_ready_summer_ssf_2h$tpi_cat_s510m_end)

data_ready_summer_ssf_2h |> 
  dplyr::group_by(step_id_) |> 
  dplyr::summarise(n = n())

# summary(data_ready_summer_ssf_2h$wind_turbines_dist_comb_end)
#hist(data_ready_summer_ssf_2h$wind_turbines_dist_comb+1, breaks = 100, xlim = c(0, 10000))
colnames(data_ready_summer_ssf_2h)
summary(data_ready_summer_ssf_2h$wind_turbines_cumulative10000_end)
summary(data_ready_summer_ssf_2h$wind_turbines_cumulative10000_start_construction)
summary(data_ready_summer_ssf_2h$wind_turbines_cumulative10000_end_construction)

#-------
# GLM, no threshold regression

# scale(ndvi) [time integrated ndvi] was included in all models, but was removed afterwards.
# we noticed the variable did not represent variation in NDVI properly, but was basically
# representing avoidance of forested areas.

base_formula <- case_ ~ strata(step_id_) + 
  sl_ + log_sl + cos_ta +
  scale(wind_turbines_cumulative1000_start_construction):(sl_ + log_sl + cos_ta) +
  scale(wind_turbines_cumulative1000_start_operation):(sl_ + log_sl + cos_ta) +
  landcover_cc_grouped_start:(sl_ + log_sl + cos_ta) +
  # landcover_cc_grouped_start*sl_ + 
  scale(dem_end) + I(scale(dem_end)^2) + scale(slope_end) + 
  scale(sos_end) + 
  log(power_lines_dist_end+1) + log(priv_road_dist_end+1) + log(public_road_dist_end+1) +
  log(house_dist_end+1) + log(mining_Krist_dist_end+1) +
  wf_name_end + 
  landcover_cc_grouped_end +
  scale(wind_turbines_cumulative1000_end_construction) +
  scale(wind_turbines_cumulative1000_end_operation) +
  landcover_cc_grouped_end:scale(wind_turbines_cumulative1000_end_construction) +
  landcover_cc_grouped_end:scale(wind_turbines_cumulative1000_end_operation)

base_formula <- case_ ~ strata(step_id_) +
  sl_ + log_sl + cos_ta +
  wind_turbines_cumulative1000_start_construction_scaled:(sl_ + log_sl + cos_ta) +
  wind_turbines_cumulative1000_start_operation_scaled:(sl_ + log_sl + cos_ta) +
  landcover_cc_grouped_start:(sl_ + log_sl + cos_ta) +
  # landcover_cc_grouped_start*sl_ +
  dem_end_scaled + I(dem_end_scaled^2) + slope_end_scaled +
  sos_end_scaled +
  log_power_lines_dist_end + log_priv_road_dist_end + log_public_road_dist_end +
  log_house_dist_end + mining_Krist_nearest2500_end_scaled +
  wf_name_end +
  landcover_cc_grouped_end +
  wind_turbines_cumulative1000_end_construction_scaled +
  wind_turbines_cumulative1000_end_operation_scaled +
  landcover_cc_grouped_end:wind_turbines_cumulative1000_end_construction_scaled +
  landcover_cc_grouped_end:wind_turbines_cumulative1000_end_operation_scaled +
  public_road_nearest100_cross +
  wind_turbines_nearest500_cross_construction +
  wind_turbines_nearest500_cross_operation

# data_ready_summer_ssf_2h <- amt::as_track(data_ready_summer_ssf_2h)
# sl_distr_params(data_ready_summer_ssf_2h)

all.vars(base_formula) %in% names(data_ready_summer_ssf_2h)

fit1 <- amt::fit_issf(formula = update(base_formula, ~ . + tpi_cat_s150m_end),
                      data = data_ready_summer_ssf_2h, model = TRUE)

fit1$model$formula
summary(fit1)

# compare TPI
fit2 <- amt::fit_issf(update(base_formula, ~ . + tpi_cat_s250m_end), 
                      data = data_ready_summer_ssf_2h)

fit3 <- amt::fit_issf(update(base_formula, ~ . + tpi_cat_s510m_end), 
                      data = data_ready_summer_ssf_2h)

# compare
(AICvals_phase <- bbmle::AICtab(fit1$model, fit2$model, fit3$model, weights = T, base = T))

# best model
best_fit <- fit1$model
summary(best_fit)

#-------
# GLM, no threshold regression, using ZoI for wind power and mining

# base formula
base_formula <- case_ ~ strata(step_id_) +
  sl_ + log_sl + cos_ta +
  wind_turbines_YYY_start_construction_scaled:(sl_ + log_sl + cos_ta) +
  wind_turbines_ZZZ_start_operation_scaled:(sl_ + log_sl + cos_ta) +
  landcover_cc_grouped_start:(sl_ + log_sl + cos_ta) +
  # landcover_cc_grouped_start*sl_ +
  dem_end_scaled + I(dem_end_scaled^2) + slope_end_scaled +
  sos_end_scaled +
  log_power_lines_dist_end + log_priv_road_dist_end + log_public_road_dist_end +
  log_house_dist_end + mining_Krist_nearestXXX_end_scaled + ## need to add scale XXX here
  wf_name_end +
  landcover_cc_grouped_end +
  wind_turbines_YYY_end_construction_scaled +
  wind_turbines_ZZZ_end_operation_scaled +
  landcover_cc_grouped_end:wind_turbines_YYY_end_construction_scaled +
  landcover_cc_grouped_end:wind_turbines_ZZZ_end_operation_scaled +
  public_road_nearest100_cross +
  wind_turbines_nearest500_cross_construction +
  wind_turbines_nearest500_cross_operation

# values for ZOI radii and metrics, TPI scales, and type of land cover variable
scals <- c(100, 250, 500, 1000, 2500, 5000, 10000)
zois <- c("nearest", "cumulative")
wind_scals <- paste0(rep(zois, each = length(scals)), rep(scals, length(zois)))
tpi_scals <- c("tpi_cat_s150m_end", "tpi_cat_s250m_end", "tpi_cat_s510m_end")
# lc_var <- c("landcover_cc")#, "viewshed_lc")

# n models = 7*2*14*3*1 = 588

# loop over models
fits <- tibble::tibble(mining = numeric(), wind = character(), 
                       tpi = character(), lc = character(), 
                       aic = numeric(), definition = character())

load(file = "output/ssf_summer_multi_zoi_table_landcover_partial_cross.rda")
tail(fits)
counter_start <- nrow(fits)
# counter_start <- 0

counter <- 1
for(i in scals) {
  for(j in wind_scals) {
    for(tt in tpi_scals) {
      # for(lc in lc_var) {
      # updt <- paste0("~ . - mining_Krist_nearestXXX_end_scaled - ",
      #                "wind_turbines_cumulativeYYY_start_construction_scaled:(sl_ + log_sl + cos_ta) - ",
      #                "wind_turbines_cumulativeZZZ_start_operation_scaled:(sl_ + log_sl + cos_ta) - ",
      #                "wind_turbines_cumulativeYYY_end_construction_scaled - ",
      #                "wind_turbines_cumulativeZZZ_end_operation_scaled - ",
      #                "landcover_cc_grouped_end:wind_turbines_cumulativeYYY_end_construction_scaled - ",
      #                "landcover_cc_grouped_end:wind_turbines_cumulativeZZZ_end_operation_scaled + ",
      #                tt, " + ",
      #                paste0("mining_Krist_nearestXXX_end_scaled", i, ") + "),
      #                paste0(lc, "*phase*scale(wind_turbines_", j, ")"))
      # form <- update(base_formula, updt)
      print(counter)
      
      if(counter > counter_start) {
      
        updt <- as.character(base_formula) |> 
          gsub(pattern = "XXX", replacement = i) |> 
          gsub(pattern = "YYY", replacement = j) |> 
          gsub(pattern = "ZZZ", replacement = j)
        
        form <- as.formula(paste(updt[2], updt[1], paste(updt[3], "+ ", tt)))
        
        print(paste0("mines = ", i, "; wind = ", j, "; tpi = ", tt, 
                     #"; lc = ", lc, 
                     " up and running..."))
        fit1 <- amt::fit_issf(formula = form,
                              data = data_ready_summer_ssf_2h, 
                              model = TRUE)
        fits[counter,1] <- i
        fits[counter,2] <- j
        fits[counter,3] <- tt
        fits[counter,4] <- "landcover_cc_grouped"
        fits[counter,5] <- AIC(fit1)
        fits[counter,6] <- paste(as.character(form)[c(2,1,3)], collapse = " ")
        
        save(fits, file = "output/ssf_summer_multi_zoi_table_landcover_partial_cross.rda")
      }
      
      counter <- counter+1
      
    }}#}
}

# compare models
(tab_models <- fits |>
    dplyr::mutate(mining = as.character(mining)) |>
    dplyr::bind_rows(
      tibble::tibble(mining = "log", wind = "cumulative5000", 
                     tpi = "tpi_cat_s150m",
                     lc = "landcover_cc_grouped",
                     aic = AIC(fit1),
                     definition = paste("case_", paste(as.character(fit1$model$formula)[c(1,3)], collapse = " ")))) |>
    dplyr::mutate(dAIC = abs(aic - min(aic))) |>
    dplyr::arrange(dAIC))

save(tab_models, file = "output/ssf_summer_multi_zoi_table_landcover_cross.rda")
load("output/ssf_summer_multi_zoi_table_landcover_cross.rda")
tab_models

# tab_models |>
#   readr::write_csv(file = "output/ssf_results/ssf_summer_AIC_table_complete.csv")

#load("output/ssf_summer_multi_zoi_table_landcover.rda")
tab_models

# save(tab_models, file = "output/ssf_summer_multi_zoi_table.rda")

#---------------
# Re-run best model
data_ready_summer_ssf_2h$tpi_cat_s150m_end <- as.factor(data_ready_summer_ssf_2h$tpi_cat_s150m_end)

form <- as.formula(tab_models$definition[3])
best_fit <- amt::fit_issf(form, 
                          data = data_ready_summer_ssf_2h, 
                          model = TRUE)
# best_fit <- fit1$model
best_fit$model$formula
summary(best_fit)

#---------------
# More info, summary, and predictions on the best model
broom::glance(best_fit$model)
broom::tidy(best_fit$model) |> print(n = 100)

broom::tidy(best_fit$model, conf.int = TRUE, exponentiate = FALSE) |> 
  dplyr::mutate(sig = ifelse(p.value < 0.05, 1, -1)) |> 
  print(n = 100)

m1_plot <- broom::tidy(best_fit$model) |> 
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
ggsave("ssf_summer_coefs_log_landcover.png", plot = m1_plot, path = "output/ssf_results/", 
       width = 20, height = 20, units = "cm", dpi = 300)

# model_call <- sapply(attr(AICvals_phase, "row.names"), function(x) paste(as.character(formula(eval(as.symbol(x))))[c(2,1,3)], collapse = " "))
# AICvals_phase |> 
#   dplyr::as_tibble(rownames = "model") |>
#   dplyr::mutate(definition = unname(model_call)) |>
#   readr::write_csv(file = "output/01_traditional_fit/ssf_summer_AIC_table_log.csv")

summary(best_fit)
coefs <- broom::tidy(best_fit$model) |> 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0)),
                p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
coefs |> 
  mutate_if(is.numeric, round, digits = 3) |> 
  readr::write_csv(file = "output/ssf_results/ssf_summer_coef_table_.csv")

# Model validation
# AUC
# library(pROC)
# roc_best_fit <- pROC::roc(data_ready_summer_ssf_2h$case_, fitted(best_fit$model))
# pROC::auc(roc_best_fit)

# CV
# library(boot)
# cv.glm()

#-------------------------------
# predictions

cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))

# averaged over all conditions, only slope

# Make a new data.frame for s1
s1 <- data.frame(
  dem_end_scaled = 0,
  slope_end_scaled = seq(from = -2, to = 2, length.out = 200),
  sos_end_scaled = mean(data_ready_summer_ssf_2h$sos_end_scaled),
  log_power_lines_dist_end = mean(data_ready_summer_ssf_2h$log_power_lines_dist_end),
  log_priv_road_dist_end = mean(data_ready_summer_ssf_2h$log_priv_road_dist_end),
  log_public_road_dist_end = mean(data_ready_summer_ssf_2h$log_public_road_dist_end),
  log_house_dist_end = mean(data_ready_summer_ssf_2h$log_house_dist_end),
  mining_Krist_nearest5000_end_scaled = mean(data_ready_summer_ssf_2h$mining_Krist_nearest5000_end_scaled),
  wf_name_end = factor("jokkmokksliden", levels = levels(data_ready_summer_ssf_2h$wf_name_end)),
  landcover_cc_grouped_end = factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)),
  tpi_cat_s150m_end = factor("lower slope", levels = levels(data_ready_summer_ssf_2h$tpi_cat_s150m_end)),
  wind_turbines_nearest5000_end_construction_scaled = 0,
  wind_turbines_nearest5000_end_operation_scaled = 0,
  landcover_cc_grouped_start = factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_start)),
  wind_turbines_nearest5000_start_construction_scaled = 0,
  wind_turbines_nearest5000_start_operation_scaled = 0,
  public_road_nearest100_cross = 0,
  wind_turbines_nearest500_cross_construction = 0,
  wind_turbines_nearest500_cross_operation = 0,
  step_id = data_ready_summer_ssf_2h$step_id_[1],
  sl_ = 100,
  log_sl = log(100),
  cos_ta = 1)

# data.frame for s2
s2 <- data.frame(
  dem_end_scaled = 0,
  slope_end_scaled = 0,
  sos_end_scaled = mean(data_ready_summer_ssf_2h$sos_end_scaled),
  log_power_lines_dist_end = mean(data_ready_summer_ssf_2h$log_power_lines_dist_end),
  log_priv_road_dist_end = mean(data_ready_summer_ssf_2h$log_priv_road_dist_end),
  log_public_road_dist_end = mean(data_ready_summer_ssf_2h$log_public_road_dist_end),
  log_house_dist_end = mean(data_ready_summer_ssf_2h$log_house_dist_end),
  mining_Krist_nearest5000_end_scaled = mean(data_ready_summer_ssf_2h$mining_Krist_nearest5000_end_scaled),
  wf_name_end = factor("jokkmokksliden", levels = levels(data_ready_summer_ssf_2h$wf_name_end)),
  landcover_cc_grouped_end = factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)),
  tpi_cat_s150m_end = factor("lower slope", levels = levels(data_ready_summer_ssf_2h$tpi_cat_s150m_end)),
  wind_turbines_nearest5000_end_construction_scaled = 0,
  wind_turbines_nearest5000_end_operation_scaled = 0,
  landcover_cc_grouped_start = factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_start)),
  wind_turbines_nearest5000_start_construction_scaled = 0,
  wind_turbines_nearest5000_start_operation_scaled = 0,
  public_road_nearest100_cross = 0,
  wind_turbines_nearest500_cross_construction = 0,
  wind_turbines_nearest500_cross_operation = 0,
  step_id = data_ready_summer_ssf_2h$step_id_[1],
  sl_ = 100,
  log_sl = log(100),
  cos_ta = 1)

# Calculate log-RSS
lr2 <- log_rss(best_fit, s1, s2, ci = "se", ci_level = 0.95)
# lr2_ci_boot <- log_rss(fit1, s1, s2, ci = "boot", ci_level = 0.95, n_boot = 1000)

# Plot using ggplot2
ggplot(lr2$df, aes(x = slope_end_scaled_x1, y = exp(log_rss))) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  xlab("Slope (SD)") +
  ylab("RSS") +
  ylim(0,2) + 
  theme_bw()

# -----
# for wind power

# only wind dist, averaged over all land cover types
rg_wind_const <- range(data_ready_summer_ssf_2h$wind_turbines_nearest5000_end_construction_scaled)
rg_wind_op <- range(data_ready_summer_ssf_2h$wind_turbines_nearest5000_end_operation_scaled)

# Make a new data.frame for s1
s1 <- data.frame(
  dem_end_scaled = 0,
  slope_end_scaled = 0,
  sos_end_scaled = mean(data_ready_summer_ssf_2h$sos_end_scaled),
  log_power_lines_dist_end = mean(data_ready_summer_ssf_2h$log_power_lines_dist_end),
  log_priv_road_dist_end = mean(data_ready_summer_ssf_2h$log_priv_road_dist_end),
  log_public_road_dist_end = mean(data_ready_summer_ssf_2h$log_public_road_dist_end),
  log_house_dist_end = mean(data_ready_summer_ssf_2h$log_house_dist_end),
  mining_Krist_nearest5000_end_scaled = mean(data_ready_summer_ssf_2h$mining_Krist_nearest5000_end_scaled),
  wf_name_end = factor("jokkmokksliden", levels = levels(data_ready_summer_ssf_2h$wf_name_end)),
  landcover_cc_grouped_end = factor(rep(levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)[-1], each = 200), levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_start)), #factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)),
  tpi_cat_s150m_end = factor("lower slope", levels = levels(data_ready_summer_ssf_2h$tpi_cat_s150m_end)),
  wind_turbines_nearest5000_end_construction_scaled = rep(seq(rg_wind_const[1], rg_wind_const[2], length.out = 200), length(levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)[-1])),
  wind_turbines_nearest5000_end_operation_scaled = rg_wind_op[1], #c(seq(rg_wind_op[1], rg_wind_op[2], length.out = 200),
  landcover_cc_grouped_start = factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_start)),
  wind_turbines_nearest5000_start_construction_scaled = 0,
  wind_turbines_nearest5000_start_operation_scaled = 0,
  public_road_nearest100_cross = 0,
  wind_turbines_nearest500_cross_construction = 0,
  wind_turbines_nearest500_cross_operation = 0,
  step_id = data_ready_summer_ssf_2h$step_id_[2],
  sl_ = 100,
  log_sl = log(100),
  cos_ta = 1)

# data.frame for s2
s2 <- data.frame(
  dem_end_scaled = 0,
  slope_end_scaled = 0,
  sos_end_scaled = mean(data_ready_summer_ssf_2h$sos_end_scaled),
  log_power_lines_dist_end = mean(data_ready_summer_ssf_2h$log_power_lines_dist_end),
  log_priv_road_dist_end = mean(data_ready_summer_ssf_2h$log_priv_road_dist_end),
  log_public_road_dist_end = mean(data_ready_summer_ssf_2h$log_public_road_dist_end),
  log_house_dist_end = mean(data_ready_summer_ssf_2h$log_house_dist_end),
  mining_Krist_nearest5000_end_scaled = mean(data_ready_summer_ssf_2h$mining_Krist_nearest5000_end_scaled),
  wf_name_end = factor("jokkmokksliden", levels = levels(data_ready_summer_ssf_2h$wf_name_end)),
  landcover_cc_grouped_end = factor("deciduous forests", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)),
  tpi_cat_s150m_end = factor("lower slope", levels = levels(data_ready_summer_ssf_2h$tpi_cat_s150m_end)),
  wind_turbines_nearest5000_end_construction_scaled = rg_wind_const[1],
  wind_turbines_nearest5000_end_operation_scaled = rg_wind_op[1], #c(seq(rg_wind_op[1], rg_wind_op[2], length.out = 200),
  landcover_cc_grouped_start = factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_start)),
  wind_turbines_nearest5000_start_construction_scaled = 0,
  wind_turbines_nearest5000_start_operation_scaled = 0,
  public_road_nearest100_cross = 0,
  wind_turbines_nearest500_cross_construction = 0,
  wind_turbines_nearest500_cross_operation = 0,
  step_id = data_ready_summer_ssf_2h$step_id_[2],
  sl_ = 100,
  log_sl = log(100),
  cos_ta = 1)

# Make a new data.frame for s1
s3 <- data.frame(
  dem_end_scaled = 0,
  slope_end_scaled = 0,
  sos_end_scaled = mean(data_ready_summer_ssf_2h$sos_end_scaled),
  log_power_lines_dist_end = mean(data_ready_summer_ssf_2h$log_power_lines_dist_end),
  log_priv_road_dist_end = mean(data_ready_summer_ssf_2h$log_priv_road_dist_end),
  log_public_road_dist_end = mean(data_ready_summer_ssf_2h$log_public_road_dist_end),
  log_house_dist_end = mean(data_ready_summer_ssf_2h$log_house_dist_end),
  mining_Krist_nearest5000_end_scaled = mean(data_ready_summer_ssf_2h$mining_Krist_nearest5000_end_scaled),
  wf_name_end = factor("jokkmokksliden", levels = levels(data_ready_summer_ssf_2h$wf_name_end)),
  landcover_cc_grouped_end = factor(rep(levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)[-1], each = 200), levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_start)), #factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)),
  tpi_cat_s150m_end = factor("lower slope", levels = levels(data_ready_summer_ssf_2h$tpi_cat_s150m_end)),
  wind_turbines_nearest5000_end_construction_scaled = rg_wind_const[1],
  wind_turbines_nearest5000_end_operation_scaled = rep(seq(rg_wind_op[1], rg_wind_op[2], length.out = 200), length(levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)[-1])),
  landcover_cc_grouped_start = factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_start)),
  wind_turbines_nearest5000_start_construction_scaled = 0,
  wind_turbines_nearest5000_start_operation_scaled = 0,
  public_road_nearest100_cross = 0,
  wind_turbines_nearest500_cross_construction = 0,
  wind_turbines_nearest500_cross_operation = 0,
  step_id = data_ready_summer_ssf_2h$step_id_[2],
  sl_ = 100,
  log_sl = log(100),
  cos_ta = 1)

# data.frame for s2
s4 <- data.frame(
  dem_end_scaled = 0,
  slope_end_scaled = 0,
  sos_end_scaled = mean(data_ready_summer_ssf_2h$sos_end_scaled),
  log_power_lines_dist_end = mean(data_ready_summer_ssf_2h$log_power_lines_dist_end),
  log_priv_road_dist_end = mean(data_ready_summer_ssf_2h$log_priv_road_dist_end),
  log_public_road_dist_end = mean(data_ready_summer_ssf_2h$log_public_road_dist_end),
  log_house_dist_end = mean(data_ready_summer_ssf_2h$log_house_dist_end),
  mining_Krist_nearest5000_end_scaled = mean(data_ready_summer_ssf_2h$mining_Krist_nearest5000_end_scaled),
  wf_name_end = factor("jokkmokksliden", levels = levels(data_ready_summer_ssf_2h$wf_name_end)),
  landcover_cc_grouped_end = factor("deciduous forests", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)),
  tpi_cat_s150m_end = factor("lower slope", levels = levels(data_ready_summer_ssf_2h$tpi_cat_s150m_end)),
  wind_turbines_nearest5000_end_construction_scaled = rg_wind_const[1],
  wind_turbines_nearest5000_end_operation_scaled = rg_wind_op[1], #c(seq(rg_wind_op[1], rg_wind_op[2], length.out = 200),
  landcover_cc_grouped_start = factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_start)),
  wind_turbines_nearest5000_start_construction_scaled = 0,
  wind_turbines_nearest5000_start_operation_scaled = 0,
  public_road_nearest100_cross = 0,
  wind_turbines_nearest500_cross_construction = 0,
  wind_turbines_nearest500_cross_operation = 0,
  step_id = data_ready_summer_ssf_2h$step_id_[2],
  sl_ = 100,
  log_sl = log(100),
  cos_ta = 1)

# Calculate log-RSS
# lr2 <- log_rss(fit1, s1, s2)
lr_during_ci_se <- log_rss(best_fit, s1, s2, ci = "se", ci_level = 0.95, n_boot = 1000)
lr_after_ci_se <- log_rss(best_fit, s3, s4, ci = "se", ci_level = 0.95, n_boot = 1000)

zoi_limit <- 0.05; radius <- 5000
# lambda1 <- log(1/zoi_limit) / radius1
lambda <- log(1/zoi_limit) / radius
# lr2$df$wind_turbines_cumulative5000_end_construction_scaled_x1 <- -log(lr2$df$wind_turbines_cumulative5000_end_construction_scaled_x1*ss + mm + 0.001)/lambda/1000
# lr2_ci_se$df$wind_turbines_cumulative5000_end_construction_scaled_x1 <- -log(lr2_ci_se$df$wind_turbines_cumulative5000_end_construction_scaled_x1*ss + mm + 0.000001)/lambda/1000
mm <- attr(data_ready_summer_ssf_2h$wind_turbines_nearest5000_end_construction_scaled, "scaled:center")
ss <- attr(data_ready_summer_ssf_2h$wind_turbines_nearest5000_end_construction_scaled, "scaled:scale")
data_ready_summer_ssf_2h$wind_turbines_nearest5000_end_construction_scaled*ss + mm
lr_during_ci_se$df$wind_dist_const <- -log(lr_during_ci_se$df$wind_turbines_nearest5000_end_construction_scaled_x1*ss + mm + 0.000001)/lambda/1000

mm <- attr(data_ready_summer_ssf_2h$wind_turbines_nearest5000_end_operation_scaled, "scaled:center")
ss <- attr(data_ready_summer_ssf_2h$wind_turbines_nearest5000_end_operation_scaled, "scaled:scale")
data_ready_summer_ssf_2h$wind_turbines_nearest5000_end_operation_scaled*ss + mm
lr_after_ci_se$df$wind_dist_op <- -log(lr_after_ci_se$df$wind_turbines_nearest5000_end_operation_scaled_x1*ss + mm + 0.000001)/lambda/1000
summary(data_ready_summer_ssf_2h$wind_turbines_nearest5000_end_operation)

fct_plot <- 1
df_all <- bind_rows(dplyr::mutate(lr_during_ci_se$df, phase = "Construction", wind_dist = wind_dist_const, wind_zoi = wind_turbines_nearest5000_end_construction_scaled_x1),
                    dplyr::mutate(lr_after_ci_se$df, phase = "Operation", wind_dist = wind_dist_op, wind_zoi = wind_turbines_nearest5000_end_operation_scaled_x1, 
                                  log_rss = fct_plot*log_rss,
                                  lwr = fct_plot*lwr, upr = fct_plot*upr))
# pr_wind_phase_df <- pr_wind_phase[sample.int(nrow(pr_wind_phase), size = 0.1*nrow(pr_wind_phase)),]

# newdat <- data.frame(case_ = 0,
#                      landcover_cc_grouped_end = factor(rep(levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)[-1], each = 200), levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_start)), #factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)),
#                      wind_turbines_nearest5000_end_operation = rep(seq(0, 10000, length.out = 200), length(levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)[-1])))
# oneimpact::predict(base_formula, newdata = newdat, coefs = coef(best_fit), include = colnames(newdat)[-1])

# Plot using ggplot2
# g_wind <- ggplot(df_all, aes(x = wind_dist, y = exp(log_rss), linetype = phase)) +
g_wind <- ggplot(df_all, aes(x = wind_dist, y = log_rss, linetype = phase)) +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "gray30") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  # geom_ribbon(aes(ymin = exp(lwr), ymax = exp(upr), linetype = phase),
  geom_ribbon(aes(ymin = lwr, ymax = upr, linetype = phase),
              # linetype = "dashed", color = "black",
              alpha = 0.25) +
  geom_line(size = 1) +
  facet_wrap(phase~ landcover_cc_grouped_end_x1, nrow = 2) +
  xlab("Distance to wind farm (km)") +
  ylab("log Relative selection strength") +
  # ylab("log(Relative selection strength)") +
  xlim(0, 10) +
  # ylim(0,5) +
  # ylim(-1,1) +
  theme_bw() +
  theme(legend.position = "none")
g_wind

ggsave(filename = "ssf_summer_wind.png", plot = g_wind, 
       path = "output/ssf_results/",
       width = 25, height = 25, units = "cm", dpi = 300)

g_wind_const <- df_all |> 
  dplyr::filter(phase == "Construction") |> 
  # ggplot(aes(x = wind_dist, y = exp(log_rss))) +
  ggplot(aes(x = wind_dist, y = log_rss, linetype = phase)) +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "gray30") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  # geom_ribbon(aes(ymin = exp(lwr), ymax = exp(upr)),
  geom_ribbon(aes(ymin = lwr, ymax = upr, linetype = phase),
              # linetype = "dashed", color = "black",
              alpha = 0.25) +
  geom_line(size = 1) +
  facet_wrap(~ landcover_cc_grouped_end_x1, nrow = 1) +
  xlab("Distance to wind farm (km)") +
  ylab("log Relative selection strength") +
  # ylab("log(Relative selection strength)") +
  xlim(0, 10) +
  # ylim(0,5) +
  # ylim(-1,1) +
  theme_bw() +
  theme(legend.position = "none")
g_wind_const

ggsave(filename = "ssf_summer_wind_const.png", plot = g_wind_const, 
       path = "output/ssf_results/",
       width = 25, height = 12, units = "cm", dpi = 300)

#---
# this below is a proof of concept, just to make sure it works

#' zoi_limit <- 0.05; radius <- 5000
#' # lambda1 <- log(1/zoi_limit) / radius1
#' lambda <- log(1/zoi_limit) / radius
#' # lr2$df$wind_turbines_cumulative5000_end_construction_scaled_x1 <- -log(lr2$df$wind_turbines_cumulative5000_end_construction_scaled_x1*ss + mm + 0.001)/lambda/1000
#' # lr2_ci_se$df$wind_turbines_cumulative5000_end_construction_scaled_x1 <- -log(lr2_ci_se$df$wind_turbines_cumulative5000_end_construction_scaled_x1*ss + mm + 0.000001)/lambda/1000
#' mm_op <- attr(data_ready_summer_ssf_2h$wind_turbines_nearest5000_end_construction_scaled, "scaled:center")
#' ss_op <- attr(data_ready_summer_ssf_2h$wind_turbines_nearest5000_end_construction_scaled, "scaled:scale")
#' data_ready_summer_ssf_2h$wind_turbines_nearest5000_end_construction_scaled*ss_op + mm_op
#' 
#' df_base <- data.frame(wind_dist = seq(0, 20000, length.out = 4001))
#' zoi_rad <- 10000
#' zoi_shape <- "exp_decay"
#' df_base$zoi_n1 <- 1*oneimpact::dist_decay(df_base$wind_dist, radius = zoi_rad, type = zoi_shape)
#' 
#' #' Function to get the closes match between two vectors
#' #' 
#' #' @param x,y numeric. The two vectors.
#' #' @param return character. If `"value"`, returns the values of y that are
#' #' closest to the values in x. If `"index"`, returns the indices of the values
#' #' in y that are closest to the values in x. 
#' get_closest_match <- function(x, y, return = c("value", "index")[1]) {
#'   
#'   # Initialize a vector to store the closest matches
#'   closest_matches <- numeric(length(x))
#'   
#'   # Loop through vector1 and find the closest match in vector2
#'   for (i in seq_along(x)) {
#'     current_value <- x[i]
#'     closest_index <- which.min(abs(y - current_value))
#'     if(return == "value") closest_matches[i] <- y[closest_index]
#'     else if(return == "index") closest_matches[i] <- closest_index
#'   }
#'   
#'   closest_matches
#' }
#' vector1 <- c(1, 3, 7, 10)
#' vector2 <- c(0, 2, 6, 8, 11)
#' get_closest_match(vector1, vector2)
#' get_closest_match(vector1, vector2, return = "index")
#' 
#' # get rows of closes match
#' rows_n1 <- get_closest_match(df_all$wind_zoi*ss_op + mm_op, 
#'                              df_base$zoi_n1, return = "index")
#' 
#' # get distance
#' df_all$dist_wind_n1 <- df_base$wind_dist[rows_n1]/1000
#' 
#' g_wind_n1 <- df_all |> 
#'   dplyr::filter(!(dist_wind_n1 == 0)) |> 
#'   ggplot() +
#'   # g_wind <- ggplot(df_all, aes(x = wind_dist, y = log_rss, linetype = phase)) +
#'   # geom_hline(yintercept = 1, linetype = "dashed", color = "gray30") +
#'   geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
#'   # geom_ribbon(aes(x = dist_wind, ymin = exp(lwr), ymax = exp(upr)),
#'   geom_ribbon(aes(x = dist_wind_n1, ymin = lwr, ymax = upr),
#'               # linetype = "dashed", color = "black",
#'               alpha = 0.25) +
#'   geom_line(aes(x = dist_wind_n1, y = log_rss), size = 1) +
#'   # geom_line(aes(x = dist_wind, y = exp(log_rss)), size = 1, linetype = 2) +
#'   facet_wrap(phase ~ landcover_cc_grouped_end_x1, nrow = 2) +
#'   xlab("Distance from wind turbines (km)") +
#'   ylab("log Relative selection strength") +
#'   # ylab("log(Relative selection strength)") +
#'   xlim(0, 10) +
#'   # ylim(0,3) +
#'   # ylim(-2,2) +
#'   theme_bw() +
#'   theme(legend.position = "none")
#' g_wind_n1
#' 
#' ggsave(filename = "ssf_autumn_wind_const.png", plot = g_wind_const, 
#'        path = "output/ssf_results/",
#'        width = 25, height = 25, units = "cm", dpi = 300)

#---
# mining
# only wind dist, averaged over all land cover types
rg_mine <- range(data_ready_summer_ssf_2h$mining_Krist_nearest5000_end_scaled)

# Make a new data.frame for s1
s1 <- data.frame(
  dem_end_scaled = 0,
  slope_end_scaled = 0,
  sos_end_scaled = mean(data_ready_summer_ssf_2h$sos_end_scaled),
  log_power_lines_dist_end = mean(data_ready_summer_ssf_2h$log_power_lines_dist_end),
  log_priv_road_dist_end = mean(data_ready_summer_ssf_2h$log_priv_road_dist_end),
  log_public_road_dist_end = mean(data_ready_summer_ssf_2h$log_public_road_dist_end),
  log_house_dist_end = mean(data_ready_summer_ssf_2h$log_house_dist_end),
  mining_Krist_nearest5000_end_scaled = seq(rg_mine[1], rg_mine[2], length.out = 200),
  wf_name_end = factor("jokkmokksliden", levels = levels(data_ready_summer_ssf_2h$wf_name_end)),
  landcover_cc_grouped_end = factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)),
  tpi_cat_s150m_end = factor("lower slope", levels = levels(data_ready_summer_ssf_2h$tpi_cat_s150m_end)),
  wind_turbines_nearest5000_end_construction_scaled = rg_wind_const[1],
  wind_turbines_nearest5000_end_operation_scaled = rg_wind_op[1], 
  landcover_cc_grouped_start = factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_start)),
  wind_turbines_nearest5000_start_construction_scaled = 0,
  wind_turbines_nearest5000_start_operation_scaled = 0,
  public_road_nearest100_cross = 0,
  wind_turbines_nearest500_cross_construction = 0,
  wind_turbines_nearest500_cross_operation = 0,
  step_id = data_ready_summer_ssf_2h$step_id_[2],
  sl_ = 100,
  log_sl = log(100),
  cos_ta = 1)

# data.frame for s2
s2 <- data.frame(
  dem_end_scaled = 0,
  slope_end_scaled = 0,
  sos_end_scaled = mean(data_ready_summer_ssf_2h$sos_end_scaled),
  log_power_lines_dist_end = mean(data_ready_summer_ssf_2h$log_power_lines_dist_end),
  log_priv_road_dist_end = mean(data_ready_summer_ssf_2h$log_priv_road_dist_end),
  log_public_road_dist_end = mean(data_ready_summer_ssf_2h$log_public_road_dist_end),
  log_house_dist_end = mean(data_ready_summer_ssf_2h$log_house_dist_end),
  mining_Krist_nearest5000_end_scaled = rg_mine[1],
  wf_name_end = factor("jokkmokksliden", levels = levels(data_ready_summer_ssf_2h$wf_name_end)),
  landcover_cc_grouped_end = factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_end)),
  tpi_cat_s150m_end = factor("lower slope", levels = levels(data_ready_summer_ssf_2h$tpi_cat_s150m_end)),
  wind_turbines_nearest5000_end_construction_scaled = rg_wind_const[1],
  wind_turbines_nearest5000_end_operation_scaled = rg_wind_op[1],
  landcover_cc_grouped_start = factor("open lands", levels = levels(data_ready_summer_ssf_2h$landcover_cc_grouped_start)),
  wind_turbines_nearest5000_start_construction_scaled = 0,
  wind_turbines_nearest5000_start_operation_scaled = 0,
  public_road_nearest100_cross = 0,
  wind_turbines_nearest500_cross_construction = 0,
  wind_turbines_nearest500_cross_operation = 0,
  step_id = data_ready_summer_ssf_2h$step_id_[2],
  sl_ = 100,
  log_sl = log(100),
  cos_ta = 1)

lr_mine <- log_rss(best_fit, s1, s2, ci = "se", ci_level = 0.95, n_boot = 1000)
# lr2_ci_boot <- log_rss(fit1, s1, s2, ci = "boot", ci_level = 0.95, n_boot = 1000)

zoi_limit <- 0.05; radius <- 5000
# lambda1 <- log(1/zoi_limit) / radius1
lambda <- log(1/zoi_limit) / radius
mm <- attr(data_ready_summer_ssf_2h$mining_Krist_nearest5000_end_scaled, "scaled:center")
ss <- attr(data_ready_summer_ssf_2h$mining_Krist_nearest5000_end_scaled, "scaled:scale")
data_ready_summer_ssf_2h$mining_Krist_nearest5000_end_scaled*ss + mm
lr_mine$df$mining_Krist_nearest5000_end_scaled_x1 <- -log(lr_mine$df$mining_Krist_nearest5000_end_scaled_x1*ss + mm + 0.000001)/lambda/1000

# fct_plot <- 1
# df_all <- bind_rows(dplyr::mutate(lr_mine$df, mine_dist = mining_Krist_nearest5000_end_scaled_x1),
#                     dplyr::mutate(lr_after_ci_se$df, phase = "Operation", wind_dist = wind_turbines_nearest5000_end_operation_scaled_x1, 
#                                   log_rss = fct_plot*log_rss,
#                                   lwr = fct_plot*lwr, upr = fct_plot*upr))

# Plot using ggplot2
g_mine <- ggplot(lr_mine$df, aes(x = exp(mining_Krist_nearest5000_end_scaled_x1)/1000)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  geom_line(aes(y = log_rss), size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              # linetype = "dashed", color = "black",
              alpha = 0.25) +
  xlab("Distance to mining (km)") +
  ylab("log(Relative selection strength)") +
  xlim(NA, 10) +
  # ylim(0, ) + 
  theme_bw()
g_mine

ggsave(filename = "ssf_summer_mine.png", plot = g_mine, 
       path = "output/ssf_results/",
       width = 15, height = 15, units = "cm", dpi = 300)


#-------------------------
# effects on movement

?amt::random_steps()
?amt::fit_distr()

sl_dist <- amt::fit_distr(data_ready_summer_ssf_2h$sl_+1, dist_name = "gamma")
ta_dist <- amt::fit_distr(data_ready_summer_ssf_2h$ta_, dist_name = "vonmises")

best_fit$sl_ <- sl_dist
best_fit$ta_ <- ta_dist

best_fit$model$formula
summary(best_fit)
# levels(data_ready_summer_ssf_2h$landcover_cc_grouped_start)
data_ready_summer_ssf_2h$wind_turbines_nearest5000_start_construction |> range()
data_ready_summer_ssf_2h$wind_turbines_nearest5000_start_construction_scaled |> range()

mm <- attr(data_ready_summer_ssf_2h$wind_turbines_nearest5000_start_construction_scaled, "scaled:center")
ss <- attr(data_ready_summer_ssf_2h$wind_turbines_nearest5000_start_construction_scaled, "scaled:scale")
# data_ready_summer_ssf_2h$wind_turbines_nearest5000_start_construction_scaled*ss + mm
# (-log(data_ready_summer_ssf_2h$wind_turbines_nearest5000_start_construction_scaled*ss + mm + 0.000001)/lambda/1000) |> range()
(1*exp(-lambda * 0) - mm)/ss
(1*exp(-lambda * 1000) - mm)/ss
(1*exp(-lambda * 10000) - mm)/ss

#----
# construction

# step lengths - on the turbines
close_sl <- update_gamma(dist = best_fit$sl_, 
                         beta_sl = best_fit$model$coefficients["sl_"] + 
                           8 * best_fit$model$coefficients["sl_:wind_turbines_nearest5000_start_construction_scaled"], 
                         beta_log_sl = best_fit$model$coefficients["log_sl"] + 
                           8 * best_fit$model$coefficients["log_sl:wind_turbines_nearest5000_start_construction_scaled"])

medium_sl <- update_gamma(dist = best_fit$sl_, 
                          beta_sl = best_fit$model$coefficients["sl_"] + 
                            4 * best_fit$model$coefficients["sl_:wind_turbines_nearest5000_start_construction_scaled"], 
                          beta_log_sl = best_fit$model$coefficients["log_sl"] + 
                            4 * best_fit$model$coefficients["log_sl:wind_turbines_nearest5000_start_construction_scaled"])

far_sl <- update_gamma(dist = best_fit$sl_, 
                       beta_sl = best_fit$model$coefficients["sl_"] + 
                         0 * best_fit$model$coefficients["sl_:wind_turbines_nearest5000_start_construction_scaled"], 
                       beta_log_sl = best_fit$model$coefficients["log_sl"] + 
                         0 * best_fit$model$coefficients["log_sl:wind_turbines_nearest5000_start_construction_scaled"])

# data.frame for plotting 
plot_sl <- data.frame(x = rep(NA, 200)) 

# x-axis is sequence of possible step lengths 
plot_sl$x <- seq(from = 0, to = 3000, length.out = 200) 
# y-axis is the probability density under the given gamma distribution 
plot_sl$`0 km` <- dgamma(x = plot_sl$x, 
                         shape = close_sl$params$shape, 
                         scale = close_sl$params$scale)
qgamma(c(0.025, 0.25, 0.5, 0.75, 0.975), shape = close_sl$params$shape, scale = close_sl$params$scale)

plot_sl$`1 km` <- dgamma(x = plot_sl$x, 
                         shape = medium_sl$params$shape, 
                         scale = medium_sl$params$scale)
qgamma(c(0.025, 0.25, 0.5, 0.75, 0.975), shape = medium_sl$params$shape, scale = medium_sl$params$scale)

plot_sl$`5 km` <- dgamma(x = plot_sl$x, 
                         shape = far_sl$params$shape, 
                         scale = far_sl$params$scale)
qgamma(c(0.025, 0.25, 0.5, 0.75, 0.975), shape = far_sl$params$shape, scale = far_sl$params$scale)

summary_sl <- data.frame(
  name = c("0 km", "1 km", "5 km"),
  dens = c(0.002, 0.0025, 0.003),
  xmin = qgamma(0.025, shape = c(close_sl$params$shape, medium_sl$params$shape, far_sl$params$shape), 
                scale = c(close_sl$params$scale, medium_sl$params$scale, far_sl$params$scale)),
  lower = qgamma(0.25, shape = c(close_sl$params$shape, medium_sl$params$shape, far_sl$params$shape), 
                 scale = c(close_sl$params$scale, medium_sl$params$scale, far_sl$params$scale)),
  median = qgamma(0.5, shape = c(close_sl$params$shape, medium_sl$params$shape, far_sl$params$shape), 
                  scale = c(close_sl$params$scale, medium_sl$params$scale, far_sl$params$scale)),
  upper = qgamma(0.75, shape = c(close_sl$params$shape, medium_sl$params$shape, far_sl$params$shape), 
                 scale = c(close_sl$params$scale, medium_sl$params$scale, far_sl$params$scale)),
  xmax = qgamma(0.925, shape = c(close_sl$params$shape, medium_sl$params$shape, far_sl$params$shape), 
                scale = c(close_sl$params$scale, medium_sl$params$scale, far_sl$params$scale))
)

# Pivot from wide to long data 
plot_sl <- plot_sl |> 
  pivot_longer(cols = -x) 

# Plot 
p1 <- ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) + 
  geom_line(size = 1) + 
  scale_color_manual(name = "Distance to WF", 
                     breaks = c("0 km", "1 km", "5 km"),
                     # breaks = c("On the WF", "1 km from the WF", "5 km from the WF"), 
                     values = c("firebrick", "gray50", "navyblue")) + 
  # Whiskers
  geom_segment(data = summary_sl, inherit.aes = FALSE, aes(x = xmin, xend = lower, y = dens, yend = dens)) +
  geom_segment(data = summary_sl, inherit.aes = FALSE, aes(x = upper, xend = xmax, y = dens, yend = dens)) +
  # IQR box
  geom_segment(data = summary_sl, inherit.aes = FALSE, 
               aes(x = lower, xend = upper, y = dens, yend = dens, color = name), 
               size = 6) +
  # Median line
  geom_segment(data = summary_sl, aes(x = median, xend = median, 
                                      y = dens - 0.0001, yend = dens + 0.0001), 
               color = "black",
               size = 1) +
  xlab("Step Length (m)") + 
  ylab("Probability Density") + theme_bw()
p1

# turning angles
# close turn-angle distribution 
close_ta <- update_vonmises(dist = best_fit$ta_, 
                            beta_cos_ta = best_fit$model$coefficients["cos_ta"] + 
                              8 * best_fit$model$coefficients["cos_ta:wind_turbines_nearest5000_start_construction_scaled"])

medium_ta <- update_vonmises(dist = best_fit$ta_, 
                             beta_cos_ta = best_fit$model$coefficients["cos_ta"] + 
                               4 * best_fit$model$coefficients["cos_ta:wind_turbines_nearest5000_start_construction_scaled"])

far_ta <- update_vonmises(dist = best_fit$ta_, 
                          beta_cos_ta = best_fit$model$coefficients["cos_ta"] + 
                            0 * best_fit$model$coefficients["cos_ta:wind_turbines_nearest5000_start_construction_scaled"])

# data.frame for plotting 
plot_ta <- data.frame(x = rep(NA, 200)) 

# x-axis is sequence of possible step lengths 
plot_ta$x <- seq(from = -pi, to = pi, length.out = 200) 

# y-axis is the probability density under the given gamma distribution 

# low 
plot_ta$`0 km` <- circular::dvonmises(x = plot_ta$x, kappa = close_ta$params$kappa, mu = 0)

plot_ta$`1 km` <- circular::dvonmises(x = plot_ta$x, kappa = medium_ta$params$kappa, mu = 0) 

plot_ta$`5 km` <- circular::dvonmises(x = plot_ta$x, kappa = far_ta$params$kappa, mu = 0) 

# Pivot from wide to long data 
plot_ta <- plot_ta |> 
  pivot_longer(cols = -x) 

# cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))

# Plot 
p2 <- ggplot(plot_ta, aes(x = x/pi*180, y = value, color = factor(name))) + 
  geom_line(size = 1) + 
  scale_color_manual(name = "Elevation", breaks = c("0 km", "1 km", "5 km"), values = c("firebrick", "gray50", "navyblue")) + 
  # scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi), 
  #                    labels = c(expression(-pi, -pi/2, 0, pi/2, pi))) + 
  scale_x_continuous(breaks = c(-180, -90, 0, 90, 180)) +
  xlab("Turn Angle (degrees)") + ylab("Probability Density") + theme_bw()
p2

# both plots
combined <- ggpubr::ggarrange(p1, p2, 
                              legend = "bottom", 
                              common.legend = TRUE)
combined

# save
summary_sl |> 
  write_csv(file = "output/ssf_results/ssf_summer_mov_parms_const.csv")

ggsave(filename = "output/ssf_results/ssf_summer_mov_const.png", combined, 
       width = 20, height = 10, units = "cm", dpi = 300)

#----
# operation

# step lengths - on the turbines
close_sl <- update_gamma(dist = best_fit$sl_, 
                         beta_sl = best_fit$model$coefficients["sl_"] + 
                           8 * best_fit$model$coefficients["sl_:wind_turbines_nearest5000_start_operation_scaled"], 
                         beta_log_sl = best_fit$model$coefficients["log_sl"] + 
                           8 * best_fit$model$coefficients["log_sl:wind_turbines_nearest5000_start_operation_scaled"])

medium_sl <- update_gamma(dist = best_fit$sl_, 
                          beta_sl = best_fit$model$coefficients["sl_"] + 
                            4 * best_fit$model$coefficients["sl_:wind_turbines_nearest5000_start_operation_scaled"], 
                          beta_log_sl = best_fit$model$coefficients["log_sl"] + 
                            4 * best_fit$model$coefficients["log_sl:wind_turbines_nearest5000_start_operation_scaled"])

far_sl <- update_gamma(dist = best_fit$sl_, 
                       beta_sl = best_fit$model$coefficients["sl_"] + 
                         0 * best_fit$model$coefficients["sl_:wind_turbines_nearest5000_start_operation_scaled"], 
                       beta_log_sl = best_fit$model$coefficients["log_sl"] + 
                         0 * best_fit$model$coefficients["log_sl:wind_turbines_nearest5000_start_operation_scaled"])

# data.frame for plotting 
plot_sl <- data.frame(x = rep(NA, 200)) 

# x-axis is sequence of possible step lengths 
plot_sl$x <- seq(from = 0, to = 5000, length.out = 200) 
# y-axis is the probability density under the given gamma distribution 
plot_sl$`0 km` <- dgamma(x = plot_sl$x, 
                         shape = close_sl$params$shape, 
                         scale = close_sl$params$scale)
qgamma(c(0.025, 0.25, 0.5, 0.75, 0.975), shape = close_sl$params$shape, scale = close_sl$params$scale)

plot_sl$`1 km` <- dgamma(x = plot_sl$x, 
                         shape = medium_sl$params$shape, 
                         scale = medium_sl$params$scale)
qgamma(c(0.025, 0.25, 0.5, 0.75, 0.975), shape = medium_sl$params$shape, scale = medium_sl$params$scale)

plot_sl$`5 km` <- dgamma(x = plot_sl$x, 
                         shape = far_sl$params$shape, 
                         scale = far_sl$params$scale)
qgamma(c(0.025, 0.25, 0.5, 0.75, 0.975), shape = far_sl$params$shape, scale = far_sl$params$scale)

summary_sl_op <- data.frame(
  name = c("0 km", "1 km", "5 km"),
  dens = c(0.002, 0.0025, 0.003),
  xmin = qgamma(0.025, shape = c(close_sl$params$shape, medium_sl$params$shape, far_sl$params$shape), 
                scale = c(close_sl$params$scale, medium_sl$params$scale, far_sl$params$scale)),
  lower = qgamma(0.25, shape = c(close_sl$params$shape, medium_sl$params$shape, far_sl$params$shape), 
                 scale = c(close_sl$params$scale, medium_sl$params$scale, far_sl$params$scale)),
  median = qgamma(0.5, shape = c(close_sl$params$shape, medium_sl$params$shape, far_sl$params$shape), 
                  scale = c(close_sl$params$scale, medium_sl$params$scale, far_sl$params$scale)),
  upper = qgamma(0.75, shape = c(close_sl$params$shape, medium_sl$params$shape, far_sl$params$shape), 
                 scale = c(close_sl$params$scale, medium_sl$params$scale, far_sl$params$scale)),
  xmax = qgamma(0.925, shape = c(close_sl$params$shape, medium_sl$params$shape, far_sl$params$shape), 
                scale = c(close_sl$params$scale, medium_sl$params$scale, far_sl$params$scale))
)

# Pivot from wide to long data 
plot_sl <- plot_sl |> 
  pivot_longer(cols = -x) 

# Plot 
p1_op <- ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) + 
  geom_line(size = 1) + 
  scale_color_manual(name = "Distance to WF", 
                     breaks = c("0 km", "1 km", "5 km"),
                     # breaks = c("On the WF", "1 km from the WF", "5 km from the WF"), 
                     values = c("firebrick", "gray50", "navyblue")) + 
  # Whiskers
  geom_segment(data = summary_sl_op, inherit.aes = FALSE, aes(x = xmin, xend = lower, y = dens, yend = dens)) +
  geom_segment(data = summary_sl_op, inherit.aes = FALSE, aes(x = upper, xend = xmax, y = dens, yend = dens)) +
  # IQR box
  geom_segment(data = summary_sl_op, inherit.aes = FALSE, 
               aes(x = lower, xend = upper, y = dens, yend = dens, color = name), 
               size = 6) +
  # Median line
  geom_segment(data = summary_sl_op, aes(x = median, xend = median, 
                                         y = dens - 0.0001, yend = dens + 0.0001), 
               color = "black",
               size = 1) +
  xlab("Step Length (m)") + 
  ylab("Probability Density") + theme_bw()
p1_op

# turning angles
# close turn-angle distribution 
close_ta <- update_vonmises(dist = best_fit$ta_, 
                            beta_cos_ta = best_fit$model$coefficients["cos_ta"] + 
                              8 * best_fit$model$coefficients["cos_ta:wind_turbines_nearest5000_start_operation_scaled"])

medium_ta <- update_vonmises(dist = best_fit$ta_, 
                             beta_cos_ta = best_fit$model$coefficients["cos_ta"] + 
                               1 * best_fit$model$coefficients["cos_ta:wind_turbines_nearest5000_start_operation_scaled"])

far_ta <- update_vonmises(dist = best_fit$ta_, 
                          beta_cos_ta = best_fit$model$coefficients["cos_ta"] + 
                            0 * best_fit$model$coefficients["cos_ta:wind_turbines_nearest5000_start_operation_scaled"])

# data.frame for plotting 
plot_ta <- data.frame(x = rep(NA, 200)) 

# x-axis is sequence of possible step lengths 
plot_ta$x <- seq(from = -pi, to = pi, length.out = 200) 

# y-axis is the probability density under the given gamma distribution 

# low 
plot_ta$`0 km` <- circular::dvonmises(x = plot_ta$x, kappa = close_ta$params$kappa, mu = 0)

plot_ta$`1 km` <- circular::dvonmises(x = plot_ta$x, kappa = medium_ta$params$kappa, mu = 0) 

plot_ta$`5 km` <- circular::dvonmises(x = plot_ta$x, kappa = far_ta$params$kappa, mu = 0) 

# Pivot from wide to long data 
plot_ta <- plot_ta |> 
  pivot_longer(cols = -x) 

# cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))

# Plot 
p2_op <- ggplot(plot_ta, aes(x = x/pi*180, y = value, color = factor(name))) + 
  geom_line(size = 1) + 
  scale_color_manual(name = "Elevation", breaks = c("0 km", "1 km", "5 km"), values = c("firebrick", "gray50", "navyblue")) + 
  # scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi), 
  #                    labels = c(expression(-pi, -pi/2, 0, pi/2, pi))) + 
  scale_x_continuous(breaks = c(-180, -90, 0, 90, 180)) +
  xlab("Turn Angle (degrees)") + ylab("Probability Density") + theme_bw()
p2_op

# both plots
combined_op <- ggpubr::ggarrange(p1_op, p2_op, 
                                 legend = "bottom", 
                                 common.legend = TRUE)
combined_op

# save
summary_sl_op |> 
  write_csv(file = "output/ssf_results/ssf_summer_mov_parms_operation.csv")

ggsave(filename = "output/ssf_results/ssf_summer_mov_operation.png", combined_op, 
       width = 20, height = 10, units = "cm", dpi = 300)

# combine all
combined_all <- ggpubr::ggarrange(p1, p2, p1_op, p2_op, 
                                  ncol = 2, nrow = 2,
                                  legend = "right", 
                                  common.legend = TRUE)
combined_all

ggsave(filename = "output/ssf_results/ssf_summer_mov_all.png", combined_all, 
       width = 25, height = 25, units = "cm", dpi = 300)

