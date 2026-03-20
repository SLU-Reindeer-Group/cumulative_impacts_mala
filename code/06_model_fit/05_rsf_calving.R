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

# grouped land cover
data_ready_calving_rsf_2h <- data_ready_calving_rsf_2h |> 
  dplyr::mutate(landcover_cc_grouped = forcats::fct_collapse(landcover_cc, 
                                                     "deciduous forests" = c("deciduous forests", "forested wetlands"),
                                                     "open lands" = c("open lands", "clear cuts")),
                landcover_cc_grouped = relevel(landcover_cc_grouped, ref = "deciduous forests"))
levels(data_ready_calving_rsf_2h$landcover_cc_grouped)

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
  wf_name + landcover_cc_grouped*phase*log(wind_turbines_dist_comb+1)# + 
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

# compare viewshed and landcover_cc_grouped - viewshed as it is does not make sense
# formula_calving3 <- update(base_formula2, ~ . + tpi_cat_s150m - 
#                              landcover_cc_grouped*phase*log(wind_dist_jsy+1) +
#                              viewshed_lc*phase*log(wind_dist_jsy+1))
# base_formula3 <- update(base_formula2, ~ . + tpi_cat_s150m -
#                              landcover_cc_grouped*phase*log(wind_turbines_dist_comb+1) +
#                              viewshed_lc*phase*log(wind_turbines_dist_comb+1))
# base_formula3.2 <- update(base_formula2, ~ . + tpi_cat_s250m -
#                              landcover_cc_grouped*phase*log(wind_turbines_dist_comb+1) +
#                              viewshed_lc*phase*log(wind_turbines_dist_comb+1))
# base_formula3.3 <- update(base_formula2, ~ . + tpi_cat_s510m -
#                              landcover_cc_grouped*phase*log(wind_turbines_dist_comb+1) +
#                              viewshed_lc*phase*log(wind_turbines_dist_comb+1))
# 
# fit3 <- glm(base_formula3,
#             data = data_ready_calving_rsf_2h, #weights = w_obs,
#             family = binomial(link = "logit"))
# fit3.2 <- glm(base_formula3.2,
#             data = data_ready_calving_rsf_2h, #weights = w_obs,
#             family = binomial(link = "logit"))
# fit3.3 <- glm(base_formula3.3,
#             data = data_ready_calving_rsf_2h, #weights = w_obs,
#             family = binomial(link = "logit"))
# 
# car::vif(fit3)
# summary(fit3)
# 
# # compare
# (AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, 
#                                 fit3, fit3.2, fit3.3, weights = T, base = T))

# best model
best_fit <- fit2
summary(best_fit)

#-------
# GLM, no threshold regression, using ZoI for wind power and mining

# base formula
base_formula <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(sos) + 
  log(power_lines_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + 
  log(trails_dist+1) + 
  log(house_dist+1) + scale(mining_Krist_nearestXXX) +
  wf_name + landcover_cc_grouped*phase*scale(wind_turbines_ZZZ)

# values for ZOI radii and metrics, TPI scales, and type of land cover variable
scals <- c(100, 250, 500, 1000, 2500, 5000, 10000)
zois <- c("nearest", "cumulative")
wind_scals <- paste0(rep(zois, each = length(scals)), rep(scals, length(zois)))
tpi_scals <- c("tpi_cat_s150m", "tpi_cat_s250m", "tpi_cat_s510m")
lc_var <- c("landcover_cc_grouped")#, "viewshed_lc")

# n models = 7*2*14*3*1 = 588

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
                   "landcover_cc_grouped*phase*scale(wind_turbines_ZZZ) + ", 
                   tt, " + ",
                   paste0("scale(mining_Krist_nearest", i, ") + "),
                   paste0(lc, "*phase*scale(wind_turbines_", j, ")"))
    form <- update(base_formula, updt)
    
    print(paste0("mines = ", i, "; wind = ", j, "; tpi = ", tt, 
                 "; lc = ", lc, " up and running..."))
    fit1 <- glm(form,
                data = data_ready_calving_rsf_2h, #weights = w_obs,
                family = binomial(link = "logit"))
    fits[counter,1] <- i
    fits[counter,2] <- j
    fits[counter,3] <- tt
    fits[counter,4] <- lc
    fits[counter,5] <- AIC(fit1)
    fits[counter,6] <- paste(as.character(form)[c(2,1,3)], collapse = " ")
    
    counter <- counter+1
  }}}
}

# re run this below

# compare models
(tab_models <- fits |>
    dplyr::mutate(mining = as.character(mining)) |>
    dplyr::bind_rows(
      tibble::tibble(mining = "log", wind = "log", 
                     tpi = "tpi_cat_150m",
                     lc = "landcover_cc_grouped",
                     aic = AIC(best_fit),
                     definition = paste(as.character(best_fit$formula)[c(2,1,3)], collapse = " "))) |>
    dplyr::mutate(dAIC = abs(aic - min(aic))) |>
    dplyr::arrange(dAIC))

# save(tab_models, file = "output/rsf_calving_multi_zoi_table_landcover_grouped.rda")
load("output/rsf_calving_multi_zoi_table_landcover_grouped.rda")
tab_models

tab_models |>
  readr::write_csv(file = "output/rsf_results/rsf_calving_AIC_table_complete.csv")

# save(tab_models, file = "output/rsf_calving_multi_zoi_table.rda")

#---------------
# Re-run best model
data_ready_calving_rsf_2h$tpi_cat_s150m <- as.factor(data_ready_calving_rsf_2h$tpi_cat_s150m)

# use grouped categories for land cover
# data_ready_calving_rsf_2h$landcover_cc_grouped <- forcats::fct_collapse(data_ready_calving_rsf_2h$landcover_cc, 
#                                                                         "deciduous forests" = c("deciduous forests", "forested wetlands"),
#                                                                         "open lands" = c("open lands", "clear cuts"))
# data_ready_calving_rsf_2h$landcover_cc_grouped <- relevel(data_ready_calving_rsf_2h$landcover_cc_grouped, ref = "deciduous forests")

form <- tab_models$definition[2]

# update formula with landcover_cc_grouped
# form <- gsub("landcover_cc", "landcover_cc_grouped", form) 

best_fit <- glm(form, 
                data = data_ready_calving_rsf_2h, #weights = w_obs,
                family = binomial(link = "logit"))

# trying with the best model with landcover
# form <- tab_models |> 
#   dplyr::filter(lc == "landcover_cc") |> 
#   (\(x) x$definition[1])()
# form <- gsub(pattern = "log(wind_turbines_dist_comb + 1)", 
#              replacement = "wind_turbines_nearest5000", form, 
#              fixed = TRUE)
# best_fit <- glm(form,
#                 data = data_ready_calving_rsf_2h, #weights = w_obs,
#                 family = binomial(link = "logit"))
best_fit$formula
summary(best_fit)
car::vif(best_fit)

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
# ggsave("rsf_calving_coefs_near_landcover.png", plot = m1_plot, path = "output/rsf_results/", 
#        width = 20, height = 20, units = "cm", dpi = 300)
ggsave("rsf_calving_coefs_near_landcover_grouped.png", plot = m1_plot, path = "output/rsf_results/", 
       width = 20, height = 20, units = "cm", dpi = 300)

model_call <- sapply(attr(AICvals_phase, "row.names"), function(x) paste(as.character(formula(eval(as.symbol(x))))[c(2,1,3)], collapse = " "))
AICvals_phase |> 
  dplyr::as_tibble(rownames = "model") |>
  dplyr::mutate(definition = unname(model_call)) |>
  readr::write_csv(file = "output/rsf_results/rsf_calving_AIC_table_log_landcover_grouped.csv")

summary(best_fit)
coefs <- broom::tidy(best_fit) |> 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0)),
                p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
coefs |> 
  mutate_if(is.numeric, round, digits = 3) |> 
  readr::write_csv(file = "output/rsf_results//rsf_calving_coef_table_landcover_grouped.csv")

# Model validation
# AUC
library(pROC)
roc_best_fit <- pROC::roc(data_ready_calving_rsf_2h$case_, fitted(best_fit))
plot(roc_best_fit)
pROC::auc(roc_best_fit) # AUC = 0.71

# CV
# library(boot)
# cv.glm()

#-------------------------------
# predictions

# cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))
cols <- c("#009E73", "#0072B2", "#D55E00")

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
#   plot(limits = c(0,1), show_title = F, show_x_title = F, col = cols) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to wind turbines (km)",
#        y = "Predicted probability of use",
#        colour = "Phase") +
#   xlim(0, 10)
# ggsave(filename = "results/calving_wind_phase_general.png", plot = g_wind_general, 
#        width = 15, height = 12, units = "cm", dpi = 300)

# wind dist, phase, and land cover, for ytteberg
# pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_turbines_dist_comb", "phase", "viewshed_lc"),
                               # typical = "median", condition = c(wf_name = "ytteberg"))
pr_wind_phase <- ggpredict(best_fit, terms = c("wind_turbines_nearest5000", "phase", "landcover_cc_grouped"),
                               typical = "median", condition = c(wf_name = "ytteberg"))
#, condition = c(landcover_cc = "coniferous forests"))
zoi_limit <- 0.05; radius <- 5000
lambda <- log(1/zoi_limit) / radius
pr_wind_phase$x <- -log(pr_wind_phase$x)/lambda/1000
pr_wind_phase_df <- pr_wind_phase[sample.int(nrow(pr_wind_phase), size = 0.1*nrow(pr_wind_phase)),]

g_wind <- pr_wind_phase_df |>
  plot(show_title = FALSE, colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to wind turbines (m)",
       y = "Predicted probability of use",
       colour = "Phase") +
  #ylim(0, 1) +
  xlim(0, 10)
g_wind

ggsave(filename = "output/rsf_results/calving_wind_phase_landcc_grouped_ytte.png", plot = g_wind,
       width = 20, height = 20, units = "cm", dpi = 300)

# wind dist, phase, and land cover, for places closer to each of the WFs
wfs <- levels(data_ready_calving_rsf_2h$wf_name)
for(i in wfs) {
  
  print(i)
  pr_wind_phase <- ggpredict(best_fit, terms = c("wind_turbines_nearest5000", "phase", "landcover_cc_grouped"),
                             typical = "median", condition = c(wf_name = i))
  #, condition = c(landcover_cc = "coniferous forests"))
  zoi_limit <- 0.05; radius <- 5000
  lambda <- log(1/zoi_limit) / radius
  pr_wind_phase$x <- -log(pr_wind_phase$x)/lambda/1000
  pr_wind_phase_df <- pr_wind_phase[sample.int(nrow(pr_wind_phase), size = 0.1*nrow(pr_wind_phase)),]
  
  g_wind <- pr_wind_phase_df |>
    plot(show_title = FALSE, colors = cols) +
    geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
    geom_vline(xintercept = 5, linetype = 2, color = "grey") +
    labs(x = "Distance to wind turbines (m)",
         y = "Relative probability of use",
         colour = "Phase") +
    #ylim(0, 1) +
    xlim(0, 10)
  g_wind
  
  ggsave(filename = paste0("output/rsf_results/rsf_calving_wind_phase_landcc_grouped", i, ".png"), plot = g_wind, 
         width = 20, height = 20, units = "cm", dpi = 300)
  
  
  # pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_turbines_dist_comb", "phase", "viewshed_lc"),
  #                                typical = "median", condition = c(wf_name = i))
  # #, condition = c(landcover_cc = "coniferous forests"))
  # pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
  # g_log_wind <- pr_log_wind_phase |> 
  #   plot(show_title = F, limits = c(0,1), colors = cols) +
  #   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  #   labs(x = "Distance to wind turbines (km)",
  #        y = "Predicted probability of use",
  #        colour = "Phase") +
  #   xlim(0, 10)
  # ggsave(filename = paste0("output/rsf_results/rsf_calving_wind_phase_view_", i, ".png"), plot = g_log_wind, 
  #        width = 20, height = 8, units = "cm", dpi = 300)
}
# pr_wind_phase <- ggpredict(fit9_nolog, terms = c("wind_dist", "phase", "landcover"))
# g_wind <- pr_wind_phase |> 
#   plot(show_title = F, show_x_title = F) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to wind turbines (km)",
#        y = "Predicted probability of use") +
#   xlim(0, 10)
# ggsave(filename = "results/calving_wind_phase_lc_no_log.png", plot = g_wind, 
#        width = 20, height = 20, units = "cm", dpi = 300)

# mining
pr_mining <- ggpredict(best_fit, terms = c("mining_Krist_nearest10000"))
zoi_limit <- 0.05; radius <- 10000
lambda <- log(1/zoi_limit) / radius
pr_mining$x <- -log(pr_mining$x)/lambda/1000
g_mining <- pr_mining |> 
  plot(show_title = F, show_x_title = F,
       limits = c(0,NA), 
       # colors = cols
       ) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to mining (km)",
       y = "Relative probability of use",
       color = "Phase") +
  xlim(0, 20)
g_mining
ggsave(filename = "rsf_calving_mining.png", plot = g_mining, 
       path = "output/rsf_results/",
       width = 15, height = 12, units = "cm", dpi = 300)

# roads
pr_public_road <- ggpredict(best_fit, terms = c("public_road_dist"))
pr_public_road$x <- pr_public_road$x/1000
g_road <- pr_public_road |>
  plot(show_title = F, show_x_title = F,
       limits = c(0,NA), 
       # colors = cols
       ) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to large roads (km)",
       y = "Relative probability of use",
       colour = "Phase") +
  xlim(0, 3)
g_road
ggsave(filename = "rsf_calving_roads.png", plot = g_road,
       path = "output/rsf_results/",
       width = 15, height = 12, units = "cm", dpi = 300)

# error with NAs
# pr_priv_road <- ggpredict(best_fit, terms = c("priv_road_dist"), condition = "zero")
# pr_priv_road$x <- pr_priv_road$x/1000
# # g_prov_road <- pr_priv_road |> 
#   plot(show_title = F, show_x_title = F,
#        limits = c(0,NA),
       # colors = cols
       # ) +
  # geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to small roads (km)",
#        y = "Relative probability of use",
#        colour = "Phase") +
#   xlim(0, 1)
# g_prov_road
# ggsave(filename = "rsf_calving_private_roads.png", plot = g_prov_road, 
#        path = "output/rsf_results/",
#        width = 15, height = 12, units = "cm", dpi = 300)

# library(patchwork)
# avg_predictions(best_fit, by = "priv_road_dist")
# pr_priv_road <- plot_predictions(best_fit, by = "priv_road_dist", rug = T, draw = F)
# pr_priv_road

#tab <- do.call(cbind.data.frame, attr(pr_priv_road, "constant.values"))
#df <- data.frame(priv_road_dist = pr_priv_road$x*1000, tab)
#df <- data.frame(priv_road_dist = 1, tab)
#pred <- predict(best_fit, df, type = "link")
#exp(log(df$priv_road_dist+1)*-0.037344)

# power lines
pr_pl <- ggpredict(best_fit, terms = c("power_lines_dist"))
pr_pl$x <- pr_pl$x/1000
g_pl <- pr_pl |> 
  plot(show_title = F, show_x_title = F,
       limits = c(0,NA), 
       # colors = cols
       ) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to power lines (km)",
       y = "Relative probability of use") +
  xlim(0, 10)
g_pl
ggsave(filename = "rsf_calving_pl.png", plot = g_pl, 
       path = "output/rsf_results/",
       width = 15, height = 12, units = "cm", dpi = 300)

# trails
pr_trail <- ggpredict(best_fit, terms = c("trails_dist"))
pr_trail$x <- pr_trail$x/1000
g_trail <- pr_trail |>
  plot(show_title = F, show_x_title = F,
       limits = c(0,NA), 
       # colors = cols
       ) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to trails (km)",
       y = "Relative probability of use") +
  xlim(0, 5)
g_trail
ggsave(filename = "rsf_calving_trails.png", plot = g_trail,
       path = "output/rsf_results/",
       width = 15, height = 12, units = "cm", dpi = 300)

# houses
pr_house <- ggpredict(best_fit, terms = c("house_dist"))
pr_house$x <- pr_house$x/1000
g_house <- pr_house |> 
  plot(show_title = F, show_x_title = F,
       limits = c(0,NA), 
       # colors = cols
       ) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to houses (km)",
       y = "Relative probability of use") +
  xlim(0, 5)
g_house
ggsave(filename = "rsf_calving_houses.png", plot = g_house, 
       path = "output/rsf_results/",
       width = 15, height = 12, units = "cm", dpi = 300)

# elevation
pr_elevation <- ggpredict(best_fit, terms = c("dem"))
g_elevation <- pr_elevation |> 
  plot(show_title = F, show_x_title = F,
       limits = c(0,NA),
       # colors = cols
       ) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Elevation (m)",
       y = "Relative probability of use",
       color = "Phase")
g_elevation
ggsave(filename = "rsf_calving_elevation.png", plot = g_elevation, 
       path = "output/rsf_results/",
       width = 15, height = 12, units = "cm", dpi = 300)

# tpi
# pr_tpi <- ggpredict(best_fit, terms = c("tpi_cat_s150m"))
# g_tpi <- pr_tpi |> 
#   plot(show_title = F, show_x_title = F,
#        limits = c(0,NA), 
#        # colors = cols
#        ) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Terrain position index category (150m)",
#        y = "Relative probability of use",
#        color = "Phase") 
# g_tpi
# ggsave(filename = "rsf_calving_tpi.png", plot = g_tpi, 
#        path = "output/rsf_results/",
#        width = 15, height = 12, units = "cm", dpi = 300)

#-------
# spatial prediction
install.load::install_load("raster", "rasterVis", "terra", "arrow")

# vindval data
load(file = "data/01_GPS/05_annotated_data/data_annotated_calving_ready_2h_rsf.RData")

# grouped land cover
data_ready_calving_rsf_2h <- data_ready_calving_rsf_2h |> 
  dplyr::mutate(landcover_cc_grouped = forcats::fct_collapse(landcover_cc, 
                                                             "deciduous forests" = c("deciduous forests", "forested wetlands"),
                                                             "open lands" = c("open lands", "clear cuts")),
                landcover_cc_grouped = relevel(landcover_cc_grouped, ref = "deciduous forests"))
levels(data_ready_calving_rsf_2h$landcover_cc_grouped)

# model comparison
load("output/rsf_calving_multi_zoi_table_landcover_grouped.rda")

# compare models
tab_models

#---------------
# Re-run best model
form <- tab_models$definition[2]
best_fit <- glm(form, 
                data = data_ready_calving_rsf_2h, #weights = w_obs,
                family = binomial(link = "logit"))

#-----------
# Load maps
maps_df <- arrow::read_parquet("maps/for_prediction/predictors.parquet")
str(maps_df)

maps_df <- maps_df |> 
  dplyr::mutate(landcover_cc_grouped = forcats::fct_collapse(landcover_cc, 
                                                             "deciduous forests" = c("deciduous forests", "forested wetlands"),
                                                             "open lands" = c("open lands", "clear cuts")),
                landcover_cc_grouped = relevel(landcover_cc_grouped, ref = "deciduous forests"))
maps_df$landcover_cc_grouped

template_map <- terra::vect("maps/availability_mala_calving_summer.gpkg")

# set sos and eos to match phase
maps_before_df <- maps_df |> 
  dplyr::rename(eos=eos_before, sos=sos_before)
maps_before_df$phase <- ifelse(maps_before_df$phase == "Operation", "Before construction", maps_before_df$phase) |> 
  factor(levels = levels(maps_df$phase))
maps_before_df$phase
# unique(maps_before_df$phase)
# predict
pred_calving_before_construction <- predict(best_fit, maps_before_df, type = "response")
# create raster
maps_before_df$pred_calv_before <- pred_calving_before_construction
str(maps_before_df)
# fill raster with predicted values
map_pred_calving_before_construction <- maps_before_df |> 
  dplyr::select(x, y, pred_calv_before) |> 
  terra::rast(type = "xyz", crs = terra::crs(template_map)) |> 
  terra::mask(mask = template_map)

names(map_pred_calving_before_construction) <- "calving_before_construction"
terra::plot(map_pred_calving_before_construction)
terra::writeRaster(map_pred_calving_before_construction, filename = "output/rsf_spatial_predictions/rsf_map_prediction_calving_before_construction_lc_grouped.tif", 
                   gdal = "COMPRESS=DEFLATE", overwrite = TRUE)

rm(pred_calving_before_construction)

# calving during/construction --------------------------------

# set sos and eos to match phase
maps_during_df <- maps_df |> 
  dplyr::rename(eos=eos_during, sos=sos_during) # remove differences due to nvdi
  # dplyr::rename(eos=eos_during, sos=sos_during)
maps_during_df$phase <- ifelse(maps_during_df$phase == "Operation", "Construction", maps_during_df$phase) |> 
  factor(levels = levels(maps_df$phase))
unique(maps_during_df$phase)
# predict
pred_calving_construction <- predict(best_fit, maps_during_df, type = "response")
# create raster
maps_during_df$pred_calv_during <- pred_calving_construction
str(maps_during_df)
# fill raster with predicted values
map_pred_calving_construction <- maps_during_df |> 
  dplyr::select(x, y, pred_calv_during) |> 
  terra::rast(type = "xyz", crs = terra::crs(template_map)) |> 
  terra::mask(mask = template_map)

names(map_pred_calving_construction) <- "calving_construction"
terra::plot(map_pred_calving_construction)
terra::writeRaster(map_pred_calving_construction, filename = "output/rsf_spatial_predictions/rsf_map_prediction_calving_construction_lc_grouped.tif", 
                   gdal = "COMPRESS=DEFLATE", overwrite = TRUE)

rm(pred_calving_construction)
rm(maps_during_df)
gc()

# calving operation/after --------------------------------

# set sos and eos to match phase
maps_after_df <- maps_df |> 
  dplyr::rename(eos=eos_after, sos=sos_after) # remove differences due to nvdi
  # dplyr::rename(eos=eos_after, sos=sos_after)
unique(maps_after_df$phase)
# predict
pred_calving_operation <- terra::predict(best_fit, maps_after_df, type = "response")
# create raster
maps_after_df$pred_calv_after <- pred_calving_operation
str(maps_after_df)
# fill raster with predicted values
map_pred_calving_operation <- maps_after_df |> 
  dplyr::select(x, y, pred_calv_after) |> 
  terra::rast(type = "xyz", crs = terra::crs(template_map)) |> 
  terra::mask(mask = template_map)

names(map_pred_calving_operation) <- "calving_operation"
terra::plot(map_pred_calving_operation)
terra::writeRaster(map_pred_calving_operation, filename = "output/rsf_spatial_predictions/rsf_map_prediction_calving_operation_lc_grouped.tif", 
                   gdal = "COMPRESS=DEFLATE", overwrite = TRUE)

rm(pred_calving_operation)
rm(maps_after_df)
gc()

#----
# plot maps

install.load::install_load("terra", "arrow")
library(tmap)

# read maps
f_m <- list.files("output/rsf_spatial_predictions/", pattern = "calving", full.names = TRUE) |> 
  grep(pattern = ".tif$", value = TRUE) |> 
  grep(pattern = "grouped", value = TRUE) |> 
  grep(pattern = "difference", value = TRUE, invert = TRUE)

maps_calving <- terra::rast(f_m)

# plot
# png("results/prediction_map_rsf_phase.png", width = 20, height = 20, units = "cm", res = 300)
# plot(maps_calving)
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

# names_phase <- c("Before construction", "Construction", "Operation")
# png("results/prediction_map_rsf_phase_calving.png", width = 30, height = 11, units = "cm", res = 300)
# plt <- maps_calving |> 
#   rasterVis::levelplot(par.settings = rasterTheme(region = rev(terrain.colors(n = 100))),
#                        names.attr = names_phase, layout = c(3,1))
# plt + latticeExtra::layer(sp.points(as(turbines[1], "Spatial"), 
#                                     col = "black", cex = 0.4, pch = 4))
# dev.off()

# library(tmaptools)
# tmaptools::palette_explorer()

# tmap_options(max.raster = c(plot = 5000, view = 2000))

map_before <- maps_calving[[1]]
map_before$calving_before_construction@pntr$range_max
p_maps_calving_before <- tmap::tm_shape(map_before) +
  tmap::tm_raster(palette = "Greens", style = "cont", title = "") +#breaks = seq(0, 0.6, by = 0.05)) +
  tmap::tm_layout(title = "Baseline habitat suitability in calving\n(before wind turbines)", legend.outside = TRUE)
p_maps_calving_before

# difference calving ####
after_minus_before <- terra::diff(maps_calving[[c(1,3)]])
names(after_minus_before) <- "Before construction -> Operation"
# plot(after_minus_before)
during_minus_before <- terra::diff(maps_calving[[c(1,2)]])
names(during_minus_before) <- "Before construction -> Construction"

# absolute difference RSF
maps_calving_diff <- c(during_minus_before, after_minus_before)
tmap::tm_shape(maps_calving_diff) +
  tmap::tm_raster(palette = "PRGn", style = "cont") +
  tmap::tm_layout()

maps_calving_prop <- 100*maps_calving_diff/maps_calving[[1]]
# maps_calving_prop <- terra::ifel(maps_calving_prop > )
p_maps_calving_diff <- tmap::tm_shape(maps_calving_prop) +
  tmap::tm_raster(palette = "PRGn", #style = "quantile")
                  breaks = c(-75, -50, -30, -10, 0, 10, 30, 50, 75),
                  title = "Change in suitability (%)") +
  tmap::tm_facets(ncol = 2)
p_maps_calving_diff

p_maps_calving <- tmap::tmap_arrange(p_maps_calving_before, p_maps_calving_diff,
                                     ncol = 1, nrow = 2)
p_maps_calving

# Add polygon mining and polygon 5km + crosses for turbines in maps
# Add scale and north
tmap::tmap_save(p_maps_calving, filename = "output/rsf_results/rsf_maps_calving_lc_grouped.png", 
                width = 20, height = 20, units = "cm", dpi = 300)

# write maps difference
maps_calving_diff |> 
  terra::writeRaster(filename = "output/rsf_spatial_predictions/rsf_map_difference_rsf_calving_lc_grouped.tif",
                     gdal = "COMPRESS=DEFLATE", overwrite = TRUE)

maps_calving_prop |> 
  terra::writeRaster(filename = "output/rsf_spatial_predictions/rsf_map_difference_prop_calving_lc_grouped.tif",
                     gdal = "COMPRESS=DEFLATE", overwrite = TRUE)
