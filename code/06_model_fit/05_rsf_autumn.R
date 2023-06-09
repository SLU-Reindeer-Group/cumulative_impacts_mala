#' ---
#' title: 'Habitat selection analysis of Swedish reindeer during autumn at regional scale: RSF at Mala sameby'
#' author: Bernardo Niebuhr
#' abstract: ""
#' ---

# --------------- label=load_packages, warning=FALSE, message=FALSE, echo=FALSE

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

library(devtools)
#devtools::install_github("phuais/multifitR")

#-----------
#' # autumn analysis
#' 
#' ## Load annotated and clean movement data

# vindval data
load(file = "data/01_GPS/05_annotated_data/data_annotated_autumn_ready_2h_rsf.RData")
data_ready_autumn_rsf_2h

#-----------------------
# Fit

# weights - not used
data_ready_autumn_rsf_2h$w_obs <- ifelse(data_ready_autumn_rsf_2h$case_, 1, 5000)

summary(data_ready_autumn_rsf_2h$wind_turbines_dist_comb)
#hist(data_ready_autumn_rsf_2h$wind_turbines_dist_comb+1, breaks = 100, xlim = c(0, 10000))

#-------
# GLM, no threshold regression

base_formula <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(eos) + 
  log(power_lines_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + 
  log(trails_dist+1) + 
  log(house_dist+1) + log(mining_Krist_dist+1) +
  wf_name + landcover_cc*phase*log(wind_turbines_dist_comb+1)

fit1 <- glm(update(base_formula, ~ . + tpi_cat_s150m), 
            data = data_ready_autumn_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))

fit1$formula
summary(fit1)
car::vif(fit1)

# backward deletion - ignore that for now
base_formula2 <- base_formula #update(base_formula, ~ . - cos_aspect)
fit2 <- glm(update(base_formula2, ~ . + tpi_cat_s150m),
            data = data_ready_autumn_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))

fit2$formula
summary(fit2)
car::vif(fit2)

# compare TPI
fit2.2 <- glm(update(base_formula2, ~ . + tpi_cat_s250m), 
            data = data_ready_autumn_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))

fit2.3 <- glm(update(base_formula2, ~ . + tpi_cat_s510m), 
              data = data_ready_autumn_rsf_2h, #weights = w_obs,
              family = binomial(link = "logit"))

# compare
(AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, weights = T, base = T))

# compare viewshed and landcover_cc
# formula_autumn3 <- update(base_formula2, ~ . + tpi_cat_s150m - 
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
            data = data_ready_autumn_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))
fit3.2 <- glm(base_formula3.2,
            data = data_ready_autumn_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))
fit3.3 <- glm(base_formula3.3,
            data = data_ready_autumn_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))

car::vif(fit3)
summary(fit3)

# compare
(AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, 
                                fit3, fit3.2, fit3.3, weights = T, base = T))

# best model
best_fit <- fit2.3
summary(best_fit)

#-------
# GLM, no threshold regression, using ZoI for wind power and mining

# base formula
base_formula <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(eos) + 
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
#models <- list()
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
    fit1 <- glm(form, 
                data = data_ready_autumn_rsf_2h, #weights = w_obs,
                family = binomial(link = "logit"))
    fits[counter,1] <- i
    fits[counter,2] <- j
    fits[counter,3] <- tt
    fits[counter,4] <- lc
    fits[counter,5] <- AIC(fit1)
    fits[counter,6] <- paste(as.character(fit1$formula)[c(2,1,3)], collapse = " ")
    
    counter <- counter+1
  }}}
}

#save(fits, file = "output/autumn_multi_zoi_table.rda")
load("output/autumn_multi_zoi_table.rda")
fits

# compare models
(tab_models <- fits |>
  dplyr::mutate(mining = as.character(mining)) |>
  dplyr::bind_rows(
    tibble::tibble(mining = "log", wind = "log", 
                   tpi = "tpi_cat_s510m",
                   lc = "landcover_cc",
                   aic = AIC(best_fit),
                   definition = paste(as.character(best_fit$formula)[c(2,1,3)], collapse = " "))) |>
  dplyr::mutate(dAIC = abs(aic - min(aic))) |>
  dplyr::relocate(dAIC, .after = aic) |> 
  dplyr::arrange(dAIC))

tab_models |>
  readr::write_csv(file = "output/01_traditional_fit/rsf_autumn_AIC_table_complete.csv")

#save(tab_models, file = "output/autumn_multi_zoi_table.rda")

#---------------
# Re-run best model for log-distance
load("output/autumn_multi_zoi_table.rda")
tab_models

form <- tab_models$definition[3]
best_fit <- glm(form, 
                data = data_ready_autumn_rsf_2h, #weights = w_obs,
                family = binomial(link = "logit"))

#---------------
# More info, summary, and predictions on the best log model

broom::glance(best_fit)
broom::tidy(best_fit) |>
  print(n = 100)

m1_plot <- broom::tidy(best_fit) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0))) %>% 
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
ggsave("rsf_autumn_coefs_log.png", plot = m1_plot, path = "output/01_traditional_fit/", 
       width = 20, height = 20, units = "cm", dpi = 300)

model_call <- sapply(attr(AICvals_phase, "row.names"), function(x) paste(as.character(formula(eval(as.symbol(x))))[c(2,1,3)], collapse = " "))
AICvals_phase %>% 
  dplyr::as_tibble(rownames = "model") %>%
  dplyr::mutate(definition = unname(model_call)) %>%
  dplyr::rename(aic = AIC) |>
  dplyr::bind_rows(tab_models) |>
  dplyr::arrange(dAIC) |>
  readr::write_csv(file = "output/01_traditional_fit/rsf_autumn_AIC_table_log.csv")

summary(best_fit)
coefs <- broom::tidy(best_fit) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0)),
                p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
coefs %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  readr::write_csv(file = "output/01_traditional_fit/rsf_autumn_coef_table_log.csv")

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
# g_wind_general <- pr_wind_general %>% 
#   plot(limits = c(0,1), show.title = F, show.x.title = F, col = cols) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to wind turbines (km)",
#        y = "Predicted probability of use",
#        colour = "Phase") +
#   xlim(0, 10)
# ggsave(filename = "results/autumn_wind_phase_general.png", plot = g_wind_general, 
#        width = 15, height = 12, units = "cm", dpi = 300)

# wind dist, phase, and land cover, for ytteberg
pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_turbines_dist_comb", "phase", "landcover_cc"),
                               typical = "median", condition = c(wf_name = "ytteberg"))
#, condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
g_log_wind <- pr_log_wind_phase %>% 
  plot(show.title = F, col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to wind turbines (m)",
       y = "Predicted probability of use",
       colour = "Phase") +
  #ylim(0, 1) +
  xlim(0, 10)
ggsave(filename = "output/01_traditional_fit/autumn_wind_phase_view_ytte.png", plot = g_log_wind, 
       width = 20, height = 20, units = "cm", dpi = 300)

# wind dist, phase, and land cover, for places closer to each of the WFs
wfs <- levels(data_ready_autumn_rsf_2h$wf_name)
for(i in wfs) {
  pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_turbines_dist_comb", "phase", "landcover_cc"),
                                 typical = "median", condition = c(wf_name = i))
  #, condition = c(landcover_cc = "coniferous forests"))
  pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
  g_log_wind <- pr_log_wind_phase %>% 
    plot(show.title = F, limits = c(0,1), colors = cols) +
    geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
    labs(x = "Distance to wind turbines (km)",
         y = "Predicted probability of use",
         colour = "Phase") +
    xlim(0, 10)
  ggsave(filename = paste0("output/01_traditional_fit/rsf_autumn_wind_phase_view_", i, ".png"), plot = g_log_wind, 
         width = 20, height = 8, units = "cm", dpi = 300)
}
# pr_wind_phase <- ggpredict(fit9_nolog, terms = c("wind_dist", "phase", "landcover"))
# g_wind <- pr_wind_phase %>% 
#   plot(show.title = F, show.x.title = F) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to wind turbines (km)",
#        y = "Predicted probability of use") +
#   xlim(0, 10)
# ggsave(filename = "results/autumn_wind_phase_lc_no_log.png", plot = g_wind, 
#        width = 20, height = 20, units = "cm", dpi = 300)

# mining
pr_mining <- ggpredict(best_fit, terms = c("mining_Krist_dist"))
pr_mining$x <- pr_mining$x/1000
g_mining <- pr_mining %>% 
  plot(show.title = F, show.x.title = F,
       #limits = c(0,1), 
       colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to mining (km)",
       y = "Predicted probability of use",
       color = "Phase") +
  xlim(0, 20)
ggsave(filename = "rsf_autumn_mining.png", plot = g_mining, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# roads
pr_public_road <- ggpredict(best_fit, terms = c("public_road_dist"))
pr_public_road$x <- pr_public_road$x/1000
g_road <- pr_public_road %>%
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to large roads (km)",
       y = "Predicted probability of use",
       colour = "Phase") +
  xlim(0, 3)
ggsave(filename = "rsf_autumn_roads.png", plot = g_road,
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# error with NAs
pr_priv_road <- ggpredict(best_fit, terms = c("priv_road_dist"))
pr_priv_road$x <- pr_priv_road$x/1000
g_prov_road <- pr_priv_road %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to small roads (km)",
       y = "Predicted probability of use",
       colour = "Phase") +
  xlim(0, 1)
ggsave(filename = "rsf_autumn_private_roads.png", plot = g_prov_road, 
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
g_pl <- pr_pl %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to power lines (km)",
       y = "Predicted probability of use") +
  xlim(0, 10)
ggsave(filename = "rsf_autumn_pl.png", plot = g_pl, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# trails
pr_trail <- ggpredict(best_fit, terms = c("trails_dist"))
pr_trail$x <- pr_trail$x/1000
g_trail <- pr_trail %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to trails (km)",
       y = "Predicted probability of use") +
  xlim(0, 5)
ggsave(filename = "rsf_autumn_trails.png", plot = g_trail,
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# houses
pr_house <- ggpredict(best_fit, terms = c("house_dist"))
pr_house$x <- pr_house$x/1000
g_house <- pr_house %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to houses (km)",
       y = "Predicted probability of use") +
  xlim(0, 5)
ggsave(filename = "rsf_autumn_houses.png", plot = g_trail, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# elevation
pr_elevation <- ggpredict(best_fit, terms = c("dem"))
g_elevation <- pr_elevation %>% 
  plot(show.title = F, show.x.title = F,
       #limits = c(0,1), 
       colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Elevation (m)",
       y = "Predicted probability of use",
       color = "Phase") + 
  xlim(0, NA)
ggsave(filename = "rsf_autumn_elevation.png", plot = g_elevation, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# tpi
pr_tpi <- ggpredict(best_fit, terms = c("tpi_cat_s510m"))
g_tpi <- pr_tpi %>% 
  plot(show.title = F, show.x.title = F,
       #limits = c(0,1), 
       colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Terrain position index category (150m)",
       y = "Predicted probability of use",
       color = "Phase") 
ggsave(filename = "rsf_autumn_tpi.png", plot = g_tpi, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

#-------
# spatial prediction


#---------------
# More info, summary, and predictions on the best model for zoi

# table
tab_models

# re-fit best model
form_best_zoi <- as.formula(tab_models$definition[1])
fit_best_zoi <- glm(form_best_zoi,
                    data = data_ready_autumn_rsf_2h,
                    family = binomial(link = "logit"))

broom::glance(fit_best_zoi)
broom::tidy(fit_best_zoi)

m1_plot <- broom::tidy(fit_best_zoi) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0))) %>% 
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
ggsave("rsf_autumn_coefs_zoi.png", plot = m1_plot, path = "output/01_traditional_fit/", 
       width = 20, height = 20, units = "cm", dpi = 300)

summary(fit_best_zoi)
coefs <- broom::tidy(fit_best_zoi) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0)),
                p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
coefs %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  readr::write_csv(file = "output/01_traditional_fit/rsf_autumn_coef_table_zoi.csv")

#-------------------------------
# predictions

cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))

# averaged over all conditions, only slope
pr_slp <- ggpredict(fit_best_zoi, terms = "slope",
                   typical = "median") 
plot(pr_slp, lim = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey")

# only wind dist, averaged over all land cover types
# pr_wind_general <- ggpredict(best_fit, terms = c("wind_turbines_dist_comb", "phase"),
#                             typical = "median")
# pr_wind_general$x <- pr_wind_general$x/1000
# g_wind_general <- pr_wind_general %>% 
#   plot(limits = c(0,1), show.title = F, show.x.title = F, col = cols) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to wind turbines (km)",
#        y = "Predicted probability of use",
#        colour = "Phase") +
#   xlim(0, 10)
# ggsave(filename = "results/autumn_wind_phase_general.png", plot = g_wind_general, 
#        width = 15, height = 12, units = "cm", dpi = 300)

# wind dist, phase, and land cover, for ytteberg
pr_zoi_wind_phase <- ggpredict(fit_best_zoi, terms = c("wind_turbines_nearest10000", "phase", "landcover_cc"),
                               typical = "median", condition = c(wf_name = "ytteberg"))
#, condition = c(landcover_cc = "coniferous forests"))
# pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
pr_zoi_wind_phase <- pr_log_wind_phase
pr_zoi_wind_phase$x <- (10000*log(pr_zoi_wind_phase$x)/log(0.05))/1000 # in km
g_zoi_wind <- pr_zoi_wind_phase %>% 
  plot(show.title = F, col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to wind turbines (m)",
       y = "Predicted probability of use",
       colour = "Phase") +
  #ylim(0, 1) +
  xlim(0, 10)
ggsave(filename = "output/01_traditional_fit/autumn_wind_phase_lc_zoi_ytte.png", plot = g_zoi_wind, 
       width = 20, height = 20, units = "cm", dpi = 300)

# wind dist, phase, and land cover, for places closer to each of the WFs
wfs <- levels(data_ready_autumn_rsf_2h$wf_name)
for(i in wfs) {
  pr_log_wind_phase <- ggpredict(fit_best_zoi, terms = c("wind_turbines_nearest10000", "phase", "landcover_cc"),
                                 typical = "median", condition = c(wf_name = i))
  #, condition = c(landcover_cc = "coniferous forests"))
  pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
  g_log_wind <- pr_log_wind_phase %>% 
    plot(show.title = F, limits = c(0,1), colors = cols) +
    geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
    labs(x = "Distance to wind turbines (km)",
         y = "Predicted probability of use",
         colour = "Phase") +
    xlim(0, 10)
  ggsave(filename = paste0("output/01_traditional_fit/rsf_autumn_wind_phase_view_", i, ".png"), plot = g_log_wind, 
         width = 20, height = 8, units = "cm", dpi = 300)
}
# pr_wind_phase <- ggpredict(fit9_nolog, terms = c("wind_dist", "phase", "landcover"))
# g_wind <- pr_wind_phase %>% 
#   plot(show.title = F, show.x.title = F) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to wind turbines (km)",
#        y = "Predicted probability of use") +
#   xlim(0, 10)
# ggsave(filename = "results/autumn_wind_phase_lc_no_log.png", plot = g_wind, 
#        width = 20, height = 20, units = "cm", dpi = 300)

#oneimpact::dist_decay
#oneimpact::exp_decay
#1 * exp(- lambda * func(x - origin))
(a <- oneimpact::exp_decay(1000, radius = 1500))
1500*log(a)/(log(0.05))

# mining
pr_mining <- ggpredict(fit_best_zoi, terms = c("mining_Krist_nearest10000"))
#pr_mining$x <- pr_mining$x/1000
pr_mining$x <- (10000*log(pr_mining$x)/log(0.05))/1000 # in km
g_mining <- pr_mining %>% 
  plot(show.title = F, show.x.title = F,
       #limits = c(0,1), 
       colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to mining (km)",
       y = "Predicted probability of use",
       color = "Phase") +
  xlim(0, 15)
g_mining
ggsave(filename = "rsf_autumn_mining_zoi.png", plot = g_mining, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# roads
pr_public_road <- ggpredict(best_fit, terms = c("public_road_dist"))
pr_public_road$x <- pr_public_road$x/1000
g_road <- pr_public_road %>%
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to large roads (km)",
       y = "Predicted probability of use",
       colour = "Phase") +
  xlim(0, 3)
ggsave(filename = "rsf_autumn_roads.png", plot = g_road,
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# error with NAs
pr_priv_road <- ggpredict(best_fit, terms = c("priv_road_dist"))
pr_priv_road$x <- pr_priv_road$x/1000
g_prov_road <- pr_priv_road %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to small roads (km)",
       y = "Predicted probability of use",
       colour = "Phase") +
  xlim(0, 1)
ggsave(filename = "rsf_autumn_private_roads.png", plot = g_prov_road, 
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
g_pl <- pr_pl %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to power lines (km)",
       y = "Predicted probability of use") +
  xlim(0, 10)
ggsave(filename = "rsf_autumn_pl.png", plot = g_pl, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# trails
pr_trail <- ggpredict(best_fit, terms = c("trails_dist"))
pr_trail$x <- pr_trail$x/1000
g_trail <- pr_trail %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to trails (km)",
       y = "Predicted probability of use") +
  xlim(0, 5)
ggsave(filename = "rsf_autumn_trails.png", plot = g_trail,
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)



# houses
pr_house <- ggpredict(best_fit, terms = c("house_dist"))
pr_house$x <- pr_house$x/1000
g_house <- pr_house %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to houses (km)",
       y = "Predicted probability of use") +
  xlim(0, 5)
ggsave(filename = "rsf_autumn_houses.png", plot = g_trail, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# elevation
pr_elevation <- ggpredict(best_fit, terms = c("dem"))
g_elevation <- pr_elevation %>% 
  plot(show.title = F, show.x.title = F,
       #limits = c(0,1), 
       colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Elevation (m)",
       y = "Predicted probability of use",
       color = "Phase") + 
  xlim(0, NA)
ggsave(filename = "rsf_autumn_elevation.png", plot = g_elevation, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# tpi
pr_tpi <- ggpredict(best_fit, terms = c("tpi_cat_s510m"))
g_tpi <- pr_tpi %>% 
  plot(show.title = F, show.x.title = F,
       #limits = c(0,1), 
       colors = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Terrain position index category (150m)",
       y = "Predicted probability of use",
       color = "Phase") 
ggsave(filename = "rsf_autumn_tpi.png", plot = g_tpi, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

#-------
# spatial prediction

install.load::install_load("raster", "rasterVis", "terra", "stars")

# vindval data
load(file = "data/01_GPS/05_annotated_data/data_annotated_autumn_ready_2h_rsf.RData")

# model comparison
load("output/rsf_autumn_multi_zoi_table.rda")

# compare models
tab_models
#---------------
# Re-run best model
form <- tab_models$definition[1]
best_fit <- glm(form, 
                data = data_ready_calving_rsf_2h, #weights = w_obs,
                family = binomial(link = "logit"))
