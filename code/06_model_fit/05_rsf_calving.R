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
install.load::install_load("amt")
install.load::install_load("glmnet")

library(devtools)
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
  wf_name + landcover_cc*phase*log(wind_turbines_dist_comb+1)

fit1 <- glm(update(base_formula, ~ . + tpi_cat_s150m), 
            data = data_ready_calving_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))

fit1$formula
summary(fit1)
car::vif(fit1)

# backward deletion
# remove cos_aspect
base_formula2 <- update(base_formula, ~ . - cos_aspect)
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

# prediction
#cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))

# wind dist, phase, and land cover, for ytteberg
#pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_turbines_dist_comb", "phase", "viewshed_lc"),
#                               typical = "median", condition = c(wf_name = "ytteberg"))
#, condition = c(landcover_cc = "coniferous forests"))
#pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
#g_log_wind <- pr_log_wind_phase %>% 
#  plot(show.title = F, colors = cols) +
#  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#  labs(x = "Distance to wind turbines (m)",
#       y = "Predicted probability of use",
#       colour = "Phase") +
#  xlim(0, 10)
#ggsave(filename = "output/01_traditional_fit/calving_wind_phase_view_ytte.png", 
#       plot = g_log_wind, 
#       width = 20, height = 10, units = "cm", dpi = 300)

#-------
# GLM, no threshold regression, using ZoI for wind power and mining

base_formula <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(sos) + 
  log(power_lines_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + 
  log(trails_dist+1) + 
  log(house_dist+1) + scale(mining_Krist_nearestXXX) +
  wf_name + landcover_cc*phase*scale(wind_turbines_ZZZ)

scals <- c(100, 250, 500, 1000, 2500, 5000, 10000)
zois <- c("nearest", "cumulative")
wind_scals <- paste0(rep(zois, each = length(scals)), rep(scals, length(zois)))
tpi_scals <- c("tpi_cat_s150m", "tpi_cat_s250m", "tpi_cat_s510m")
lc_var <- c("landcover_cc", "viewshed_lc")

# n models = 7*2*14*3*2 = 1176

# using land cover
fits <- tibble::tibble(mining = numeric(), wind = character(), 
                       tpi = character(), lc = character(), 
                       aic = numeric())
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
                data = data_ready_calving_rsf_2h, #weights = w_obs,
                family = binomial(link = "logit"))
    fits[counter,1] <- i
    fits[counter,2] <- j
    fits[counter,3] <- tt
    fits[counter,4] <- lc
    fits[counter,5] <- AIC(fit1)
    
    counter <- counter+1
  }}}
}

# save(fits, file = "output/calving_multi_zoi_table.rda")
load("output/calving_multi_zoi_table.rda")
fits

# compare models
(tab_models <- fits |>
  dplyr::mutate(mining = as.character(mining)) |>
  dplyr::bind_rows(
    tibble::tibble(mining = "log", wind = "log", 
                   tpi = "tpi_cat_s150m",
                   lc = "viewshed_lc",
                   aic = AIC(best_fit))) |>
  dplyr::mutate(dAIC = abs(aic - min(aic))) |>
  dplyr::arrange(dAIC))

#############
# Add model definition here!!! and re.export

tab_models |>
  readr::write_csv(file = "output/01_traditional_fit/rsf_calving_AIC_table_complete.csv")

#---------------
# More info, summary, and predictions on the best model

broom::glance(best_fit)
broom::tidy(best_fit)

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
ggsave("rsf_calving_coefs_log.png", plot = m1_plot, path = "output/01_traditional_fit/", 
       width = 20, height = 20, units = "cm", dpi = 300)

model_call <- sapply(attr(AICvals_phase, "row.names"), function(x) paste(as.character(formula(eval(as.symbol(x))))[c(2,1,3)], collapse = " "))
AICvals_phase %>% 
  dplyr::as_tibble(rownames = "model") %>%
  dplyr::mutate(definition = unname(model_call)) %>%
  readr::write_csv(file = "output/01_traditional_fit/rsf_calving_AIC_table_log.csv")

summary(best_fit)
coefs <- broom::tidy(best_fit) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0)),
                p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
coefs %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  readr::write_csv(file = "output/01_traditional_fit/rsf_calving_coef_table_log.csv")


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
# ggsave(filename = "results/calving_wind_phase_general.png", plot = g_wind_general, 
#        width = 15, height = 12, units = "cm", dpi = 300)

# wind dist, phase, and land cover, for ytteberg
pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_turbines_dist_comb", "phase", "viewshed_lc"),
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
ggsave(filename = "output/01_traditional_fit/calving_wind_phase_view_ytte.png", plot = g_log_wind, 
       width = 20, height = 10, units = "cm", dpi = 300)

# wind dist, phase, and land cover, for places closer to each of the WFs
wfs <- levels(data_ready_calving_rsf_2h$wf_name)
for(i in wfs) {
  pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_turbines_dist_comb", "phase", "viewshed_lc"),
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
  ggsave(filename = paste0("output/01_traditional_fit/rsf_calving_wind_phase_view_", i, ".png"), plot = g_log_wind, 
         width = 20, height = 8, units = "cm", dpi = 300)
}
# pr_wind_phase <- ggpredict(fit9_nolog, terms = c("wind_dist", "phase", "landcover"))
# g_wind <- pr_wind_phase %>% 
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
g_mining <- pr_mining %>% 
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
g_road <- pr_public_road %>%
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
g_prov_road <- pr_priv_road %>% 
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
g_pl <- pr_pl %>% 
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
g_trail <- pr_trail %>% 
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
g_house <- pr_house %>% 
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
g_elevation <- pr_elevation %>% 
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
g_tpi <- pr_tpi %>% 
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

#-------
# define thresholds for distance to the nearest wind turbine
# define interactions with phase by hand
w <- log(data_ready_calving_rsf_2h$wind_turbines_dist_comb + 1)
w1 <- ifelse(w > log(1000), w - log(1000), 0)
w2 <- ifelse(w > log(2000), w - log(2000), 0)
w3 <- ifelse(w > log(3000), w - log(3000), 0)
w4 <- ifelse(w > log(4000), w - log(4000), 0)
w5 <- ifelse(w > log(5000), w - log(5000), 0)
w6 <- ifelse(w > log(6000), w - log(6000), 0)
w7 <- ifelse(w > log(8000), w - log(8000), 0)
w8 <- ifelse(w > log(10000), w - log(10000), 0)
w9 <- ifelse(w > log(15000), w - log(15000), 0)

# define interaction with phase for thresholds for distance to the nearest wind turbine
p <- data_ready_calving_rsf_2h$phase
p1 <- ifelse(data_ready_calving_rsf_2h$phase == "Construction", 1, 0)
p2 <- ifelse(data_ready_calving_rsf_2h$phase == "Operation", 1, 0)
wp1 <- w*p1
wp2 <- w*p2
w1p1 <- w1*p1
w1p2 <- w1*p2
w2p1 <- w2*p1
w2p2 <- w2*p2
w3p1 <- w3*p1
w3p2 <- w3*p2
w4p1 <- w4*p1
w4p2 <- w4*p2
w5p1 <- w5*p1
w5p2 <- w5*p2
w6p1 <- w6*p1
w6p2 <- w6*p2
w7p1 <- w7*p1
w7p2 <- w7*p2
w8p1 <- w8*p1
w8p2 <- w8*p2
w9p1 <- w9*p1
w9p2 <- w9*p2

# formula
formula_calving <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(sos) + 
  log(power_lines_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_Krist_dist+1) +
  log(trails_dist+1) + log(house_dist+1) + 
  landcover_cc*phase*wind_turbines + wf_name

formula_calving <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(sos) + 
  log(power_lines_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_Krist_dist+1) +
  log(trails_dist+1) + log(house_dist+1) +
  landcover_cc + phase + wf_name

# model matrix
M <- model.matrix(formula_calving, data = data_ready_calving_rsf_2h)
dim(M)
head(M)
length(w) == nrow(M)

# add variables in the matri X
X <- cbind(M, w, w1, w2, w3, w4, w5, w6, w7, w8, w9,
           wp1, wp2, w1p1, w1p2, w2p1, w2p2, w3p1, w3p2, w4p1, w4p2,
           w5p1, w5p2, w6p1, w6p2, w7p1, w7p2, w8p1, w8p2, w9p1, w9p2)
# response variable
y <- data_ready_calving_rsf_2h$case_

# penalty factors for variables
weights <- c(rep(0, ncol(M)), 0, log(c(1,2,3,4,5,6,8,10,15)*1000),
             0, 0, rep(log(c(1,2,3,4,5,6,8,10,15)*1000), times = rep(2, 9)))

length(weights) == ncol(X)

# fit
cv.lasso.w <- try(cv.glmnet(X, y, family = 'binomial', alpha = 1, parallel = TRUE, standardize = FALSE,
                            type.measure = 'deviance', penalty.factor = penalties))

# save object
save(cv.lasso.w, file = "output/test_fit_cv_lasso_w.rda")
# load(file = "output/test_fit_cv_lasso_w.rda")

# check coefficients
coefficients(cv.lasso.w)
plot(cv.lasso.w)
coef(cv.lasso.w, s = "lambda.min")
coef(cv.lasso.w, s = "lambda.1se")

library(broom)
library(ggplot2)
broom::glance(cv.lasso.w)
broom::tidy(cv.lasso.w)

tidied_cv <- tidy(cv.lasso.w)
glance_cv <- glance(cv.lasso.w)

# ROC
# rocs <- roc.glmnet(cv.lasso$fit.preval, newy = y)

# save object
save(cv.lasso.w, file = "output/test_fit_cv_lasso_w.rda")

#-----------
# # improvement
# # NOT! added observation weights according to Fieberg et al 2021 - NO!
# # changed formula specification
# # add mining distance in a threshold setup
dat <- data_ready_calving_rsf_2h

dat$w <- log(dat$wind_turbines_dist_comb + 1)
dat$w1 <- ifelse(dat$w > log(1000), dat$w - log(1000), 0)
dat$w2 <- ifelse(dat$w > log(2000), dat$w - log(2000), 0)
dat$w3 <- ifelse(dat$w > log(3000), dat$w - log(3000), 0)
dat$w4 <- ifelse(dat$w > log(4000), dat$w - log(4000), 0)
dat$w5 <- ifelse(dat$w > log(5000), dat$w - log(5000), 0)
dat$w6 <- ifelse(dat$w > log(6000), dat$w - log(6000), 0)
dat$w7 <- ifelse(dat$w > log(8000), dat$w - log(8000), 0)
dat$w8 <- ifelse(dat$w > log(10000), dat$w - log(10000), 0)
dat$w9 <- ifelse(dat$w > log(15000), dat$w - log(15000), 0)

dat$m <- log(dat$mining_Krist_dist + 1)
dat$m1 <- ifelse(dat$m > log(1000), dat$m - log(1000), 0)
dat$m2 <- ifelse(dat$m > log(2000), dat$m - log(2000), 0)
dat$m3 <- ifelse(dat$m > log(3000), dat$m - log(3000), 0)
dat$m4 <- ifelse(dat$m > log(4000), dat$m - log(4000), 0)
dat$m5 <- ifelse(dat$m > log(5000), dat$m - log(5000), 0)
dat$m6 <- ifelse(dat$m > log(6000), dat$m - log(6000), 0)
dat$m7 <- ifelse(dat$m > log(8000), dat$m - log(8000), 0)
dat$m8 <- ifelse(dat$m > log(10000), dat$m - log(10000), 0)
dat$m9 <- ifelse(dat$m > log(15000), dat$m - log(15000), 0)

# formula
base_formula <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(sos) + 
  log(power_lines_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + 
  log(trails_dist+1) + log(house_dist+1) +
  landcover_cc + phase + wf_name

formula_calving_w <- update(base_formula, ~ . + w + w1 + w2 + w3 + w4 + w5 + w6 + 
                              w7 + w8 + w9 +
                              phase:w + phase:w1 + phase:w2 + phase:w3 + phase:w4 + 
                              phase:w5 + phase:w6 + phase:w7 + phase:w8 + phase:w9)

formula_calving_wm <- update(formula_calving_w, ~ . + m + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9)

# model matrix
base_M <- model.matrix(base_formula, data = dat)
dim(base_M)
head(base_M)
  
M <- model.matrix(formula_calving_wm, data = dat)
dim(M)
head(M)
length(dat$w) == nrow(M)

# add variables in the matri X
X <- M
# response variable
y <- dat$case_

# penalty factors for variables
penalties <- c(rep(0, ncol(base_M)), #rep(0, length(levels(dat$phase))-1), 
               0, log(c(1,2,3,4,5,6,8,10,15)*1000),
               0, log(c(1,2,3,4,5,6,8,10,15)*1000),
               0, 0, rep(log(c(1,2,3,4,5,6,8,10,15)*1000), times = rep(2, 9)))

length(penalties) == ncol(X)

# fit
cv.lasso.wm <- try(cv.glmnet(X, y, family = 'binomial', alpha = 1, parallel = TRUE, standardize = FALSE,
                          type.measure = 'deviance', penalty.factor = penalties))#, weights = dat$w_obs))

# check coefficients
coefficients(cv.lasso.wm)
plot(cv.lasso.wm)
coef(cv.lasso.wm, s = "lambda.min")
coef(cv.lasso.wm, s = "lambda.1se")

# save
# save(cv.lasso.wm, file = "output/test_fit_cv_lasso_wm.rda")
# load(file = "output/test_fit_cv_lasso_wm.rda")

#-----------
# add viewshed and interaction (not landcover)

dat <- data_ready_calving_rsf_2h

dat$w <- log(dat$wind_turbines_dist_comb + 1)
dat$w1 <- ifelse(dat$w > log(1000), dat$w - log(1000), 0)
dat$w2 <- ifelse(dat$w > log(2000), dat$w - log(2000), 0)
dat$w3 <- ifelse(dat$w > log(3000), dat$w - log(3000), 0)
dat$w4 <- ifelse(dat$w > log(4000), dat$w - log(4000), 0)
dat$w5 <- ifelse(dat$w > log(5000), dat$w - log(5000), 0)
dat$w6 <- ifelse(dat$w > log(6000), dat$w - log(6000), 0)
dat$w7 <- ifelse(dat$w > log(8000), dat$w - log(8000), 0)
dat$w8 <- ifelse(dat$w > log(10000), dat$w - log(10000), 0)
dat$w9 <- ifelse(dat$w > log(15000), dat$w - log(15000), 0)

dat$m <- log(dat$mining_Krist_dist + 1)
dat$m1 <- ifelse(dat$m > log(1000), dat$m - log(1000), 0)
dat$m2 <- ifelse(dat$m > log(2000), dat$m - log(2000), 0)
dat$m3 <- ifelse(dat$m > log(3000), dat$m - log(3000), 0)
dat$m4 <- ifelse(dat$m > log(4000), dat$m - log(4000), 0)
dat$m5 <- ifelse(dat$m > log(5000), dat$m - log(5000), 0)
dat$m6 <- ifelse(dat$m > log(6000), dat$m - log(6000), 0)
dat$m7 <- ifelse(dat$m > log(8000), dat$m - log(8000), 0)
dat$m8 <- ifelse(dat$m > log(10000), dat$m - log(10000), 0)
dat$m9 <- ifelse(dat$m > log(15000), dat$m - log(15000), 0)

# formula
base_formula <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(sos) + 
  log(power_lines_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + 
  log(trails_dist+1) + log(house_dist+1) +
  wf_name + viewshed_lc + phase

formula_calving_w <- update(base_formula, ~ . + w + w1 + w2 + w3 + w4 + w5 + w6 + w7 + w8 + w9 +
                              viewshed_lc*phase*w + viewshed_lc*phase*w1 + viewshed_lc*phase*w2 + 
                              viewshed_lc*phase*w3 + viewshed_lc*phase*w4 + viewshed_lc*phase*w5 + 
                              viewshed_lc*phase*w6 + viewshed_lc*phase*w7 + viewshed_lc*phase*w8 + 
                              viewshed_lc*phase*w9)

formula_calving_wm <- update(formula_calving_w, ~ . + m + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9)

# model matrix
base_M <- model.matrix(base_formula, data = dat)
dim(base_M)
head(base_M)

M <- model.matrix(formula_calving_wm, data = dat)
dim(M)
head(M)
length(dat$w) == nrow(M)

# add variables in the matri X
X <- M
# response variable
y <- dat$case_

# penalty factors for variables
penalties <- c(rep(0, ncol(base_M)), #rep(0, length(levels(dat$phase))-1), 
               0, log(c(1,2,3,4,5,6,8,10,15)*1000),
               0, 0, 
               0, log(c(1,2,3,4,5,6,8,10,15)*1000),
               0, 0, 0, 0, 
               rep(c(0, log(c(1,2,3,4,5,6,8,10,15)*1000)), times = rep(4, 10)),
               rep(c(0, log(c(1,2,3,4,5,6,8,10,15)*1000)), times = rep(4, 10)))

length(penalties) == ncol(X)

# fit
cv.lasso.wm.viewshed <- try(cv.glmnet(X, y, family = 'binomial', alpha = 1, parallel = TRUE, standardize = FALSE,
                                   type.measure = 'deviance', penalty.factor = penalties))#, weights = dat$w_obs))

# save
save(cv.lasso.wm.viewshed, file = "output/test_fit_cv_lasso_wm_viewshed.rda")
# load(file = "output/test_fit_cv_lasso_wm_viewshed.rda")

# check coefficients
coefficients(cv.lasso.wm.viewshed)
plot(cv.lasso.wm.viewshed)
coef(cv.lasso.wm.viewshed, s = "lambda.min")
coef(cv.lasso.wm.viewshed, s = "lambda.1se")

#-----------
# add interaction with landcover
formula_calving <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  log(pl_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_dist+1) +
  log(trail_dist+1) + log(house_dist+1) +
  landcover_cc*phase*w + phase*w1 + phase*w2 + wf_name

M <- model.matrix(formula_calving, data = vindval_data_ready_calving_rsf_2h)

#--------------
# Model with ZoI nearest

dat <- data_ready_calving_rsf_2h

# wind
dat$w100 <- dat$wind_turbines_nearest100
dat$w250 <- dat$wind_turbines_nearest250
dat$w500 <- dat$wind_turbines_nearest500
dat$w1000 <- dat$wind_turbines_nearest1000
dat$w2500 <- dat$wind_turbines_nearest2500
dat$w5000 <- dat$wind_turbines_nearest5000
dat$w10000 <- dat$wind_turbines_nearest10000

# mining
dat$m100 <- dat$mining_Krist_nearest100
dat$m250 <- dat$mining_Krist_nearest250
dat$m500 <- dat$mining_Krist_nearest500
dat$m1000 <- dat$mining_Krist_nearest1000
dat$m2500 <- dat$mining_Krist_nearest2500
dat$m5000 <- dat$mining_Krist_nearest5000
dat$m10000 <- dat$mining_Krist_nearest10000

# formula
base_formula <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(sos) + 
  log(power_lines_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + 
  log(trails_dist+1) + log(house_dist+1) +
  wf_name + landcover_cc + phase

formula_calving_w <- update(base_formula, ~ . + w100 + w250 + w500 + w1000 + w2500 + w5000 + w10000 +
                              phase*w100 + phase*w250 + phase*w500 + 
                              phase*w1000 + phase*w2500 + phase*w5000 + 
                              phase*w10000)

formula_calving_wm <- update(formula_calving_w, ~ . + m100 + m250 + m500 + m1000 + m2500 + m5000 + m10000)

# model matrix
base_M <- model.matrix(base_formula, data = dat)
dim(base_M)
head(base_M)

M <- model.matrix(formula_calving_wm, data = dat)
dim(M)
head(M)
length(dat$w100) == nrow(M)

# add variables in the matri X
X <- M
# response variable
y <- dat$case_

# penalty factors for variables
penalties <- c(rep(0, ncol(base_M)), #rep(0, length(levels(dat$phase))-1), 
               #c(0,2.5,5,10,25,50,100),
               #0, 0, 
               0, log(c(2.5,5,10,25,50,100)*100),
               #c(0,2.5,5,10,25,50,100),
               0, log(c(2.5,5,10,25,50,100)*100),
               #0, 0, 0, 0, 
               rep(c(0,log(c(2.5,5,10,25,50,100)*100)), times = rep(2, 7)))
               #rep(c(0, log(c(1,2,3,4,5,6,8,10,15)*1000)), times = rep(4, 10)))

#penalties <- c(rep(0, ncol(base_M)), #rep(0, length(levels(dat$phase))-1), 
#               c(0, log(c(2.5,5,10,25,50,100)*100)),
#               c(0, log(c(2.5,5,10,25,50,100)*100)),
#               rep(c(0, log(c(2.5,5,10,25,50,100)*100)), times = rep(2, 7)))

length(penalties) == ncol(X)

# fit
cv.lasso.wm.zoi_nearest <- try(cv.glmnet(X, y, family = 'binomial', alpha = 1, parallel = TRUE, standardize = FALSE,
                                   type.measure = 'deviance', penalty.factor = penalties))#, weights = dat$w_obs))

# save
save(cv.lasso.wm.zoi_nearest, file = "output/test_fit_cv_lasso_wm_zoi_nearest.rda")

# check coefficients
(coefs <- coefficients(cv.lasso.wm.zoi_nearest))
coefs_df <- tibble::tibble(var = coefs@Dimnames[[1]][(coefs@i+1)], beta = coefs@x)

m_base <- glm(formula_calving_wm, family = binomial(link = "logit"), data = dat)
summary(m_base)

#-------------------
# Model with ZoI cumulative

dat <- data_ready_calving_rsf_2h

# wind
dat$w100 <- dat$wind_turbines_cumulative100
dat$w250 <- dat$wind_turbines_cumulative250
dat$w500 <- dat$wind_turbines_cumulative500
dat$w1000 <- dat$wind_turbines_cumulative1000
dat$w2500 <- dat$wind_turbines_cumulative2500
dat$w5000 <- dat$wind_turbines_cumulative5000
dat$w10000 <- dat$wind_turbines_cumulative10000

# mining
dat$m100 <- dat$mining_Krist_nearest100
dat$m250 <- dat$mining_Krist_nearest250
dat$m500 <- dat$mining_Krist_nearest500
dat$m1000 <- dat$mining_Krist_nearest1000
dat$m2500 <- dat$mining_Krist_nearest2500
dat$m5000 <- dat$mining_Krist_nearest5000
dat$m10000 <- dat$mining_Krist_nearest10000

# formula
base_formula <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(sos) + 
  log(power_lines_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + 
  log(trails_dist+1) + log(house_dist+1) +
  wf_name + landcover_cc + phase

formula_calving_w <- update(base_formula, ~ . + w100 + w250 + w500 + w1000 + w2500 + w5000 + w10000 +
                              phase*w100 + phase*w250 + phase*w500 + 
                              phase*w1000 + phase*w2500 + phase*w5000 + 
                              phase*w10000)

formula_calving_wm <- update(formula_calving_w, ~ . + m100 + m250 + m500 + m1000 + m2500 + m5000 + m10000)

# model matrix
base_M <- model.matrix(base_formula, data = dat)
dim(base_M)
head(base_M)

M <- model.matrix(formula_calving_wm, data = dat)
dim(M)
head(M)
length(dat$w100) == nrow(M)

# add variables in the matri X
X <- M
# response variable
y <- dat$case_

# penalty factors for variables
penalties <- c(rep(0, ncol(base_M)), #rep(0, length(levels(dat$phase))-1), 
               c(0, log(c(2.5,5,10,25,50,100)*100)),
               #0, 0, 
               #0, log(c(1,2,3,4,5,6,8,10,15)*1000),
               c(0, log(c(2.5,5,10,25,50,100)*100)),
               #0, 0, 0, 0, 
               rep(c(0, log(c(2.5,5,10,25,50,100)*100)), times = rep(2, 7)))
               #rep(c(0, log(c(1,2,3,4,5,6,8,10,15)*1000)), times = rep(4, 10)))

length(penalties) == ncol(X)

# fit
cv.lasso.wm.zoi_cumulative <- try(cv.glmnet(X, y, family = 'binomial', alpha = 1, parallel = TRUE, standardize = FALSE,
                                   type.measure = 'deviance', penalty.factor = penalties))#, weights = dat$w_obs))

# save
save(cv.lasso.wm.zoi_cumulative, file = "output/test_fit_cv_lasso_wm_zoi_cumulative.rda")

# check coefficients
(coefs <- coefficients(cv.lasso.wm.zoi_cumulative))
coefs_df <- tibble::tibble(var = coefs@Dimnames[[1]][(coefs@i+1)], beta = coefs@x)

m_base <- glm(formula_calving_wm, family = binomial(link = "logit"), data = dat)
summary(m_base)


w1 <- wind_turbines_cumulative100
w2 <- wind_turbines_cumulative500 #ifelse(w > log(1000), w - log(1000), 0)
w3 <- wind_turbines_cumulative1000
w4 <- wind_turbines_cumulative2500
w5 <- wind_turbines_cumulative5000 #ifelse(w > log(1000), w - log(1000), 0)
w6 <- wind_turbines_cumulative10000

# 1) Re-run these tests including NDVI
# 2) Run these models using cumulative and nearest ZOI
# 2.5) Operationalize the predictions of these models
# 3) Include interactions land cover or viewshed and interpret
# 4) Make predictions on maps
