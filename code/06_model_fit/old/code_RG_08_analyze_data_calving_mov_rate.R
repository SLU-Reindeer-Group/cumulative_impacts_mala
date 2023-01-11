#' ---
#' title: 'Movement rate analysis of Swedish reindeer during calving: Mala sameby'
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
install.load::install_load("tidyverse", "lubridate", "ggridges", "GGally", "wesanderson")
install.load::install_load("survival", "broom", "bbmle", "ggeffects")
install.load::install_load("amt")

#-----------
#' # Load annotated and clean movement data

# fix rate = 2h
load(file = "output/data_annotated_calving_ready_2h_mov_rate.RData")
data_ready_calving_mov_track_2h

# limited viewshed for amliden
dist_cutoff <- 1.5e4
wfs <- c("Jokkmokksliden", "Storliden", "Ytterberg", "Åmliden")
data_ready_calving_mov_track_2h <- data_ready_calving_mov_track_2h %>% 
  dplyr::mutate(viewshed_aml15km = ifelse(wind_dist_aml >  dist_cutoff, 0, viewshed_amliden_mala_binary),
                cos_aspect = cos(aspect),
                wind_dist_comb = pmin(wind_dist_jsy, wind_dist_aml),
                dist_wf_closest = apply(cbind(wind_dist_jokk, wind_dist_stor, wind_dist_ytte, wind_dist_aml), 1, which.min),
                wf_name = factor(dist_wf_closest, levels = 1:4, labels = wfs))

# apply(cbind(data_ready_calving_rsf_2h$wind_dist_jokk, data_ready_calving_rsf_2h$wind_dist_stor), MARGIN = 1, which.min)
data_ready_calving_mov_track_2h$wf_name

data_ready <- data_ready_calving_mov_track_2h %>% 
  dplyr::mutate(sl = (sl_+1)/1000)

#-----------
#' # Analysis with 2h fix rate, phase
#' 

# View(data_ready)

# phase
data_ready$phase



#-------------------------------
# Re-check correlations

data_corr <- data_ready %>% 
  dplyr::slice(sample(1:nrow(.), 5000, replace = F))

g_corr <- data_corr %>% 
  dplyr::select(dem_std, slope_std, #contains("tpi_cat"),
                lichen_std, #landcover_cc, 
                log_wind, wind_dist,
                viewshed_lc,
                log_house, log_public_road, log_priv_road, log_snow_mob,
                pl_dist) %>% 
  dplyr::mutate(viewshed_lc = as.numeric(viewshed_lc)) %>% 
  ggpairs()
# g_corr_dem
ggsave("g_corr_dataset_movrate_calving.png", plot = g_corr, path = "output", 
       width = 30, height = 30, units = "cm", dpi = 300)

# dem only
data_corr$tpi_cat_s150m %>% as.factor %>% levels
g_corr_dem <- data_corr %>% 
  dplyr::select(dem_std, slope_std, contains("tpi_cat")) %>% 
  ggpairs()
# g_corr_dem
ggsave("g_corr_dataset_dem_calving.png", plot = g_corr_dem, path = "output", 
       width = 20, height = 20, units = "cm", dpi = 300)

# dem and priv_road are correlated (0.71)
# public roads and pl are correlaetd (0.77)

#-------------------------------
# Fit models - phase

formula_phase <- sl ~ scale(dem) + I(scale(dem)^2) +
  scale(slope) + cos_aspect + wf_name +
  landcover_cc + phase*scale(wind_dist_comb)*wf_name +
  phase*I(scale(wind_dist_comb)^2) +
  scale(mining_dist) + 
  scale(house_dist) + 
  scale(public_road_dist) +
  scale(trail_dist) +
  scale(pl_dist)

# # start with full model only
# fit1_phase <- glm(update(formula_phase, ~ . + tpi_cat_s150m),
#                   data = data_ready, family = Gamma(link = "log"))
# 
# summary(fit1_phase)
# car::vif(fit1_phase)

# backward deletion
formula_phase2 <- update(formula_phase, ~ . - cos_aspect)

# fit2_phase <- glm(update(formula_phase2, ~ . + tpi_cat_s150m),
#                   data = data_ready, family = Gamma(link = "log"))
# 
# fit2_phase$formula
# summary(fit2_phase)
# car::vif(fit2_phase)

# compare with full model for other TPI scales
fit2.2_phase <- glm(update(formula_phase2, ~ . + tpi_cat_s250m),
                    data = data_ready, family = Gamma(link = "log"))

# 
# fit2.3_phase <- glm(update(formula_phase2, ~ . + tpi_cat_s510m),
#                     data = data_ready, family = Gamma(link = "log"))
# 
# 
# (AICvals_phase <- bbmle::AICtab(fit2_phase, fit2.2_phase, fit2.3_phase, 
#                                 weights = T, base = T))

summary(fit2.2_phase)

# compare with viewshed_lc, 
# fit3_phase <- with(data_ready,
#                    glm(sl/1000 ~ scale(dem) + I(scale(dem)^2) +
#                          phase*scale(dem) +
#                          scale(slope) + cos_aspect +
#                          tpi_cat_s510m + phase*scale(snow_depth_m) + 
#                          phase*scale(lichen) +
#                          phase*scale(wind_dist)*viewshed_lc +
#                          phase*scale(house_dist) + 
#                          phase*scale(public_road_dist) +
#                          phase*scale(snowmob_track_dist), 
#                        family = Gamma(link = "log")))

# car::vif(fit3_phase)
# 
# (AICvals_phase <- bbmle::AICtab(fit2_phase, fit2.2_phase, 
#                                 fit2.3_phase, fit3_phase, 
#                                 weights = T, base = T))

# bad model

# (AICvals_phase <- bbmle::AICtab(fit2_phase, fit2.2_phase, 
#                                 fit2.3_phase, fit3_phase, 
#                                 fit4_phase,
#                                 weights = T, base = T))

# best model
# best model 6 july 2021 fit2.2_phase
best_fit_phase <- fit2.2_phase
best_fit_phase_calving <- best_fit_phase
summary(best_fit_phase)

broom::glance(best_fit_phase)
broom::tidy(best_fit_phase)

# plot coefs
m1_plot <- broom::tidy(best_fit_phase) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0))) %>% 
  ggplot(aes(x = term, y = estimate, color = significant)) +
  # geom_point(show.legend = F) +
  geom_pointrange(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                  position = position_dodge(width = 0.7), size = 0.8,
                  show.legend = F) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Coefficient", x = "") +
  theme_light() + 
  coord_flip()
m1_plot
ggsave("calving_coefs_mov_rate_phase_calving.png", plot = m1_plot, path = "results", 
       width = 12, height = 15, units = "cm", dpi = 300)

# save tables
model_call <- sapply(attr(AICvals_phase, "row.names"), function(x) as.character(getCall(eval(as.symbol(x)))))[2,]
AICvals_phase %>% 
  dplyr::as_tibble(rownames = "model") %>% 
  dplyr::mutate(definition = unname(model_call)) %>% 
  readr::write_csv(file = "results/AIC_table_calving_mov_rate_phase.csv")

summary(best_fit_phase)
coefs <- broom::tidy(best_fit_phase) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0)),
                p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
coefs %>% 
  dplyr::mutate_if(is.numeric, round, digits = 3) %>% 
  readr::write_csv(file = "results/coef_table_calving_mov_rate_phase.csv")

#------------------------------------------------------
# Prediction
fit2_phase$call

summary(best_fit_phase)
cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))

pr_log_wind_phase <- ggpredict(best_fit_phase, terms = c("wind_dist_comb", "phase", "wf_name"), 
                               typical = "median", )#, condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_phase_calving <- pr_log_wind_phase
save(pr_log_wind_phase_calving, file="results/prediction_move_rate_.RData")

pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
g_log_wind_phase <- pr_log_wind_phase %>% 
  plot(show.title = F, show.x.title = F, col = cols, limit = c(0, 0.6)) +
  labs(x = "Distance to wind turbines (km)",
       y = "Movement rate (km/2h)",
       colour = "Phase") +
  #facet_wrap(~facet, scales = "free_y") +
  xlim(0, 10)
g_log_wind_phase
ggsave(filename = "results/prediction_mov_rate_phase1_calving.png", plot = g_log_wind_phase, 
       width = 16, height = 16, units = "cm", dpi = 300)



pr_log_wind_phase3 <- ggpredict(best_fit_phase, terms = c("pl_dist"),
                                typical = "median", condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_phase3$x <- pr_log_wind_phase3$x/1000
g_log_wind3 <- pr_log_wind_phase3 %>% 
  plot(show.title = F, show.x.title = F, col = cols, limit = c(0, .6)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Distance to power lines (km)",
       y = "Movement rate (km/2h)",
       colour = "Phase")
g_log_wind3
ggsave(filename = "results/prediction_mov_rate_phase3_calving.png", plot = g_log_wind3, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_log_wind_phase4 <- ggpredict(best_fit_phase, terms = c("dem"),
                                typical = "median", condition = c(landcover_cc = "coniferous forests"))
g_log_wind4 <- pr_log_wind_phase4 %>% 
  plot(show.title = F, show.x.title = F, col = cols, limit = c(0, 0.6)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Elevation (m)",
       y = "Movement rate (km/2h)",
       colour = "Phase") +
  xlim(NA, 650)
g_log_wind4
ggsave(filename = "results/prediction_mov_rate_phase4_calving.png", plot = g_log_wind4, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_log_wind_phase5 <- ggpredict(best_fit_phase, terms = c("trail_dist"),
                                typical = "median", condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_phase5$x <- pr_log_wind_phase5$x/1000
g_log_wind5 <- pr_log_wind_phase5 %>% 
  plot(show.title = F, show.x.title = F, col = cols, limit = c(0, 0.6)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Distance to trails (km)",
       y = "Movement rate (km/2h)",
       colour = "Phase")
g_log_wind5
ggsave(filename = "results/prediction_mov_rate_phase5_calving.png", plot = g_log_wind5, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_log_wind_phase_house <- ggpredict(best_fit_phase, terms = c("house_dist"),
                                     typical = "median", condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_phase_house$x <- pr_log_wind_phase_house$x/1000
g_log_wind_house <- pr_log_wind_phase_house %>% 
  plot(show.title = F, show.x.title = F, col = cols, limit = c(0, .6)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Distance to houses (km)",
       y = "Movement rate (km/2h)",
       colour = "Phase")
g_log_wind_house
ggsave(filename = "results/prediction_mov_rate_phase6_calving.png", plot = g_log_wind_house, 
       width = 15, height = 12, units = "cm", dpi = 300)


# public roads
pr_log_wind_phase_road <- ggpredict(best_fit_phase, terms = c("public_road_dist"),
                                    typical = "median", condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_phase_road$x <- pr_log_wind_phase_road$x/1000
g_log_wind_road <- pr_log_wind_phase_road %>% 
  plot(show.title = F, show.x.title = F, col = cols, limit = c(0, .6)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Distance to roads (km)",
       y = "Movement rate (km/2h)",
       colour = "Phase")
g_log_wind_road
ggsave(filename = "results/prediction_mov_rate_phase7_calving.png", plot = g_log_wind_road, 
       width = 15, height = 12, units = "cm", dpi = 300)

# slope
pr_log_wind_phase_slope <- ggpredict(best_fit_phase, terms = c("slope"),
                                      typical = "median", condition = c(landcover_cc = "coniferous forests"))
g_log_wind_slope <- pr_log_wind_phase_slope %>% 
  plot(show.title = F, show.x.title = F, col = cols, limit = c(0, .6)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Slope",
       y = "Movement rate (km/2h)",
       colour = "Phase")
g_log_wind_slope
ggsave(filename = "results/prediction_mov_rate_phase8_calving.png", plot = g_log_wind_slope, 
       width = 15, height = 12, units = "cm", dpi = 300)


best_fit_phase$call
summary(best_fit_phase)

# combine
# combine plots
g_phase_combine <- ggpubr::ggarrange(g_log_wind_house,
                                     g_log_wind_road + ylab(""), 
                                     g_log_wind3 + ylab(""),
                                     g_log_wind5,
                                     g_log_wind4 + ylab(""),
                                     labels = c("A", "B", "C", "D", "E"), 
                                     common.legend = T, legend = "right", 
                                     ncol = 3, nrow = 2)
g_phase_combine
ggsave(filename = "results/prediction_mov_rate_phase_combined_calving.png", plot = g_phase_combine, 
       width = 25, height = 15, units = "cm", dpi = 300)


#----------------------------------------------------------------
#---------------------------------------------------------------
# model for predators and sound

data_pred <- data_ready %>% 
  dplyr::filter(year >= 2015 & year <= 2018)
# dplyr::mutate(wolf = 100*wolf,
#               wolverine = 10*wolverine)

data_pred$wolf %>% range(na.rm = T)
data_pred$wolverine %>% range(na.rm = T)

formula_pred <- sl ~ scale(slope) + cos_aspect + 
      scale(bear)*scale(dem) + scale(wolverine)*scale(dem) + 
      I(scale(dem)^2) +
      landcover_cc + wf_name + 
      scale(bear)*scale(wind_dist_comb) + scale(wolverine)*scale(wind_dist_comb) +
      scale(bear)*scale(mining_dist) + scale(wolverine)*scale(mining_dist) +
      scale(house_dist) + scale(trail_dist) + scale(pl_dist) +
      scale(bear)*scale(public_road_dist) + scale(wolverine)*scale(public_road_dist)

# full model to start with
fit_pred_1 <- glm(update(formula_pred, ~ . + tpi_cat_s150m),
                  data = data_pred, family = Gamma(link = "log"))

car::vif(fit_pred_1)
summary(fit_pred_1)

# backward deletion
formula_pred2 <- update(formula_pred, ~ . - cos_aspect - I(scale(dem)^2))

fit_pred_2 <- glm(update(formula_pred2, ~ . + tpi_cat_s150m), 
                  data = data_pred, family = Gamma(link = "log"))

summary(fit_pred_2)
car::vif(fit_pred_2)

# compare with full model for other TPI scales
fit_pred_2.2 <- glm(update(formula_pred2, ~ . + tpi_cat_s250m), 
                    data = data_pred, family = Gamma(link = "log"))


fit_pred_2.3 <- glm(update(formula_pred2, ~ . + tpi_cat_s510m), 
                    data = data_pred, family = Gamma(link = "log"))


(AICvals_pred <- bbmle::AICtab(fit_pred_2, fit_pred_2.2, fit_pred_2.3, 
                               weights = T, base = T))

# compare viewshed and viewshed_lc
# fit_pred_2 <- with(data_pred,
#                    glm(sl/1000 ~ scale(slope) + tpi_cat_s510m +
#                          scale(wolf)*scale(dem) + scale(wolverine)*scale(dem) + 
#                          I(scale(dem)^2) +
#                          scale(lichen) +
#                          scale(wolf)*scale(snow_depth_m) + scale(wolverine)*scale(snow_depth_m) +
#                          scale(wolf)*scale(wind_dist)*viewshed_lc + 
#                          scale(wolverine)*scale(wind_dist)*viewshed_lc +
#                          scale(house_dist) +  + scale(snowmob_track_dist) +
#                          scale(wolf)*scale(public_road_dist) + scale(wolverine)*scale(public_road_dist), 
#                        family = Gamma(link = "log")))

# summary(fit_pred_2)

# sound
formula_pred3 <- update(formula_pred2, ~ . + tpi_cat_s510m -
                          scale(bear)*scale(wind_dist_comb) - scale(wolverine)*scale(wind_dist_comb) +
                          scale(bear)*scale(sound_intensity) + scale(wolverine)*scale(sound_intensity))

fit_pred_3 <- glm(formula_pred3, 
                  data = data_pred, family = Gamma(link = "log"))


# check
car::vif(fit_pred_3)
summary(fit_pred_3)

(AICvals_pred <- bbmle::AICtab(fit_pred_2, fit_pred_2.2, fit_pred_2.3, fit_pred_3,
                               weights = T, base = T))

#----------------
# best model
#best model 6 july fit_pred_2.3
best_fit_pred <- fit_pred_2.3
best_fit_pred_calving <- best_fit_pred

summary(best_fit_pred)

broom::glance(best_fit_pred)
broom::tidy(best_fit_pred)

# plot coefs
m1_plot <- broom::tidy(best_fit_pred) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0))) %>% 
  ggplot(aes(x = term, y = estimate, color = significant)) +
  # geom_point(show.legend = F) +
  geom_pointrange(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                  position = position_dodge(width = 0.7), size = 0.8,
                  show.legend = F) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Relative Selection Strength", x = "") +
  theme_light() + 
  coord_flip()
m1_plot
ggsave("coefs_pred_mov_rate_calving.png", plot = m1_plot, path = "results", 
       width = 12, height = 15, units = "cm", dpi = 300)

# save tables
model_call <- sapply(attr(AICvals_pred, "row.names"), function(x) as.character(getCall(eval(as.symbol(x)))))[2,]
AICvals_pred %>% 
  dplyr::as_tibble(rownames = "model") %>% 
  dplyr::mutate(definition = unname(model_call)) %>%
  readr::write_csv(file = "results/AIC_table_pred_mov_rate_calving.csv")

summary(best_fit_pred)
coefs <- broom::tidy(best_fit_pred) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0)),
                p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
coefs %>% 
  dplyr::mutate_if(is.numeric, round, digits = 3) %>% 
  readr::write_csv(file = "results/coef_table_pred_mov_rate_calving.csv")

#---------------------
# Prediction

#--------------------
# Most parsimonious model for predators

fit_pred_2.3

cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))
library(wesanderson)
colsbear<- c(wes_palette("FantasticFox1", 3))
colswolv <- c(wes_palette("GrandBudapest1", 3))

pr_log_wind_pred <- ggpredict(best_fit_pred_calving, terms = c("wind_dist_comb", "bear", "wf_name"), 
                              typical = "median", condition = c("landcover_cc = coniferous forests"))
pr_log_wind_pred <- ggpredict(best_fit_pred_calving, terms = c("wind_dist_comb", "bear"), 
                              typical = "median", condition = c("landcover_cc = coniferous forests"))

pr_log_wind_pred$x <- pr_log_wind_pred$x/1000
g_log_wind_pred <- pr_log_wind_pred %>% 
  plot(show.title = F, show.x.title = F, col = colsbear, lim = c(0, .6)) +
  # geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to wind turbines (km)",
       y = "Movement rate (km/2h)",
       colour = "Bear\nabundance") +
  xlim(0, 10)
g_log_wind_pred
ggsave(filename = "results/prediction_mov_rate_pred1_calving.png", plot = g_log_wind_pred, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_log_wind_pred2 <- ggpredict(best_fit_pred_calving, terms = c("wind_dist_comb", "wolverine", "wf_name"), 
                               typical = "median", condition = c("landcover_cc = coniferous forests"))

pr_log_wind_pred2 <- ggpredict(best_fit_pred_calving, terms = c("wind_dist_comb", "wolverine"), 
                               typical = "median", condition = c("landcover_cc = coniferous forests"))


pr_log_wind_pred2$x <- pr_log_wind_pred2$x/1000
g_log_wind_pred2 <- pr_log_wind_pred2 %>% 
  plot(show.title = F, show.x.title = F, col = colswolv, lim = c(0, .7)) +
  # geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to wind turbines (km)",
       y = "Movement rate (km/2h)",
       colour = "Wolverine\nabundance") +
  xlim(0, 10)
g_log_wind_pred2
ggsave(filename = "results/prediction_mov_rate_pred2_calving.png", plot = g_log_wind_pred2, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_log_wind_phase_calving_bear <- pr_log_wind_pred
pr_log_wind_phase_calving_wolverine <- pr_log_wind_pred2
save(pr_log_wind_phase_calving_bear, pr_log_wind_phase_calving_wolverine, file="results/prediction_move_rate_pred.RData")


# mines
pr_log_wind_pred3 <- ggpredict(best_fit_pred, terms = c("mining_dist", "wolverine"),
                               typical = "median", condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_pred3$x <- pr_log_wind_pred3$x/1000
g_log_wind3 <- pr_log_wind_pred3 %>% 
  plot(show.title = F, show.x.title = F, col = colswolv, limit = c(0, .6)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Distance to mines (km)",
       y = "Movement rate (km/2h)",
       colour = "Wolverine\nabundance") +
  xlim(0, 30)
g_log_wind3
ggsave(filename = "results/prediction_mov_rate_pred3_calving.png", plot = g_log_wind3, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_log_wind_pred3_2 <- ggpredict(best_fit_pred, terms = c("mining_dist", "bear"),
                               typical = "median", condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_pred3_2$x <- pr_log_wind_pred3_2$x/1000
g_log_wind3_2 <- pr_log_wind_pred3_2 %>% 
  plot(show.title = F, show.x.title = F, col = colsbear, limit = c(0, .6)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Distance to mines (km)",
       y = "Movement rate (km/2h)",
       colour = "Bear\nabundance") +
  xlim(0, 30)
g_log_wind3_2
ggsave(filename = "results/prediction_mov_rate_pred3_2_calving.png", plot = g_log_wind3_2, 
       width = 15, height = 12, units = "cm", dpi = 300)

# dem
pr_log_wind_pred4 <- ggpredict(best_fit_pred, terms = c("dem", "wolverine"),
                               typical = "median", condition = c(landcover_cc = "coniferous forests"))
g_log_wind4 <- pr_log_wind_pred4 %>% 
  plot(show.title = F, show.x.title = F, col = colswolv, limit = c(0, .6)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Elevation (m)",
       y = "Movement rate (km/2h)",
       colour = "Wolverine\nabundance") +
  xlim(NA, 650)
g_log_wind4
ggsave(filename = "results/prediction_mov_rate_pred4_calving.png", plot = g_log_wind4, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_log_wind_pred4_2 <- ggpredict(best_fit_pred, terms = c("dem", "bear"),
                                 typical = "median", condition = c(landcover_cc = "coniferous forests"))
g_log_wind4_2 <- pr_log_wind_pred4_2 %>% 
  plot(show.title = F, show.x.title = F, col = colsbear, limit = c(0, .45)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Elevation (m)",
       y = "Movement rate (km/2h)",
       colour = "Bear\nabundance") +
  xlim(NA, 650)
g_log_wind4_2
ggsave(filename = "results/prediction_mov_rate_pred4_2_calving.png", plot = g_log_wind4_2, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_log_wind_pred_house <- ggpredict(best_fit_pred, terms = c("house_dist"),
                                    typical = "median", condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_pred_house$x <- pr_log_wind_pred_house$x/1000
g_log_wind_house <- pr_log_wind_pred_house %>% 
  plot(show.title = F, show.x.title = F, col = cols, lim = c(0, .45)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Distance to houses (km)",
       y = "Movement rate (km/2h)")
g_log_wind_house
ggsave(filename = "results/mov_rate_predicted_prob_pred_house_calving.png", plot = g_log_wind_house, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_log_wind_pred_pl <- ggpredict(best_fit_pred, terms = c("pl_dist"),
                                      typical = "median", condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_pred_pl$x <- pr_log_wind_pred_pl$x/1000
g_log_wind_pl <- pr_log_wind_pred_pl %>% 
  plot(show.title = F, show.x.title = F, col = cols, lim = c(0, .45)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Distance to power lines (km)",
       y = "Movement rate (km/2h)")
g_log_wind_pl
ggsave(filename = "results/mov_rate_predicted_prob_pred_pl_calving.png", plot = g_log_wind_pl, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_log_wind_pred_trail_dist <- ggpredict(best_fit_pred, terms = c("trail_dist"),
                                     typical = "median", condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_pred_trail_dist$x <- pr_log_wind_pred_trail_dist$x/1000
g_log_wind_trail_dist <- pr_log_wind_pred_trail_dist %>% 
  plot(show.title = F, show.x.title = F, col = cols, lim = c(0, .6)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Distance to trails (km)",
       y = "Movement rate (km/2h)") +
  xlim(0, 30)
g_log_wind_trail_dist
ggsave(filename = "results/mov_rate_predicted_prob_pred_trail_dist_calving.png", plot = g_log_wind_trail_dist, 
       width = 18, height = 8, units = "cm", dpi = 300)

# roads
pr_log_wind_pred6 <- ggpredict(best_fit_pred, terms = c("public_road_dist", "wolverine"),
                               typical = "median", condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_pred6$x <- pr_log_wind_pred6$x/1000
g_log_wind6 <- pr_log_wind_pred6 %>%
  plot(show.title = F, show.x.title = F, col = colswolv, limit = c(0, 0.6)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Distance to roads (km)",
       y = "Movement rate (km/2h)",
       colour = "Wolverine\nabundance")
g_log_wind6

pr_log_wind_pred6_2 <- ggpredict(best_fit_pred, terms = c("public_road_dist", "bear"),
                                 typical = "median", condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_pred6_2$x <- pr_log_wind_pred6_2$x/1000
g_log_wind6_2 <- pr_log_wind_pred6_2 %>%
  plot(show.title = F, show.x.title = F, col = colsbear, limit = c(0, 0.45)) +
  # geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  labs(x = "Distance to roads (km)",
       y = "Movement rate (km/2h)",
       colour = "Bear\nabundance")
g_log_wind6_2

# combine
# combine plots
g_pred_combine1 <- ggpubr::ggarrange(g_log_wind_pred2, 
                                     g_log_wind6 + ylab(""),
                                     g_log_wind3 + ylab(""),
                                     g_log_wind4 + ylab(""),
                                     labels = c("A", "B", "C", "D"), 
                                     common.legend = T, legend = "top", 
                                     ncol = 4, nrow = 1)

g_pred_combine2 <- ggpubr::ggarrange(g_log_wind_pred, 
                                     g_log_wind6_2 + ylab(""),
                                     g_log_wind3_2 + ylab(""),
                                     g_log_wind4_2 + ylab(""),
                                     labels = c("E", "F", "G", "H"), 
                                     common.legend = T, legend = "bottom", 
                                     ncol = 4, nrow = 1)

g_phase_combine <- ggpubr::ggarrange(g_pred_combine1,
                                     g_pred_combine2, 
                                     ncol = 1, nrow = 2)
g_phase_combine
ggsave(filename = "results/prediction_mov_rate_pred_combined_calving.png", plot = g_phase_combine, 
       width = 30, height = 15, units = "cm", dpi = 300)
