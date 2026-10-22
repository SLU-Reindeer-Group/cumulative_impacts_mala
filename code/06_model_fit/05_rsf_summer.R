#' ---
#' title: 'Habitat selection analysis of Swedish reindeer during summer at regional scale: RSF at Mala sameby'
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

# library(devtools)
# devtools::install_github("phuais/multifitR")

#-----------
#' # summer analysis
#' 
#' ## Load annotated and clean movement data

# vindval data
load(file = "data/01_GPS/05_annotated_data/data_annotated_summer_ready_2h_rsf.RData")
data_ready_summer_rsf_2h

#-----------------------
# Fit

# weights - not used
data_ready_summer_rsf_2h$w_obs <- ifelse(data_ready_summer_rsf_2h$case_, 1, 5000)

data_ready_summer_rsf_2h <- data_ready_summer_rsf_2h |> 
  dplyr::mutate(landcover_cc_grouped = forcats::fct_collapse(landcover_cc, 
                                                             "deciduous forests" = c("deciduous forests", "forested wetlands"),
                                                             "open lands" = c("open lands", "clear cuts")),
                landcover_cc_grouped = relevel(landcover_cc_grouped, ref = "deciduous forests"))
levels(data_ready_summer_rsf_2h$landcover_cc_grouped)

summary(data_ready_summer_rsf_2h$wind_turbines_dist_comb)
#hist(data_ready_summer_rsf_2h$wind_turbines_dist_comb+1, breaks = 100, xlim = c(0, 10000))

#-------
# GLM, no threshold regression

base_formula <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(sos) + 
  log(power_lines_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + 
  log(trails_dist+1) + 
  log(house_dist+1) + log(mining_Krist_dist+1) +
  wf_name + landcover_cc_grouped*phase*log(wind_turbines_dist_comb+1)

fit1 <- glm(update(base_formula, ~ . + tpi_cat_s150m), 
            data = data_ready_summer_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))

fit1$formula
summary(fit1)
car::vif(fit1)

# backward deletion - ignore it for now
base_formula2 <- base_formula #update(base_formula, ~ . - cos_aspect)
fit2 <- glm(update(base_formula2, ~ . + tpi_cat_s150m),
            data = data_ready_summer_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))

fit2$formula
summary(fit2)
car::vif(fit2)

# compare TPI
fit2.2 <- glm(update(base_formula2, ~ . + tpi_cat_s250m), 
            data = data_ready_summer_rsf_2h, #weights = w_obs,
            family = binomial(link = "logit"))

fit2.3 <- glm(update(base_formula2, ~ . + tpi_cat_s510m), 
              data = data_ready_summer_rsf_2h, #weights = w_obs,
              family = binomial(link = "logit"))

# compare
(AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, weights = T, base = T))

# compare viewshed and landcover_cc_grouped
# formula_summer3 <- update(base_formula2, ~ . + tpi_cat_s150m - 
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
#             data = data_ready_summer_rsf_2h, #weights = w_obs,
#             family = binomial(link = "logit"))
# fit3.2 <- glm(base_formula3.2,
#             data = data_ready_summer_rsf_2h, #weights = w_obs,
#             family = binomial(link = "logit"))
# fit3.3 <- glm(base_formula3.3,
#             data = data_ready_summer_rsf_2h, #weights = w_obs,
#             family = binomial(link = "logit"))
# 
# car::vif(fit3)
# summary(fit3)
# 
# # compare
# (AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, 
#                                 fit3, fit3.2, fit3.3, weights = T, base = T))

# best model
best_fit <- fit2.3
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
                data = data_ready_summer_rsf_2h, #weights = w_obs,
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

#save(fits, file = "output/summer_multi_zoi_table.rda")
#load("output/summer_multi_zoi_table.rda")
fits

# compare models
(tab_models <- fits |>
  dplyr::mutate(mining = as.character(mining)) |>
  dplyr::bind_rows(
    tibble::tibble(mining = "log", wind = "log", 
                   tpi = "tpi_cat_s510m",
                   lc = "landcover_cc_grouped",
                   aic = AIC(best_fit),
                   definition = paste(as.character(best_fit$formula)[c(2,1,3)], collapse = " "))) |>
  dplyr::mutate(dAIC = abs(aic - min(aic))) |>
  dplyr::relocate(dAIC, .after = aic) |> 
  dplyr::arrange(dAIC))

tab_models |>
  readr::write_csv(file = "output/rsf_results/rsf_summer_AIC_table_complete.csv")

# save(tab_models, file = "output/rsf_summer_multi_zoi_table_landcover_grouped.rda")

#---------------
# Re-run best model
load("output/rsf_summer_multi_zoi_table_landcover_grouped.rda")
tab_models

form <- tab_models$definition[2]
best_fit <- glm(form, 
                data = data_ready_summer_rsf_2h, #weights = w_obs,
                family = binomial(link = "logit"))

#---------------
# More info, summary, and predictions on the best model

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
ggsave("rsf_summer_coefs_log_lc_grouped.png", plot = m1_plot, path = "output/rsf_results//", 
       width = 20, height = 20, units = "cm", dpi = 300)

model_call <- sapply(attr(AICvals_phase, "row.names"), function(x) paste(as.character(formula(eval(as.symbol(x))))[c(2,1,3)], collapse = " "))
AICvals_phase %>% 
  dplyr::as_tibble(rownames = "model") %>%
  dplyr::mutate(definition = unname(model_call)) %>%
  readr::write_csv(file = "output/rsf_results//rsf_summer_AIC_table_log_lc_grouped.csv")

summary(best_fit)
coefs <- broom::tidy(best_fit) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0)),
                p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
coefs %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  readr::write_csv(file = "output/rsf_results//rsf_summer_coef_table_log_lc_grouped.csv")


#-------------------------------
# predictions

cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))
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
# g_wind_general <- pr_wind_general %>% 
#   plot(limits = c(0,1), show_title = F, show_x_title = F, col = cols) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to wind turbines (km)",
#        y = "Predicted probability of use",
#        colour = "Phase") +
#   xlim(0, 10)
# ggsave(filename = "results/summer_wind_phase_general.png", plot = g_wind_general, 
#        width = 15, height = 12, units = "cm", dpi = 300)

# wind dist, phase, and land cover, for ytteberg
pr_wind_phase <- ggpredict(best_fit,  terms = c("wind_turbines_cumulative10000", "phase", "landcover_cc_grouped"),
                           typical = "median", condition = c(wf_name = "ytteberg"))
#, condition = c(landcover_cc_grouped = "coniferous forests"))
# zoi_limit <- 0.05; radius <- 10000
# lambda <- log(1/zoi_limit) / radius
# pr_wind_phase$x <- -log(pr_wind_phase$x)/lambda/1000
# pr_wind_phase_df <- pr_wind_phase[sample.int(nrow(pr_wind_phase), size = 0.1*nrow(pr_wind_phase)),]

g_wind <- pr_wind_phase |> #_df |>
  plot(show_title = FALSE, 
       colors = cols) +
  # geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Density of wind turbines",
       y = "Predicted probability of use",
       colour = "Phase")
  # ylim(0.05, 0.2) +
  # xlim(0, 5)
g_wind

ggsave(filename = "output/rsf_results//summer_wind_phase_landcc_ytte_density.png", plot = g_wind,
       width = 20, height = 20, units = "cm", dpi = 300)

#---
# coming back to distance scale
zoi_limit <- 0.05; radius <- 10000
# lambda1 <- log(1/zoi_limit) / radius1
lambda <- log(1/zoi_limit) / radius
# lr2$df$wind_turbines_cumulative5000_end_construction_scaled_x1 <- -log(lr2$df$wind_turbines_cumulative5000_end_construction_scaled_x1*ss + mm + 0.001)/lambda/1000
# lr2_ci_se$df$wind_turbines_cumulative5000_end_construction_scaled_x1 <- -log(lr2_ci_se$df$wind_turbines_cumulative5000_end_construction_scaled_x1*ss + mm + 0.000001)/lambda/1000

df_base <- data.frame(wind_dist = seq(0, 40000, length.out = 4001))
zoi_rad <- 10000
zoi_shape <- "exp_decay"
df_base$zoi_n1 <- 1*oneimpact::dist_decay(df_base$wind_dist, radius = zoi_rad, type = zoi_shape)
df_base$zoi_n10 <- 20*oneimpact::dist_decay(df_base$wind_dist, radius = zoi_rad, type = zoi_shape)
summary(df_base)
summary(pr_wind_phase$x)

#' Function to get the closes match between two vectors
#' 
#' @param x,y numeric. The two vectors.
#' @param return character. If `"value"`, returns the values of y that are
#' closest to the values in x. If `"index"`, returns the indices of the values
#' in y that are closest to the values in x. 
get_closest_match <- function(x, y, return = c("value", "index")[1]) {
  
  # Initialize a vector to store the closest matches
  closest_matches <- numeric(length(x))
  
  # Loop through vector1 and find the closest match in vector2
  for (i in seq_along(x)) {
    current_value <- x[i]
    closest_index <- which.min(abs(y - current_value))
    if(return == "value") closest_matches[i] <- y[closest_index]
    else if(return == "index") closest_matches[i] <- closest_index
  }
  
  closest_matches
}
vector1 <- c(1, 3, 7, 10)
vector2 <- c(0, 2, 6, 8, 11)
get_closest_match(vector1, vector2)
get_closest_match(vector1, vector2, return = "index")

# get rows of closes match
pr_wind_phase_dist <- pr_wind_phase[sample.int(nrow(pr_wind_phase), size = 0.1*nrow(pr_wind_phase)),]

rows_n1 <- get_closest_match(pr_wind_phase_dist$x, 
                             df_base$zoi_n1, return = "index")
rows_n10 <- get_closest_match(pr_wind_phase_dist$x, 
                              df_base$zoi_n10, return = "index")

# get distance
pr_wind_phase_dist$dist_wind_n1 <- df_base$wind_dist[rows_n1]/1000
pr_wind_phase_dist$dist_wind_n10 <- df_base$wind_dist[rows_n10]/1000

pr_wind_phase_dist <- tibble::as_tibble(pr_wind_phase_dist)

# plot construction
g_wind_dist <- pr_wind_phase_dist |> 
  dplyr::filter(dist_wind_n10 > 0) |> 
  tidyr::pivot_longer(cols = starts_with("dist_wind_"), 
                      names_to = "density_turbines", names_prefix = "dist_wind_n", 
                      values_to = "dist_wind") |> 
  dplyr::mutate(density_turbines = paste("n =", density_turbines)) |>
  dplyr::filter(!(density_turbines == "n = 1" & dist_wind == 0)) |> 
  dplyr::rename(Phase = group) |> 
  ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray30") +
  geom_ribbon(aes(x = dist_wind, ymin = conf.low, ymax = conf.high,
                  color = Phase, fill = Phase), alpha = 0.25) +
  geom_line(aes(x = dist_wind, y = predicted, color = Phase), linewidth = 1) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  facet_wrap(density_turbines ~ facet, nrow = 2) +
  labs(x = "Distance from wind turbines (km)", 
       y = "Predicted probability of use") +
  xlim(0, 10) +
  ylim(0, 1) +
  # ylim(-2,2) +
  theme_bw()
g_wind_dist

ggsave(filename = "summer_wind_phase_landcc_ytte_dist.png", plot = g_wind_dist, 
       path = "output/rsf_results/",
       width = 25, height = 25, units = "cm", dpi = 300)

# zoi_limit <- 0.05; radius <- 10000
# lambda <- log(1/zoi_limit) / radius
# pr_wind_phase$x <- -log(pr_wind_phase$x)/lambda/1000
# pr_wind_phase_df <- pr_wind_phase[sample.int(nrow(pr_wind_phase), size = 0.1*nrow(pr_wind_phase)),]
# 
# g_wind <- pr_wind_phase_df |>
#   plot(show_title = FALSE, 
#        colors = cols) +
#   # geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "ZOI of wind turbines (m)",
#        y = "Predicted probability of use",
#        colour = "Phase") +
#   # ylim(0.05, 0.2) +
#   xlim(NA, 10)
# g_wind
# 
# ggsave(filename = "output/rsf_results//summer_wind_phase_landcc_ytte_dist_negative.png", plot = g_wind,
#        width = 20, height = 20, units = "cm", dpi = 300)
# 
# 
# # wind dist, phase, and land cover, for places closer to each of the WFs
# wfs <- levels(data_ready_summer_rsf_2h$wf_name)
# for(i in wfs) {
#   
#   print(i)
#   pr_wind_phase <- ggpredict(best_fit, terms = c("wind_turbines_cumulative10000", "phase", "landcover_cc_grouped"),
#                              typical = "median", condition = c(wf_name = i))
#   #, condition = c(landcover_cc_grouped = "coniferous forests"))
#   zoi_limit <- 0.05; radius <- 10000
#   lambda <- log(1/zoi_limit) / radius
#   pr_wind_phase$x <- -log(pr_wind_phase$x)/lambda/1000
#   pr_wind_phase_df <- pr_wind_phase[sample.int(nrow(pr_wind_phase), size = 0.1*nrow(pr_wind_phase)),]
#   
#   g_wind <- pr_wind_phase_df |>
#     plot(show_title = FALSE, colors = cols) +
#     geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#     geom_vline(xintercept = 5, linetype = 2, color = "grey") +
#     labs(x = "Distance to wind turbines (m)",
#          y = "Relative probability of use",
#          colour = "Phase") +
#     #ylim(0, 1) +
#     xlim(0, 10)
#   g_wind
#   
#   ggsave(filename = paste0("output/rsf_results//rsf_summer_wind_phase_landcc_grouped_", i, ".png"), plot = g_wind, 
#          width = 20, height = 20, units = "cm", dpi = 300)
#   
# }

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
ggsave(filename = "rsf_summer_mining_lc_grouped.png", plot = g_mining, 
       path = "output/rsf_results//",
       width = 15, height = 12, units = "cm", dpi = 300)

# roads
pr_public_road <- ggpredict(best_fit, terms = c("public_road_dist"))
pr_public_road$x <- pr_public_road$x/1000
g_road <- pr_public_road %>%
  plot(show_title = F, show_x_title = F,
       limits = c(0,NA), 
       #colors = cols
       ) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to large roads (km)",
       y = "Predicted probability of use",
       colour = "Phase") +
  xlim(0, 3)
g_road
ggsave(filename = "rsf_summer_roads.png", plot = g_road,
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# error with NAs
# pr_priv_road <- ggpredict(best_fit, terms = c("priv_road_dist"))
# pr_priv_road$x <- pr_priv_road$x/1000
# g_prov_road <- pr_priv_road %>% 
#   plot(show_title = F, show_x_title = F,
#        limits = c(0,1), colors = cols) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to small roads (km)",
#        y = "Predicted probability of use",
#        colour = "Phase") +
#   xlim(0, 1)
# ggsave(filename = "rsf_summer_private_roads.png", plot = g_prov_road, 
#        path = "output/01_traditional_fit/",
#        width = 15, height = 12, units = "cm", dpi = 300)

#tab <- do.call(cbind.data.frame, attr(pr_priv_road, "constant.values"))
#df <- data.frame(priv_road_dist = pr_priv_road$x*1000, tab)
#df <- data.frame(priv_road_dist = 1, tab)
#pred <- predict(best_fit, df, type = "link")
#exp(log(df$priv_road_dist+1)*-0.037344)

# power lines
pr_pl <- ggpredict(best_fit, terms = c("power_lines_dist"))
pr_pl$x <- pr_pl$x/1000
g_pl <- pr_pl %>% 
  plot(show_title = F, show_x_title = F,
       limits = c(0, NA), 
       #colors = cols
       ) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to power lines (km)",
       y = "Predicted probability of use") +
  xlim(0, 10)
g_pl
ggsave(filename = "rsf_summer_pl.png", plot = g_pl, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# trails
pr_trail <- ggpredict(best_fit, terms = c("trails_dist"))
pr_trail$x <- pr_trail$x/1000
g_trail <- pr_trail %>% 
  plot(show_title = F, show_x_title = F,
       limits = c(0, NA), 
       #colors = cols
       ) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to trails (km)",
       y = "Predicted probability of use") +
  xlim(0, 5)
g_trail
ggsave(filename = "rsf_summer_trails.png", plot = g_trail,
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# houses
pr_house <- ggpredict(best_fit, terms = c("house_dist"))
pr_house$x <- pr_house$x/1000
g_house <- pr_house %>% 
  plot(show_title = F, show_x_title = F,
       limits = c(0, NA), 
       #colors = cols
       ) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to houses (km)",
       y = "Predicted probability of use") +
  xlim(0, 5)
g_house
ggsave(filename = "rsf_summer_houses.png", plot = g_trail, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# elevation
pr_elevation <- ggpredict(best_fit, terms = c("dem"))
g_elevation <- pr_elevation %>% 
  plot(show_title = F, show_x_title = F,
       limits = c(0, NA), 
       #colors = cols
       ) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Elevation (m)",
       y = "Predicted probability of use",
       color = "Phase")# + 
  # xlim(0, NA)
g_elevation
ggsave(filename = "rsf_summer_elevation.png", plot = g_elevation, 
       path = "output/rsf_results//",
       width = 15, height = 12, units = "cm", dpi = 300)

# tpi
pr_tpi <- ggpredict(best_fit, terms = c("tpi_cat_s510m"))
g_tpi <- pr_tpi %>% 
  plot(show_title = F, show_x_title = F,
       limits = c(0, NA), 
       # colors = cols
       ) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Terrain position index category (150m)",
       y = "Predicted probability of use",
       color = "Phase") 
ggsave(filename = "rsf_summer_tpi.png", plot = g_tpi, 
       path = "output/01_traditional_fit/",
       width = 15, height = 12, units = "cm", dpi = 300)

# SOS
# pr_sos <- ggpredict(best_fit, terms = c("sos"))
# g_sos <- pr_sos %>% 
#   plot(show_title = F, show_x_title = F,
#        limits = c(0, NA), 
#        # colors = cols
#   ) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Start of growing season (doy)",
#        y = "Predicted probability of use") 
# g_sos
# ggsave(filename = "rsf_summer_tpi.png", plot = g_tpi, 
#        path = "output/01_traditional_fit/",
#        width = 15, height = 12, units = "cm", dpi = 300)

#-------
# spatial prediction

install.load::install_load("raster", "rasterVis", "terra", "arrow")

# vindval data
load(file = "data/01_GPS/05_annotated_data/data_annotated_summer_ready_2h_rsf.RData")

data_ready_summer_rsf_2h <- data_ready_summer_rsf_2h |> 
  dplyr::mutate(landcover_cc_grouped = forcats::fct_collapse(landcover_cc, 
                                                             "deciduous forests" = c("deciduous forests", "forested wetlands"),
                                                             "open lands" = c("open lands", "clear cuts")),
                landcover_cc_grouped = relevel(landcover_cc_grouped, ref = "deciduous forests"))
levels(data_ready_summer_rsf_2h$landcover_cc_grouped)

# model comparison
load("output/rsf_summer_multi_zoi_table_landcover_grouped.rda")

# compare models
tab_models

#---------------
# Re-run best model
form <- tab_models$definition[2]
best_fit <- glm(form, 
                data = data_ready_summer_rsf_2h, #weights = w_obs,
                family = binomial(link = "logit"))

car::vif(best_fit) |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "term") |> 
  tibble::as_tibble() |> 
  readr::write_csv("output/rsf_summer_vif_table.csv")

best_fit$rank
data_ready_summer_rsf_2h$Object_ID |> unique() |> length()

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

template_map <- terra::vect("maps/availability_mala_late_summer.gpkg")

# set sos and eos to match phase
maps_before_df <- maps_df |> 
  dplyr::rename(eos=eos_before, sos=sos_before)
maps_before_df$phase <- ifelse(maps_before_df$phase == "Operation", "Before construction", maps_before_df$phase) |> 
  factor(levels = levels(maps_df$phase))
unique(maps_before_df$phase)
# predict
pred_summer_before_construction <- predict(best_fit, maps_before_df, type = "response")
# create raster
maps_before_df$pred_sum_before <- pred_summer_before_construction
str(maps_before_df)
# fill raster with predicted values
map_pred_summer_before_construction <- maps_before_df |> 
  dplyr::select(x, y, pred_sum_before) |> 
  terra::rast(type = "xyz", crs = terra::crs(template_map)) |> 
  terra::mask(mask = template_map)

names(map_pred_summer_before_construction) <- "summer_before_construction"
terra::plot(map_pred_summer_before_construction)
terra::writeRaster(map_pred_summer_before_construction, filename = "output/rsf_spatial_predictions/rsf_map_prediction_summer_before_construction_lc_grouped.tif", 
                   gdal = "COMPRESS=DEFLATE", overwrite = TRUE)

rm(pred_summer_before_construction)
rm(maps_before_df)

# summer during/construction --------------------------------

# set sos and eos to match phase
maps_during_df <- maps_df |>
  dplyr::rename(eos=eos_during, sos=sos_during) # remove differences due to nvdi
# dplyr::rename(eos=eos_during, sos=sos_during)
maps_during_df$phase <- ifelse(maps_during_df$phase == "Operation", "Construction", maps_during_df$phase) |> 
  factor(levels = levels(maps_df$phase))
unique(maps_during_df$phase)
# predict
pred_summer_construction <- predict(best_fit, maps_during_df, type = "response")
# create raster
maps_during_df$pred_sum_during <- pred_summer_construction
str(maps_during_df)
# fill raster with predicted values
map_pred_summer_construction <- maps_during_df |> 
  dplyr::select(x, y, pred_sum_during) |> 
  terra::rast(type = "xyz", crs = terra::crs(template_map)) |> 
  terra::mask(mask = template_map)

names(map_pred_summer_construction) <- "summer_construction"
terra::plot(map_pred_summer_construction)
terra::writeRaster(map_pred_summer_construction, filename = "output/rsf_spatial_predictions/rsf_map_prediction_summer_construction_lc_grouped.tif", 
                   gdal = "COMPRESS=DEFLATE", overwrite = TRUE)

rm(pred_summer_construction)
rm(maps_during_df)

# summer operation/after --------------------------------

# set sos and eos to match phase
maps_after_df <- maps_df |> 
  dplyr::rename(eos=eos_after, sos=sos_after) # remove differences due to nvdi
# dplyr::rename(eos=eos_after, sos=sos_after)
unique(maps_after_df$phase)
# predict
pred_summer_operation <- terra::predict(best_fit, maps_after_df, type = "response")
# create raster
maps_after_df$pred_sum_after <- pred_summer_operation
str(maps_after_df)
# fill raster with predicted values
map_pred_summer_operation <- maps_after_df |> 
  dplyr::select(x, y, pred_sum_after) |> 
  terra::rast(type = "xyz", crs = terra::crs(template_map)) |> 
  terra::mask(mask = template_map)

names(map_pred_summer_operation) <- "summer_operation"
terra::plot(map_pred_summer_operation)
terra::writeRaster(map_pred_summer_operation, filename = "output/rsf_spatial_predictions/rsf_map_prediction_summer_operation_lc_grouped.tif", 
                   gdal = "COMPRESS=DEFLATE", overwrite = TRUE)

rm(pred_summer_operation)
rm(maps_after_df)

#----
# plot maps

install.load::install_load("terra", "arrow")
library(tmap)

# read maps
f_m <- list.files("output/rsf_spatial_predictions/", pattern = "summer", full.names = TRUE) |> 
  grep(pattern = ".tif$", value = TRUE) |> 
  grep(pattern = "lc_grouped", value = TRUE) |> 
  grep(pattern = "difference", invert = TRUE, value = TRUE)

maps_summer <- terra::rast(f_m)

# plot
# png("results/prediction_map_rsf_phase.png", width = 20, height = 20, units = "cm", res = 300)
# plot(maps_summer)
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
# png("results/prediction_map_rsf_phase_summer.png", width = 30, height = 11, units = "cm", res = 300)
# plt <- maps_summer |> 
#   rasterVis::levelplot(par.settings = rasterTheme(region = rev(terrain.colors(n = 100))),
#                        names.attr = names_phase, layout = c(3,1))
# plt + latticeExtra::layer(sp.points(as(turbines[1], "Spatial"), 
#                                     col = "black", cex = 0.4, pch = 4))
# dev.off()

# library(tmaptools)
# tmaptools::palette_explorer()

tmap_options(max.raster = c(plot = 5000, view = 2000))

map_before <- maps_summer[[1]]
map_before$summer_before_construction@pntr$range_max
p_maps_summer_before <- tmap::tm_shape(map_before) +
  tmap::tm_raster(palette = "Greens", style = "cont", title = "") +#breaks = seq(0, 0.6, by = 0.05)) +
  tmap::tm_layout(title = "Baseline habitat suitability in summer\n(before wind turbines)", legend.outside = TRUE)
p_maps_summer_before

# difference summer ####
after_minus_before <- terra::diff(maps_summer[[c(1,3)]])
names(after_minus_before) <- "Before construction -> Operation"
# plot(after_minus_before)
during_minus_before <- terra::diff(maps_summer[[c(1,2)]])
names(during_minus_before) <- "Before construction -> Construction"

# absolute difference RSF
maps_summer_diff <- c(during_minus_before, after_minus_before)
tmap::tm_shape(maps_summer_diff) +
  tmap::tm_raster(palette = "PRGn", style = "cont") +
  tmap::tm_layout()

maps_summer_prop <- 100*maps_summer_diff/maps_summer[[1]]
# maps_summer_prop <- terra::ifel(maps_summer_prop > )
p_maps_summer_diff <- tmap::tm_shape(maps_summer_prop) +
  tmap::tm_raster(palette = "PRGn", #style = "quantile")
                  breaks = c(-75, -50, -30, -10, 0, 10, 30, 50, 75),
                  title = "Change in suitability (%)") +
  tmap::tm_facets(ncol = 2)
p_maps_summer_diff

p_maps_summer <- tmap::tmap_arrange(p_maps_summer_before, p_maps_summer_diff,
                                     ncol = 1, nrow = 2)
p_maps_summer

# Add polygon mining and polygon 5km + crosses for turbines in maps
# Add scale and north
tmap::tmap_save(p_maps_summer, filename = "output/rsf_results/rsf_maps_summer_lc_grouped.png", 
                width = 20, height = 20, units = "cm", dpi = 300)

# write maps difference
maps_summer_diff |> 
  terra::writeRaster(filename = "output/rsf_spatial_predictions/rsf_map_difference_rsf_summer_lc_grouped.tif",
                     gdal = "COMPRESS=DEFLATE", overwrite = TRUE)

maps_summer_prop |> 
  terra::writeRaster(filename = "output/rsf_spatial_predictions/rsf_map_difference_prop_summer_lc_grouped.tif",
                     gdal = "COMPRESS=DEFLATE", overwrite = TRUE)











