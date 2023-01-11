#' ---
#' title: 'Habitat selection analysis of Swedish reindeer during calving at regional scale: RSF at Mala sameby'
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
install.load::install_load("amt")

#-----------
#' # Calving analysis
#' 
#' ## Load annotated and clean movement data

# vindval data
load(file = "data/01_GPS/05_annotated_data/vindval_data_annotated_calving_ready_2h_rsf.RData")
vindval_data_ready_calving_rsf_2h <- data_ready_calving_rsf_2h

# limited viewshed for amliden
dist_cutoff <- 1.5e4
wfs <- c("jokkmokksliden", "storliden", "ytteberg", "amliden")
vindval_data_ready_calving_rsf_2h <- vindval_data_ready_calving_rsf_2h %>% 
  dplyr::mutate(viewshed_aml15km = ifelse(wind_dist_aml >  dist_cutoff, 0, viewshed_amliden_mala_binary),
                cos_aspect = cos(aspect),
                wind_dist_comb = pmin(wind_dist_jsy, wind_dist_aml),
                dist_wf_closest = apply(cbind(wind_dist_jokk, wind_dist_stor, wind_dist_ytte, wind_dist_aml), 1, which.min),
                wf_name = factor(dist_wf_closest, levels = 1:4, labels = wfs))

# apply(cbind(data_ready_calving_rsf_2h$wind_dist_jokk, data_ready_calving_rsf_2h$wind_dist_stor), MARGIN = 1, which.min)
vindval_data_ready_calving_rsf_2h$wf_name

# keep 9 random points for each used point
table(vindval_data_ready_calving_rsf_2h$id, vindval_data_ready_calving_rsf_2h$case_)
plot(vindval_data_ready_calving_rsf_2h$case_[1:10000])

n_points <- 9 + 1
vindval_data_ready_calving_rsf_2h_9pt <- vindval_data_ready_calving_rsf_2h %>% 
  plyr::ddply(., "id",
              function(x) {
                n <- sum(x$case_ == 1)
                return(x[1:(n_points*n),])
              }, .progress = "text") %>% 
  tibble::as_tibble()
vindval_data_ready_calving_rsf_2h_9pt
plot(vindval_data_ready_calving_rsf_2h_9pt$case_[1:10000])
table(vindval_data_ready_calving_rsf_2h_9pt$id, vindval_data_ready_calving_rsf_2h_9pt$case_)
vindval_data_ready_calving_rsf_2h <- vindval_data_ready_calving_rsf_2h_9pt
vindval_data_ready_calving_rsf_2h$year %>% table

# calving
load(file = "data/01_GPS/05_annotated_data/data_annotated_calving_ready_2h_rsf.RData")
data_ready_calving_rsf_2h

levels(data_ready_calving_rsf_2h$landcover)

#-----------
# check correlation
data_corr <- data_ready_calving_rsf_2h %>% 
  dplyr::slice(sample(1:nrow(.), 5000, replace = F))

g_corr <- data_corr %>% 
  dplyr::select(dem,
                wind_dist, wind_dist_jsy,
                wind_dist_aml,
                viewshed_lc, viewshed_jsy15km, 
                viewshed_aml15km,
                mining_dist, mining_dist_Krist,
                house_dist, public_road_dist, 
                priv_road_dist, pl_dist, trail_dist) %>% 
  dplyr::mutate(viewshed_lc = as.numeric(viewshed_lc)) %>% 
  ggpairs()
# g_corr_dem
ggsave("g_corr_dataset_rsf.png", plot = g_corr, path = "output", 
       width = 40, height = 40, units = "cm", dpi = 300)

#-----------
#' ## Analysis for calving with phase
#' 
#' Pooled data
#' Period: 2008-2011, 2015-2018

# correlated:
# formula_calving <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
#   log(pl_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_dist+1) +
#   log(trail_dist+1) + log(house_dist+1) +
#   landcover_cc*phase*log(wind_dist_jsy+1) +
#   landcover_cc*phase*log(wind_dist_aml+1)

# defining weights to be able to consider the binomial GLM as a IPP model
# define weight = 1 for used locations and > 1000 (here, 5000) for available locations
# according to Fieberg et al. 2021. 
# Fieberg, J, Signer, J, Smith, B, Avgar, T. A ‘How to’ guide for interpreting 
# parameters in habitat‐selection analyses. J Anim Ecol. 2021; 90: 1027– 1043.
# https://doi.org/10.1111/1365‐2656.13441 

vindval_data_ready_calving_rsf_2h$w <- ifelse(vindval_data_ready_calving_rsf_2h$case_, 1, 5000)
data_ready_calving_rsf_2h$w <- ifelse(data_ready_calving_rsf_2h$case_, 1, 5000)

formula_calving <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  log(pl_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_dist+1) +
  log(trail_dist+1) + log(house_dist+1) +
  landcover_cc*phase*log(wind_dist_comb+1)+ wf_name

formula_calving <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  scale(power_lines_nearest500) + scale(priv_road_nearest500) + scale(public_road_nearest500) + 
  scale(mining_all_nearest500) +
  scale(trails_nearest500) + scale(house_nearest500) +
  landcover_cc*phase*scale(wind_turbines_nearest500)+ wf_name

# landcover vs viewshed
fit1 <- glm(update(formula_calving, ~ . + tpi_cat_s150m), 
            data = data_ready_calving_rsf_2h, weights = w,
            family = binomial(link = "logit"))

fit1_v <- glm(update(formula_calving, ~ . + tpi_cat_s150m), 
            data = vindval_data_ready_calving_rsf_2h, weights = w,
            family = binomial(link = "logit"))

fit1$formula
summary(fit1)
car::vif(fit1)

# backward deletion
# nothing to remove
formula_calving2 <- formula_calving
  # update(formula_calving, ~ . - log(public_road_dist + 1))
fit2 <- glm(update(formula_calving2, ~ . + tpi_cat_s150m),
            data = data_ready_calving_rsf_2h, #weights = w,
            family = binomial(link = "logit"))

fit2$formula
summary(fit2)
car::vif(fit2)

# compare TPI
fit2.2 <- glm(update(formula_calving2, ~ . + tpi_cat_s250m), 
            data = data_ready_calving_rsf_2h, #weights = w,
            family = binomial(link = "logit"))

fit2.3 <- glm(update(formula_calving2, ~ . + tpi_cat_s510m), 
              data = data_ready_calving_rsf_2h, #weights = w,
              family = binomial(link = "logit"))

# compare
(AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, weights = T, base = T))

# compare viewshed and landcover_cc
# formula_calving3 <- update(formula_calving2, ~ . + tpi_cat_s150m - 
#                              landcover_cc*phase*log(wind_dist_jsy+1) +
#                              viewshed_lc*phase*log(wind_dist_jsy+1))
formula_calving3 <- update(formula_calving2, ~ . + tpi_cat_s150m -
                             landcover_cc*phase*log(wind_dist_comb+1) +
                             viewshed_lc*phase*log(wind_dist_comb+1))

fit3 <- glm(formula_calving3,
            data = data_ready_calving_rsf_2h, #weights = w,
            family = binomial(link = "logit"))

car::vif(fit3)

# compare
(AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, fit3, weights = T, base = T))

#---------------
# most parsimonious model
best_fit <- fit2

summary(best_fit)

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
ggsave("calving_coefs.png", plot = m1_plot, path = "results", 
       width = 20, height = 20, units = "cm", dpi = 300)

# save tables
# model_call <- sapply(attr(AICvals_phase, "row.names"), function(x) as.character(getCall(eval(as.symbol(x)))))[2,]
model_call <- sapply(attr(AICvals_phase, "row.names"), function(x) paste(as.character(formula(eval(as.symbol(x))))[c(2,1,3)], collapse = " "))
AICvals_phase %>% 
  dplyr::as_tibble(rownames = "model") %>%
  dplyr::mutate(definition = unname(model_call)) %>%
  readr::write_csv(file = "results/AIC_table_calving.csv")

summary(best_fit)
coefs <- broom::tidy(best_fit) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0)),
                p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
coefs %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  readr::write_csv(file = "results/coef_table_calving.csv")

#-------------------------------
# predictions

cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))

# averaged over all conditions, only slope
pr_slp <- ggpredict(best_fit, terms = "slope",
                   typical = "median") 
plot(pr_slp, lim = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey")

# only wind dist, averaged over all land cover types
# pr_wind_general <- ggpredict(best_fit, terms = c("wind_dist_comb", "phase"),
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
pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_dist_comb", "phase", "landcover_cc"),
                               typical = "median", condition = c(wf_name = "ytteberg"))
#, condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
g_log_wind <- pr_log_wind_phase %>% 
  plot(show.title = F, lim = c(0, 1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to wind turbines (m)",
       y = "Predicted probability of use",
       colour = "Phase") +
  xlim(0, 10)
ggsave(filename = "results/calving_wind_phase_lc_ytte.png", plot = g_log_wind, 
       width = 20, height = 20, units = "cm", dpi = 300)

# wind dist, phase, and land cover, for places closer to each of the WFs
for(i in wfs) {
  pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_dist_comb", "phase", "landcover_cc"),
                                 typical = "median", condition = c(wf_name = i))
  #, condition = c(landcover_cc = "coniferous forests"))
  pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
  g_log_wind <- pr_log_wind_phase %>% 
    plot(show.title = F, lim = c(0, 1), col = cols) +
    geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
    labs(x = "Distance to wind turbines (m)",
         y = "Predicted probability of use",
         colour = "Phase") +
    xlim(0, 10)
  ggsave(filename = paste0("results/calving_wind_phase_lc_", i, ".png"), plot = g_log_wind, 
         width = 20, height = 20, units = "cm", dpi = 300)
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
pr_mining <- ggpredict(best_fit, terms = c("mining_dist", "phase"))
pr_mining$x <- pr_mining$x/1000
g_mining <- pr_mining %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to mining (m)",
       y = "Predicted probability of use",
       color = "Phase") +
  xlim(0, 10)
ggsave(filename = "results/calving_mining.png", plot = g_mining, 
       width = 15, height = 12, units = "cm", dpi = 300)

# roads
pr_public_road <- ggpredict(best_fit, terms = c("public_road_dist"))
pr_public_road$x <- pr_public_road$x/1000
g_road <- pr_public_road %>%
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to large roads (m)",
       y = "Predicted probability of use",
       colour = "Phase") +
  xlim(0, 3)
ggsave(filename = "results/calving_roads.png", plot = g_road,
       width = 15, height = 12, units = "cm", dpi = 300)

pr_priv_road <- ggpredict(best_fit, terms = c("priv_road_dist"))
pr_priv_road$x <- pr_priv_road$x/1000
g_prov_road <- pr_priv_road %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to small roads (m)",
       y = "Predicted probability of use",
       colour = "Phase") +
  xlim(0, 1)
ggsave(filename = "results/calving_private_roads.png", plot = g_prov_road, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_pl <- ggpredict(best_fit, terms = c("pl_dist"))
pr_pl$x <- pr_pl$x/1000
g_pl <- pr_pl %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to power lines (m)",
       y = "Predicted probability of use") +
  xlim(0, 10)
ggsave(filename = "results/calving_pl.png", plot = g_pl, 
       width = 15, height = 12, units = "cm", dpi = 300)


pr_trail <- ggpredict(best_fit, terms = c("trail_dist"))
pr_trail$x <- pr_trail$x/1000
g_trail <- pr_trail %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to trails (m)",
       y = "Predicted probability of use") +
  xlim(0, 5)
ggsave(filename = "results/calving_trail_rsf.png", plot = g_trail, 
       width = 15, height = 12, units = "cm", dpi = 300)

#----------------------
# Figures in Swedish

pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_dist_comb", "phase", "landcover_cc"),
                               typical = "median", condition = c(wf_name = "ytteberg"))



levels(pr_log_wind_phase$facet) <- c("Kalhygge","Exploaterade omr","Öppna omr","Tallskog","Lövskog" ,"Skogbeklädda myrar","Myrar" )
levels(pr_log_wind_phase$group) <- c("Före byggfas", "Byggfas", "Driftsfas")
attr(pr_log_wind_phase, "legend.title") <- "Fas"
attr(pr_log_wind_phase, "legend.labels") <- c("Före byggfas", "Byggfas", "Driftsfas")



#, condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
g_log_wind <- pr_log_wind_phase %>% 
  plot(show.title = F, lim = c(0, 1), col = cols) +
  facet_wrap(~facet, ncol = 4)+
 # geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Avstånd till vindkraftverk (m)",
       y = "Predikterat habitatval (±KI)",
       colour = "Fas") +
  theme(legend.justification=c(1,1), legend.position=c(0.95,0.4))+
  xlim(0, 10)
g_log_wind

pr_public_road <- ggpredict(best_fit_calving, terms = c("public_road_dist", "phase"))

str(pr_public_road)
pr_public_road$group <- forcats::fct_recode(pr_public_road$group,
                                            "före" = "before", 
                                            under = "during",
                                            efter = "after")
attr(pr_public_road, "legend.labels") <- c("före", "under", "efter")
attr(pr_public_road, "legend.title") <- "Period"

g_road <- pr_public_road %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1)) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Avstånd till allmänna vägar (m)",
       y = "Predikterat val av betesområde") +
  xlim(0, 3000)
ggsave(filename = "results/calving_roads_swe.png", plot = g_road, 
       width = 10, height = 10, units = "cm", dpi = 300)

pr_priv_road <- ggpredict(best_fit_calving, terms = c("priv_road_dist", "phase"))
g_prov_road <- pr_priv_road %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1)) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Avstånd till enskilda vägar (m)",
       y = "Predikterat val av betesområde") +
  xlim(0, 1000)
ggsave(filename = "results/calving_private_roads_swe.png", plot = g_prov_road, 
       width = 10, height = 10, units = "cm", dpi = 300)

pr_pl <- ggpredict(best_fit_calving, terms = c("pl_dist", "phase"))
g_pl <- pr_pl %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1)) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Avstånd till kraftledningar (m)",
       y = "Predikterat val av betesområde") +
  xlim(0, 10000)
ggsave(filename = "results/calving_pl_swe.png", plot = g_pl, 
       width = 10, height = 10, units = "cm", dpi = 300)


#------------------------------------------------------------
# Models for predators and sound

data_pred_calving <- data_ready_calving_rsf_2h %>% 
  dplyr::filter(year >= 2015 & year <= 2018)

formula_calving_pred <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  log(pl_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_dist+1) +
  log(trail_dist+1) + log(house_dist+1) +
  landcover_cc + wf_name + 
  scale(bear)*log(wind_dist_comb+1) +
  scale(wolverine)*log(wind_dist_comb+1)

# landcover vs viewshed
fit1 <- glm(update(formula_calving_pred, ~ . + tpi_cat_s150m), 
            data = data_pred_calving, family = binomial)

fit1$formula
summary(fit1)
car::vif(fit1)

# backward deletion
# nothing to remove
formula_calving_pred2 <- update(formula_calving_pred, ~ . - cos_aspect)

fit2 <- glm(update(formula_calving_pred2, ~ . + tpi_cat_s150m),
            data = data_pred_calving, family = binomial)

fit2$formula
summary(fit2)
car::vif(fit2)

# compare TPI
fit2.2 <- glm(update(formula_calving_pred2, ~ . + tpi_cat_s250m), 
              data = data_pred_calving, family = binomial)

fit2.3 <- glm(update(formula_calving_pred2, ~ . + tpi_cat_s510m), 
              data = data_pred_calving, family = binomial)

# compare
(AICvals_pred <- bbmle::AICtab(fit2, fit2.2, fit2.3, weights = T, base = T))

# # compare sound and dist
# formula_calving_pred3 <- update(formula_calving_pred2, ~ . + tpi_cat_s150m -
#                              scale(bear)*log(wind_dist_comb+1) +
#                              scale(bear)*scale(sound_intensity) -
#                              scale(wolverine)*log(wind_dist_comb+1) +
#                              scale(wolverine)*scale(sound_intensity))
# 
# fit3 <- glm(formula_calving_pred3,
#             data = data_pred_calving, family = binomial)
# car::vif(fit3)

# compare
# (AICvals_pred <- bbmle::AICtab(fit2, fit2.2, fit2.3, fit3, weights = T, base = T))

#---------------
# most parsimonious model
best_fit <- fit2

summary(best_fit)

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
ggsave("calving_coefs_pred.png", plot = m1_plot, path = "results", 
       width = 20, height = 20, units = "cm", dpi = 300)

# save tables
# model_call <- sapply(attr(AICvals_pred, "row.names"), function(x) as.character(getCall(eval(as.symbol(x)))))[2,]
model_call <- sapply(attr(AICvals_pred, "row.names"), function(x) paste(as.character(formula(eval(as.symbol(x))))[c(2,1,3)], collapse = " "))
AICvals_pred %>% 
  dplyr::as_tibble(rownames = "model") %>%
  dplyr::mutate(definition = unname(model_call)) %>%
  readr::write_csv(file = "results/AIC_table_calving_pred.csv")

summary(best_fit)
coefs <- broom::tidy(best_fit) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0)),
                p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
coefs %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  readr::write_csv(file = "results/coef_table_calving_pred.csv")

#-------------------------------
# predictions

cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))

# only wind dist, averaged over all land cover types
pr_wind_general <- ggpredict(best_fit, terms = c("wind_dist_comb", "bear"),
                             typical = "median")
pr_wind_general$x <- pr_wind_general$x/1000
g_wind_general <- pr_wind_general %>% 
  plot(limits = c(0,1), show.title = F, show.x.title = F, col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to wind turbines (km)",
       y = "Predicted probability of use",
       colour = "Bear\nabundance") +
  xlim(0, 10)
ggsave(filename = "results/calving_wind_pred_general_pred.png", plot = g_wind_general, 
       width = 15, height = 12, units = "cm", dpi = 300)

# wind dist, pred, and land cover, for ytteberg
pr_log_wind_pred <- ggpredict(best_fit, terms = c("wind_dist_comb", "wolverine", "bear"),
                               typical = "median", condition = c(wf_name = "ytteberg",
                                                                 landcover_cc = "coniferous forests"))
pr_log_wind_pred$x <- pr_log_wind_pred$x/1000
pr_log_wind_pred$group <- forcats::fct_recode(pr_log_wind_pred$group,
                                              low = levels(pr_log_wind_pred$group)[1],
                                              intermediate = levels(pr_log_wind_pred$group)[2],
                                              high = levels(pr_log_wind_pred$group)[3])
# pr_log_wind_pred$facet <- forcats::fct_recode(pr_log_wind_pred$facet,
#                                               low = levels(pr_log_wind_pred$facet)[1],
#                                               intermediate = levels(pr_log_wind_pred$facet)[2],
#                                               high = levels(pr_log_wind_pred$facet)[3])

g_log_wind <- pr_log_wind_pred %>% 
  plot(show.title = F, lim = c(0, 1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to wind turbines (m)",
       y = "Predicted probability of use",
       colour = "Wolverine\nabundance") +
  xlim(0, 10)
ggsave(filename = "results/calving_wind_pred_wolverine_bear_pred.png", plot = g_log_wind, 
       width = 20, height = 10, units = "cm", dpi = 300)

# mining
pr_mining <- ggpredict(best_fit, terms = c("mining_dist", "wolverine"))
pr_mining$x <- pr_mining$x/1000
g_mining <- pr_mining %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to mining (m)",
       y = "Predicted probability of use",
       color = "Wolverine\nabundance") +
  xlim(0, 10)
ggsave(filename = "results/calving_mining_pred.png", plot = g_mining, 
       width = 15, height = 12, units = "cm", dpi = 300)

test <- glm(update(formula_calving_pred2, ~ . + tpi_cat_s150m - I(scale(dem)^2)),
            data = data_pred_calving, family = binomial)

# roads
pr_public_road <- ggpredict(test, terms = c("public_road_dist [all]", "wolverine"))
pr_public_road$x <- pr_public_road$x/1000
g_road <- pr_public_road %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to large roads (m)",
       y = "Predicted probability of use",
       colour = "Wolverine\nabundance") +
  xlim(0, 3)
ggsave(filename = "results/calving_roads_pred.png", plot = g_road, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_priv_road <- ggpredict(test, terms = c("priv_road_dist [all]"))
pr_priv_road$x <- pr_priv_road$x/1000
g_prov_road <- pr_priv_road %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to small roads (m)",
       y = "Predicted probability of use",
       colour = "pred") +
  xlim(0, 1)
ggsave(filename = "results/calving_private_roads_pred.png", plot = g_prov_road, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_pl <- ggpredict(best_fit, terms = c("pl_dist", "wolverine"))
pr_pl$x <- pr_pl$x/1000
g_pl <- pr_pl %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to power lines (m)",
       y = "Predicted probability of use") +
  xlim(0, 10)
ggsave(filename = "results/calving_pl_pred.png", plot = g_pl, 
       width = 15, height = 12, units = "cm", dpi = 300)

