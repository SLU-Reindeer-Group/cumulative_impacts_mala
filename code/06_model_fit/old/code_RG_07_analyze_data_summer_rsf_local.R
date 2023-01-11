#' ---
#' title: 'Habitat selection analysis of Swedish reindeer during summer at local scale: RSF at Mala sameby'
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
#' # summer analysis
#' 
#' ## Load annotated and clean movement data

# summer
load(file = "output/data_annotated_summer_ready_2h_rsf_local.RData")
data_ready_summer_rsf_2h

levels(data_ready_summer_rsf_2h$landcover)

# limited viewshed for amliden
dist_cutoff <- 1.5e4
wfs <- c("Jokkmokksliden", "Storliden", "Ytterberg", "Åmliden")
data_ready_summer_rsf_2h <- data_ready_summer_rsf_2h %>% 
  dplyr::mutate(viewshed_aml15km = ifelse(wind_dist_aml >  dist_cutoff, 0, viewshed_amliden_mala_binary),
                cos_aspect = cos(aspect),
                wind_dist_comb = pmin(wind_dist_jsy, wind_dist_aml),
                dist_wf_closest = apply(cbind(wind_dist_jokk, wind_dist_stor, wind_dist_ytte, wind_dist_aml), 1, which.min),
                wf_name = factor(dist_wf_closest, levels = 1:4, labels = wfs))

# apply(cbind(data_ready_summer_rsf_2h$wind_dist_jokk, data_ready_summer_rsf_2h$wind_dist_stor), MARGIN = 1, which.min)
data_ready_summer_rsf_2h$wf_name

#-----------
# check correlation
data_corr <- data_ready_summer_rsf_2h %>% 
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
ggsave("g_corr_dataset_rsf_summer_local.png", plot = g_corr, path = "output", 
       width = 40, height = 40, units = "cm", dpi = 300)

#-----------
#' ## Analysis for summer with phase
#' 
#' Pooled data
#' Period: 2008-2011, 2015-2018

# correlated:
# formula_summer <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
#   log(pl_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_dist+1) +
#   log(trail_dist+1) + log(house_dist+1) +
#   landcover_cc*phase*log(wind_dist_jsy+1) +
#   landcover_cc*phase*log(wind_dist_aml+1)

# trails are not correlated with wind dist during summer

formula_summer <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  log(pl_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_dist_Krist+1) +
  log(house_dist+1) + log(trail_dist+1) + 
  landcover_cc*phase*log(wind_dist_comb+1) + wf_name

# # landcover vs viewshed
# fit1 <- glm(update(formula_summer, ~ . + tpi_cat_s150m), 
#             data = data_ready_summer_rsf_2h, family = binomial)
# 
# fit1$formula
# summary(fit1)
# car::vif(fit1)

# backward deletion
# nothing to remove
formula_summer2 <- update(formula_summer, ~ . - cos_aspect - log(pl_dist+1))
# update(formula_summer, ~ . - landcover_cc:phase:log(wind_dist_aml+1)
#                          - landcover_cc:log(wind_dist_aml+1))
# fit2 <- glm(update(formula_summer2, ~ . + tpi_cat_s150m),
#             data = data_ready_summer_rsf_2h, family = binomial)
# 
# fit2$formula
# summary(fit2)
# car::vif(fit2)
# 
# # compare TPI
# fit2.2 <- glm(update(formula_summer2, ~ . + tpi_cat_s250m), 
#               data = data_ready_summer_rsf_2h, family = binomial)

fit2.3 <- glm(update(formula_summer2, ~ . + tpi_cat_s510m), 
              data = data_ready_summer_rsf_2h, family = binomial)

# compare
# (AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, weights = T, base = T))

# # compare viewshed and landcover_cc
# # formula_summer3 <- update(formula_summer2, ~ . + tpi_cat_s150m - 
# #                              landcover_cc*phase*log(wind_dist_jsy+1) +
# #                              viewshed_lc*phase*log(wind_dist_jsy+1))
# formula_summer3 <- update(formula_summer2, ~ . + tpi_cat_s510m -
#                              landcover_cc*phase*log(wind_dist_comb+1) +
#                              viewshed_lc*phase*log(wind_dist_comb+1))
# 
# fit3 <- glm(formula_summer3,
#             data = data_ready_summer_rsf_2h, family = binomial)
# 
# car::vif(fit3)
# 
# # compare
# (AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, fit3, weights = T, base = T))
# 
#---------------
# most parsimonious model
best_fit <- fit2.3
best_fit_summer <- best_fit

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
ggsave("summer_coefs_local.png", plot = m1_plot, path = "results", 
       width = 20, height = 20, units = "cm", dpi = 300)

# save tables
# model_call <- sapply(attr(AICvals_phase, "row.names"), function(x) as.character(getCall(eval(as.symbol(x)))))[2,]
model_call <- sapply(attr(AICvals_phase, "row.names"), function(x) paste(as.character(formula(eval(as.symbol(x))))[c(2,1,3)], collapse = " "))
AICvals_phase %>% 
  dplyr::as_tibble(rownames = "model") %>%
  dplyr::mutate(definition = unname(model_call)) %>%
  readr::write_csv(file = "results/AIC_table_summer_local.csv")

summary(best_fit)
coefs <- broom::tidy(best_fit) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0)),
                p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
coefs %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  readr::write_csv(file = "results/coef_table_summer_rsf_local.csv")

#-------------------------------
# predictions

best_fit$formula
summary(best_fit)
cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))

# averaged over all conditions, only slope
pr_slp <- ggpredict(best_fit, terms = "slope",
                    typical = "median") 
plot(pr_slp, lim = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey")

# only wind dist, averaged over all land cover types
pr_wind_general <- ggpredict(best_fit, terms = c("wind_dist_comb", "phase"),
                             typical = "median")
pr_wind_general$x <- pr_wind_general$x/1000
g_wind_general <- pr_wind_general %>% 
  plot(limits = c(0,1), show.title = F, show.x.title = F, col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to wind turbines (km)",
       y = "Predicted probability of use",
       colour = "Phase") +
  xlim(0, 10)
ggsave(filename = "results/summer_wind_phase_general_local.png", plot = g_wind_general, 
       width = 15, height = 12, units = "cm", dpi = 300)

# wind dist, phase, and land cover, for ytteberg
pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_dist_comb", "phase", "landcover_cc"),
                               typical = "median", condition = c(wf_name = "ytteberg"))
#, condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
g_log_wind_all <- pr_log_wind_phase %>% 
  plot(show.title = F, lim = c(0, 1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to wind turbines (m)",
       y = "Predicted probability of use",
       colour = "Phase") +
  xlim(0, 10)

g_log_wind_all

ggsave(filename = "results/summer_wind_phase_lc_ytte_local.png", plot = g_log_wind, 
       width = 20, height = 20, units = "cm", dpi = 300)

# # wind dist, phase, and land cover, for places closer to each of the WFs
# for(i in wfs) {
#   pr_log_wind_phase <- ggpredict(best_fit, terms = c("wind_dist_comb", "phase", "landcover_cc"),
#                                  typical = "median", condition = c(wf_name = i))
#   #, condition = c(landcover_cc = "coniferous forests"))
#   pr_log_wind_phase$x <- pr_log_wind_phase$x/1000
#   g_log_wind <- pr_log_wind_phase %>% 
#     plot(show.title = F, lim = c(0, 1), col = cols) +
#     geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#     labs(x = "Distance to wind turbines (m)",
#          y = "Predicted probability of use",
#          colour = "Phase") +
#     xlim(0, 10)
#   ggsave(filename = paste0("results/summer_wind_phase_lc_", i, "_local.png"), plot = g_log_wind, 
#          width = 20, height = 20, units = "cm", dpi = 300)
# }
# pr_wind_phase <- ggpredict(fit9_nolog, terms = c("wind_dist", "phase", "landcover"))
# g_wind <- pr_wind_phase %>% 
#   plot(show.title = F, show.x.title = F) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to wind turbines (km)",
#        y = "Predicted probability of use") +
#   xlim(0, 10)
# ggsave(filename = "results/summer_wind_phase_lc_no_log_local.png", plot = g_wind, 
#        width = 20, height = 20, units = "cm", dpi = 300)

# mining
pr_mining <- ggpredict(best_fit, terms = c("mining_dist_Krist", "phase"))
pr_mining$x <- pr_mining$x/1000
g_mining <- pr_mining %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to mining (m)",
       y = "Predicted probability of use",
       color = "Phase") +
  xlim(0, 10)
g_mining
ggsave(filename = "results/summer_mining_local.png", plot = g_mining, 
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
ggsave(filename = "results/summer_roads_local.png", plot = g_road, 
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
ggsave(filename = "results/summer_private_roads_local.png", plot = g_prov_road, 
       width = 15, height = 12, units = "cm", dpi = 300)

# pr_pl <- ggpredict(best_fit, terms = c("pl_dist"))
# pr_pl$x <- pr_pl$x/1000
# g_pl <- pr_pl %>% 
#   plot(show.title = F, show.x.title = F,
#        limits = c(0,1), col = cols) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to power lines (m)",
#        y = "Predicted probability of use") +
#   xlim(0, 10)
# ggsave(filename = "results/summer_pl_local.png", plot = g_pl, 
#        width = 15, height = 12, units = "cm", dpi = 300)


pr_trail <- ggpredict(best_fit, terms = c("trail_dist"))
pr_trail$x <- pr_trail$x/1000
g_trail <- pr_trail %>%
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to trails (m)",
       y = "Predicted probability of use") +
  xlim(0, 5)
ggsave(filename = "results/summer_trail_rsf_local.png", plot = g_trail,
       width = 15, height = 12, units = "cm", dpi = 300)

#----------------------
# Figures in Swedish ####
cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))

pr_log_wind_phase_summer <- ggpredict(best_fit_summer, terms = c("wind_dist_comb", "phase", "landcover_cc"),
                               typical = "median", condition = c(wf_name = "Ytterberg"))

levels(pr_log_wind_phase_summer$facet) <- c("Kalhygge","Exploaterade omr","Öppna omr","Barrskog","Lövskog" ,"Skogbeklädda myrar","Myrar" )
levels(pr_log_wind_phase_summer$group) <- c("Före byggfas", "Byggfas", "Driftsfas")
attr(pr_log_wind_phase_summer, "legend.title") <- "Period"
attr(pr_log_wind_phase_summer, "legend.labels") <- c("Före byggfas", "Byggfas", "Driftsfas")

save(pr_log_wind_phase_summer, file="pr_log_wind_phase_summer_local.RData")


facets <- c("Kalhygge","Barrskog","Lövskog","Myrar" )
pr_log_wind_phase_sum3 <- pr_log_wind_phase_summer[pr_log_wind_phase_summer$facet %in% facets,]
#, condition = c(landcover_cc = "coniferous forests"))
pr_log_wind_phase_sum3$x <- pr_log_wind_phase_sum3$x/1000
g_log_wind <- pr_log_wind_phase_sum3 %>% 
  plot(show.title = F, lim = c(0, 1), col = cols) +
  facet_wrap(~facet, ncol = 4)+
  #geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Avstånd till vindkraftverk (km)",
       y = "Predikterat habitatval (±KI)",
       colour = "Period") +
 # theme(legend.justification=c(1,1), legend.position=c(0.95,0.4))+
  theme(legend.position="bottom")+
  xlim(0, 10)
g_log_wind


ggsave(filename = "results/summer_wind_phase_rsf_local_swe.png", plot = g_log_wind, 
       width = 25, height = 11, units = "cm", dpi = 300)


pr_public_road <- ggpredict(best_fit_summer, terms = c("public_road_dist", "phase"))

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
ggsave(filename = "results/summer_roads_swe_local.png", plot = g_road, 
       width = 10, height = 10, units = "cm", dpi = 300)

pr_priv_road <- ggpredict(best_fit_summer, terms = c("priv_road_dist", "phase"))
g_prov_road <- pr_priv_road %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1)) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Avstånd till enskilda vägar (m)",
       y = "Predikterat val av betesområde") +
  xlim(0, 1000)
ggsave(filename = "results/summer_private_roads_swe_local.png", plot = g_prov_road, 
       width = 10, height = 10, units = "cm", dpi = 300)

pr_pl <- ggpredict(best_fit_summer, terms = c("pl_dist", "phase"))
g_pl <- pr_pl %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1)) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Avstånd till kraftledningar (m)",
       y = "Predikterat val av betesområde") +
  xlim(0, 10000)
ggsave(filename = "results/summer_pl_swe_local.png", plot = g_pl, 
       width = 10, height = 10, units = "cm", dpi = 300)


#------------------------------------------------------------
# Models for predators and sound

data_pred_summer <- data_ready_summer_rsf_2h %>% 
  dplyr::filter(year >= 2015 & year <= 2018)

formula_summer_pred <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  log(pl_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_dist_Krist+1) +
  log(house_dist+1) + landcover_cc + wf_name + 
  scale(bear)*log(wind_dist_comb+1) +
  scale(wolverine)*log(wind_dist_comb+1)

# full model
fit1 <- glm(update(formula_summer_pred, ~ . + tpi_cat_s150m), 
            data = data_pred_summer, family = binomial)

fit1$formula
summary(fit1)
car::vif(fit1)

# backward deletion
# nothing to remove
formula_summer_pred2 <- update(formula_summer_pred, ~ . - I(scale(dem)^2) -
                                 cos_aspect - log(pl_dist+1) -
                                 scale(bear):log(wind_dist_comb+1) - scale(bear))

fit2 <- glm(update(formula_summer_pred2, ~ . + tpi_cat_s150m),
            data = data_pred_summer, family = binomial)

fit2$formula
summary(fit2)
car::vif(fit2)

# compare TPI
fit2.2 <- glm(update(formula_summer_pred2, ~ . + tpi_cat_s250m), 
              data = data_pred_summer, family = binomial)

fit2.3 <- glm(update(formula_summer_pred2, ~ . + tpi_cat_s510m), 
              data = data_pred_summer, family = binomial)

# compare
(AICvals_pred <- bbmle::AICtab(fit2, fit2.2, fit2.3, weights = T, base = T))

# # compare sound and dist
# formula_summer_pred3 <- update(formula_summer_pred2, ~ . + tpi_cat_s150m -
#                              scale(bear)*log(wind_dist_comb+1) +
#                              scale(bear)*scale(sound_intensity) -
#                              scale(wolverine)*log(wind_dist_comb+1) +
#                              scale(wolverine)*scale(sound_intensity))
# 
# fit3 <- glm(formula_summer_pred3,
#             data = data_pred_summer, family = binomial)
# car::vif(fit3)

# compare
# (AICvals_pred <- bbmle::AICtab(fit2, fit2.2, fit2.3, fit3, weights = T, base = T))

#---------------
# most parsimonious model
best_fit <- fit2.3

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
ggsave("summer_coefs_pred_local.png", plot = m1_plot, path = "results", 
       width = 20, height = 20, units = "cm", dpi = 300)

# save tables
# model_call <- sapply(attr(AICvals_pred, "row.names"), function(x) as.character(getCall(eval(as.symbol(x)))))[2,]
model_call <- sapply(attr(AICvals_pred, "row.names"), function(x) paste(as.character(formula(eval(as.symbol(x))))[c(2,1,3)], collapse = " "))
AICvals_pred %>% 
  dplyr::as_tibble(rownames = "model") %>%
  dplyr::mutate(definition = unname(model_call)) %>%
  readr::write_csv(file = "results/AIC_table_summer_pred_local.csv")

summary(best_fit)
coefs <- broom::tidy(best_fit) %>% 
  dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0)),
                p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
coefs %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  readr::write_csv(file = "results/coef_table_summer_pred_local.csv")

#-------------------------------
# predictions

cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))
library(wesanderson)
colsbear<- c(wes_palette("FantasticFox1", 3))
colswolv <- c(wes_palette("GrandBudapest1", 3))

# only wind dist, averaged over all land cover types
pr_wind_general <- ggpredict(best_fit, terms = c("wind_dist_comb [all]", "wolverine"),
                             typical = "median")
pr_wind_general$x <- pr_wind_general$x/1000
g_wind_general <- pr_wind_general %>% 
  plot(limits = c(0,1), show.title = F, show.x.title = F, col = colswolv) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to wind turbines (km)",
       y = "Predicted probability of use",
       colour = "Wolverine\nabundance") +
  xlim(0, 10)
ggsave(filename = "results/summer_wind_pred_general_pred_local.png", plot = g_wind_general, 
       width = 15, height = 12, units = "cm", dpi = 300)

# wind dist, pred, and land cover, for ytteberg
# pr_log_wind_pred <- ggpredict(best_fit, terms = c("wind_dist_comb", "wolverine", "bear"),
#                               typical = "median", condition = c(wf_name = "ytteberg",
#                                                                 landcover_cc = "coniferous forests"))
# pr_log_wind_pred$x <- pr_log_wind_pred$x/1000
# pr_log_wind_pred$group <- forcats::fct_recode(pr_log_wind_pred$group,
#                                               low = levels(pr_log_wind_pred$group)[1],
#                                               intermediate = levels(pr_log_wind_pred$group)[2],
#                                               high = levels(pr_log_wind_pred$group)[3])
# pr_log_wind_pred$facet <- forcats::fct_recode(pr_log_wind_pred$facet,
#                                               low = levels(pr_log_wind_pred$facet)[1],
#                                               intermediate = levels(pr_log_wind_pred$facet)[2],
#                                               high = levels(pr_log_wind_pred$facet)[3])

# g_log_wind <- pr_log_wind_pred %>% 
#   plot(show.title = F, lim = c(0, 1), col = colswolv) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to wind turbines (m)",
#        y = "Predicted probability of use",
#        colour = "Wolverine\nabundance") +
#   xlim(0, 10)
# ggsave(filename = "results/summer_wind_pred_wolverine_bear_pred_local.png", plot = g_log_wind, 
#        width = 20, height = 10, units = "cm", dpi = 300)

# mining
pr_mining <- ggpredict(best_fit, terms = c("mining_dist_Krist [all]", "wolverine"))
pr_mining$x <- pr_mining$x/1000
g_mining <- pr_mining %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to mining (m)",
       y = "Predicted probability of use",
       color = "Wolverine\nabundance") +
  xlim(0, 10)
ggsave(filename = "results/summer_mining_pred_local.png", plot = g_mining, 
       width = 15, height = 12, units = "cm", dpi = 300)

# roads
pr_public_road <- ggpredict(best_fit, terms = c("public_road_dist [all]", "wolverine"))
pr_public_road$x <- pr_public_road$x/1000
g_road <- pr_public_road %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = colswolv) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to large roads (m)",
       y = "Predicted probability of use",
       colour = "Wolverine\nabundance") +
  xlim(0, 3)
ggsave(filename = "results/summer_roads_pred_local.png", plot = g_road, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_priv_road <- ggpredict(best_fit, terms = c("priv_road_dist [all]"))
pr_priv_road$x <- pr_priv_road$x/1000
g_prov_road <- pr_priv_road %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to small roads (m)",
       y = "Predicted probability of use",
       colour = "pred") +
  xlim(0, 1)
ggsave(filename = "results/summer_private_roads_pred_local.png", plot = g_prov_road, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_pl <- ggpredict(best_fit, terms = c("house_dist [all]", "wolverine"))
pr_pl$x <- pr_pl$x/1000
g_pl <- pr_pl %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = colswolv) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Distance to houses (m)",
       y = "Predicted probability of use") +
  xlim(0, 5)
ggsave(filename = "results/summer_house_pred_local.png", plot = g_pl, 
       width = 15, height = 12, units = "cm", dpi = 300)

