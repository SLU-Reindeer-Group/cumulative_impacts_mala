#' ---
#' title: 'Habitat selection analysis of Swedish reindeer: Mala'
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

# calving
load(file = "output/data_annotated_calving_ready_2h_rsf.RData")
data_ready_calving_rsf_2h

levels(data_ready_calving_rsf_2h$landcover)

# limited viewshed for amliden
dist_cutoff <- 1.5e4
wfs <- c("Jokkmokksliden", "Storliden", "Ytterberg", "Åmliden")
data_ready_calving_rsf_2h <- data_ready_calving_rsf_2h %>% 
  dplyr::mutate(viewshed_aml15km = ifelse(wind_dist_aml >  dist_cutoff, 0, viewshed_amliden_mala_binary),
                cos_aspect = cos(aspect),
                wind_dist_comb = pmin(wind_dist_jsy, wind_dist_aml),
                dist_wf_closest = apply(cbind(wind_dist_jokk, wind_dist_stor, wind_dist_ytte, wind_dist_aml), 1, which.min),
                wf_name = factor(dist_wf_closest, levels = 1:4, labels = wfs))

# apply(cbind(data_ready_calving_rsf_2h$wind_dist_jokk, data_ready_calving_rsf_2h$wind_dist_stor), MARGIN = 1, which.min)
data_ready_calving_rsf_2h$wf_name
table(data_ready_calving_rsf_2h$wf_name)

table(data_ready_calving_rsf_2h$viewshed_lc2)
data_ready_calving_rsf_2h$viewshed_lc2 <- as.character(data_ready_calving_rsf_2h$viewshed_lc)
#data_ready_calving_rsf_2h$viewshed_lc <- as.numeric(data_ready_calving_rsf_2h$viewshed_lc)

data_ready_calving_rsf_2h$dist_wf_5km <- cut(data_ready_calving_rsf_2h$wind_dist_comb, breaks= c(0,5000, 75000),label= c(0, 1), include.lowest=TRUE)
table(data_ready_calving_rsf_2h$dist_wf_5km)
levels(data_ready_calving_rsf_2h$dist_wf_5km) <- c("wf<5km", "wf>5km")
data_ready_calving_rsf_2h$viewshed_lc_5km <- ifelse(data_ready_calving_rsf_2h$dist_wf_5km=="wf<5km",data_ready_calving_rsf_2h$viewshed_lc2,"out of sight" )

table(data_ready_calving_rsf_2h$viewshed_lc_5km)
data_ready_calving_rsf_2h$viewshed_lc_5km <- as.factor(data_ready_calving_rsf_2h$viewshed_lc_5km)
# keep only one random point for each used point
table(data_ready_calving_rsf_2h$id, data_ready_calving_rsf_2h$case_)
plot(data_ready_calving_rsf_2h$case_[1:10000])

data_ready_calving_rsf_2h_1pt <- data_ready_calving_rsf_2h %>% 
  plyr::ddply(., "id",
              function(x) {
                n <- sum(x$case_ == 1)
                return(x[1:(2*n),])
              }, .progress = "text") %>% 
  tibble::as_tibble()
data_ready_calving_rsf_2h_1pt
plot(data_ready_calving_rsf_2h_1pt$case_[1:10000])
table(data_ready_calving_rsf_2h_1pt$id, data_ready_calving_rsf_2h_1pt$case_)
data_ready_calving_rsf_2h <- data_ready_calving_rsf_2h_1pt
data_ready_calving_rsf_2h$year %>% table

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
# 
# # construction * log_wind, landcover
# formula_calving <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
#   log(pl_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_dist+1) +
#   log(trail_dist+1) + log(house_dist+1) +
#   landcover_cc*phase*log(wind_dist_jsy+1) +
#   landcover_cc*phase*log(wind_dist_aml+1)

formula_calving <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
  log(pl_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_dist+1) +
  log(trail_dist+1) + log(house_dist+1) +
  landcover_cc*phase*log(wind_dist_comb+1)+ wf_name

# # landcover vs viewshed
# fit1 <- glm(update(formula_calving, ~ . + tpi_cat_s150m), 
#             data = data_ready_calving_rsf_2h, family = binomial)
# 
# fit1$formula
# summary(fit1)
# car::vif(fit1)
# 
# # backward deletion
# nothing to remove
formula_calving2 <- formula_calving
  # update(formula_calving, ~ . - landcover_cc:phase:log(wind_dist_aml+1)
  #                          - landcover_cc:log(wind_dist_aml+1))
fit2 <- glm(update(formula_calving2, ~ . + tpi_cat_s150m),
            data = data_ready_calving_rsf_2h, family = binomial)

fit2$formula
summary(fit2)
car::vif(fit2)
# 
# # compare TPI
# fit2.2 <- glm(update(formula_calving2, ~ . + tpi_cat_s250m), 
#             data = data_ready_calving_rsf_2h, family = binomial)
# summary(fit2.2)
# fit2.3 <- glm(update(formula_calving2, ~ . + tpi_cat_s510m), 
#               data = data_ready_calving_rsf_2h, family = binomial)
# summary(fit2.3)
# # compare
# (AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, weights = T, base = T))
# 
# # compare viewshed and landcover_cc
# # formula_calving3 <- update(formula_calving2, ~ . + tpi_cat_s150m - 
# #                              landcover_cc*phase*log(wind_dist_jsy+1) +
# #                              viewshed_lc*phase*log(wind_dist_jsy+1))
# 
# 
# 
# 
# formula_calving3 <- update(formula_calving2, ~ . + tpi_cat_s150m -
#                              landcover_cc*phase*log(wind_dist_comb+1) +
#                              viewshed_lc*phase*log(wind_dist_comb+1))
# 
# fit3 <- glm(formula_calving3,
#             data = data_ready_calving_rsf_2h, family = binomial)
# summary(fit3)
# car::vif(fit3)
# 
# # compare
# (AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, fit3, weights = T, base = T))
# 
# formula_calving4 <- case_ ~ scale(dem) + I(scale(dem)^2) + scale(slope) + cos_aspect + 
#   log(pl_dist+1) + log(priv_road_dist+1) + log(public_road_dist+1) + log(mining_dist+1) +
#   log(trail_dist+1) + log(house_dist+1) +
#   + wf_name + viewshed_lc_5km*phase +log(wind_dist_comb+1)*phase
# 
# fit4 <- glm(formula_calving4,
#             data = data_ready_calving_rsf_2h, family = binomial)
# 
# car::vif(fit4)
# summary(fit4)
# 
# (AICvals_phase <- bbmle::AICtab(fit2, fit2.2, fit2.3, fit3, fit4, weights = T, base = T))
# 
# 
# #---------------
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
  readr::write_csv(file = "results/coef_table_calving2.csv")

#-------------------------------
# predictions ####

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
g_wind_general
ggsave(filename = "results/calving_wind_phase_general.png", plot = g_wind_general, 
       width = 15, height = 12, units = "cm", dpi = 300)

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
g_log_wind
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

# mining swe ####
pr_mining <- ggpredict(best_fit, terms = c("mining_dist", "phase"))
levels(pr_mining$group) <- c("Före byggfas", "Byggfas", "Driftsfas")
attr(pr_mining, "legend.title") <- "Fas"

pr_mining$x <- pr_mining$x/1000
g_mining <- pr_mining %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Avstånd till gruvor (m)",
       y = "Predikterat habitatval (± KI)",
       color = "Fas") +
  xlim(0, 10)
ggsave(filename = "results/calving_mining_swe.png", plot = g_mining, 
       width = 15, height = 12, units = "cm", dpi = 300)

# roads
pr_public_road <- ggpredict(best_fit, terms = c("public_road_dist"))
attr(pr_public_road, "legend.title") <- "Fas"
pr_public_road$x <- pr_public_road$x/1000
g_road <- pr_public_road %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Avstånd till allmänna vägar (km)",
       y = "Predikterat habitatval (± KI)",
       colour = "Fas") +
  xlim(0, 3)
g_road
ggsave(filename = "results/calving_roads_swe.png", plot = g_road, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_priv_road <- ggpredict(best_fit, terms = c("priv_road_dist"))
attr(pr_priv_road, "legend.title") <- "Fas"
pr_priv_road$x <- pr_priv_road$x/1000
g_prov_road <- pr_priv_road %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Avstånd till enskilda vägar (m)",
       y = "Predikterat habitatval (± KI)",
       colour = "Fas") +
  xlim(0, 1)
ggsave(filename = "results/calving_private_roads_swe.png", plot = g_prov_road, 
       width = 15, height = 12, units = "cm", dpi = 300)

pr_pl <- ggpredict(best_fit, terms = c("pl_dist"))
#attr(pr_pl, "legend.title") <- "Fas"
pr_pl$x <- pr_pl$x/1000
g_pl <- pr_pl %>% 
  plot(show.title = F, show.x.title = F,
       limits = c(0,1), col = cols) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Avstånd till kraftledningar (m)",
       y = "Predikterat habitatval (± KI)") +
  xlim(0, 10)
ggsave(filename = "results/calving_pl_swe.png", plot = g_pl, 
       width = 15, height = 12, units = "cm", dpi = 300)


g_calv_combine <- ggpubr::ggarrange(g_mining, 
                                    g_road + ylab(""),
                                    g_prov_road,
                                    g_pl,
                                     labels = c("a", "b", "c", "d"), 
                                     common.legend = T, legend = "right", 
                                     ncol = 2, nrow = 2)

#----------------------
# Figures in Swedish #####
cols <- rev(RColorBrewer::brewer.pal(3, "Set1"))

best_fit_calving <- best_fit

pr_log_wind_phase <- ggpredict(best_fit_calving, terms = c("wind_dist_comb", "phase", "landcover_cc"))
str(pr_log_wind_phase)

levels(pr_log_wind_phase$facet) <- c("Kalhygge","Exploaterade omr","Öppna omr","Barrskog","Lövskog" ,"Skogbeklädda myrar","Myrar" )
levels(pr_log_wind_phase$group) <- c("Före byggfas", "Byggfas", "Driftsfas")
facets <- c("Kalhygge","Barrskog","Lövskog","Myrar" )

pr_log_wind_phase2 <- pr_log_wind_phase[pr_log_wind_phase$facet %in% facets,]
# pr_log_wind_phase$group <- forcats::fct_recode(pr_log_wind_phase$group,
#                                                före = "before",
#                                                under = "during",
#                                                efter = "after")

#lu <- c("våtmark", "jordbruksmark", "annan öppen mark", "exploaterade områden",
#        "tallskog", "barrskog", "lövskog", "kalhyggen", "skog på våtmark")
#pr_log_wind_phase$facet <- lu[pr_log_wind_phase$facet]
#factor(lu[pr_log_wind_phase$facet], levels = lu, labels = lu)

#attr(pr_log_wind_phase, "legend.labels") <- c("före", "under", "efter")
attr(pr_log_wind_phase, "legend.title") <- "Period"

g_log_wind2 <- pr_log_wind_phase2 %>% 
  plot(show.title = F, show.x.title = F, limits = c(0,1), col=cols) +
  facet_wrap(~facet, ncol=4)+
 # geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Avstånd till vindkraftverk (m)",
       y = "Predikterat habitatval (±KI)") +
  xlim(0, 10000)+
 # theme(legend.justification=c(1,1), legend.position=c(0.95,0.4))+
  theme(legend.position="bottom")
  
g_log_wind2

# ggsave(filename = "results/calving_wind_phase_lc_swe_new.png", plot = g_log_wind, 
#        width = 25, height = 15, units = "cm", dpi = 300)

ggsave(filename = "results/calving_wind_phase_lc_swe_newnew_3.png", plot = g_log_wind2, 
       width = 25, height = 12, units = "cm", dpi = 300)

#_____________________________________________________#
source("~/OneDrive - Sveriges Lantbruksuniversitet/Vindval/R_revised2/summary.R")
# percent decrease in Pred per km ####
pr_log_wind_phase2$distvindclass <- cut(pr_log_wind_phase2$x, breaks= c(0, 200,950,1056, 1950, 2061,2963, 3066,3968,4071,4973, 5076,10000),
                                 label= c("200","950","1056","1950","2061","2963", "3066","3968","4071","4973", "5076","10000"), 
                                 include.lowest=TRUE)
sumdist2nd <-summarySE(pr_log_wind_phase2, measurevar="predicted", groupvars=c("distvindclass", "facet", "group") )
sumdist2nd
write.table(sumdist2nd, file="Predmargvalues_rsf_calving.txt")
#_____________________________________________________#

# # only wind dist, averaged over all land cover types
# pr_wind_general <- ggpredict(best_fit, terms = c("wind_dist_comb", "phase"),
#                              typical = "median")
# 
# levels(pr_wind_general$facet) <- c("Kalhygge","Exploaterade omr","Öppna omr","Tallskog","Lövskog" ,"Skogbeklädda myrar","Myrar" )
# levels(pr_wind_general$group) <- c("Före byggfas", "Byggfas", "Driftsfas")
# attr(pr_wind_general, "legend.title") <- "Fas"
# 
# pr_wind_general$x <- pr_wind_general$x/1000
# g_wind_general <- pr_wind_general %>% 
#   plot(limits = c(0,1), show.title = F, show.x.title = F, col = cols) +
#   geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
#   labs(x = "Distance to wind turbines (km)",
#        y = "Predicted probability of use",
#        colour = "Phase") +
#   xlim(0, 10)
#   
# g_wind_general
# ggsave(filename = "results/calving_wind_phase_general_swe.png", plot = g_wind_general, 
#        width = 15, height = 12, units = "cm", dpi = 300)



# wind dist, phase, and land cover, for ytteberg ####
pr_log_wind_phase <- ggpredict(best_fit_calving, terms = c("wind_dist_comb", "phase", "landcover_cc"),
                               typical = "median", condition = c(wf_name = "Ytterberg"))

levels(pr_log_wind_phase$facet) <- c("Kalhygge","Exploaterade omr","Öppna omr","Barrskog","Lövskog" ,"Skogbeklädda myrar","Myrar" )
levels(pr_log_wind_phase$group) <- c("Före byggfas", "Byggfas", "Driftsfas")
#attr(pr_log_wind_phase, "legend.title") <- "Period"
#attr(pr_log_wind_phase, "legend.labels") <- c("Före byggfas", "Byggfas", "Driftsfas")
pr_log_wind_phase3 <- pr_log_wind_phase[pr_log_wind_phase$facet %in% facets,]

#, condition = c(landcover_cc = "coniferous forests"))
#pr_log_wind_phase3$x <- pr_log_wind_phase3$x/1000
g_log_wind3 <- pr_log_wind_phase3 %>% 
  plot(show.title = F, lim = c(0, 1), col = cols) +
  facet_wrap(~facet, ncol = 4)+
  #geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  labs(x = "Avstånd till vindkraftverk (m)",
       y = "Predikterat habitatval (±KI)",
       colour = "Period") +
  #theme(legend.justification=c(1,1), legend.position=c(0.95,0.4))+
  theme(legend.position="bottom")+
  xlim(0, 10000)
g_log_wind3

ggsave(filename = "results/calving_wind_phase_lc_ytte_swe.png", plot = g_log_wind3, 
       width = 25, height = 12, units = "cm", dpi = 300)


#_____________________________________________________#
source("~/OneDrive - Sveriges Lantbruksuniversitet/Vindval/R_revised2/summary.R")
# percent decrease in Pred per km ####
pr_log_wind_phase$distvindclass <- cut(pr_log_wind_phase$x, breaks= c(0, 200,950,1056, 1950, 2061,2963, 3066,3968,4071,4973, 5076,10000),
                                        label= c("200","950","1056","1950","2061","2963", "3066","3968","4071","4973", "5076","10000"), 
                                        include.lowest=TRUE)
sumdist2nd <-summarySE(pr_log_wind_phase, measurevar="predicted", groupvars=c("distvindclass", "facet", "group") )
sumdist2nd
write.csv(sumdist2nd, file="results/Predmargvalues_rsf_calving.csv")
#_____________________________________________________#



pr_public_road <- ggpredict(best_fit_calving, terms = c("public_road_dist", "phase"))

str(pr_public_road)
pr_public_road$group <- forcats::fct_recode(pr_public_road$group,
                                            före = "before", 
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

