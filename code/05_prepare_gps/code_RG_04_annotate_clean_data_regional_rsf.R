#' ---
#' title: 'Annotate GPS for habitat selection analysis of Swedish reindeer at regional scale: RSF at Mala'
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
install.load::install_load("dplyr", "tidyr", "lubridate", "ggridges", "GGally")
install.load::install_load("sf", "terra", "raster", "stars")
install.load::install_load("broom", "ggeffects")
install.load::install_load("amt")

#-----------
#' # Load movement data

# movement data
mov_calving <- readRDS("data/01_GPS/04_cleaned_data_indsMore14days/data_cleaned_calving_RG.rds") |> 
  dplyr::as_tibble() |> 
  dplyr::select(Collar_ID, Object_ID, id, burst, year, x, y, timestamp)

mov_summer <- readRDS("data/01_GPS/04_cleaned_data_indsMore14days/data_cleaned_summer_RG.rds") |> 
  dplyr::as_tibble() |> 
  dplyr::select(Collar_ID, Object_ID, id, burst, year, x, y, timestamp)

mov_autumn <- readRDS("data/01_GPS/04_cleaned_data_indsMore14days/data_cleaned_autumn_RG.rds") |> 
  dplyr::as_tibble() |> 
  dplyr::select(Collar_ID, Object_ID, id, burst, year, x, y, timestamp)

# aux data
details_years <- read.csv("data/01_GPS/details_snow_free_mala_clean.csv", stringsAsFactors = F) |> 
  dplyr::as_tibble() |> 
  dplyr::mutate(phase = factor(construction) |> 
                  forcats::fct_relevel("after", after = Inf) |> 
                  forcats::fct_recode("Before construction" = "before",
                                      "Construction" = "during",
                                      "Operation" = "after")) |> 
  dplyr::select(year, phase)
details_years$phase

mov_calving <- mov_calving |> 
  dplyr::left_join(details_years, by = "year")

mov_summer <- mov_summer |> 
  dplyr::left_join(details_years, by = "year")

mov_autumn <- mov_autumn |> 
  dplyr::left_join(details_years, by = "year")

#-----------
#' # Load background data

# projection to use
# crs_to_use <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"
crs_to_use <- 3006
crs_proj <- paste0("EPSG:", crs_to_use)

# maps folder and files
maps_folder <- "maps"

# whether or not to plot maps
plot_maps <- FALSE

# whether to perform analysis for 1 individual only, for testing
analysis_1ind <- FALSE

#---
# list maps

# availability area
availability_files <- list.files(maps_folder, pattern = ".gpkg$", full.names = T) |> 
  grep(pattern = "availability", value = T)

# maps
maps_files <- list.files(maps_folder, pattern = ".tif$", full.names = T)

# maps to ignore (here, for winter)
ignore <- c("wolf", "urban", "snowmob", "landcover_smd.tif", "bbmm",
            "mining_all_bin_zoi_cumulative", "building",
            "railway")

# maps to read
maps_files_read <- grep(pattern = paste(ignore, collapse = "|"), 
                        maps_files, value = T, invert = T)

#---
# read maps

# availability areas
availability_calving <- availability_files |> 
  grep(pattern = "calving", value = T) |> 
  sf::st_read() |> 
  dplyr::select(available = 1) |> 
  sf::st_transform(crs = crs_to_use)

availability_summer <- availability_files |> 
  grep(pattern = "late", value = T) |> 
  sf::st_read() |> 
  dplyr::select(available = 1) |> 
  sf::st_transform(crs = crs_to_use)

availability_autumn <- availability_files |> 
  grep(pattern = "autumn", value = T) |> 
  sf::st_read() |> 
  dplyr::select(available = 1) |> 
  sf::st_transform(crs = crs_to_use)

# maps 
maps <- terra::rast(maps_files_read)
maps_rast <- raster::stack(maps_files_read)

# rename some maps
names(maps) <- sub("wind_dist", "wind_turbines_dist", names(maps))
names(maps) <- sub("mining_dist_Krist", "mining_Krist_dist", names(maps))
names(maps) <- sub("mining_dist", "mining_all_dist", names(maps))
names(maps) <- sub("pl_dist", "power_lines_dist", names(maps))

names(maps_rast) <- sub("wind_dist", "wind_turbines_dist", names(maps))
names(maps_rast) <- sub("mining_dist_Krist", "mining_Krist_dist", names(maps))
names(maps_rast) <- sub("mining_dist", "mining_all_dist", names(maps))
names(maps_rast) <- sub("pl_dist", "power_lines_dist", names(maps))

# read classes for land cover map
lc_class <- read.csv("maps/nmd_classes_eng_reclassified.csv", header = F, 
                     col.names = c("code", "class"), stringsAsFactors = F) |> 
  dplyr::arrange(code)

# plots, just to visualize
predators <- c("bear", "wolverine")
landuse <- c("nmd", "smd")

# plot no predators and no land use
no_pred_lu <- grep(pattern = paste(c(predators, landuse), collapse = "|"),
                   names(maps), value = T, invert = T)

if(plot_maps) {

  maps_nopred <- maps[[which(names(maps) %in% no_pred_lu)]]
  terra::plot(maps_nopred[[c(1, 3:11, 32:35, 47:49)]])
  terra::plot(maps_nopred[[c(12:31)]])
  
  terra::plot(maps[["viewshed_mala_binary"]])
  raster::plot(maps_rast[["viewshed_mala_binary"]], colNA = "white")
  
  terra::plot(maps[["landcover_nmd_mod"]])
  raster::plot(maps_rast[["landcover_nmd_mod"]], colNA = "white")
  
  png("output/background_maps_infrastructure1.png", width = 40, height = 30,
      units = "cm", res = 300)
  terra::plot(maps_nopred[[c(1:12, 33:36)]])#, 3:11, 32:35, 47:49)]])
  dev.off()
  
  png("output/background_maps_infrastructure2.png", width = 30, height = 20,
      units = "cm", res = 300)
  terra::plot(maps_nopred[[grepl("wind", names(maps_nopred))]])
  dev.off()
  
  png("output/background_maps_infrastructure3.png", width = 30, height = 20,
      units = "cm", res = 300)
  terra::plot(maps_nopred[[grepl("sound", names(maps_nopred))]])
  dev.off()
  
  png("output/background_maps_infrastructure3.png", width = 10, height = 10,
      units = "cm", res = 300)
  raster::plot(maps_rast[["viewshed_mala_binary"]])
  dev.off()
  
  png("output/background_maps_infrastructure_view_amliden.png", width = 10, height = 10,
      units = "cm", res = 300)
  raster::plot(maps_rast[["viewshed_amliden_mala_binary"]])
  dev.off()
  
  png("output/background_maps_infrastructure_view_ytterberg.png", width = 10, height = 10,
      units = "cm", res = 300)
  raster::plot(maps_rast[["viewshed_ytterberg_mala_binary"]])
  dev.off()
  
  png("output/background_maps_infrastructure_view_jokk.png", width = 10, height = 10,
      units = "cm", res = 300)
  raster::plot(maps_rast[["viewshed_jokkmokksliden_mala_binary"]])
  dev.off()
  
  png("output/background_maps_infrastructure_view_stor.png", width = 10, height = 10,
      units = "cm", res = 300)
  raster::plot(maps_rast[["viewshed_storliden_mala_binary"]])
  dev.off()
  
  # calculate viewshed with 10km
  dist_cuttoff <- 1.5e4
  
  # storliden
  viewshed_stor10km <- maps_rast[["viewshed_storliden_mala_binary"]]
  values(viewshed_stor10km) <- ifelse(maps_rast[["wind_dist_stor"]][] > dist_cuttoff, 
                                      0, viewshed_stor10km[])
  plot(maps_rast[["viewshed_storliden_mala_binary"]])
  plot(viewshed_stor10km)
  
  # jokkmokksliden
  viewshed_jokk10km <- maps_rast[["viewshed_jokkmokksliden_mala_binary"]]
  values(viewshed_jokk10km) <- ifelse(maps_rast[["wind_dist_jokk"]][] > dist_cuttoff, 
                                      0, viewshed_jokk10km[])
  plot(maps_rast[["viewshed_jokkmokksliden_mala_binary"]])
  plot(viewshed_jokk10km)
  
  # ytteberg
  viewshed_ytte10km <- maps_rast[["viewshed_ytterberg_mala_binary"]]
  values(viewshed_ytte10km) <- ifelse(maps_rast[["wind_dist_ytte"]][] > dist_cuttoff, 
                                      0, viewshed_ytte10km[])
  plot(maps_rast[["viewshed_ytterberg_mala_binary"]])
  plot(viewshed_ytte10km)
  
  # combine viewshed from jokkmokksliden, storliden, and ytteberg
  viewshed_combined <- max(raster::stack(viewshed_stor10km,
                                         viewshed_jokk10km,
                                         viewshed_ytte10km))
  plot(viewshed_combined)
  writeRaster(viewshed_combined, filename = "maps/viewshed_jsy15km.tif")
  
  # save as layer
  maps_rast$viewshed_jsy15km <- viewshed_combined
  maps$viewshed_jsy15km <- rast(viewshed_combined)
  
  png("output/background_maps_infrastructure_view_jsy.png", width = 10, height = 10,
      units = "cm", res = 300)
  raster::plot(maps_rast[["viewshed_jsy15km"]])
  dev.off()
  
  png("output/background_maps_landcover.png", width = 10, height = 10,
      units = "cm", res = 300)
  raster::plot(maps_rast[["landcover_nmd_mod"]])
  dev.off()
  
  # plot predators
  pred <- grep(pattern = paste(predators, collapse = "|"),
               names(maps), value = T)
  
  plot(maps[[which(names(maps) %in% pred)]])
  
  png("output/background_maps_predators.png", width = 40, height = 30,
      units = "cm", res = 300)
  terra::plot(maps[[which(names(maps) %in% pred)]], )
  dev.off()
}

### try to visualize with rasterVis
# levelplot may be nice to visualize the prdators

# --------------- label=check_correlation_background_variables

#-----------
#' # check correlation among variables

# check correlation among variables using random points within the study area
rand <- amt::random_points(availability_calving, n = 1000) |> 
  sf::st_as_sf(coords = 2:3) |> 
  terra::vect()

rand_background <- terra::extract(maps, rand) |> 
  as.data.frame()

# test later how faster it is!!
# test <- amt::random_points(availability, n = 1000) |> 
#   sf::st_as_sf(coords = 2:3) 
# raster::extract(maps_rast, test)

# pair plots

corr_matrix <- cor(rand_background[-1], use = "complete.obs", method = "spearman")

get_corr <- function(x, th = 0.5, removeNA = TRUE) {
  l <- list()
  for(i in 1:nrow(x)) {
    l[[i]] <- x[i,][x[i,] > th]
    if(removeNA) l[[i]] <- l[[i]][!is.na(l[[i]])]
  }
  names(l) <- rownames(x)
  l
}

infra_types <- c("house", "mining", "power_lines", "priv_road", "public_road", 
                 "trail", "wind_turbine")
corr_ignore <- function(x, infra = "") {
  l <- list()
  for(i in seq_along(x)) {
    ll <- x[[i]]
    included <- unlist(sapply(infra, function(p) grep(pattern = p, names(x)[i])))
    if(length(included) > 0) {
      name <- names(included)
      ll[grep(name, names(ll))] <- NA
      ll <- ll[!is.na(ll)]
    }
    l[[i]] <- ll
  }
  names(l) <- names(x)
  l
}

corr_list_all <- get_corr(corr_matrix)
corr_list <- corr_ignore(corr_list_all, infra = infra_types)

# dem
# g_corr_dem <- rand_background |> 
#   dplyr::select(contains("dem"), contains("tpi")) |> 
#   ggpairs()
# g_corr_dem
# ggsave("g_corr_dem_calving.png", plot = g_corr_dem, path = "output", 
#        width = 20, height = 20, units = "cm", dpi = 300)

corr_dem <- corr_list[grepl("dem|tpi", names(corr_list))] 

#' dem is related to wolverine abundance in 2013, 14, 17, 18

# infrastrucure
# g_corr_infra <- rand_background |> 
#   dplyr::select(contains("dem") & !contains("wol") & !contains("tpi") & !contains("bear") &
#                   !contains("sound")) |> 
#   ggpairs()
# g_corr_infra
# ggsave("g_corr_infra_calving.png", plot = g_corr_infra, path = "output", 
#        width = 40, height = 40, units = "cm", dpi = 300)

corr_infra <- corr_list[grep("wol|tpi|bear|sound", names(corr_list), invert = T)] 

# houses
corr_infra[grepl("house", names(corr_infra))]
# mining
corr_infra[grepl("mining", names(corr_infra))]
# power_lines
corr_infra[grepl("power_lines", names(corr_infra))]
# priv_road
corr_infra[grepl("priv_road", names(corr_infra))]
# public_road
corr_infra[grepl("public_road", names(corr_infra))]
# trail
corr_infra[grepl("trail", names(corr_infra))]
# wind_turbine
corr_infra[grepl("wind_turbine", names(corr_infra))]

#' house_near/cum correlated with public_roads_near/cum for most radii (ca. 0.5-0.58)
#' house_cum1000 correlated with mining_Krist2500-10000 (0.59) and trails10000 (.52) amd bear_2013,15,16 (0.56-0.64)
#' mining_all correlated with bears at most radii and years
#' mining_all_near_cum correlated with priv_road_cum5000-10000 at all radii (0.64)
#' mining_Krist correlated with bears at most radii and years
#' mining_Krist_near_cum correlated with priv_road_cum5000 (0.59) and trails_cum1000 (.62) at all radii
#'   and with houses_cum1000 (.55) at the 2500-1000 radii
#' power_lines_cum/near correlated with wind_turbines_cum/near at most radii (0.58-.73)
#' priv_road_cum10000 correlated with mining(0.64-.66) at all radii, trails_cum10000 (.6) 
#'   and turbines (0.6)
#' trails_cum correlated with turbines (0.7)
#' wind_turbines correlated with power lines in general (0.66) and priv_roads_cum5000-10000 (0.6)
#'   and trail_cum5000-10000 (0.6-.71)

# predators
# g_corr_pred <- rand_background |> 
#   dplyr::select(contains("wol") | contains("bear")) |> 
#   ggpairs()
# g_corr_pred
# ggsave("g_corr_pred_calving.png", plot = g_corr_pred, path = "output", 
#        width = 40, height = 40, units = "cm", dpi = 300)
# 
# # sound
# g_corr_sound <- rand_background |> 
#   dplyr::select(contains("wind") | contains("sound")) |> 
#   ggpairs()
# g_corr_sound
# ggsave("g_corr_sound_calving.png", plot = g_corr_sound, path = "output", 
#        width = 40, height = 40, units = "cm", dpi = 300)

# bear and wolverine densities correlated in 2018 (0.64)

# --------------- label=ssf_one_individual

#-----------
#' # Analysis for one individual - sample of the idea 

# coord_system <- as.character(maps_rast@crs)

if(analysis_1ind) {

  # select individual
  mov_track_1 <- mov_calving |> 
    amt::make_track(.x = x, .y = y, .t = timestamp, Collar_ID, Object_ID, id, burst, year, 
                    phase, crs = sp::CRS(crs_proj)) |> 
    dplyr::filter(id == "rt_RG_08_037_2009")
  
  # check regularity
  samp_rate <- mov_track_1 |> 
    amt::summarize_sampling_rate()
  
  # resample
  stps <- mov_track_1 |> 
    amt::track_resample(rate = hours(round(samp_rate$median)), tolerance = minutes(15)) |> 
    amt::filter_min_n_burst(min_n = 3) |> 
    amt::steps_by_burst(keep_cols = "start")
  
  # explore covariates
  eda1 <- stps |> 
    amt::extract_covariates(maps_rast, where = "start") |> 
    dplyr::mutate(landcover_nmd_mod = factor(landcover_nmd_mod, 
                                             levels = lc_class$code,
                                             labels = lc_class$class))
  
  # organize covariates
  eda1 <- eda1 |> 
    dplyr::rename(dem = dem_10m, aspect = dem_aspect, slope = dem_slope,
                  landcover = landcover_nmd_mod,
                  viewshed = viewshed_mala_binary) |>
    dplyr::rename_with(function(x) gsub("_bin_zoi", "", x), grep("bin", names(eda1))) |> 
    dplyr::rename_with(function(x) gsub("_exp_decay", "", x), grep("exp_decay", names(eda1))) |> 
    dplyr::mutate(
      bear = case_when(
        year == 2012 ~ bear_2012,
        year == 2013 ~ bear_2013,
        year == 2014 ~ bear_2014,
        year == 2015 ~ bear_2015,
        year == 2016 ~ bear_2016,
        year == 2017 ~ bear_2017,
        year == 2018 ~ bear_2018),
      wolverine = case_when(
        year == 2013 ~ wolverine_2013,
        year == 2014 ~ wolverine_2014,
        year == 2015 ~ wolverine_2015,
        year == 2016 ~ wolverine_2016,
        year == 2017 ~ wolverine_2017,
        year == 2018 ~ wolverine_2018,
        year == 2019 ~ wolverine_2019),
    ) |> 
    dplyr::select(-contains("bear_"), -contains("wolverine_"))
    
  # Some plots to explore
  
  # step length
  eda1 |> 
    ggplot() +
    geom_boxplot(aes(x = landcover, y = sl_))
  
  eda1 |> 
    ggplot() +
    geom_point(aes(x = wind_turbines_nearest1000, y = sl_)) 
  
  g1 <- ggplot(data = eda1, aes(x = sl_, y = landcover, fill = landcover)) +
    ggridges::geom_density_ridges(alpha = 0.5) +
    theme_bw() +
    theme(legend.position = "none")
  g1
  
  # turning angle
  eda1 |> 
    ggplot() +
    geom_boxplot(aes(x = landcover, y = ta_))
  
  g2 <- ggplot(data = eda1, aes(x = ta_ * 180 / pi, y = landcover, fill = landcover)) +
    ggridges::geom_density_ridges(alpha = 0.5) +
    theme_bw() +
    theme(legend.position = "none")
  g2
  
  #------------------------
  # rsf for one individual
  
  # select individual
  mov_track_1 <- mov_calving |> 
    amt::mk_track(.x = x, .y = y, .t = timestamp, Collar_ID, Object_ID, id, burst, year, 
                  phase, crs = sp::CRS(crs_proj)) |> 
    dplyr::filter(id == "rt_RG_08_037_2009")
  
  # check regularity
  samp_rate <- mov_track_1 |> 
    amt::summarize_sampling_rate()
  
  # resample
  mov_track_rsp_1 <- mov_track_1 |> 
    amt::track_resample(rate = hours(round(samp_rate$median)), tolerance = minutes(15)) |> 
    amt::filter_min_n_burst(min_n = 3) |> 
    amt::steps_by_burst(keep_cols = "start") |> 
    dplyr::select(x_ = x1_, y_ = y1_, t_ = t1_, Collar_ID, Object_ID, id, year,
                  phase) |> 
    dplyr::mutate(case_ = TRUE) |> 
    as.data.frame() |> 
    amt::mk_track(x_, y_, t_, all_cols = T)
  
  # create random points within the available area
  n_random_points <- nrow(mov_track_rsp_1)
  rand_pts1 <- amt::random_points(availability_calving, n = 10*n_random_points)
  
  # combine
  mov_track_comb_1 <- mov_track_rsp_1 |> 
    dplyr::bind_rows(rand_pts1) |> 
    dplyr::mutate_at(vars(Collar_ID:phase), function(x) x[1]) |>
    dplyr::mutate(case_ = as.numeric(case_))
  
  
  # combine
  mov_track_annotated_1 <- mov_track_comb_1 |> 
    amt::extract_covariates(maps_rast, where = "start") |> 
    dplyr::mutate(landcover_nmd_mod = factor(landcover_nmd_mod, 
                                             levels = lc_class$code,
                                             labels = lc_class$class)) |> 
    # dplyr::mutate(log_building = log(building_dist+1),
    #               log_house = log(house_dist+1),
    #               log_pl = log(pl_dist+1),
    #               log_priv_road = log(priv_road_dist+1),
    #               log_public_road = log(public_road_dist+1),
    #               log_mining = log(mining_dist+1),
    #               log_trails = log(trail_dist+1),
    #               log_wind = log(wind_dist+1)) |> 
    dplyr::rename(dem = dem_10m, aspect = dem_aspect, slope = dem_slope,
                  landcover = landcover_nmd_mod) %>%
    dplyr::rename_with(function(x) gsub("_bin_zoi", "", x), grep("bin", names(.))) %>%
    dplyr::rename_with(function(x) gsub("_exp_decay", "", x), grep("exp_decay", names(.))) %>%
    dplyr::mutate(
      bear = case_when(
        year == 2012 ~ bear_2012,
        year == 2013 ~ bear_2013,
        year == 2014 ~ bear_2014,
        year == 2015 ~ bear_2015,
        year == 2016 ~ bear_2016,
        year == 2017 ~ bear_2017,
        year == 2018 ~ bear_2018),
      wolverine = case_when(
        year == 2013 ~ wolverine_2013,
        year == 2014 ~ wolverine_2014,
        year == 2015 ~ wolverine_2015,
        year == 2016 ~ wolverine_2016,
        year == 2017 ~ wolverine_2017,
        year == 2018 ~ wolverine_2018,
        year == 2019 ~ wolverine_2019),
    ) |> 
    dplyr::select(-contains("bear_"), -contains("wolverine_"))
  
  # check ranges
  # apply(points(mov_track_rsp_1$x_, mov_track_rsp_1$y_, col = 2, pch = 19), 2, range, na.rm = T) |> t()
  
  # plot
  png("output/example_rsf_points.png", width = 15, height = 15,
      units = "cm", res = 300)
  par(mfrow = c(2,2))
  # used
  plot(mov_track_rsp_1$x_, mov_track_rsp_1$y_, type = "n",
       xlab = "", ylab = "")
  plot(maps_rast$landcover_nmd_mod, add = T)
  points(mov_track_rsp_1$x_, mov_track_rsp_1$y_, pch = 19, 
         col = rgb(1, 0, 0, 0.5))
  plot(maps_rast$landcover_nmd_mod)
  with(filter(points(mov_track_rsp_1$x_, mov_track_rsp_1$y_, col = 2, pch = 19), case_ == F), points(x_, y_, pch = 19, col = grey(0.1, 0.5)))
  points(mov_track_rsp_1$x_, mov_track_rsp_1$y_, col = 2, pch = 19)
  par(mfrow = c(1,1))
  dev.off()
  
  # run models
  m0 <- with(mov_track_annotated_1,
             glm(case_ ~ landcover + dem + slope, family = binomial))
  summary(m0)
  
  m1 <- with(mov_track_annotated_1,
             glm(case_ ~ slope + scale(wind_turbines_cumulative10000) + landcover, family = binomial))
  summary(m1)
  
  broom::glance(m1)
  broom::tidy(m1, conf.int = T)
  
  m1_plot <- broom::tidy(m1, conf.int = T) |> 
    dplyr::mutate(significant = as.factor(if_else(p.value < 0.05, 1, 0))) |> 
    ggplot(aes(x = term, y = estimate, color = significant)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                    position = position_dodge(width = 0.7), size = 0.8,
                    show.legend = F) +
    geom_hline(yintercept = 0, lty = 2) +
    labs(y = "Relative Selection Strength", x = "") +
    theme_light() + 
    coord_flip()
  m1_plot
  ggsave("model_example_1ind.png", plot = m1_plot, path = "output", 
         width = 12, height = 15, units = "cm", dpi = 300)
  
  pr1 <- ggpredict(m1, terms = "landcover")
  plot(pr1) +
    coord_flip() +
    geom_hline(yintercept = 0.5, linetype = 2, color = "grey")
  
  ggpredict(m1, terms = "slope [all]") |> 
    plot() +
    geom_hline(yintercept = 0.5, linetype = 2, color = "grey")
  
  ggpredict(m1, terms = "wind_turbines_cumulative10000") |> 
    plot(add.data = F) +
    geom_hline(yintercept = 0.5, linetype = 2, color = "grey")
  
  #-----------------------
  
  # visualization, each individual with the availability polygon
  ggplot(mov_track_1) +
    geom_sf(data = availability_calving) +
    geom_point(aes(x_, y_)) +
    labs(x = "x", y = "y")
  
  # visualization of used and random points
  ggplot() +
    geom_sf(data = availability_calving) +
    geom_point(aes(x_, y_), col = "grey50", data = dplyr::filter(mov_track_comb_1, case_ == F)) +
    geom_point(aes(x_, y_), col = "red", data = dplyr::filter(mov_track_comb_1, case_ == T)) +
    labs(x = "x", y = "y")
}

# --------------- label=all_inds

#-----------
#' # Annotation for all individuals

# transform into track object - calving
mov_track_calving <- mov_calving |> 
  amt::mk_track(.x = x, .y = y, .t = timestamp, Collar_ID, Object_ID, id, burst, year, 
                phase, crs = sp::CRS(crs_proj))

mov_track_summer <- mov_summer |> 
  amt::mk_track(.x = x, .y = y, .t = timestamp, Collar_ID, Object_ID, id, burst, year, 
                phase, crs = sp::CRS(crs_proj))

mov_track_autumn <- mov_autumn |> 
  amt::mk_track(.x = x, .y = y, .t = timestamp, Collar_ID, Object_ID, id, burst, year, 
                phase, crs = sp::CRS(crs_proj))

# calculate sampling rate statistics for all individuals - calving
samp_rate <- mov_track_calving |> 
  amt::summarize_sampling_rate_many(cols = c("burst"), time_unit = "hour")
#View(samp_rate)

# most common
samp_rate |> 
  dplyr::left_join(
    mov_track_calving |>
      dplyr::select(burst, year),
    by = "burst") |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(avg_mean = mean(mean),
                   avg_median = mean(median),
                   med_mean = median(mean),
                   med_median = median(median))

# more common: 2h until 2018

# calculate sampling rate statistics for all individuals - summer
samp_rate <- mov_track_summer |> 
  amt::summarize_sampling_rate_many(cols = c("burst"), time_unit = "hour")
#View(samp_rate)

# most common
samp_rate |> 
  dplyr::left_join(
    mov_track_summer |>
      dplyr::select(burst, year),
    by = "burst") |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(avg_mean = mean(mean),
                   avg_median = mean(median),
                   med_mean = median(mean),
                   med_median = median(median))

# more common: 2h until 2018

# calculate sampling rate statistics for all individuals - autumn
samp_rate <- mov_track_autumn |> 
  amt::summarize_sampling_rate_many(cols = c("burst"), time_unit = "hour")
#View(samp_rate)

# most common
samp_rate |> 
  dplyr::left_join(
    mov_track_autumn |>
      dplyr::select(burst, year),
    by = "burst") |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(avg_mean = mean(mean),
                   avg_median = mean(median),
                   med_mean = median(mean),
                   med_median = median(median))

# more common: 2h until 2018

#------------
# resample and generate random points (ua: used-available)
# using 2h fix rate, with 15min tolerance

# number of random positions for each used location position
n_random_pts <- 10

# calving
mov_track_ua_calving_2h <- mov_track_calving |> 
  tidyr::nest(data = c(x_, y_, t_, Collar_ID, Object_ID, burst, year, phase)) |> 
  dplyr::mutate(trk = map(data, function(d, avalability = availability_calving,
                                         fix_rate_h = 2, tolerance_min = 15) {
    cat(paste(d$Object_ID[1], d$year[1], "\n"))
    # resample
    mov_track_rsp_1 <- d |> 
      amt::track_resample(rate = hours(fix_rate_h), tolerance = minutes(tolerance_min)) |> 
      amt::filter_min_n_burst(min_n = 3) |>
      dplyr::mutate(case_ = TRUE)
    
    # get NULL if this individual had too sparse data or too few points after
    # separating the sub-bursts
    if(nrow(mov_track_rsp_1) < 5*(24/fix_rate_h)) { print("NA"); return(NULL)  }
    
    # create random points within the available area
    n_random_points <- n_random_pts*nrow(mov_track_rsp_1)
    rand_pts1 <- amt::random_points(avalability, n = n_random_points) |> 
      dplyr::mutate(Collar_ID = mov_track_rsp_1$Collar_ID[1],
                    Object_ID = mov_track_rsp_1$Object_ID[1],
                    year = mov_track_rsp_1$year[1],
                    phase = mov_track_rsp_1$phase[1])
    
    # combine
    mov_track_comb_1 <- mov_track_rsp_1 |> 
      dplyr::bind_rows(rand_pts1) |> 
      #dplyr::mutate_at(vars(Collar_ID:phase), function(x) x[1]) |>
      dplyr::mutate(case_ = as.numeric(case_))
    
    mov_track_comb_1
  }))

# summer
mov_track_ua_summer_2h <- mov_track_summer |> 
  tidyr::nest(data = c(x_, y_, t_, Collar_ID, Object_ID, burst, year, phase)) |> 
  dplyr::mutate(trk = map(data, function(d, avalability1 = availability_calving,
                                         availability2 = availability_summer,
                                         date_break = lubridate::yday(lubridate::ymd("20080731")),
                                         fix_rate_h = 2, tolerance_min = 15) {
    
    cat(paste(d$Object_ID[1], d$year[1], "\n"))
    # resample
    mov_track_rsp_1 <- d |> 
      amt::track_resample(rate = hours(fix_rate_h), tolerance = minutes(tolerance_min)) |> 
      amt::filter_min_n_burst(min_n = 3) |>
      dplyr::mutate(case_ = TRUE)
    
    # get NULL if this individual had too sparse data or too few points after
    # separating the sub-bursts
    if(nrow(mov_track_rsp_1) < 5*(24/fix_rate_h)) { print("NA"); return(NULL)  }
    
    # first half
    nrow1 <- mov_track_rsp_1 |> 
      dplyr::filter(lubridate::yday(t_) <= date_break) |> 
      nrow()
    
    nrow2 <- mov_track_rsp_1 |> 
      dplyr::filter(lubridate::yday(t_) > date_break) |> 
      nrow()
    
    # create random points within the available area
    rand_pts1 <- amt::random_points(avalability1, n = n_random_pts*nrow1) |> 
      dplyr::mutate(Collar_ID = mov_track_rsp_1$Collar_ID[1],
                    Object_ID = mov_track_rsp_1$Object_ID[1],
                    year = mov_track_rsp_1$year[1],
                    phase = mov_track_rsp_1$phase[1])
    
    rand_pts2 <- amt::random_points(availability2, n = n_random_pts*nrow2) |> 
      dplyr::mutate(Collar_ID = mov_track_rsp_1$Collar_ID[1],
                    Object_ID = mov_track_rsp_1$Object_ID[1],
                    year = mov_track_rsp_1$year[1],
                    phase = mov_track_rsp_1$phase[1])
    
    # combine
    mov_track_comb_1 <- mov_track_rsp_1 |> 
      dplyr::bind_rows(rand_pts1, rand_pts2) |> 
      #dplyr::mutate_at(vars(Collar_ID:phase), function(x) x[1]) |>
      dplyr::mutate(case_ = as.numeric(case_))
    
    mov_track_comb_1
  }))

# autumn
mov_track_ua_autumn_2h <- mov_track_autumn |> 
  tidyr::nest(data = c(x_, y_, t_, Collar_ID, Object_ID, burst, year, phase)) |> 
  dplyr::mutate(trk = map(data, function(d, avalability = availability_autumn,
                                         fix_rate_h = 2, tolerance_min = 15) {
    cat(paste(d$Object_ID[1], d$year[1], "\n"))
    # resample
    mov_track_rsp_1 <- d |> 
      amt::track_resample(rate = hours(fix_rate_h), 
                          tolerance = minutes(tolerance_min)) |> 
      amt::filter_min_n_burst(min_n = 3) |>
      dplyr::mutate(case_ = TRUE)
    
    # get NULL if this individual had too sparse data or too few points after
    # separating the sub-bursts
    if(nrow(mov_track_rsp_1) < 5*(24/fix_rate_h)) { print("NA"); return(NULL)  }
    
    # create random points within the available area
    n_random_points <- n_random_pts*nrow(mov_track_rsp_1)
    rand_pts1 <- amt::random_points(avalability, n = n_random_points) |> 
      dplyr::mutate(Collar_ID = mov_track_rsp_1$Collar_ID[1],
                    Object_ID = mov_track_rsp_1$Object_ID[1],
                    year = mov_track_rsp_1$year[1],
                    phase = mov_track_rsp_1$phase[1])
    
    # combine
    mov_track_comb_1 <- mov_track_rsp_1 |> 
      dplyr::bind_rows(rand_pts1) |> 
      #dplyr::mutate_at(vars(Collar_ID:phase), function(x) x[1]) |>
      dplyr::mutate(case_ = as.numeric(case_))
    
    mov_track_comb_1
  }))

#-----------
# extract covariates

# calving
mov_track_ua_calving_2h_annotated <- mov_track_ua_calving_2h |> 
  dplyr::mutate(trk_ann = map(trk, function(d) {
    
    cat(paste(d$Object_ID[1], d$year[1], "\n"))
    # extract covariates
    if(is.null(d)) 
      return(NULL)
    
    pts <- d |> 
      sf::st_as_sf(coords = 1:2) |> 
      terra::vect()
    
    rand_background <- terra::extract(maps, pts) |> 
      dplyr::as_tibble()
    
    d |>
      dplyr::bind_cols(rand_background) |> 
      dplyr::mutate(landcover_nmd_mod = factor(landcover_nmd_mod, 
                                               levels = lc_class$code,
                                               labels = lc_class$class)) |>
      dplyr::rename(dem = dem_10m, aspect = dem_aspect, slope = dem_slope,
                    landcover = landcover_nmd_mod) %>%
      dplyr::rename_with(function(x) gsub("_bin_zoi", "", x), grep("bin", names(.))) %>%
      dplyr::rename_with(function(x) gsub("_exp_decay", "", x), grep("exp_decay", names(.))) %>%
      dplyr::mutate(
        bear = case_when(
          year == 2012 ~ bear_2012,
          year == 2013 ~ bear_2013,
          year == 2014 ~ bear_2014,
          year == 2015 ~ bear_2015,
          year == 2016 ~ bear_2016,
          year == 2017 ~ bear_2017,
          year == 2018 ~ bear_2018),
        wolverine = case_when(
          year == 2013 ~ wolverine_2013,
          year == 2014 ~ wolverine_2014,
          year == 2015 ~ wolverine_2015,
          year == 2016 ~ wolverine_2016,
          year == 2017 ~ wolverine_2017,
          year == 2018 ~ wolverine_2018,
          year == 2019 ~ wolverine_2019),
      ) |> 
      dplyr::select(-contains("bear_"), -contains("wolverine_"))
  }))

# summer
mov_track_ua_summer_2h_annotated <- mov_track_ua_summer_2h |> 
  dplyr::mutate(trk_ann = map(trk, function(d) {
    
    cat(paste(d$Object_ID[1], d$year[1], "\n"))
    # extract covariates
    if(is.null(d)) 
      return(NULL)
    
    pts <- d |> 
      sf::st_as_sf(coords = 1:2) |> 
      terra::vect()
    
    rand_background <- terra::extract(maps, pts) |> 
      dplyr::as_tibble()
    
    d |>
      dplyr::bind_cols(rand_background) |> 
      dplyr::mutate(landcover_nmd_mod = factor(landcover_nmd_mod, 
                                               levels = lc_class$code,
                                               labels = lc_class$class)) |>
      dplyr::rename(dem = dem_10m, aspect = dem_aspect, slope = dem_slope,
                    landcover = landcover_nmd_mod) %>%
      dplyr::rename_with(function(x) gsub("_bin_zoi", "", x), grep("bin", names(.))) %>%
      dplyr::rename_with(function(x) gsub("_exp_decay", "", x), grep("exp_decay", names(.))) %>%
      dplyr::mutate(
        bear = case_when(
          year == 2012 ~ bear_2012,
          year == 2013 ~ bear_2013,
          year == 2014 ~ bear_2014,
          year == 2015 ~ bear_2015,
          year == 2016 ~ bear_2016,
          year == 2017 ~ bear_2017,
          year == 2018 ~ bear_2018),
        wolverine = case_when(
          year == 2013 ~ wolverine_2013,
          year == 2014 ~ wolverine_2014,
          year == 2015 ~ wolverine_2015,
          year == 2016 ~ wolverine_2016,
          year == 2017 ~ wolverine_2017,
          year == 2018 ~ wolverine_2018,
          year == 2019 ~ wolverine_2019),
      ) |> 
      dplyr::select(-contains("bear_"), -contains("wolverine_"))
  }))

# autumn
mov_track_ua_autumn_2h_annotated <- mov_track_ua_autumn_2h |> 
  dplyr::mutate(trk_ann = map(trk, function(d) {
    
    cat(paste(d$Object_ID[1], d$year[1], "\n"))
    # extract covariates
    if(is.null(d)) 
      return(NULL)
    
    pts <- d |> 
      sf::st_as_sf(coords = 1:2) |> 
      terra::vect()
    
    rand_background <- terra::extract(maps, pts) |> 
      dplyr::as_tibble()
    
    d |>
      dplyr::bind_cols(rand_background) |> 
      dplyr::mutate(landcover_nmd_mod = factor(landcover_nmd_mod, 
                                               levels = lc_class$code,
                                               labels = lc_class$class)) |>
      dplyr::rename(dem = dem_10m, aspect = dem_aspect, slope = dem_slope,
                  landcover = landcover_nmd_mod) %>%
      dplyr::rename_with(function(x) gsub("_bin_zoi", "", x), grep("bin", names(.))) %>%
      dplyr::rename_with(function(x) gsub("_exp_decay", "", x), grep("exp_decay", names(.))) %>%
      dplyr::mutate(
        bear = case_when(
          year == 2012 ~ bear_2012,
          year == 2013 ~ bear_2013,
          year == 2014 ~ bear_2014,
          year == 2015 ~ bear_2015,
          year == 2016 ~ bear_2016,
          year == 2017 ~ bear_2017,
          year == 2018 ~ bear_2018),
        wolverine = case_when(
          year == 2013 ~ wolverine_2013,
          year == 2014 ~ wolverine_2014,
          year == 2015 ~ wolverine_2015,
          year == 2016 ~ wolverine_2016,
          year == 2017 ~ wolverine_2017,
          year == 2018 ~ wolverine_2018,
          year == 2019 ~ wolverine_2019),
      ) |> 
      dplyr::select(-contains("bear_"), -contains("wolverine_"))
  }))

# here we save the data with covariates extracted for used and available location
save(mov_track_ua_calving_2h_annotated, file = "data/01_GPS/05_annotated_data/tmp_data_annotated_inds_2h_calving_rsf.RData")
save(mov_track_ua_summer_2h_annotated, file = "data/01_GPS/05_annotated_data/tmp_data_annotated_inds_2h_summer_rsf.RData")
save(mov_track_ua_autumn_2h_annotated, file = "data/01_GPS/05_annotated_data/tmp_data_annotated_inds_2h_autumn_rsf.RData")

#-----------
# Calving

# load(file = "data/01_GPS/05_annotated_data/tmp_data_annotated_inds_2h_calving_rsf.RData")
mov_track_ua_calving_2h_annotated

# 2h fix rate
# unnest annotated data to standardize covariates
mov_track_ua_calving_2h_annotated_inds <- mov_track_ua_calving_2h_annotated |> 
  dplyr::filter(!sapply(mov_track_ua_calving_2h_annotated$trk, is.null)) |> 
  dplyr::select(-c(data, trk)) |> 
  tidyr::unnest_legacy(cols = c(trk_ann)) 

str(mov_track_ua_calving_2h_annotated_inds)
summary(mov_track_ua_calving_2h_annotated_inds)

# test plot used and available for distance to WF
if(plot_maps) {
  png("output/use_ava_dist_wind_mala.png",
      width = 10, height = 15, units = "cm", res = 300)
  
  par(mfrow = c(3,2))

  for(i in c("Before construction", "Construction", "Operation")) {
    
    mov_track_ua_calving_2h_annotated_inds %>%
      dplyr::filter(.$case_ == T & phase == i) |>
      pull(wind_turbines_dist_jsy) |>
      hist(main = paste0("used steps, ", i), xlim = c(0, 4e4),
           xlab = "distance to WF (m)")
    
    mov_track_ua_calving_2h_annotated_inds |>
      dplyr::filter(case_ == F & phase == i) |>
      pull(wind_turbines_dist_jsy) |>
      hist(main = paste0("available steps, ", i), xlim = c(0, 4e4),
           xlab = "distance to WF (m)")
  }
  dev.off()
}

if(plot_maps) {
  png("output/use_ava_zoi_wind_mala.png",
      width = 10, height = 15, units = "cm", res = 300)
  
  par(mfrow = c(3,2))
  
  for(i in c("Before construction", "Construction", "Operation")) {
    
    mov_track_ua_calving_2h_annotated_inds %>%
      dplyr::filter(.$case_ == T & phase == i) |>
      pull(wind_turbines_nearest2500) |>
      hist(main = paste0("used steps, ", i), xlim = c(0, .2), breaks = 100,
           xlab = "distance to WF (m)", ylim = c(0, 5000))
    
    mov_track_ua_calving_2h_annotated_inds |>
      dplyr::filter(case_ == F & phase == i) |>
      pull(wind_turbines_nearest2500) |>
      hist(main = paste0("available steps, ", i), xlim = c(0, .2), breaks = 100,
           xlab = "distance to WF (m)", ylim = c(0, 5000))
  }
  dev.off()
}
# function to standardize variables by subtracting the mean and dividing by 2*SD
std_2sd <- function(x, na.rm = T) (x - mean(x, na.rm = na.rm))/(2*sd(x, na.rm = na.rm))
std_2sd(mov_track_ua_calving_2h_annotated_inds$dem) # test

# standardize covariates

#----------------
# TPI as category

# calculate SD of TPI using random points
rand <- amt::random_points(availability_autumn, n = 10000) |> 
  sf::st_as_sf(coords = 2:3) |> 
  terra::vect()

rand_background <- terra::extract(maps, rand) |> 
  as.data.frame() |> 
  dplyr::select(contains("tpi"))

SD <- apply(rand_background, 2, sd, na.rm = TRUE)

mov_track_ua_calving_2h_annotated_inds_tpi <- 
  mov_track_ua_calving_2h_annotated_inds |> 
  dplyr::mutate(
    tpi_cat_s150m = case_when(
      tpi_s150m <= -SD[1] ~ "valley",
      tpi_s150m > -SD[1] & tpi_s150m <= -SD[1]/2  ~ "lower slope",
      tpi_s150m > -SD[1]/2 & tpi_s150m <= 0  ~ "flat area",
      tpi_s150m > 0 & tpi_s150m <= SD[1]/2  ~ "medium slope",
      tpi_s150m > SD[1]/2 & tpi_s150m <= SD[1]  ~ "upper slope",
      TRUE ~ "ridge"
    ),
    tpi_cat_s250m = case_when(
      tpi_s250m <= -SD[2] ~ "valley",
      tpi_s250m > -SD[2] & tpi_s250m <= -SD[2]/2  ~ "lower slope",
      tpi_s250m > -SD[2]/2 & tpi_s250m <= 0  ~ "flat area",
      tpi_s250m > 0 & tpi_s250m <= SD[2]/2  ~ "medium slope",
      tpi_s250m > SD[2]/2 & tpi_s250m <= SD[2]  ~ "upper slope",
      TRUE ~ "ridge"
    ),
    tpi_cat_s510m = case_when(
      tpi_s510m <= -SD[3] ~ "valley",
      tpi_s510m > -SD[3] & tpi_s510m <= -SD[3]/2  ~ "lower slope",
      tpi_s510m > -SD[3]/2 & tpi_s510m <= 0  ~ "flat area",
      tpi_s510m > 0 & tpi_s510m <= SD[3]/2  ~ "medium slope",
      tpi_s510m > SD[3]/2 & tpi_s510m <= SD[3]  ~ "upper slope",
      TRUE ~ "ridge"
    )
  )

# visualize
mov_track_ua_calving_2h_annotated_inds_tpi %>%
  dplyr::slice(sample(1:nrow(.), size = 11000, replace = F)) |> 
  dplyr::filter(case_ == F) |> 
  sf::st_as_sf(coords = c("x_", "y_")) |> 
  ggplot() +
  geom_sf(aes(colour = tpi_cat_s510m))

#---------------------
# landcover with clear cuts

mov_track_ua_calving_2h_annotated_inds_tpi$clear_cuts_2020
mov_track_ua_calving_2h_annotated_inds_tpi$landcover
mov_track_ua_calving_2h_annotated_inds_tpi$landcover_smd_clearcuts2007

year_NMD <- 2018

mov_track_ua_calving_2h_annotated_inds_tpi_landcc <- mov_track_ua_calving_2h_annotated_inds_tpi |> 
  dplyr::mutate(landcover_cc = case_when(
    clear_cuts_2020 > 0 & lubridate::year(t_) > clear_cuts_2020 ~ "clear cuts",
    clear_cuts_2020 > 0 & lubridate::year(t_) < clear_cuts_2020 & clear_cuts_2020 <= year_NMD ~ as.character(landcover_smd_clearcuts2007),
    TRUE ~ as.character(landcover)
  ))

mov_track_ua_calving_2h_annotated_inds_tpi_landcc$landcover_cc |> as.factor() |> levels()

mov_track_ua_calving_2h_annotated_inds_tpi_landcc <- mov_track_ua_calving_2h_annotated_inds_tpi_landcc |> 
  dplyr::mutate(landcover_cc = case_when(
    landcover_cc == "4" ~ "anthropogenic areas",
    landcover_cc == "32" ~ "arable lands",
    landcover_cc == "43" | landcover_cc == "44" | landcover_cc == "45" | landcover_cc == "pine forests" | landcover_cc == "other coniferous forests" ~ "coniferous forests",
    landcover_cc == "40" | landcover_cc == "48" ~ "deciduous forests",
    landcover_cc == "46" ~ "forested wetlands",
    landcover_cc == "54" ~ "clear cuts",
    landcover_cc == "55" | landcover_cc == "56" ~ "coniferous forests",
    landcover_cc == "71" | landcover_cc == "72" ~ "open wetlands",
    TRUE ~ landcover_cc
  ) |> as.factor(),
  landcover_cc = forcats::fct_collapse(landcover_cc, mires = c("open wetlands", "heath"),
                                       "open lands" = c("arable lands", "other open lands")) |> 
    droplevels() |> 
    forcats::fct_relevel("clear cuts", after = 0)
  )

mov_track_ua_calving_2h_annotated_inds_tpi_landcc$landcover_cc
mov_track_ua_calving_2h_annotated_inds_tpi_landcc$landcover_cc |> levels()

data_used <- mov_track_ua_calving_2h_annotated_inds_tpi_landcc |> 
  dplyr::filter(case_ == 1)
table(data_used$landcover_cc,
      data_used$phase)

# create viewshed variables with 3 categories
lc_class
# in sight open(1): in sight with open wetlands, arable lands, 
#    other open lands, anthropogenic, water, heath, tundra
# in sight cover(2): in sight with pine forests, other coniferous, 
#    deciduous forests, forested wetlands
# out of sight(0): out of sight
labs_view <- c("out of sight", "in sight open", "in sight cover")
mov_track_ua_calving_2h_annotated_inds_tpi_landcc_view <- 
  mov_track_ua_calving_2h_annotated_inds_tpi_landcc |>
  dplyr::mutate(viewshed_lc = ifelse(viewshed_jsy15km == 1 & grepl("forest", landcover_cc),
                                     2, viewshed_jsy15km) |> 
                  factor(levels = c(0, 1, 2), labels = labs_view))

mov_track_ua_calving_2h_annotated_inds_tpi_landcc_view$viewshed_lc

# visualize
mov_track_ua_calving_2h_annotated_inds_tpi_landcc_view %>% 
  dplyr::slice(sample(1:nrow(.), size = 11000, replace = F)) |> 
  dplyr::filter(case_ == 1) |> 
  sf::st_as_sf(coords = c("x_", "y_")) |> 
  ggplot() +
  geom_sf(aes(colour = viewshed_lc))

#--------------- 
# get data ready to analysis

# remove individuals present only during later part of the calving season
# (those who had their calves within corrals in 2017-18)
(remove_calving <- readr::read_csv("data/01_GPS/individuals_to_remove_calving_Mala_2021_05_d02.csv") |> 
   pull(Object_ID_year))

data_ready_calving_rsf_2h <- mov_track_ua_calving_2h_annotated_inds_tpi_landcc_view |> 
  dplyr::filter(landcover_cc != "water") |> 
  dplyr::filter(!(id %in% remove_calving)) |> 
  dplyr::filter(!if_any(contains(c("cumulative", "nearest")), is.na)) |> 
  dplyr::mutate(landcover_cc = droplevels(landcover_cc),
                viewshed = factor(viewshed_jsy, levels = 0:1, labels = c("out of sight", "in sight")),
                across(starts_with("sound_model"), ~ if_else(is.na(.x), 0, .x)))

# check
str(data_ready_calving_rsf_2h)
summary(data_ready_calving_rsf_2h)
apply(data_ready_calving_rsf_2h, 2, function(x) sum(is.na(x)))

# data_ready_calving_rsf_2h |>
#   dplyr::filter(is.na(house_cumulative10000)) |>
#   dplyr::filter(is.na(sound_model_mala_55)) |>
#   dplyr::slice(1:1000) |> 
#   sf::st_as_sf(coords = c("x_", "y_")) |>
#   ggplot() +
#   geom_sf(aes(colour = 1))

# limited viewshed for amliden
dist_cutoff <- 1.5e4
wfs <- c("jokkmokksliden", "storliden", "ytteberg", "amliden")
data_ready_calving_rsf_2h <- data_ready_calving_rsf_2h %>% 
  dplyr::mutate(viewshed_aml15km = ifelse(wind_turbines_dist_aml >  dist_cutoff, 0, viewshed_amliden_mala_binary),
                cos_aspect = cos(aspect),
                wind_turbines_dist_comb = pmin(wind_turbines_dist_jsy, wind_turbines_dist_aml),
                dist_wf_closest = apply(cbind(wind_turbines_dist_jokk, wind_turbines_dist_stor, wind_turbines_dist_ytte, wind_turbines_dist_aml), 1, which.min),
                wf_name = factor(dist_wf_closest, levels = 1:4, labels = wfs))

# apply(cbind(data_ready_calving_rsf_2h$wind_dist_jokk, data_ready_calving_rsf_2h$wind_dist_stor), MARGIN = 1, which.min)
data_ready_calving_rsf_2h$wf_name

# load NDVI data
ndvi_sum_calving_beck <- stars::read_stars(.x = "maps/ndvi/ok/ndvi_sum_calving_beck.tif")
ndvi_sum_calving_elmore <- stars::read_stars(.x = "maps/ndvi/ok/ndvi_sum_calving_elmore.tif")
# load sos data
sos_beck <- stars::read_stars(.x = "maps/ndvi/ok/sos_beck.tif")
sos_elmore <- stars::read_stars(.x = "maps/ndvi/ok/sos_elmore.tif")

# monthday
md <- "0501"

# merge ndvi maps
ndvi_maps <- c(ndvi_sum_calving_beck, ndvi_sum_calving_elmore)
years_data <- attr(ndvi_maps, "dimensions")$band$values
ndvi_maps <- stars::st_set_dimensions(ndvi_maps, 3, values = lubridate::ymd(paste0(years_data, md)), names = "time")

# merge start of season maps
sos_maps <- c(sos_beck[,,,years_data], sos_elmore[,,,years_data])
sos_maps <- stars::st_set_dimensions(sos_maps, 3, values = lubridate::ymd(paste0(years_data, md)), names = "time")

#load("data/01_GPS/05_annotated_data/data_annotated_calving_ready_2h_rsf.RData")
# data_ready_calving_rsf_2h

# select relevant data for extraction
dat_annot <- data_ready_calving_rsf_2h |>
  dplyr::select(id, x_, y_, t_, year) |> 
  sf::st_as_sf(coords = c("x_", "y_"), crs = 3006) |>
  sf::st_transform(crs = 4326) |>
  dplyr::mutate(t_interp = lubridate::ymd(paste0(year, md)))
 
# stars::st_extract(ndvi_maps, dat_annot, time_column = "t_interp")
# stars::st_extract(sos_maps, dat_annot, time_column = "t_interp")

# annotate in space and time, get average between methods
ndvi <- stars::st_extract(ndvi_maps, dat_annot, time_column = "t_interp") |> 
  dplyr::rename(beck = 1, elmore = 2) |> 
  sf::st_drop_geometry()
ndvi$ndvi = rowMeans(ndvi[c("beck", "elmore")])
sos <- stars::st_extract(sos_maps, dat_annot, time_column = "t_interp") |> 
  dplyr::rename(beck = 1, elmore = 2) |> 
  sf::st_drop_geometry()
sos$sos = rowMeans(sos[c("beck", "elmore")])

data_ready_calving_rsf_2h <- cbind(data_ready_calving_rsf_2h, ndvi["ndvi"], sos["sos"]) |>
  dplyr::filter(!if_any(contains(c("sos", "ndvi")), is.na)) 

# check NAs
apply(data_ready_calving_rsf_2h, 2, function(x) sum(is.na(x)))

# keep 9 random points for each used point
table(data_ready_calving_rsf_2h$id, data_ready_calving_rsf_2h$case_)
plot(data_ready_calving_rsf_2h$case_[1:10000])

n_points <- 9 + 1
data_ready_calving_rsf_2h_9pt <- data_ready_calving_rsf_2h %>% 
  plyr::ddply(., "id",
              function(x) {
                n <- sum(x$case_ == 1)
                return(x[1:(n_points*n),])
              }, .progress = "text") %>% 
  tibble::as_tibble()
data_ready_calving_rsf_2h_9pt
plot(data_ready_calving_rsf_2h_9pt$case_[1:10000])
table(data_ready_calving_rsf_2h_9pt$id, data_ready_calving_rsf_2h_9pt$case_)
data_ready_calving_rsf_2h <- data_ready_calving_rsf_2h_9pt
data_ready_calving_rsf_2h$year %>% table()

# save
save(data_ready_calving_rsf_2h, file = "data/01_GPS/05_annotated_data/data_annotated_calving_ready_2h_rsf.RData")

#-----------
# Summer

# load(file = "data/01_GPS/05_annotated_data/tmp_data_annotated_inds_2h_summer_rsf.RData")
mov_track_ua_summer_2h_annotated

# 2h fix rate
# unnest annotated data to standardize covariates
mov_track_ua_summer_2h_annotated_inds <- mov_track_ua_summer_2h_annotated %>% 
  dplyr::filter(!sapply(.$trk, is.null)) |> 
  dplyr::select(-c(data, trk)) |> 
  tidyr::unnest_legacy(cols = c(trk_ann)) |> 
  dplyr::filter(year <= 2018)

str(mov_track_ua_summer_2h_annotated_inds)
summary(mov_track_ua_summer_2h_annotated_inds)

if(plot_maps) {
  png("output/use_ava_dist_wind_mala.png",
    width = 10, height = 15, units = "cm", res = 300)
  
  par(mfrow = c(3,2))
  
  for(i in c("Before construction", "Construction", "Operation")) {
    
    mov_track_ua_summer_2h_annotated_inds |>
      dplyr::filter(case_ == T & phase == i) |>
      pull(wind_turbines_dist_jsy) |>
      hist(main = paste0("used steps, ", i), xlim = c(0, 4e4),
           xlab = "distance to WF (m)")
    
    mov_track_ua_summer_2h_annotated_inds |>
      dplyr::filter(case_ == F & phase == i) |>
      pull(wind_turbines_dist_jsy) |>
      hist(main = paste0("available steps, ", i), xlim = c(0, 4e4),
           xlab = "distance to WF (m)")
  }
  dev.off()
}

# function to standardize variables by subtracting the mean and dividing by 2*SD
std_2sd <- function(x, na.rm = T) (x - mean(x, na.rm = na.rm))/(2*sd(x, na.rm = na.rm))
std_2sd(mov_track_ua_summer_2h_annotated_inds$dem) # test

# standardize covariates

#----------------
# TPI as category

# calculate SD of TPI using random points
rand <- amt::random_points(availability_autumn, n = 10000) |> 
  sf::st_as_sf(coords = 2:3) |> 
  terra::vect()

rand_background <- terra::extract(maps, rand) |> 
  as.data.frame() |> 
  dplyr::select(contains("tpi"))

SD <- apply(rand_background, 2, sd, na.rm = TRUE)

mov_track_ua_summer_2h_annotated_inds_tpi <- 
  mov_track_ua_summer_2h_annotated_inds |> 
  dplyr::mutate(
    tpi_cat_s150m = case_when(
      tpi_s150m <= -SD[1] ~ "valley",
      tpi_s150m > -SD[1] & tpi_s150m <= -SD[1]/2  ~ "lower slope",
      tpi_s150m > -SD[1]/2 & tpi_s150m <= 0  ~ "flat area",
      tpi_s150m > 0 & tpi_s150m <= SD[1]/2  ~ "medium slope",
      tpi_s150m > SD[1]/2 & tpi_s150m <= SD[1]  ~ "upper slope",
      TRUE ~ "ridge"
    ),
    tpi_cat_s250m = case_when(
      tpi_s250m <= -SD[2] ~ "valley",
      tpi_s250m > -SD[2] & tpi_s250m <= -SD[2]/2  ~ "lower slope",
      tpi_s250m > -SD[2]/2 & tpi_s250m <= 0  ~ "flat area",
      tpi_s250m > 0 & tpi_s250m <= SD[2]/2  ~ "medium slope",
      tpi_s250m > SD[2]/2 & tpi_s250m <= SD[2]  ~ "upper slope",
      TRUE ~ "ridge"
    ),
    tpi_cat_s510m = case_when(
      tpi_s510m <= -SD[3] ~ "valley",
      tpi_s510m > -SD[3] & tpi_s510m <= -SD[3]/2  ~ "lower slope",
      tpi_s510m > -SD[3]/2 & tpi_s510m <= 0  ~ "flat area",
      tpi_s510m > 0 & tpi_s510m <= SD[3]/2  ~ "medium slope",
      tpi_s510m > SD[3]/2 & tpi_s510m <= SD[3]  ~ "upper slope",
      TRUE ~ "ridge"
    )
  )

# visualize
mov_track_ua_summer_2h_annotated_inds_tpi %>%
  dplyr::slice(sample(1:nrow(.), size = 11000, replace = F)) |> 
  dplyr::filter(case_ == F) |> 
  sf::st_as_sf(coords = c("x_", "y_")) |> 
  ggplot() +
  geom_sf(aes(colour = tpi_cat_s510m))


#---------------------
# landcover with clear cuts

mov_track_ua_summer_2h_annotated_inds_tpi$clear_cuts_2020
mov_track_ua_summer_2h_annotated_inds_tpi$landcover
mov_track_ua_summer_2h_annotated_inds_tpi$landcover_smd_clearcuts2007

year_NMD <- 2018

mov_track_ua_summer_2h_annotated_inds_tpi_landcc <- mov_track_ua_summer_2h_annotated_inds_tpi |> 
  dplyr::mutate(landcover_cc = case_when(
    clear_cuts_2020 > 0 & lubridate::year(t_) > clear_cuts_2020 ~ "clear cuts",
    clear_cuts_2020 > 0 & lubridate::year(t_) < clear_cuts_2020 & clear_cuts_2020 <= year_NMD ~ as.character(landcover_smd_clearcuts2007),
    TRUE ~ as.character(landcover)
  ))

mov_track_ua_summer_2h_annotated_inds_tpi_landcc$landcover_cc |> as.factor() |> levels()

mov_track_ua_summer_2h_annotated_inds_tpi_landcc <- mov_track_ua_summer_2h_annotated_inds_tpi_landcc |> 
  dplyr::mutate(landcover_cc = case_when(
    landcover_cc == "4" ~ "anthropogenic areas",
    landcover_cc == "32" ~ "arable lands",
    landcover_cc == "43" | landcover_cc == "44" | landcover_cc == "45" | landcover_cc == "pine forests" | landcover_cc == "other coniferous forests" ~ "coniferous forests",
    landcover_cc == "40" | landcover_cc == "48" ~ "deciduous forests",
    landcover_cc == "41" | landcover_cc == "46" | landcover_cc == "49" ~ "forested wetlands",
    landcover_cc == "54" ~ "clear cuts",
    landcover_cc == "55" | landcover_cc == "56" ~ "coniferous forests",
    landcover_cc == "71" | landcover_cc == "72" ~ "open wetlands",
    TRUE ~ landcover_cc
  ) |> as.factor(),
  landcover_cc = forcats::fct_collapse(landcover_cc, mires = c("open wetlands", "heath"),
                                       "open lands" = c("arable lands", "tundra/rocks", "other open lands")) |> 
    droplevels() |> 
    forcats::fct_relevel("clear cuts", after = 0)
  )

mov_track_ua_summer_2h_annotated_inds_tpi_landcc$landcover_cc
mov_track_ua_summer_2h_annotated_inds_tpi_landcc$landcover_cc |> levels()

data_used <- mov_track_ua_summer_2h_annotated_inds_tpi_landcc |> 
  dplyr::filter(case_ == 1)
table(data_used$landcover_cc,
      data_used$phase)

# create viewshed variables with 3 categories
lc_class
# in sight open(1): in sight with open wetlands, arable lands, 
#    other open lands, anthropogenic, water, heath, tundra
# in sight cover(2): in sight with pine forests, other coniferous, 
#    deciduous forests, forested wetlands
# out of sight(0): out of sight
labs_view <- c("out of sight", "in sight open", "in sight cover")
mov_track_ua_summer_2h_annotated_inds_tpi_landcc_view <- 
  mov_track_ua_summer_2h_annotated_inds_tpi_landcc |>
  dplyr::mutate(viewshed_lc = ifelse(viewshed_jsy15km == 1 & grepl("forest", landcover_cc),
                                     2, viewshed_jsy15km) |> 
                  factor(levels = c(0, 1, 2), labels = labs_view))

mov_track_ua_summer_2h_annotated_inds_tpi_landcc_view$viewshed_lc

# visualize
mov_track_ua_summer_2h_annotated_inds_tpi_landcc_view %>% 
  dplyr::slice(sample(1:nrow(.), size = 11000, replace = F)) |> 
  dplyr::filter(case_ == 1) |> 
  sf::st_as_sf(coords = c("x_", "y_")) |> 
  ggplot() +
  geom_sf(aes(colour = viewshed_lc))

#--------------- 
# get data ready to analysis
data_ready_summer_rsf_2h <- mov_track_ua_summer_2h_annotated_inds_tpi_landcc_view |> 
  dplyr::filter(landcover_cc != "water") |> 
  dplyr::filter(!if_any(contains(c("cumulative", "nearest")), is.na)) |>
  dplyr::mutate(landcover_cc = droplevels(landcover_cc),
                viewshed = factor(viewshed_jsy, levels = 0:1, labels = c("out of sight", "in sight")),
                across(starts_with("sound_model"), ~ if_else(is.na(.x), 0, .x)))

# check
str(data_ready_summer_rsf_2h)
summary(data_ready_summer_rsf_2h)
apply(data_ready_summer_rsf_2h, 2, function(x) sum(is.na(x)))

# data_ready_summer_rsf_2h |>
#   dplyr::filter(is.na(house_cumulative10000)) |>
#   dplyr::filter(is.na(sound_model_mala_55)) |>
#   dplyr::filter(is.na(ndvi)) |>
#   dplyr::slice(1:1000) |> 
#   sf::st_as_sf(coords = c("x_", "y_")) |>
#   ggplot() +
#   geom_sf(aes(colour = 1))

# limited viewshed for amliden
dist_cutoff <- 1.5e4
wfs <- c("jokkmokksliden", "storliden", "ytteberg", "amliden")
data_ready_summer_rsf_2h <- data_ready_summer_rsf_2h %>% 
  dplyr::mutate(viewshed_aml15km = ifelse(wind_turbines_dist_aml >  dist_cutoff, 0, viewshed_amliden_mala_binary),
                cos_aspect = cos(aspect),
                wind_turbines_dist_comb = pmin(wind_turbines_dist_jsy, wind_turbines_dist_aml),
                dist_wf_closest = apply(cbind(wind_turbines_dist_jokk, wind_turbines_dist_stor, wind_turbines_dist_ytte, wind_turbines_dist_aml), 1, which.min),
                wf_name = factor(dist_wf_closest, levels = 1:4, labels = wfs))

# apply(cbind(data_ready_summer_rsf_2h$wind_turbines_dist_jokk, data_ready_summer_rsf_2h$wind_turbines_dist_stor), MARGIN = 1, which.min)
data_ready_summer_rsf_2h$wf_name

# load NDVI data
ndvi_sum_summer_beck <- stars::read_stars(.x = "maps/ndvi/ok/ndvi_sum_summer_beck.tif")
ndvi_sum_summer_elmore <- stars::read_stars(.x = "maps/ndvi/ok/ndvi_sum_summer_elmore.tif")
# load sos data
sos_beck <- stars::read_stars(.x = "maps/ndvi/ok/sos_beck.tif")
sos_elmore <- stars::read_stars(.x = "maps/ndvi/ok/sos_elmore.tif")

# monthday
md <- "0715"

# merge ndvi maps
ndvi_maps <- c(ndvi_sum_summer_beck, ndvi_sum_summer_elmore)
years_data <- attr(ndvi_maps, "dimensions")$band$values
ndvi_maps <- stars::st_set_dimensions(ndvi_maps, 3, values = lubridate::ymd(paste0(years_data, md)), names = "time")

# merge start of season maps
sos_maps <- c(sos_beck[,,,years_data], sos_beck[,,,years_data])
sos_maps <- stars::st_set_dimensions(sos_maps, 3, values = lubridate::ymd(paste0(years_data, md)), names = "time")

#load("data/01_GPS/05_annotated_data/data_annotated_summer_ready_2h_rsf.RData")
# data_ready_summer_rsf_2h

# select relevant data for extraction
dat_annot <- data_ready_summer_rsf_2h |>
  dplyr::select(id, x_, y_, t_, year) |> 
  sf::st_as_sf(coords = c("x_", "y_"), crs = 3006) |>
  sf::st_transform(crs = 4326) |>
  dplyr::mutate(t_interp = lubridate::ymd(paste0(year, md)))
 
# stars::st_extract(ndvi_maps, dat_annot, time_column = "t_interp")
# stars::st_extract(sos_maps, dat_annot, time_column = "t_interp")

# annotate in space and time, get average between methods
ndvi <- stars::st_extract(ndvi_maps, dat_annot, time_column = "t_interp") |> 
  dplyr::rename(beck = 1, elmore = 2) |> 
  sf::st_drop_geometry()
ndvi$ndvi = rowMeans(ndvi[c("beck", "elmore")])
sos <- stars::st_extract(sos_maps, dat_annot, time_column = "t_interp") |> 
  dplyr::rename(beck = 1, elmore = 2) |> 
  sf::st_drop_geometry()
sos$sos = rowMeans(sos[c("beck", "elmore")])

data_ready_summer_rsf_2h <- cbind(data_ready_summer_rsf_2h, ndvi["ndvi"], sos["sos"]) |>
  dplyr::filter(!if_any(contains(c("sos", "ndvi")), is.na)) 

# check NAs
apply(data_ready_calving_rsf_2h, 2, function(x) sum(is.na(x)))

# keep 9 random points for each used point
table(data_ready_summer_rsf_2h$id, data_ready_summer_rsf_2h$case_)
plot(data_ready_summer_rsf_2h$case_[1:10000])

n_points <- 9 + 1
data_ready_summer_rsf_2h_9pt <- data_ready_summer_rsf_2h %>% 
  plyr::ddply(., "id",
              function(x) {
                n <- sum(x$case_ == 1)
                return(x[1:(n_points*n),])
              }, .progress = "text") %>% 
  tibble::as_tibble()
data_ready_summer_rsf_2h_9pt
plot(data_ready_summer_rsf_2h_9pt$case_[1:10000])
table(data_ready_summer_rsf_2h_9pt$id, data_ready_summer_rsf_2h_9pt$case_)
data_ready_summer_rsf_2h <- data_ready_summer_rsf_2h_9pt
data_ready_summer_rsf_2h$year %>% table()

save(data_ready_summer_rsf_2h, file = "data/01_GPS/05_annotated_data/data_annotated_summer_ready_2h_rsf.RData")

#-----------
# Autumn

# load(file = "data/01_GPS/05_annotated_data/tmp_data_annotated_inds_2h_autumn_rsf.RData")
mov_track_ua_autumn_2h_annotated

# 2h fix rate
# unnest annotated data to standardize covariates
mov_track_ua_autumn_2h_annotated_inds <- mov_track_ua_autumn_2h_annotated |> 
  dplyr::filter(!sapply(mov_track_ua_autumn_2h_annotated$trk, is.null)) |> 
  dplyr::select(-c(data, trk)) |> 
  tidyr::unnest_legacy(cols = c(trk_ann))

str(mov_track_ua_autumn_2h_annotated_inds)
summary(mov_track_ua_autumn_2h_annotated_inds)

if(plot_maps) {
  png("output/use_ava_dist_wind_mala.png",
      width = 10, height = 15, units = "cm", res = 300)
  par(mfrow = c(3,2))
  
  for(i in c("Before construction", "Construction", "Operation")) {
    
    mov_track_ua_autumn_2h_annotated_inds |>
      dplyr::filter(case_ == T & phase == i) |>
      pull(wind_turbines_dist_jsy) |>
      hist(main = paste0("used steps, ", i), xlim = c(0, 4e4),
           xlab = "distance to WF (m)")
    
    mov_track_ua_autumn_2h_annotated_inds |>
      dplyr::filter(case_ == F & phase == i) |>
      pull(wind_turbines_dist_jsy) |>
      hist(main = paste0("available steps, ", i), xlim = c(0, 4e4),
           xlab = "distance to WF (m)")
  }
  dev.off()
}

# function to standardize variables by subtracting the mean and dividing by 2*SD
std_2sd <- function(x, na.rm = T) (x - mean(x, na.rm = na.rm))/(2*sd(x, na.rm = na.rm))
std_2sd(mov_track_ua_autumn_2h_annotated_inds$dem) # test

# standardize covariates

#----------------
# TPI as category

# calculate SD of TPI using random points
rand <- amt::random_points(availability_autumn, n = 10000) |> 
  sf::st_as_sf(coords = 2:3) |> 
  terra::vect()

rand_background <- terra::extract(maps, rand) |> 
  as.data.frame() |> 
  dplyr::select(contains("tpi"))

SD <- apply(rand_background, 2, sd, na.rm = TRUE)

mov_track_ua_autumn_2h_annotated_inds_tpi <- 
  mov_track_ua_autumn_2h_annotated_inds |> 
  dplyr::mutate(
    tpi_cat_s150m = case_when(
      tpi_s150m <= -SD[1] ~ "valley",
      tpi_s150m > -SD[1] & tpi_s150m <= -SD[1]/2  ~ "lower slope",
      tpi_s150m > -SD[1]/2 & tpi_s150m <= 0  ~ "flat area",
      tpi_s150m > 0 & tpi_s150m <= SD[1]/2  ~ "medium slope",
      tpi_s150m > SD[1]/2 & tpi_s150m <= SD[1]  ~ "upper slope",
      TRUE ~ "ridge"
    ),
    tpi_cat_s250m = case_when(
      tpi_s250m <= -SD[2] ~ "valley",
      tpi_s250m > -SD[2] & tpi_s250m <= -SD[2]/2  ~ "lower slope",
      tpi_s250m > -SD[2]/2 & tpi_s250m <= 0  ~ "flat area",
      tpi_s250m > 0 & tpi_s250m <= SD[2]/2  ~ "medium slope",
      tpi_s250m > SD[2]/2 & tpi_s250m <= SD[2]  ~ "upper slope",
      TRUE ~ "ridge"
    ),
    tpi_cat_s510m = case_when(
      tpi_s510m <= -SD[3] ~ "valley",
      tpi_s510m > -SD[3] & tpi_s510m <= -SD[3]/2  ~ "lower slope",
      tpi_s510m > -SD[3]/2 & tpi_s510m <= 0  ~ "flat area",
      tpi_s510m > 0 & tpi_s510m <= SD[3]/2  ~ "medium slope",
      tpi_s510m > SD[3]/2 & tpi_s510m <= SD[3]  ~ "upper slope",
      TRUE ~ "ridge"
    )
  )

# visualize
mov_track_ua_autumn_2h_annotated_inds_tpi %>% 
  dplyr::slice(sample(1:nrow(.), size = 11000, replace = F)) |> 
  dplyr::filter(case_ == F) |> 
  sf::st_as_sf(coords = c("x_", "y_")) |> 
  ggplot() +
  geom_sf(aes(colour = tpi_cat_s510m))


#---------------------
# landcover with clear cuts

mov_track_ua_autumn_2h_annotated_inds_tpi$clear_cuts_2020
mov_track_ua_autumn_2h_annotated_inds_tpi$landcover
mov_track_ua_autumn_2h_annotated_inds_tpi$landcover_smd_clearcuts2007

year_NMD <- 2018

mov_track_ua_autumn_2h_annotated_inds_tpi_landcc <- mov_track_ua_autumn_2h_annotated_inds_tpi |> 
  dplyr::mutate(landcover_cc = case_when(
    clear_cuts_2020 > 0 & lubridate::year(t_) > clear_cuts_2020 ~ "clear cuts",
    clear_cuts_2020 > 0 & lubridate::year(t_) < clear_cuts_2020 & clear_cuts_2020 <= year_NMD ~ as.character(landcover_smd_clearcuts2007),
    TRUE ~ as.character(landcover)
  ))

mov_track_ua_autumn_2h_annotated_inds_tpi_landcc$landcover_cc |> as.factor() |> levels()

mov_track_ua_autumn_2h_annotated_inds_tpi_landcc <- mov_track_ua_autumn_2h_annotated_inds_tpi_landcc |> 
  dplyr::mutate(landcover_cc = case_when(
    landcover_cc == "9" ~ "anthropogenic areas",
    landcover_cc == "32" ~ "arable lands",
    landcover_cc == "43" | landcover_cc == "44" | landcover_cc == "45" | landcover_cc == "pine forests" | landcover_cc == "other coniferous forests" ~ "coniferous forests",
    landcover_cc == "40" | landcover_cc == "48" ~ "deciduous forests",
    landcover_cc == "41" | landcover_cc == "46" | landcover_cc == "49" ~ "forested wetlands",
    landcover_cc == "54" ~ "clear cuts",
    landcover_cc == "55" | landcover_cc == "56" ~ "coniferous forests",
    landcover_cc == "71" | landcover_cc == "72" ~ "open wetlands",
    TRUE ~ landcover_cc
  ) |> as.factor(),
  landcover_cc = forcats::fct_collapse(landcover_cc, mires = c("open wetlands", "heath"),
                                       "open lands" = c("arable lands", "other open lands")) |> 
    droplevels() |> 
    forcats::fct_relevel("clear cuts", after = 0)
  )

mov_track_ua_autumn_2h_annotated_inds_tpi_landcc$landcover_cc
mov_track_ua_autumn_2h_annotated_inds_tpi_landcc$landcover_cc |> levels()

data_used <- mov_track_ua_autumn_2h_annotated_inds_tpi_landcc |> 
  dplyr::filter(case_ == 1)
table(data_used$landcover_cc,
      data_used$phase)

data_available <- mov_track_ua_autumn_2h_annotated_inds_tpi_landcc |> 
  dplyr::filter(case_ == 0)
table(data_available$landcover_cc,
      data_available$phase)

# create viewshed variables with 3 categories
lc_class
# in sight open(1): in sight with open wetlands, arable lands, 
#    other open lands, anthropogenic, water, heath, tundra
# in sight cover(2): in sight with pine forests, other coniferous, 
#    deciduous forests, forested wetlands
# out of sight(0): out of sight
labs_view <- c("out of sight", "in sight open", "in sight cover")
mov_track_ua_autumn_2h_annotated_inds_tpi_landcc_view <- 
  mov_track_ua_autumn_2h_annotated_inds_tpi_landcc |>
  dplyr::mutate(viewshed_lc = ifelse(viewshed_jsy15km == 1 & grepl("forest", landcover_cc),
                                     2, viewshed_jsy15km) |> 
                  factor(levels = c(0, 1, 2), labels = labs_view))

mov_track_ua_autumn_2h_annotated_inds_tpi_landcc_view$viewshed_lc

# visualize
mov_track_ua_autumn_2h_annotated_inds_tpi_landcc_view %>% 
  dplyr::slice(sample(1:nrow(.), size = 11000, replace = F)) |> 
  dplyr::filter(case_ == 1) |> 
  sf::st_as_sf(coords = c("x_", "y_")) |> 
  ggplot() +
  geom_sf(aes(colour = viewshed_lc))

#--------------- 
# get data ready to analysis
data_ready_autumn_rsf_2h <- mov_track_ua_autumn_2h_annotated_inds_tpi_landcc_view |> 
  dplyr::filter(landcover_cc != "water" & landcover_cc != "tundra/rocks") |> 
  dplyr::filter(!if_any(contains(c("cumulative", "nearest")), is.na)) |>
  dplyr::mutate(landcover_cc = droplevels(landcover_cc),
                viewshed = factor(viewshed_jsy, levels = 0:1, labels = c("out of sight", "in sight")),
                across(starts_with("sound_model"), ~ if_else(is.na(.x), 0, .x)))

# check
str(data_ready_autumn_rsf_2h)
summary(data_ready_autumn_rsf_2h)
apply(data_ready_autumn_rsf_2h, 2, function(x) sum(is.na(x)))

# data_ready_autumn_rsf_2h |>
#   dplyr::filter(is.na(house_cumulative10000)) |>
#   dplyr::filter(is.na(sound_model_mala_55)) |>
#   dplyr::filter(is.na(ndvi)) |>
#   dplyr::slice(1:1000) |> 
#   sf::st_as_sf(coords = c("x_", "y_")) |>
#   ggplot() +
#   geom_sf(aes(colour = 1))

# limited viewshed for amliden
dist_cutoff <- 1.5e4
wfs <- c("jokkmokksliden", "storliden", "ytteberg", "amliden")
data_ready_autumn_rsf_2h <- data_ready_autumn_rsf_2h %>% 
  dplyr::mutate(viewshed_aml15km = ifelse(wind_turbines_dist_aml >  dist_cutoff, 0, viewshed_amliden_mala_binary),
                cos_aspect = cos(aspect),
                wind_turbines_dist_comb = pmin(wind_turbines_dist_jsy, wind_turbines_dist_aml),
                dist_wf_closest = apply(cbind(wind_turbines_dist_jokk, wind_turbines_dist_stor, wind_turbines_dist_ytte, wind_turbines_dist_aml), 1, which.min),
                wf_name = factor(dist_wf_closest, levels = 1:4, labels = wfs))

# apply(cbind(data_ready_autumn_rsf_2h$wind_turbines_dist_jokk, data_ready_autumn_rsf_2h$wind_turbines_dist_stor), MARGIN = 1, which.min)
data_ready_autumn_rsf_2h$wf_name

# load NDVI data
ndvi_sum_autumn_beck <- stars::read_stars(.x = "maps/ndvi/ok/ndvi_sum_autumn_beck.tif")
ndvi_sum_autumn_elmore <- stars::read_stars(.x = "maps/ndvi/ok/ndvi_sum_autumn_elmore.tif")
# load sos data
eos_beck <- stars::read_stars(.x = "maps/ndvi/ok/eos_beck.tif")
eos_elmore <- stars::read_stars(.x = "maps/ndvi/ok/eos_elmore.tif")

# monthday
md <- "0915"

# merge ndvi maps
ndvi_maps <- c(ndvi_sum_summer_beck, ndvi_sum_summer_elmore)
years_data <- attr(ndvi_maps, "dimensions")$band$values
ndvi_maps <- stars::st_set_dimensions(ndvi_maps, 3, values = lubridate::ymd(paste0(years_data, md)), names = "time")

# merge start of season maps
eos_maps <- c(eos_beck[,,,years_data], eos_beck[,,,years_data])
eos_maps <- stars::st_set_dimensions(eos_maps, 3, values = lubridate::ymd(paste0(years_data, md)), names = "time")

#load("data/01_GPS/05_annotated_data/data_annotated_autumn_ready_2h_rsf.RData")
# data_ready_autumn_rsf_2h

# select relevant data for extraction
dat_annot <- data_ready_autumn_rsf_2h |>
  dplyr::select(id, x_, y_, t_, year) |> 
  sf::st_as_sf(coords = c("x_", "y_"), crs = 3006) |>
  sf::st_transform(crs = 4326) |>
  dplyr::mutate(t_interp = lubridate::ymd(paste0(year, md)))
 
# stars::st_extract(ndvi_maps, dat_annot, time_column = "t_interp")
# stars::st_extract(sos_maps, dat_annot, time_column = "t_interp")

# annotate in space and time, get average between methods
ndvi <- stars::st_extract(ndvi_maps, dat_annot, time_column = "t_interp") |> 
  dplyr::rename(beck = 1, elmore = 2) |> 
  sf::st_drop_geometry()
ndvi$ndvi = rowMeans(ndvi[c("beck", "elmore")])
eos <- stars::st_extract(eos_maps, dat_annot, time_column = "t_interp") |> 
  dplyr::rename(beck = 1, elmore = 2) |> 
  sf::st_drop_geometry()
eos$eos = rowMeans(eos[c("beck", "elmore")])

data_ready_autumn_rsf_2h <- cbind(data_ready_autumn_rsf_2h, ndvi["ndvi"], eos["eos"]) |>
  dplyr::filter(!if_any(contains(c("eos", "ndvi")), is.na)) 

# check NAs
apply(data_ready_autumn_rsf_2h, 2, function(x) sum(is.na(x)))

# keep 9 random points for each used point
table(data_ready_autumn_rsf_2h$id, data_ready_autumn_rsf_2h$case_)
plot(data_ready_autumn_rsf_2h$case_[1:10000])

n_points <- 9 + 1
data_ready_autumn_rsf_2h_9pt <- data_ready_autumn_rsf_2h %>% 
  dplyr::filter(!is.na(x_)) %>%
  plyr::ddply(., "id",
              function(x) {
                n <- sum(x$case_ == 1)
                return(x[1:(n_points*n),])
              }, .progress = "text") %>% 
  tibble::as_tibble()
data_ready_autumn_rsf_2h_9pt
plot(data_ready_autumn_rsf_2h_9pt$case_[1:10000])
table(data_ready_autumn_rsf_2h_9pt$id, data_ready_autumn_rsf_2h_9pt$case_)
data_ready_autumn_rsf_2h <- data_ready_autumn_rsf_2h_9pt |> 
  dplyr::filter(!is.na(x_) & !is.na(y_))
data_ready_autumn_rsf_2h$year %>% table()

save(data_ready_autumn_rsf_2h, file = "data/01_GPS/05_annotated_data/data_annotated_autumn_ready_2h_rsf.RData")


#-------------------------
# re-check number of points and monitoring of individuals per year and season

# calving
(summary1 <- data_ready_calving_rsf_2h |> 
   dplyr::filter(case_ == 1) |> 
   dplyr::group_by(year) |> 
   dplyr::summarise(n = n(),
                    n_inds = length(unique(id)),
                    avg_n = round(n/n_inds)))

(n_tot_calving <- sum(summary1$n_inds))

(summary2 <- data_ready_calving_rsf_2h |> 
    dplyr::filter(case_ == 1) |> 
    dplyr::group_by(year, id) |> 
    dplyr::summarise(start = min(t_),
                     end = max(t_),
                     duration_days = round(difftime(end, start, units = "days")),
                     n = n())) |> 
  print(n = 300)

(summary3 <- data_ready_calving_rsf_2h |>
    dplyr::filter(case_ == 1) |> 
    dplyr::group_by(year, id) %>% 
    dplyr::summarise(n_case1 = n()) |>
    dplyr::left_join(
      data_ready_calving_rsf_2h |>
        dplyr::filter(case_ == 0) |> 
        dplyr::group_by(year, id) %>% 
        dplyr::summarise(n_case0 = n()),
    by = c("year", "id")) |>
    dplyr::mutate(ava_pt_per_used_pt = n_case0/n_case1)) |> 
  print(n = 300)

(summary4 <- summary2 |> 
    dplyr::group_by(year) |> 
    dplyr::summarise(mean_duration = mean(duration_days),
                     avg_n = mean(n)))

(summary1 <- summary1 |> 
    dplyr::left_join(summary4[,1:2], by = "year"))

(summary2 <- summary2 |>
    dplyr::left_join(summary3[,c(1,2,4,5)], by = c("year", "id")))

# write summary tables
readr::write_csv(summary1, file = "data/01_GPS/05_annotated_data/summary_n_per_season_calving.csv")
readr::write_csv(summary2, file = "data/01_GPS/05_annotated_data/summary_n_per_individual_calving.csv")


# summer
(summary1 <- data_ready_summer_rsf_2h |> 
   dplyr::filter(case_ == 1) |> 
   dplyr::group_by(year) |> 
   dplyr::summarise(n = n(),
                    n_inds = length(unique(id)),
                    avg_n = round(n/n_inds)))

(n_tot_summer <- sum(summary1$n_inds))

(summary2 <- data_ready_summer_rsf_2h |> 
    dplyr::filter(case_ == 1) |> 
    dplyr::group_by(year, id) |> 
    dplyr::summarise(start = min(t_),
                     end = max(t_),
                     duration_days = round(difftime(end, start, units = "days")),
                     n = n())) |> 
  print(n = 300)

(summary3 <- data_ready_summer_rsf_2h |>
    dplyr::filter(case_ == 1) |> 
    dplyr::group_by(year, id) %>% 
    dplyr::summarise(n_case1 = n()) |>
    dplyr::left_join(
      data_ready_summer_rsf_2h |>
        dplyr::filter(case_ == 0) |> 
        dplyr::group_by(year, id) %>% 
        dplyr::summarise(n_case0 = n()),
    by = c("year", "id")) |>
    dplyr::mutate(ava_pt_per_used_pt = n_case0/n_case1)) |> 
  print(n = 300)

(summary4 <- summary2 |> 
    dplyr::group_by(year) |> 
    dplyr::summarise(mean_duration = mean(duration_days),
                     avg_n = mean(n)))

(summary1 <- summary1 |> 
    dplyr::left_join(summary4[,1:2], by = "year"))

(summary2 <- summary2 |>
    dplyr::left_join(summary3[,c(1,2,4,5)], by = c("year", "id")))

# write summary tables
readr::write_csv(summary1, file = "data/01_GPS/05_annotated_data/summary_n_per_season_summer.csv")
readr::write_csv(summary2, file = "data/01_GPS/05_annotated_data/summary_n_per_individual_summer.csv")


# autumn
(summary1 <- data_ready_autumn_rsf_2h |> 
   dplyr::filter(case_ == 1) |> 
   dplyr::group_by(year) |> 
   dplyr::summarise(n = n(),
                    n_inds = length(unique(id)),
                    avg_n = round(n/n_inds)))

(n_tot_autumn <- sum(summary1$n_inds))

(summary2 <- data_ready_autumn_rsf_2h |> 
    dplyr::filter(case_ == 1) |> 
    dplyr::group_by(year, id) |> 
    dplyr::summarise(start = min(t_),
                     end = max(t_),
                     duration_days = round(difftime(end, start, units = "days")),
                     n = n())) |> 
  print(n = 300)

(summary3 <- data_ready_autumn_rsf_2h |>
    dplyr::filter(case_ == 1) |> 
    dplyr::group_by(year, id) %>% 
    dplyr::summarise(n_case1 = n()) |>
    dplyr::left_join(
      data_ready_autumn_rsf_2h |>
        dplyr::filter(case_ == 0) |> 
        dplyr::group_by(year, id) %>% 
        dplyr::summarise(n_case0 = n()),
    by = c("year", "id")) |>
    dplyr::mutate(ava_pt_per_used_pt = n_case0/n_case1)) |> 
  print(n = 300)

(summary4 <- summary2 |> 
    dplyr::group_by(year) |> 
    dplyr::summarise(mean_duration = mean(duration_days),
                     avg_n = mean(n)))

(summary1 <- summary1 |> 
    dplyr::left_join(summary4[,1:2], by = "year"))

(summary2 <- summary2 |>
    dplyr::left_join(summary3[,c(1,2,4,5)], by = c("year", "id")))

# write summary tables
readr::write_csv(summary1, file = "data/01_GPS/05_annotated_data/summary_n_per_season_autumn.csv")
readr::write_csv(summary2, file = "data/01_GPS/05_annotated_data/summary_n_per_individual_autumn.csv")

# total of individuals
tibble::tribble(
  ~season, ~n_inds,
  "calving", n_tot_calving,
  "summer", n_tot_summer,
  "autumn", n_tot_autumn
) |>
  readr::write_csv(file = "data/01_GPS/05_annotated_data/summary_n_tot_individ_per_season.csv")
