#' ---
#' title: 'Annotate GPS for movement rate analysis of Swedish reindeer: Mala sameby'
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
install.load::install_load("tidyverse", "lubridate", "ggridges", "GGally")
install.load::install_load("sf", "terra", "raster")
install.load::install_load("broom", "ggeffects")
install.load::install_load("amt")

#-----------
#' # Load movement data

# movement data
mov_calving <- readRDS("data/data_cleaned_calving_RG.rds") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(Collar_ID, Object_ID, id, burst, year, x, y, timestamp)

mov_summer <- readRDS("data/data_cleaned_summer_RG.rds") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(Collar_ID, Object_ID, id, burst, year, x, y, timestamp)

mov_autumn <- readRDS("data/data_cleaned_autumn_RG.rds") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(Collar_ID, Object_ID, id, burst, year, x, y, timestamp)

# aux data
details_years <- read.csv("data/details_snow_free_mala_clean.csv", stringsAsFactors = F) %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(phase = factor(construction) %>% 
                  forcats::fct_relevel("after", after = Inf) %>% 
                  forcats::fct_recode("Before construction" = "before",
                                      "Construction" = "during",
                                      "Operation" = "after")) %>% 
  dplyr::select(year, phase)
details_years$phase

mov_calving <- mov_calving %>% 
  dplyr::left_join(details_years, by = "year")

mov_summer <- mov_summer %>% 
  dplyr::left_join(details_years, by = "year")

mov_autumn <- mov_autumn %>% 
  dplyr::left_join(details_years, by = "year")

#-----------
#' # Load background data

# projection to use
crs_to_use <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"

# maps folder and files
maps_folder <- "maps"

#---
# list maps

# availability area
availability_files <- list.files(maps_folder, pattern = ".gpkg$", full.names = T) %>% 
  grep(pattern = "availability", value = T)

# maps
maps_files <- list.files(maps_folder, pattern = ".tif$", full.names = T)

# maps to ignore (here, for winter)
ignore <- c("wolf", "urban", "snowmob", "landcover_smd.tif", "bbmm")

# maps to read
maps_files_read <- grep(pattern = paste(ignore, collapse = "|"), 
                        maps_files, value = T, invert = T)

#---
# read maps

# availability areas
availability_calving <- availability_files %>% 
  grep(pattern = "calving", value = T) %>% 
  sf::st_read() %>% 
  dplyr::select(available = 1) %>% 
  sf::st_transform(crs = crs_to_use)

availability_summer <- availability_files %>% 
  grep(pattern = "late", value = T) %>% 
  sf::st_read() %>% 
  dplyr::select(available = 1) %>% 
  sf::st_transform(crs = crs_to_use)

availability_autumn <- availability_files %>% 
  grep(pattern = "autumn", value = T) %>% 
  sf::st_read() %>% 
  dplyr::select(available = 1) %>% 
  sf::st_transform(crs = crs_to_use)

# maps 
maps <- terra::rast(maps_files_read)
maps_rast <- raster::stack(maps_files_read)

# read classes for land cover map
lc_class <- read.csv("data/nmd_classes_eng_reclassified.csv", header = F, 
                     col.names = c("code", "class"), stringsAsFactors = F) %>% 
  dplyr::arrange(code)

# plots, just to visualize
predators <- c("bear", "wolverine")
landuse <- c("nmd", "smd")

# plot no predators and no land use
no_pred_lu <- grep(pattern = paste(c(predators, landuse), collapse = "|"),
                   names(maps), value = T, invert = T)

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

### try to visualize with rasterVis
# levelplot may be nice to visualize the prdators


# --------------- label=all_inds

#-----------
#' # Annotation for all individuals

# transform into track object - calving
mov_track_calving <- mov_calving %>% 
  amt::mk_track(.x = x, .y = y, .t = timestamp, Collar_ID, Object_ID, id, burst, year, 
                phase, crs = sp::CRS(crs_to_use))

mov_track_summer <- mov_summer %>% 
  amt::mk_track(.x = x, .y = y, .t = timestamp, Collar_ID, Object_ID, id, burst, year, 
                phase, crs = sp::CRS(crs_to_use))

mov_track_autumn <- mov_autumn %>% 
  amt::mk_track(.x = x, .y = y, .t = timestamp, Collar_ID, Object_ID, id, burst, year, 
                phase, crs = sp::CRS(crs_to_use))

# calculate sampling rate statistics for all individuals - calving
samp_rate <- mov_track_calving %>% 
  amt::summarize_sampling_rate_many(cols = c("burst"))
View(samp_rate)

# most common
samp_rate %>% 
  dplyr::left_join(
    mov_track_calving %>%
      dplyr::select(burst, year),
    by = "burst") %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(avg_mean = mean(mean),
                   avg_median = mean(median),
                   med_mean = median(mean),
                   med_median = median(median))

# more common: 2h until 2018

# calculate sampling rate statistics for all individuals - summer
samp_rate <- mov_track_summer %>% 
  amt::summarize_sampling_rate_many(cols = c("burst"))
View(samp_rate)

# most common
samp_rate %>% 
  dplyr::left_join(
    mov_track_summer %>%
      dplyr::select(burst, year),
    by = "burst") %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(avg_mean = mean(mean),
                   avg_median = mean(median),
                   med_mean = median(mean),
                   med_median = median(median))

# more common: 2h until 2018

# calculate sampling rate statistics for all individuals - autumn
samp_rate <- mov_track_autumn %>% 
  amt::summarize_sampling_rate_many(cols = c("burst"))
View(samp_rate)

# most common
samp_rate %>% 
  dplyr::left_join(
    mov_track_autumn %>%
      dplyr::select(burst, year),
    by = "burst") %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(avg_mean = mean(mean),
                   avg_median = mean(median),
                   med_mean = median(mean),
                   med_median = median(median))

# more common: 2h until 2018

#------------
# resample and and annotate points
# using 2h fix rate, with 15min tolerance

# calving
# from 2008 to 2018, using 2h fix rate
mov_track_calving_annotated_2h <- mov_track_calving %>% 
  tidyr::nest(data = c(x_, y_, t_, Collar_ID, Object_ID, burst, year, phase)) %>% 
  dplyr::mutate(trk = map(data, function(d, fix_rate_min = 120,
                                         tolerance_min = 15,
                                         availability_polygon = availability_calving) {
    
    print(d$Object_ID[1])
    
    stps <- d %>% 
      amt::track_resample(rate = minutes(fix_rate_min), tolerance = minutes(tolerance_min)) %>% 
      amt::filter_min_n_burst(min_n = 3)
    
    if(nrow(stps) == 0) {
      return(NULL)
    } else {
      
      stps <- stps %>% amt::steps_by_burst(keep_cols = "start")
      
      stps_sf <- stps %>% 
        sf::st_as_sf(coords = c("x1_", "y1_")) %>% 
        terra::vect()
      
      background_data <- terra::extract(maps, stps_sf) %>% 
        as.data.frame()
      
      stps_back <- stps %>% 
        dplyr::bind_cols(background_data) %>% 
        dplyr::arrange(t1_) %>% 
        dplyr::mutate(landcover_nmd_mod = factor(landcover_nmd_mod, 
                                                 levels = lc_class$code,
                                                 labels = lc_class$class)) %>%
        dplyr::mutate(log_building = log(building_dist+1),
                      log_house = log(house_dist+1),
                      log_pl = log(pl_dist+1),
                      log_priv_road = log(priv_road_dist+1),
                      log_public_road = log(public_road_dist+1),
                      log_mining = log(mining_dist+1),
                      log_trails = log(trail_dist+1),
                      log_wind = log(wind_dist+1)) %>% 
        dplyr::rename(dem = dem_10m, aspect = dem_aspect, slope = dem_slope,
                      landcover = landcover_nmd_mod) %>%
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
        ) %>% 
        dplyr::select(-contains("bear_"), -contains("wolverine_"))
      
      stps_back
    }
  }))

# summer
mov_track_summer_annotated_2h <- mov_track_summer %>% 
  tidyr::nest(data = c(x_, y_, t_, Collar_ID, Object_ID, burst, year, phase)) %>% 
  dplyr::mutate(trk = map(data, function(d, fix_rate_min = 120,
                                         tolerance_min = 15,
                                         availability_polygon = availability_calving) {
    
    print(d$Object_ID[1])
    
    stps <- d %>% 
      amt::track_resample(rate = minutes(fix_rate_min), tolerance = minutes(tolerance_min)) %>% 
      amt::filter_min_n_burst(min_n = 3)
    
    if(nrow(stps) == 0) {
      return(NULL)
    } else {
      
      stps <- stps %>% amt::steps_by_burst(keep_cols = "start")
      
      stps_sf <- stps %>% 
        sf::st_as_sf(coords = c("x1_", "y1_")) %>% 
        terra::vect()
      
      background_data <- terra::extract(maps, stps_sf) %>% 
        as.data.frame()
      
      stps_back <- stps %>% 
        dplyr::bind_cols(background_data) %>% 
        dplyr::arrange(t1_) %>% 
        dplyr::mutate(landcover_nmd_mod = factor(landcover_nmd_mod, 
                                                 levels = lc_class$code,
                                                 labels = lc_class$class)) %>%
        dplyr::mutate(log_building = log(building_dist+1),
                      log_house = log(house_dist+1),
                      log_pl = log(pl_dist+1),
                      log_priv_road = log(priv_road_dist+1),
                      log_public_road = log(public_road_dist+1),
                      log_mining = log(mining_dist+1),
                      log_trails = log(trail_dist+1),
                      log_wind = log(wind_dist+1)) %>% 
        dplyr::rename(dem = dem_10m, aspect = dem_aspect, slope = dem_slope,
                      landcover = landcover_nmd_mod) %>%
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
        ) %>% 
        dplyr::select(-contains("bear_"), -contains("wolverine_"))
      
      stps_back
    }
  }))


# autumn
mov_track_autumn_annotated_2h <- mov_track_autumn %>% 
  tidyr::nest(data = c(x_, y_, t_, Collar_ID, Object_ID, burst, year, phase)) %>% 
  dplyr::mutate(trk = map(data, function(d, fix_rate_min = 120,
                                         tolerance_min = 15,
                                         availability_polygon = availability_calving) {
    
    print(d$Object_ID[1])
    
    stps <- d %>% 
      amt::track_resample(rate = minutes(fix_rate_min), tolerance = minutes(tolerance_min)) %>% 
      amt::filter_min_n_burst(min_n = 3)
    
    if(nrow(stps) == 0) {
      return(NULL)
    } else {
      
      stps <- stps %>% amt::steps_by_burst(keep_cols = "start")
      
      stps_sf <- stps %>% 
        sf::st_as_sf(coords = c("x1_", "y1_")) %>% 
        terra::vect()
      
      background_data <- terra::extract(maps, stps_sf) %>% 
        as.data.frame()
      
      stps_back <- stps %>% 
        dplyr::bind_cols(background_data) %>% 
        dplyr::arrange(t1_) %>% 
        dplyr::mutate(landcover_nmd_mod = factor(landcover_nmd_mod, 
                                                 levels = lc_class$code,
                                                 labels = lc_class$class)) %>%
        dplyr::mutate(log_building = log(building_dist+1),
                      log_house = log(house_dist+1),
                      log_pl = log(pl_dist+1),
                      log_priv_road = log(priv_road_dist+1),
                      log_public_road = log(public_road_dist+1),
                      log_mining = log(mining_dist+1),
                      log_trails = log(trail_dist+1),
                      log_wind = log(wind_dist+1)) %>% 
        dplyr::rename(dem = dem_10m, aspect = dem_aspect, slope = dem_slope,
                      landcover = landcover_nmd_mod) %>%
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
        ) %>% 
        dplyr::select(-contains("bear_"), -contains("wolverine_"))
      
      stps_back
    }
  }))


# here we save the data with covariates extracted for used and available location
save(mov_track_calving_annotated_2h, file = "output/data_annotated_inds_2h_calving_mov_rate.RData")
save(mov_track_summer_annotated_2h, file = "output/data_annotated_inds_2h_summer_mov_rate.RData")
save(mov_track_autumn_annotated_2h, file = "output/data_annotated_inds_2h_autumn_mov_rate.RData")

# load(file = "output/data_annotated_")
mov_track_calving_annotated_2h

#-----------
# Calving

# 2h fix rate
# unnest annotated data to standardize covariates
mov_track_calving_annotated_2h_inds <- mov_track_calving_annotated_2h %>% 
  dplyr::filter(!sapply(.$trk, is.null)) %>% 
  dplyr::select(-c(data)) %>% 
  tidyr::unnest(cols = c(trk)) %>% 
  dplyr::filter(year <= 2018)

str(mov_track_calving_annotated_2h_inds)
summary(mov_track_calving_annotated_2h_inds)

# function to standardize variables by subtracting the mean and dividing by 2*SD
std_2sd <- function(x, na.rm = T) (x - mean(x, na.rm = na.rm))/(2*sd(x, na.rm = na.rm))
std_2sd(mov_track_calving_annotated_2h_inds$dem) # test

# standardize covariates

#----------------
# TPI as category

# calculate SD of TPI using random points
rand <- amt::random_points(availability_autumn, n = 10000) %>% 
  sf::st_as_sf(coords = 2:3) %>% 
  terra::vect()

rand_background <- terra::extract(maps, rand) %>% 
  as.data.frame() %>% 
  dplyr::select(contains("tpi"))

SD <- apply(rand_background, 2, sd, na.rm = TRUE)

mov_track_calving_annotated_2h_inds_tpi <- 
  mov_track_calving_annotated_2h_inds %>% 
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
mov_track_calving_annotated_2h_inds_tpi %>% 
  dplyr::slice(sample(1:nrow(.), size = 11000, replace = F)) %>% 
  sf::st_as_sf(coords = c("x1_", "y1_")) %>% 
  ggplot() +
  geom_sf(aes(colour = tpi_cat_s510m))


#---------------------
# landcover with clear cuts

mov_track_calving_annotated_2h_inds_tpi$clear_cuts_2020
mov_track_calving_annotated_2h_inds_tpi$landcover
mov_track_calving_annotated_2h_inds_tpi$landcover_smd_clearcuts2007

year_NMD <- 2018

mov_track_calving_annotated_2h_inds_tpi_landcc <- mov_track_calving_annotated_2h_inds_tpi %>% 
  dplyr::mutate(landcover_cc = case_when(
    clear_cuts_2020 > 0 & lubridate::year(t1_) > clear_cuts_2020 ~ "clear cuts",
    clear_cuts_2020 > 0 & lubridate::year(t1_) < clear_cuts_2020 & clear_cuts_2020 <= year_NMD ~ as.character(landcover_smd_clearcuts2007),
    TRUE ~ as.character(landcover)
  ))

mov_track_calving_annotated_2h_inds_tpi_landcc$landcover_cc %>% as.factor %>% levels

mov_track_calving_annotated_2h_inds_tpi_landcc <- mov_track_calving_annotated_2h_inds_tpi_landcc %>% 
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
  ) %>% as.factor,
  landcover_cc = forcats::fct_collapse(landcover_cc, mires = c("open wetlands", "heath"),
                                       "open lands" = c("arable lands", "other open lands")) %>% 
    droplevels() %>% 
    forcats::fct_relevel("clear cuts", after = 0)
  )

mov_track_calving_annotated_2h_inds_tpi_landcc$landcover_cc
mov_track_calving_annotated_2h_inds_tpi_landcc$landcover_cc %>% levels

data_used <- mov_track_calving_annotated_2h_inds_tpi_landcc
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
mov_track_calving_annotated_2h_inds_tpi_landcc_view <- 
  mov_track_calving_annotated_2h_inds_tpi_landcc %>%
  dplyr::mutate(viewshed_lc = ifelse(viewshed_jsy15km == 1 & grepl("forest", landcover_cc),
                                     2, viewshed_jsy15km) %>% 
                  factor(levels = c(0, 1, 2), labels = labs_view))

mov_track_calving_annotated_2h_inds_tpi_landcc_view$viewshed_lc

# visualize
mov_track_calving_annotated_2h_inds_tpi_landcc_view %>% 
  dplyr::slice(sample(1:nrow(.), size = 11000, replace = F)) %>% 
  sf::st_as_sf(coords = c("x1_", "y1_")) %>% 
  ggplot() +
  geom_sf(aes(colour = viewshed_lc))


#---------------------------
# sound models

# read wind speed and direction data
wind_data <- readr::read_csv2("data/wind_speed_direction_smhi_mala_brannan.csv", skip = 10) %>% 
  dplyr::select(date = Datum, time = 2, wind_direction = 3, wind_speed = 5) %>% 
  dplyr::mutate(wind_speed = as.numeric(wind_speed),
                timestamp = lubridate::ymd_hms(paste(as.character(date), as.character(time))),
                wind_dir_cat = case_when(
                  wind_direction >= 315 | wind_direction < 45 ~ "N",
                  wind_direction >= 45 & wind_direction < 135 ~ "E",
                  wind_direction >= 135 & wind_direction < 225 ~ "S",
                  wind_direction >= 225 & wind_direction < 315 ~ "W",
                )) %>% 
  dplyr::select(-c(date, time))

wind_data

# read temperature data
temp_data <- readr::read_csv2("data/temperature_smhi_mala_brannan.csv", skip = 9) %>% 
  dplyr::select(date = Datum, time = 2, temperature = 3) %>% 
  dplyr::mutate(temperature = as.numeric(temperature),
                timestamp = lubridate::ymd_hms(paste(as.character(date), as.character(time))),
                temp_cat = ifelse(temperature <= 0, "negative", "positive")) %>% 
  dplyr::select(-c(date, time))

climate_data <- wind_data %>% 
  dplyr::left_join(temp_data, by = "timestamp"
  ) %>% 
  dplyr::select(timestamp, wind_speed, wind_dir_cat, temp_cat)

climate_data

# fill NA
climate_data$temp_cat_fill <- climate_data$temp_cat
for(i in 1:nrow(climate_data)) {
  if(i != 1 & is.na(climate_data$temp_cat[i])) {
    climate_data$temp_cat_fill[i] <- climate_data$temp_cat_fill[i-1]
  }
}
sum(is.na(climate_data$temp_cat))
sum(is.na(climate_data$temp_cat_fill))

# merge this with the data
mov_track_calving_annotated_2h_inds_tpi_landcc_view_wind <- mov_track_calving_annotated_2h_inds_tpi_landcc_view %>% 
  dplyr::bind_cols(
    mov_track_calving_annotated_2h_inds_tpi_landcc_view %>% 
      dplyr::select(t1_) %>% 
      dplyr::mutate(t1_round = lubridate::floor_date(t1_, unit = "hours")) %>% 
      dplyr::left_join(climate_data, by = c("t1_round" = "timestamp")) %>% 
      dplyr::select(!contains("t1"))
  )

# read sound definitions
sound_definitions <- read.csv("data/categories_sound_models_mala.csv")

# define
mov_track_calving_annotated_2h_inds_tpi_landcc_view_wind <- mov_track_calving_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::mutate(sound_intensity = case_when(
    temp_cat == "negative" & wind_dir_cat == "N" & wind_speed >= 6 ~ sound_model_mala_53,
    temp_cat == "negative" & wind_dir_cat == "E" & wind_speed >= 6 ~ sound_model_mala_54,
    temp_cat == "negative" & wind_dir_cat == "S" & wind_speed >= 6 ~ sound_model_mala_55,
    temp_cat == "negative" & wind_dir_cat == "W" & wind_speed >= 6 ~ sound_model_mala_56,
    temp_cat == "positive" & wind_dir_cat == "N" & wind_speed >= 6 ~ sound_model_mala_57,
    temp_cat == "positive" & wind_dir_cat == "E" & wind_speed >= 6 ~ sound_model_mala_58,
    temp_cat == "positive" & wind_dir_cat == "S" & wind_speed >= 6 ~ sound_model_mala_59,
    temp_cat == "positive" & wind_dir_cat == "W" & wind_speed >= 6 ~ sound_model_mala_60,
    temp_cat == "negative" & wind_dir_cat == "N" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_61,
    temp_cat == "negative" & wind_dir_cat == "E" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_62,
    temp_cat == "negative" & wind_dir_cat == "S" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_63,
    temp_cat == "negative" & wind_dir_cat == "W" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_64,
    temp_cat == "positive" & wind_dir_cat == "N" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_65,
    temp_cat == "positive" & wind_dir_cat == "E" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_66,
    temp_cat == "positive" & wind_dir_cat == "S" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_67,
    temp_cat == "positive" & wind_dir_cat == "W" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_68,
    TRUE ~ 0
  ))

# check
mov_track_calving_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::filter(wind_speed < 3) %>% 
  pull(sound_intensity) %>% summary

mov_track_calving_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::filter(wind_speed > 6) %>% 
  pull(sound_intensity) %>% summary


# clean dataset
mov_track_calving_annotated_2h_inds_tpi_landcc_view_wind <- mov_track_calving_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::select(-c(contains("sound_model"), wind_speed, wind_dir_cat, temp_cat))



#--------------- 
# get data ready to analysis
data_ready_calving_mov_track_2h <- mov_track_calving_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::filter(landcover_cc != "water") %>% 
  dplyr::mutate(landcover_cc = droplevels(landcover_cc),
                viewshed = factor(viewshed_jsy, levels = 0:1, labels = c("out of sight", "in sight")),
                cos_aspect = cos(aspect))

save(data_ready_calving_mov_track_2h, file = "output/data_annotated_calving_ready_2h_mov_rate.RData")


#-----------
# Summer

# 2h fix rate
# unnest annotated data to standardize covariates
mov_track_summer_annotated_2h_inds <- mov_track_summer_annotated_2h %>% 
  dplyr::filter(!sapply(.$trk, is.null)) %>% 
  dplyr::select(-c(data)) %>% 
  tidyr::unnest(cols = c(trk)) %>% 
  dplyr::filter(year <= 2018)

str(mov_track_summer_annotated_2h_inds)
summary(mov_track_summer_annotated_2h_inds)

# function to standardize variables by subtracting the mean and dividing by 2*SD
std_2sd <- function(x, na.rm = T) (x - mean(x, na.rm = na.rm))/(2*sd(x, na.rm = na.rm))
std_2sd(mov_track_summer_annotated_2h_inds$dem) # test

# standardize covariates

#----------------
# TPI as category

# calculate SD of TPI using random points
rand <- amt::random_points(availability_autumn, n = 10000) %>% 
  sf::st_as_sf(coords = 2:3) %>% 
  terra::vect()

rand_background <- terra::extract(maps, rand) %>% 
  as.data.frame() %>% 
  dplyr::select(contains("tpi"))

SD <- apply(rand_background, 2, sd, na.rm = TRUE)

mov_track_summer_annotated_2h_inds_tpi <- 
  mov_track_summer_annotated_2h_inds %>% 
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
mov_track_summer_annotated_2h_inds_tpi %>% 
  dplyr::slice(sample(1:nrow(.), size = 11000, replace = F)) %>% 
  sf::st_as_sf(coords = c("x1_", "y1_")) %>% 
  ggplot() +
  geom_sf(aes(colour = tpi_cat_s510m))


#---------------------
# landcover with clear cuts

mov_track_summer_annotated_2h_inds_tpi$clear_cuts_2020
mov_track_summer_annotated_2h_inds_tpi$landcover
mov_track_summer_annotated_2h_inds_tpi$landcover_smd_clearcuts2007

year_NMD <- 2018

mov_track_summer_annotated_2h_inds_tpi_landcc <- mov_track_summer_annotated_2h_inds_tpi %>% 
  dplyr::mutate(landcover_cc = case_when(
    clear_cuts_2020 > 0 & lubridate::year(t1_) > clear_cuts_2020 ~ "clear cuts",
    clear_cuts_2020 > 0 & lubridate::year(t1_) < clear_cuts_2020 & clear_cuts_2020 <= year_NMD ~ as.character(landcover_smd_clearcuts2007),
    TRUE ~ as.character(landcover)
  ))

mov_track_summer_annotated_2h_inds_tpi_landcc$landcover_cc %>% as.factor %>% levels

mov_track_summer_annotated_2h_inds_tpi_landcc <- mov_track_summer_annotated_2h_inds_tpi_landcc %>% 
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
  ) %>% as.factor,
  landcover_cc = forcats::fct_collapse(landcover_cc, mires = c("open wetlands"),
                                       "open lands" = c("arable lands", "tundra/rocks", "other open lands")) %>% 
    droplevels() %>% 
    forcats::fct_relevel("clear cuts", after = 0)
  )

mov_track_summer_annotated_2h_inds_tpi_landcc$landcover_cc
mov_track_summer_annotated_2h_inds_tpi_landcc$landcover_cc %>% levels

data_used <- mov_track_summer_annotated_2h_inds_tpi_landcc
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
mov_track_summer_annotated_2h_inds_tpi_landcc_view <- 
  mov_track_summer_annotated_2h_inds_tpi_landcc %>%
  dplyr::mutate(viewshed_lc = ifelse(viewshed_jsy15km == 1 & grepl("forest", landcover_cc),
                                     2, viewshed_jsy15km) %>% 
                  factor(levels = c(0, 1, 2), labels = labs_view))

mov_track_summer_annotated_2h_inds_tpi_landcc_view$viewshed_lc

# visualize
mov_track_summer_annotated_2h_inds_tpi_landcc_view %>% 
  dplyr::slice(sample(1:nrow(.), size = 11000, replace = F)) %>% 
  sf::st_as_sf(coords = c("x1_", "y1_")) %>% 
  ggplot() +
  geom_sf(aes(colour = viewshed_lc))


#---------------------------
# sound models

# read wind speed and direction data
wind_data <- readr::read_csv2("data/wind_speed_direction_smhi_mala_brannan.csv", skip = 10) %>% 
  dplyr::select(date = Datum, time = 2, wind_direction = 3, wind_speed = 5) %>% 
  dplyr::mutate(wind_speed = as.numeric(wind_speed),
                timestamp = lubridate::ymd_hms(paste(as.character(date), as.character(time))),
                wind_dir_cat = case_when(
                  wind_direction >= 315 | wind_direction < 45 ~ "N",
                  wind_direction >= 45 & wind_direction < 135 ~ "E",
                  wind_direction >= 135 & wind_direction < 225 ~ "S",
                  wind_direction >= 225 & wind_direction < 315 ~ "W",
                )) %>% 
  dplyr::select(-c(date, time))

wind_data

# read temperature data
temp_data <- readr::read_csv2("data/temperature_smhi_mala_brannan.csv", skip = 9) %>% 
  dplyr::select(date = Datum, time = 2, temperature = 3) %>% 
  dplyr::mutate(temperature = as.numeric(temperature),
                timestamp = lubridate::ymd_hms(paste(as.character(date), as.character(time))),
                temp_cat = ifelse(temperature <= 0, "negative", "positive")) %>% 
  dplyr::select(-c(date, time))

climate_data <- wind_data %>% 
  dplyr::left_join(temp_data, by = "timestamp"
  ) %>% 
  dplyr::select(timestamp, wind_speed, wind_dir_cat, temp_cat)

climate_data

# fill NA
climate_data$temp_cat_fill <- climate_data$temp_cat
for(i in 1:nrow(climate_data)) {
  if(i != 1 & is.na(climate_data$temp_cat[i])) {
    climate_data$temp_cat_fill[i] <- climate_data$temp_cat_fill[i-1]
  }
}
sum(is.na(climate_data$temp_cat))
sum(is.na(climate_data$temp_cat_fill))

# merge this with the data
mov_track_summer_annotated_2h_inds_tpi_landcc_view_wind <- mov_track_summer_annotated_2h_inds_tpi_landcc_view %>% 
  dplyr::bind_cols(
    mov_track_summer_annotated_2h_inds_tpi_landcc_view %>% 
      dplyr::select(t1_) %>% 
      dplyr::mutate(t1_round = lubridate::floor_date(t1_, unit = "hours")) %>% 
      dplyr::left_join(climate_data, by = c("t1_round" = "timestamp")) %>% 
      dplyr::select(!contains("t1"))
  )

# read sound definitions
sound_definitions <- read.csv("data/categories_sound_models_mala.csv")

# define
mov_track_summer_annotated_2h_inds_tpi_landcc_view_wind <- mov_track_summer_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::mutate(sound_intensity = case_when(
    temp_cat == "negative" & wind_dir_cat == "N" & wind_speed >= 6 ~ sound_model_mala_53,
    temp_cat == "negative" & wind_dir_cat == "E" & wind_speed >= 6 ~ sound_model_mala_54,
    temp_cat == "negative" & wind_dir_cat == "S" & wind_speed >= 6 ~ sound_model_mala_55,
    temp_cat == "negative" & wind_dir_cat == "W" & wind_speed >= 6 ~ sound_model_mala_56,
    temp_cat == "positive" & wind_dir_cat == "N" & wind_speed >= 6 ~ sound_model_mala_57,
    temp_cat == "positive" & wind_dir_cat == "E" & wind_speed >= 6 ~ sound_model_mala_58,
    temp_cat == "positive" & wind_dir_cat == "S" & wind_speed >= 6 ~ sound_model_mala_59,
    temp_cat == "positive" & wind_dir_cat == "W" & wind_speed >= 6 ~ sound_model_mala_60,
    temp_cat == "negative" & wind_dir_cat == "N" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_61,
    temp_cat == "negative" & wind_dir_cat == "E" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_62,
    temp_cat == "negative" & wind_dir_cat == "S" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_63,
    temp_cat == "negative" & wind_dir_cat == "W" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_64,
    temp_cat == "positive" & wind_dir_cat == "N" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_65,
    temp_cat == "positive" & wind_dir_cat == "E" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_66,
    temp_cat == "positive" & wind_dir_cat == "S" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_67,
    temp_cat == "positive" & wind_dir_cat == "W" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_68,
    TRUE ~ 0
  ))

# check
mov_track_summer_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::filter(wind_speed < 3) %>% 
  pull(sound_intensity) %>% summary

mov_track_summer_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::filter(wind_speed > 6) %>% 
  pull(sound_intensity) %>% summary


# clean dataset
mov_track_summer_annotated_2h_inds_tpi_landcc_view_wind <- mov_track_summer_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::select(-c(contains("sound_model"), wind_speed, wind_dir_cat, temp_cat))



#--------------- 
# get data ready to analysis
data_ready_summer_mov_track_2h <- mov_track_summer_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::filter(landcover_cc != "water") %>% 
  dplyr::mutate(landcover_cc = droplevels(landcover_cc),
                viewshed = factor(viewshed_jsy, levels = 0:1, labels = c("out of sight", "in sight")),
                cos_aspect = cos(aspect))

save(data_ready_summer_mov_track_2h, file = "output/data_annotated_summer_ready_2h_mov_rate.RData")


#-----------
# Autumn

# 2h fix rate
# unnest annotated data to standardize covariates
mov_track_autumn_annotated_2h_inds <- mov_track_autumn_annotated_2h %>% 
  dplyr::filter(!sapply(.$trk, is.null)) %>% 
  dplyr::select(-c(data)) %>% 
  tidyr::unnest(cols = c(trk)) %>% 
  dplyr::filter(year <= 2018)

str(mov_track_autumn_annotated_2h_inds)
summary(mov_track_autumn_annotated_2h_inds)

# function to standardize variables by subtracting the mean and dividing by 2*SD
std_2sd <- function(x, na.rm = T) (x - mean(x, na.rm = na.rm))/(2*sd(x, na.rm = na.rm))
std_2sd(mov_track_autumn_annotated_2h_inds$dem) # test

# standardize covariates

#----------------
# TPI as category

# calculate SD of TPI using random points
rand <- amt::random_points(availability_autumn, n = 10000) %>% 
  sf::st_as_sf(coords = 2:3) %>% 
  terra::vect()

rand_background <- terra::extract(maps, rand) %>% 
  as.data.frame() %>% 
  dplyr::select(contains("tpi"))

SD <- apply(rand_background, 2, sd, na.rm = TRUE)

mov_track_autumn_annotated_2h_inds_tpi <- 
  mov_track_autumn_annotated_2h_inds %>% 
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
mov_track_autumn_annotated_2h_inds_tpi %>% 
  dplyr::slice(sample(1:nrow(.), size = 11000, replace = F)) %>% 
  sf::st_as_sf(coords = c("x1_", "y1_")) %>% 
  ggplot() +
  geom_sf(aes(colour = tpi_cat_s510m))


#---------------------
# landcover with clear cuts

mov_track_autumn_annotated_2h_inds_tpi$clear_cuts_2020
mov_track_autumn_annotated_2h_inds_tpi$landcover
mov_track_autumn_annotated_2h_inds_tpi$landcover_smd_clearcuts2007

year_NMD <- 2018

mov_track_autumn_annotated_2h_inds_tpi_landcc <- mov_track_autumn_annotated_2h_inds_tpi %>% 
  dplyr::mutate(landcover_cc = case_when(
    clear_cuts_2020 > 0 & lubridate::year(t1_) > clear_cuts_2020 ~ "clear cuts",
    clear_cuts_2020 > 0 & lubridate::year(t1_) < clear_cuts_2020 & clear_cuts_2020 <= year_NMD ~ as.character(landcover_smd_clearcuts2007),
    TRUE ~ as.character(landcover)
  ))

mov_track_autumn_annotated_2h_inds_tpi_landcc$landcover_cc %>% as.factor %>% levels

mov_track_autumn_annotated_2h_inds_tpi_landcc <- mov_track_autumn_annotated_2h_inds_tpi_landcc %>% 
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
  ) %>% as.factor,
  landcover_cc = forcats::fct_collapse(landcover_cc, mires = c("open wetlands"),
                                       "open lands" = c("arable lands", "other open lands")) %>% 
    droplevels() %>% 
    forcats::fct_relevel("clear cuts", after = 0)
  )

mov_track_autumn_annotated_2h_inds_tpi_landcc$landcover_cc
mov_track_autumn_annotated_2h_inds_tpi_landcc$landcover_cc %>% levels

data_used <- mov_track_autumn_annotated_2h_inds_tpi_landcc
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
mov_track_autumn_annotated_2h_inds_tpi_landcc_view <- 
  mov_track_autumn_annotated_2h_inds_tpi_landcc %>%
  dplyr::mutate(viewshed_lc = ifelse(viewshed_jsy15km == 1 & grepl("forest", landcover_cc),
                                     2, viewshed_jsy15km) %>% 
                  factor(levels = c(0, 1, 2), labels = labs_view))

mov_track_autumn_annotated_2h_inds_tpi_landcc_view$viewshed_lc

# visualize
mov_track_autumn_annotated_2h_inds_tpi_landcc_view %>% 
  dplyr::slice(sample(1:nrow(.), size = 11000, replace = F)) %>% 
  sf::st_as_sf(coords = c("x1_", "y1_")) %>% 
  ggplot() +
  geom_sf(aes(colour = viewshed_lc))


#---------------------------
# sound models

# read wind speed and direction data
wind_data <- readr::read_csv2("data/wind_speed_direction_smhi_mala_brannan.csv", skip = 10) %>% 
  dplyr::select(date = Datum, time = 2, wind_direction = 3, wind_speed = 5) %>% 
  dplyr::mutate(wind_speed = as.numeric(wind_speed),
                timestamp = lubridate::ymd_hms(paste(as.character(date), as.character(time))),
                wind_dir_cat = case_when(
                  wind_direction >= 315 | wind_direction < 45 ~ "N",
                  wind_direction >= 45 & wind_direction < 135 ~ "E",
                  wind_direction >= 135 & wind_direction < 225 ~ "S",
                  wind_direction >= 225 & wind_direction < 315 ~ "W",
                )) %>% 
  dplyr::select(-c(date, time))

wind_data

# read temperature data
temp_data <- readr::read_csv2("data/temperature_smhi_mala_brannan.csv", skip = 9) %>% 
  dplyr::select(date = Datum, time = 2, temperature = 3) %>% 
  dplyr::mutate(temperature = as.numeric(temperature),
                timestamp = lubridate::ymd_hms(paste(as.character(date), as.character(time))),
                temp_cat = ifelse(temperature <= 0, "negative", "positive")) %>% 
  dplyr::select(-c(date, time))

climate_data <- wind_data %>% 
  dplyr::left_join(temp_data, by = "timestamp"
  ) %>% 
  dplyr::select(timestamp, wind_speed, wind_dir_cat, temp_cat)

climate_data

# fill NA
climate_data$temp_cat_fill <- climate_data$temp_cat
for(i in 1:nrow(climate_data)) {
  if(i != 1 & is.na(climate_data$temp_cat[i])) {
    climate_data$temp_cat_fill[i] <- climate_data$temp_cat_fill[i-1]
  }
}
sum(is.na(climate_data$temp_cat))
sum(is.na(climate_data$temp_cat_fill))

# merge this with the data
mov_track_autumn_annotated_2h_inds_tpi_landcc_view_wind <- mov_track_autumn_annotated_2h_inds_tpi_landcc_view %>% 
  dplyr::bind_cols(
    mov_track_autumn_annotated_2h_inds_tpi_landcc_view %>% 
      dplyr::select(t1_) %>% 
      dplyr::mutate(t1_round = lubridate::floor_date(t1_, unit = "hours")) %>% 
      dplyr::left_join(climate_data, by = c("t1_round" = "timestamp")) %>% 
      dplyr::select(!contains("t1"))
  )

# read sound definitions
sound_definitions <- read.csv("data/categories_sound_models_mala.csv")

# define
mov_track_autumn_annotated_2h_inds_tpi_landcc_view_wind <- mov_track_autumn_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::mutate(sound_intensity = case_when(
    temp_cat == "negative" & wind_dir_cat == "N" & wind_speed >= 6 ~ sound_model_mala_53,
    temp_cat == "negative" & wind_dir_cat == "E" & wind_speed >= 6 ~ sound_model_mala_54,
    temp_cat == "negative" & wind_dir_cat == "S" & wind_speed >= 6 ~ sound_model_mala_55,
    temp_cat == "negative" & wind_dir_cat == "W" & wind_speed >= 6 ~ sound_model_mala_56,
    temp_cat == "positive" & wind_dir_cat == "N" & wind_speed >= 6 ~ sound_model_mala_57,
    temp_cat == "positive" & wind_dir_cat == "E" & wind_speed >= 6 ~ sound_model_mala_58,
    temp_cat == "positive" & wind_dir_cat == "S" & wind_speed >= 6 ~ sound_model_mala_59,
    temp_cat == "positive" & wind_dir_cat == "W" & wind_speed >= 6 ~ sound_model_mala_60,
    temp_cat == "negative" & wind_dir_cat == "N" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_61,
    temp_cat == "negative" & wind_dir_cat == "E" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_62,
    temp_cat == "negative" & wind_dir_cat == "S" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_63,
    temp_cat == "negative" & wind_dir_cat == "W" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_64,
    temp_cat == "positive" & wind_dir_cat == "N" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_65,
    temp_cat == "positive" & wind_dir_cat == "E" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_66,
    temp_cat == "positive" & wind_dir_cat == "S" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_67,
    temp_cat == "positive" & wind_dir_cat == "W" & wind_speed >= 3 & wind_speed < 6 ~ sound_model_mala_68,
    TRUE ~ 0
  ))

# check
mov_track_autumn_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::filter(wind_speed < 3) %>% 
  pull(sound_intensity) %>% summary

mov_track_autumn_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::filter(wind_speed > 6) %>% 
  pull(sound_intensity) %>% summary


# clean dataset
mov_track_autumn_annotated_2h_inds_tpi_landcc_view_wind <- mov_track_autumn_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::select(-c(contains("sound_model"), wind_speed, wind_dir_cat, temp_cat))



#--------------- 
# get data ready to analysis
data_ready_autumn_mov_track_2h <- mov_track_autumn_annotated_2h_inds_tpi_landcc_view_wind %>% 
  dplyr::filter(landcover_cc != "water") %>% 
  dplyr::mutate(landcover_cc = droplevels(landcover_cc),
                viewshed = factor(viewshed_jsy, levels = 0:1, labels = c("out of sight", "in sight")),
                cos_aspect = cos(aspect))

save(data_ready_autumn_mov_track_2h, file = "output/data_annotated_autumn_ready_2h_mov_rate.RData")
