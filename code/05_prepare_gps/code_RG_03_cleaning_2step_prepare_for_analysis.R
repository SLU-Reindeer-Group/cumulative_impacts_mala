#' ---
#' title: 'Cleaning GPS reindeer data for Mala herding disctrict - step 2 - general cleaning'
#' author: Bernardo Niebuhr, based on code by Erik Cronvall and Anna Skarin
#' ---

#---- global_options, include=FALSE
library(knitr)
knitr::opts_chunk$set(eval = FALSE, tidy = TRUE, tidy.opts=list(width.cutoff=60))

#---- settings

# clean workspace
rm(list = ls())

# load libraries
library(install.load)
install.load::install_load("odbc", "DBI")
install.load::install_load("tidyverse", "lubridate")
install.load::install_load("sf", "adehabitatLT")

# parameters
crs_to_use <- 3006

#---- load_gps_data

#' # 1 Load GPS data
#'
#' Here we connect to WRAM and get the data from there for the Sami herding districts of interest.
#' Alternatively, if the data is not available in WRAM, it can be loaded from a file, but it would be
#' nice to try to keep the same standard column names.
#'
#' There may also be the case that part of the data is in WRAM and another part in files got directly from
#' herders or from the GPS provider.
#'
#' We also check whether there is any collar in WRAM which is not recorded in the Object_Info table, or any
#' indidivual from the Object_Info table whose data is missing in WRAM.

# connection to the main dataset
con <- dbConnect(odbc(),
                 Driver   = "SQL Server",
                 Server   = "alces.slu.se\\gps",
                 Database = "GPS_Ren")

# get Object_Info table from WRAM, only Collars from the herding district of interest
district_codes <- c("RG") # this could be a vector of several districts also
codes_wrapped <- paste(district_codes, collapse = "|")

object_info <- dbReadTable(con, "Object_Info") %>%
  dplyr::as_tibble() %>%
  dplyr::filter(grepl(codes_wrapped, ADGroup))

# get movement data already saved from WRAM, Natrix (FIT), Telespor and Tracker

# folder with data saved
folder_before <- "output/RG_mala/before_cleaning/"

# Load FIT data from WRAM and Natrix
load(file = paste0(folder_before, "gps_raw_wram_natrix_corrected_2020_11_d20.RData"))
gps_raw_wram_natrix_obj

# Load Tracker data
load(file = paste0(folder_before, "gps_tracker_jan_oct2020_2020_11_d20.RData"))
gps_tracker_jan_oct2020

gps_tracker_jan_oct2020_clean <- gps_tracker_jan_oct2020 %>% 
  dplyr::distinct(Collar_ID, GMT_date, .keep_all = TRUE)

# Load Telespor data
load(file = paste0(folder_before, "gps_raw_telespor_2020_01_d22.RData"))
gps_raw_telespor

# put all the data together
gps_raw_filter <- dplyr::bind_rows(gps_raw_wram_natrix_obj, gps_tracker_jan_oct2020_clean, gps_raw_telespor)

#---- load_env_data

#' # 2 Load ancillary spatial data
#'
#' Now we load spatial data needed for anotating data: the limits of the herding districts, corrals,
#' houses, buildings, and urban areas. We also create buffers around them to be able to use them as spatial
#' filters while accounting for GPS errors.

# folder
spatial_folder <- "spatial_data"

# list files
spatial_files <- list.files(spatial_folder, pattern = ".gpkg|.shp", full.names = T, recursive = T)

# load layers

# sameby limits
sameby_limits <- grep("sameby_limits", spatial_files, value = T) %>%
  sf::st_read() %>% 
  sf::st_transform(crs = crs_to_use)

# corrals
corrals <- grep("corrals_IRENMARK", spatial_files, value = T) %>%
  sf::st_read() %>% 
  dplyr::filter(SAMEBY1 == "Mala", BESKRIVNIN == "Arbetshage") %>% 
  sf::st_transform(crs = crs_to_use)

# beteshage
beteshage <- grep("corrals_IRENMARK", spatial_files, value = T) %>%
  sf::st_read() %>% 
  dplyr::filter(SAMEBY1 == "Mala", BESKRIVNIN == "Beteshage") %>% 
  sf::st_transform(crs = crs_to_use)

# availability areas
availability_calving_summer <- grep("mala_calving", spatial_files, value = T) %>% 
  sf::st_read() %>% 
  sf::st_transform(crs = crs_to_use)

availability_late_summer <- grep("mala_late", spatial_files, value = T) %>% 
  sf::st_read() %>% 
  sf::st_transform(crs = crs_to_use)

availability_autumn <- grep("mala_autumn", spatial_files, value = T) %>% 
  sf::st_read() %>% 
  sf::st_transform(crs = crs_to_use)

#---
# buffer for herding district (30 km), corrals (100 m)
# for corrals this may be realted to possible errors in mapping

district_names <- c("Mala")
district_buffer <- sameby_limits %>%
  dplyr::filter(NAMN %in% district_names) %>%
  sf::st_buffer(dist = 30000) %>% 
  sf::st_transform(crs = st_crs(corrals))

corrals_buffer <- corrals %>%
  sf::st_buffer(dist = 200)

beteshage_buffer <- beteshage %>%
  sf::st_buffer(dist = 200)

#---- load_auxiliary_files

#' # 3 Load auxiliary files
#' 
#' Here we load auxiliary files. This may be the dates when the season of interest starts and ends for each year,
#' lists of duplicated individuals, and tables with periods when the collars were off animals or when animals were herded.

# load aux functions, such as the one to change year name
# or the one to check positions within/around corrals
source("code/code_general_00_aux_functions.R")

# folder
files_folder <- "data/RG_mala/"

# list files
aux_files <- list.files(files_folder, pattern = ".csv", full.names = T, recursive = T)

# details winter
details_years <- grep("details", aux_files, value = T) %>% 
  grep(pattern = "mala_clean", value = T) %>% 
  read.csv(stringsAsFactors = F) %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(construction = factor(construction, levels = c("before", "during", "after"))) %>% 
  dplyr::mutate_at(vars(contains("date")), lubridate::ymd, tz = "UTC")

# repeated animals, to be removed
individuals_to_remove <- grep("individuals", aux_files, value = T) %>% 
  readr::read_csv() %>% 
  pull(Object_ID_year)
individuals_to_remove

# load herding events and select only the reviewed ones
# here the function readr::read_csv is already transforming the timestamps into POSIX, UTC, but otherwise
# it may be necessary to do it by hand
events <- grep("herding_events", aux_files, value = T) %>% 
  readr::read_csv() %>% 
  dplyr::filter(reviewed_1_0 != 0) # take only individuals and events that were reviewed mannualy

# values:
# 0 = normal animal movement
# 1 = collar removed from animal
# 2 = herding event with animals transported between corrals or locations
# 6 = herding (visit to corral); in the end both 2 and 6 were used to the same thing
# 7 = in migration (herded or not, we are not interested on that)
# 8 = probably calf markig
# 9 = errors, spikes
# 99 = ignore this line
# all the other values correspond to an actual animal moving

# moments when the collars were taken out or were out of animals
# not animals moving
collars_out <- events %>% 
  dplyr::filter(code == 1) 

# moments when the animals were herded
herding_events <- events %>% 
  dplyr::filter(code %in% c(2, 6, 7, 8))

# spikes
spikes <- events %>% 
  dplyr::filter(code == 9)

# check dates
# the codes below must be checked to guarantee that there are no errors in the sheet checked mannualy

# dates consistent?
any(herding_events$timestamp_begin < herding_events$timestamp_end, na.rm = T) # no event ends before it starts, good!
# which(herding_events$timestamp_begin < herding_events$timestamp_end)
# herding_events %>% slice(which(herding_events$timestamp_begin < herding_events$timestamp_end))

# duration of events, days
herding_events <- herding_events %>% 
  dplyr::mutate(duration = difftime(timestamp_begin, timestamp_end, units = "days"))

tab  <- hist(as.numeric(herding_events$duration), breaks = 200)
tab <- data.frame(breaks = tab$breaks[-length(tab$breaks)], counts = tab$counts)

# check very long periods of herding
herding_events %>% dplyr::filter(duration > 100) # ok, in all of them the collar is not in an animal

herding_events <- herding_events %>% 
  dplyr::select(-duration)


#------ filter_data

#' # 3 Remove data with low DOP and perform spatial and temporal filters
#'
#' Now we perform the first steps of cleaning. First we perform a spatial filter, keeping only positions inside the study area
#' (here, the availability area for winter). We also remove "bad quality" (DOP > 5 or n. of sattelites for the position < 4) and missing data.
#' Then, we perform a temporal filter, keeping only the data for the season of interest, what can vary or not among years.
#' Finally, ...

# dates for the temporal filter, in case we want general and equivalent dates for all years
# temp.filter <- c(lubridate::yday('2019-09-01'), lubridate::yday('2020-05-15')) # winter

# years we are going to work with
years_to_study <- c(2008:2011, 2015:2016)
# sparse data for 2007
# no data for 2012-2014
# not checked for 2017-2020


#------------------------
# Cleaning for Calving season

# remove bad quality and duplicates, keep points within Mala, keep only snow-free season
gps_calving_clean1 <- gps_raw_filter %>%
  # remove bad quality data
  dplyr::filter((DOP < 5 | is.na(DOP)) & (Sats_used >= 4 | is.na(Sats_used))) %>% 
  # remove NA
  dplyr::filter(!is.na(Longitude), !is.na(Latitude)) %>% 
  # remove duplicates
  dplyr::distinct(Object_ID, GMT_date, .keep_all = T) %>% 
  # spatial filter
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  sf::st_transform(crs = crs_to_use) %>%
  sf::st_join(availability_calving_summer, join = st_intersects) %>%
  dplyr::filter(!is.na(NAMN)) %>%
  dplyr::select(1:ncol(gps_raw_filter)) %>%
  # include positions as columns
  dplyr::bind_cols(as_tibble(sf::st_coordinates(.))) %>% 
  dplyr::rename(Longitude = X, Latitude = Y) %>% 
  sf::st_drop_geometry() %>% 
  # rename timestamp column
  dplyr::rename(timestamp = GMT_date) %>%
  # add column correspondent to the year of the winter (starting in January) and the individual-year
  dplyr::mutate(year = lubridate::year(timestamp),
                Object_ID_year = paste0(Object_ID, "_", year)) %>% 
  # sort by year and collarID
  dplyr::arrange(year, Object_ID) %>% 
  # temporal filter, the logical & or | may change depending on the season
  # this is for the same season/days in every year
  # dplyr::filter(lubridate::yday(GMT_date) >= temp_filter[1] |
  #                 lubridate::yday(GMT_date) <= temp_filter[2]) %>% 
  dplyr::filter(year %in% years_to_study)

# dates for the temporal filter, in case we want different dates per year
# subset data of dates
details_years_to_study <- details_years %>% 
  dplyr::filter(year %in% years_to_study)

# dates for the temporal filter for seasons - calving - here we need a vector for each data point
temp_filter_begin <- details_years_to_study$date_start_calving[as.factor(gps_calving_clean1$year)]
temp_filter_end <- details_years_to_study$date_end_calving[as.factor(gps_calving_clean1$year)]

# temporal filter for season, the logical & or | may change depending on the season
# here it is winter
gps_calving_clean2 <- gps_calving_clean1 %>%
  dplyr::filter(timestamp >= temp_filter_begin &
                  timestamp <= temp_filter_end)

# remove individuals to be removed, and break in the herding events.
gps_calving_clean3 <- gps_calving_clean2 %>% 
  # sort by year and collarID
  dplyr::arrange(year, Object_ID) %>% 
  # select only the relevant columns
  dplyr::select(Collar_ID, Object_ID, Object_ID_year, year, Longitude, Latitude, timestamp) %>% 
  # remove duplicated individuals
  dplyr::filter(!(Object_ID_year %in% individuals_to_remove))

#--- 
# first we mark the herding events

# clean herding events and divide them into bursts
herding_events <- herding_events %>% 
  dplyr::select(Object_ID_year, year, timestamp_end, timestamp_begin, code) %>% 
  dplyr::filter(year %in% years_to_study)

# filtering relevant dates when animals where herded
temp_filter_begin_herding <- details_years_to_study$date_start_calving[as.factor(herding_events$year)]
temp_filter_end_herding <- details_years_to_study$date_end_calving[as.factor(herding_events$year)]

herding_events_calving <- herding_events %>%
  dplyr::filter(!(timestamp_begin < temp_filter_begin_herding),
                !(timestamp_end > temp_filter_end_herding)) 
herding_events_calving

# mark lines corresponding to herding
# here we use the function lines_to_remove
where_herding <- lines_to_remove(dataset = gps_calving_clean3, when_to_remove = herding_events_calving)

# to remove
# gps_clean6 <- gps_clean5[-where_herding,]

# to mark and remove later, if we want
gps_calving_clean4 <- gps_calving_clean3 %>% 
  dplyr::mutate(herding = 0)
gps_calving_clean4$herding[where_herding$lines_to_remove] <- 1

#---
# now we remove individuals when the collar was Off or not in an animal
# code == 1
# keep only what correspond to collar off or herding events
collars_out <- collars_out %>% 
  dplyr::select(Object_ID_year, year, timestamp_end, timestamp_begin, code) %>% 
  dplyr::filter(year %in% years_to_study)

# filtering relevant dates when collars where out of animals
temp_filter_begin_colout <- details_years_to_study$date_start_calving[as.factor(collars_out$year)]
temp_filter_end_colout <- details_years_to_study$date_start_calving[as.factor(collars_out$year)]

collars_out_calving <- collars_out %>%
  dplyr::filter(!(timestamp_begin < temp_filter_begin_colout),
                !(timestamp_end > temp_filter_end_colout)) 
collars_out_calving

# here we use the function lines_to_remove from the aux script file
where_collar_out <- lines_to_remove(dataset = gps_calving_clean4, when_to_remove = collars_out_calving)

# first we rename the individuals using the row number identified above
# this must be done before removeing the lines when the collar was removed
gps_calving_clean5 <- gps_calving_clean4

where_collar_out$lines_of_new_individuals
gps_calving_clean5$Object_ID_year[where_collar_out$lines_of_new_individuals]

# make new names for all
# we add _newind to the end of the Object_ID_year_name
new_names <- gps_calving_clean5$Object_ID_year
stringr::str_sub(new_names, -5, -6) <- "_newind"
new_names

# replace
gps_calving_clean5$Object_ID_year[where_collar_out$lines_of_new_individuals] <- new_names[where_collar_out$lines_of_new_individuals]

# check
unique(gps_calving_clean4$Object_ID_year)
unique(gps_calving_clean5$Object_ID_year)

# remove lines when collar was out
gps_calving_clean5 <- gps_calving_clean5[-where_collar_out$lines_to_remove,]

# mark and remove moments when animals were within/around corrals
# considering a radius of 200 m
gps_calving_clean5_sf <- gps_calving_clean5 %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), 
               crs = crs_to_use, remove = F)

# check overlap with different facilities
# here we use the function check_within

# check overlap with different facilities
gps_in_facilities <- check_within(gps_locations = gps_calving_clean5_sf, 
                                  areas = corrals_buffer, feature_name = "corrals", min_points = 1)
when_in_corral <- which(!is.na(gps_in_facilities$corrals_polygon))

gps_calving_clean5 <- gps_calving_clean5[-when_in_corral,]

# check the period of monitoring of each individual
(summary1 <- gps_calving_clean5 %>% 
    dplyr::group_by(year) %>% 
    summarise(n = n(),
              n_inds = length(unique(Object_ID_year)),
              avg_n = n/n_inds))

(n_tot <- sum(summary1$n_inds))

(summary2 <- gps_calving_clean5 %>% 
    dplyr::group_by(year, Object_ID_year) %>% 
    summarise(start = min(timestamp),
              end = max(timestamp),
              duration = round(difftime(end, start, units = "days")),
              n = n())) %>% 
  print(n = 350)

# remove individuals with less than 14 days of monitoring
min_days <- 14

inds_few_days <- summary2 %>% 
  dplyr::filter(as.numeric(duration) <= min_days)

gps_calving_clean6 <- gps_calving_clean5 %>% 
  dplyr::filter(!(Object_ID_year %in% inds_few_days$Object_ID_year))

# check again and summarize
(summary1 <- gps_calving_clean6 %>% 
    dplyr::group_by(year) %>% 
    summarise(n = n(),
              n_inds = length(unique(Object_ID_year)),
              avg_n = n/n_inds))

(n_tot <- sum(summary1$n_inds))

(summary2 <- gps_calving_clean6 %>% 
    dplyr::group_by(year, Object_ID_year) %>% 
    summarise(start = min(timestamp),
              end = max(timestamp),
              duration = round(difftime(end, start, units = "days")),
              n = n())) %>% 
  print(n = 300)

summary2 %>% 
  group_by(year) %>% 
  summarise(mean_duration = mean(duration),
            avg_n = mean(n))

#--- 
# now we come back to herding to divide data into bursts (separated
# by herding events) and remove the herding events

# divide into bursts using data from the herding events

# transform data into ltraj
gps_calving_clean6_aux <- as.data.frame(gps_calving_clean6)
gps_ltraj <- as.ltraj(xy = gps_calving_clean6_aux %>% dplyr::select(x = Longitude, y = Latitude),
                      date = gps_calving_clean6_aux$timestamp, id = gps_calving_clean6_aux$Object_ID_year,
                      infolocs = gps_calving_clean6_aux[c("Collar_ID", "Object_ID", "year", "herding")],
                      proj4string = sp::CRS(paste0("+init=epsg:", crs_to_use)))
gps_ltraj

# function to separate bursts
foo <- function(herding) {
  return(herding == 1)
}

# separate into bursts
gps_calving_clean7 <- cutltraj(gps_ltraj, "foo(herding)", nextr = F) %>% 
  ld()

# remove bursts with too few positions
(summary_bursts1 <- gps_calving_clean7 %>%
    dplyr::group_by(year, id) %>%
    summarise(n = n(),
              n_bursts = length(unique(burst)),
              avg_n = n/n_bursts))

(n_tot_bursts <- sum(summary_bursts1$n_bursts))

(summary_bursts2 <- gps_calving_clean7 %>% 
    dplyr::group_by(year, burst) %>% 
    summarise(start = min(date),
              end = max(date),
              duration = round(difftime(end, start, units = "days")),
              n = n())) %>% 
  print(n = 300)

# remove bursts with less than 7 days
min_days_burst <- 14
fix_rate <- 4
min_points_burst <- 14*24/fix_rate

burst_few_days <- summary_bursts2 %>% 
  dplyr::filter(as.numeric(duration) <= min_days_burst | n <= min_points_burst)

gps_calving_clean8 <- gps_calving_clean7 %>% 
  dplyr::filter(!(burst %in% burst_few_days$burst))

# rename bursts
gps_ltraj_rename <- gps_calving_clean8 %>% 
  dl()

summ_cut <- summary(gps_ltraj_rename) %>% 
  group_by(id) %>% 
  summarize(rows = length(burst))
# function to rename
name_burst <- function(name, n_bursts) paste0(name, "_", 1:n_bursts)
# test
name_burst("maria", 2)
# new names
new_names <- unlist(mapply(name_burst, summ_cut$id, summ_cut$rows))
# rename and remove herding events
gps_calving_clean9 <- gps_ltraj_rename %>% 
  ld() %>% 
  dplyr::mutate(burst = new_names[as.factor(burst)],
                id = as.character(id)) %>% 
  dplyr::as_tibble() %>% 
  dplyr::arrange(year, id, burst) %>% 
  dplyr::select(-herding)

# summarize final number of animals
(summary1 <- gps_calving_clean9 %>% 
    dplyr::group_by(year) %>% 
    summarise(n = n(),
              n_inds = length(unique(id)),
              avg_n = round(n/n_inds)))

(n_tot <- sum(summary1$n_inds))

(summary2 <- gps_calving_clean9 %>% 
    dplyr::group_by(year, id) %>% 
    summarise(start = min(date),
              end = max(date),
              duration_days = round(difftime(end, start, units = "days")),
              n = n())) %>% 
  print(n = 300)

summary3 <- summary2 %>% 
  group_by(year) %>% 
  summarise(mean_duration = mean(duration_days),
            avg_n = mean(n))

summary1 <- summary1 %>% 
  dplyr::left_join(summary3[,1:2], by = "year")

# write summary tables
readr::write_csv(summary1, file = "output/RG_mala/summary_n_per_season_calving.csv")
readr::write_csv(summary2, file = "output/RG_mala/summary_n_per_individual_calving.csv")

#------------------------
# Cleaning for Summer season

# first half of the summer: until July 31

# remove bad quality and duplicates, keep points within Mala, keep only snow-free season
gps_summer_clean1a <- gps_raw_filter %>%
  # remove bad quality data
  dplyr::filter((DOP < 5 | is.na(DOP)) & (Sats_used >= 4 | is.na(Sats_used))) %>% 
  # remove NA
  dplyr::filter(!is.na(Longitude), !is.na(Latitude)) %>% 
  # remove duplicates
  dplyr::distinct(Object_ID, GMT_date, .keep_all = T) %>% 
  # spatial filter
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  sf::st_transform(crs = crs_to_use) %>%
  sf::st_join(availability_calving_summer, join = st_intersects) %>%
  dplyr::filter(!is.na(NAMN)) %>%
  dplyr::select(1:ncol(gps_raw_filter)) %>%
  # include positions as columns
  dplyr::bind_cols(as_tibble(sf::st_coordinates(.))) %>% 
  dplyr::rename(Longitude = X, Latitude = Y) %>% 
  sf::st_drop_geometry() %>% 
  # rename timestamp column
  dplyr::rename(timestamp = GMT_date) %>%
  # add column correspondent to the year of the winter (starting in January) and the individual-year
  dplyr::mutate(year = lubridate::year(timestamp),
                Object_ID_year = paste0(Object_ID, "_", year)) %>% 
  # sort by year and collarID
  dplyr::arrange(year, Object_ID) %>% 
  # temporal filter, the logical & or | may change depending on the season
  # this is for the same season/days in every year
  # dplyr::filter(lubridate::yday(GMT_date) >= temp_filter[1] |
  #                 lubridate::yday(GMT_date) <= temp_filter[2]) %>% 
  dplyr::filter(year %in% years_to_study)

# dates for the temporal filter, in case we want different dates per year
# subset data of dates
details_years_to_study <- details_years %>% 
  dplyr::filter(year %in% years_to_study)

mid_summer <- lubridate::ymd_hms(paste0(years_to_study, "-08-01 00:00:00"))

# dates for the temporal filter for seasons - summer - here we need a vector for each data point
temp_filter_begin <- details_years_to_study$date_start_summer[as.factor(gps_summer_clean1a$year)]
temp_filter_end <- (mid_summer - days(1))[as.factor(gps_summer_clean1a$year)]

# temporal filter for season, the logical & or | may change depending on the season
# here it is winter
gps_summer_clean2a <- gps_summer_clean1a %>%
  dplyr::filter(timestamp > temp_filter_begin &
                  timestamp <= temp_filter_end)

#-----

# second half of the summer: from Aug 1st to July 15th

# remove bad quality and duplicates, keep points within Mala, keep only snow-free season
gps_summer_clean1b <- gps_raw_filter %>%
  # remove bad quality data
  dplyr::filter((DOP < 5 | is.na(DOP)) & (Sats_used >= 4 | is.na(Sats_used))) %>% 
  # remove NA
  dplyr::filter(!is.na(Longitude), !is.na(Latitude)) %>% 
  # remove duplicates
  dplyr::distinct(Object_ID, GMT_date, .keep_all = T) %>% 
  # spatial filter
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  sf::st_transform(crs = crs_to_use) %>%
  sf::st_join(availability_late_summer, join = st_intersects) %>%
  dplyr::filter(!is.na(NAMN)) %>%
  dplyr::select(1:ncol(gps_raw_filter)) %>%
  # include positions as columns
  dplyr::bind_cols(as_tibble(sf::st_coordinates(.))) %>% 
  dplyr::rename(Longitude = X, Latitude = Y) %>% 
  sf::st_drop_geometry() %>% 
  # rename timestamp column
  dplyr::rename(timestamp = GMT_date) %>%
  # add column correspondent to the year of the winter (starting in January) and the individual-year
  dplyr::mutate(year = lubridate::year(timestamp),
                Object_ID_year = paste0(Object_ID, "_", year)) %>% 
  # sort by year and collarID
  dplyr::arrange(year, Object_ID) %>% 
  # temporal filter, the logical & or | may change depending on the season
  # this is for the same season/days in every year
  # dplyr::filter(lubridate::yday(GMT_date) >= temp_filter[1] |
  #                 lubridate::yday(GMT_date) <= temp_filter[2]) %>% 
  dplyr::filter(year %in% years_to_study)

# dates for the temporal filter for seasons - summer - here we need a vector for each data point
temp_filter_begin <- mid_summer[as.factor(gps_summer_clean1b$year)]
temp_filter_end <- details_years_to_study$date_end_summer[as.factor(gps_summer_clean1b$year)]

# temporal filter for season, the logical & or | may change depending on the season
# here it is winter
gps_summer_clean2b <- gps_summer_clean1b %>%
  dplyr::filter(timestamp >= temp_filter_begin &
                  timestamp <= temp_filter_end)

# remove individuals to be removed, and break in the herding events.
gps_summer_clean3 <- gps_summer_clean2a %>% 
  # combine both half data parts of the summer
  dplyr::bind_rows(gps_summer_clean2b) %>% 
  # sort by year and collarID
  dplyr::arrange(year, Object_ID) %>% 
  # select only the relevant columns
  dplyr::select(Collar_ID, Object_ID, Object_ID_year, year, Longitude, Latitude, timestamp) %>% 
  # remove duplicated individuals
  dplyr::filter(!(Object_ID_year %in% individuals_to_remove))

#--- 
# first we mark the herding events

# clean herding events and divide them into bursts
herding_events <- herding_events %>% 
  dplyr::select(Object_ID_year, year, timestamp_end, timestamp_begin, code) %>% 
  dplyr::filter(year %in% years_to_study)

# filtering relevant dates when animals where herded
temp_filter_begin_herding <- details_years_to_study$date_start_summer[as.factor(herding_events$year)]
temp_filter_end_herding <- details_years_to_study$date_end_summer[as.factor(herding_events$year)]

herding_events_summer <- herding_events %>%
  dplyr::filter(!(timestamp_begin < temp_filter_begin_herding),
                !(timestamp_end > temp_filter_end_herding)) 
herding_events_summer

# mark lines corresponding to herding
# here we use the function lines_to_remove
where_herding <- lines_to_remove(dataset = gps_summer_clean3, when_to_remove = herding_events_summer)

# to remove
# gps_clean6 <- gps_clean5[-where_herding,]

# to mark and remove later, if we want
gps_summer_clean4 <- gps_summer_clean3 %>% 
  dplyr::mutate(herding = 0)
gps_summer_clean4$herding[where_herding$lines_to_remove] <- 1

#---
# now we remove individuals when the collar was Off or not in an animal
# code == 1
# keep only what correspond to collar off or herding events
collars_out <- collars_out %>% 
  dplyr::select(Object_ID_year, year, timestamp_end, timestamp_begin, code) %>% 
  dplyr::filter(year %in% years_to_study)

# filtering relevant dates when collars where out of animals
temp_filter_begin_colout <- details_years_to_study$date_start_summer[as.factor(collars_out$year)]
temp_filter_end_colout <- details_years_to_study$date_start_summer[as.factor(collars_out$year)]

collars_out_summer <- collars_out %>%
  dplyr::filter(!(timestamp_begin < temp_filter_begin_colout),
                !(timestamp_end > temp_filter_end_colout)) 
collars_out_summer

# here we use the function lines_to_remove from the aux script file
where_collar_out <- lines_to_remove(dataset = gps_summer_clean4, when_to_remove = collars_out_summer)

# first we rename the individuals using the row number identified above
# this must be done before removeing the lines when the collar was removed
gps_summer_clean5 <- gps_summer_clean4

where_collar_out$lines_of_new_individuals
gps_summer_clean5$Object_ID_year[where_collar_out$lines_of_new_individuals]

# make new names for all
# we add _newind to the end of the Object_ID_year_name
new_names <- gps_summer_clean5$Object_ID_year
stringr::str_sub(new_names, -5, -6) <- "_newind"
new_names

# replace
gps_summer_clean5$Object_ID_year[where_collar_out$lines_of_new_individuals] <- new_names[where_collar_out$lines_of_new_individuals]

# check
unique(gps_summer_clean4$Object_ID_year)
unique(gps_summer_clean5$Object_ID_year)

# remove lines when collar was out
gps_summer_clean5 <- gps_summer_clean5[-where_collar_out$lines_to_remove,]

# mark and remove moments when animals were within/around corrals
# considering a radius of 200 m
gps_summer_clean5_sf <- gps_summer_clean5 %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), 
               crs = crs_to_use, remove = F)

# check overlap with different facilities
# here we use the function check_within

# check overlap with different facilities
gps_in_facilities <- check_within(gps_locations = gps_summer_clean5_sf, 
                                  areas = corrals_buffer, feature_name = "corrals", min_points = 1)
when_in_corral <- which(!is.na(gps_in_facilities$corrals_polygon))

gps_summer_clean5 <- gps_summer_clean5[-when_in_corral,]

# check the period of monitoring of each individual
(summary1 <- gps_summer_clean5 %>% 
    dplyr::group_by(year) %>% 
    summarise(n = n(),
              n_inds = length(unique(Object_ID_year)),
              avg_n = n/n_inds))

(n_tot <- sum(summary1$n_inds))

(summary2 <- gps_summer_clean5 %>% 
    dplyr::group_by(year, Object_ID_year) %>% 
    summarise(start = min(timestamp),
              end = max(timestamp),
              duration = round(difftime(end, start, units = "days")),
              n = n())) %>% 
  print(n = 350)

# remove individuals with less than 14 days of monitoring
min_days <- 14

inds_few_days <- summary2 %>% 
  dplyr::filter(as.numeric(duration) <= min_days)

gps_summer_clean6 <- gps_summer_clean5 %>% 
  dplyr::filter(!(Object_ID_year %in% inds_few_days$Object_ID_year))

# check again and summarize
(summary1 <- gps_summer_clean6 %>% 
    dplyr::group_by(year) %>% 
    summarise(n = n(),
              n_inds = length(unique(Object_ID_year)),
              avg_n = n/n_inds))

(n_tot <- sum(summary1$n_inds))

(summary2 <- gps_summer_clean6 %>% 
    dplyr::group_by(year, Object_ID_year) %>% 
    summarise(start = min(timestamp),
              end = max(timestamp),
              duration = round(difftime(end, start, units = "days")),
              n = n())) %>% 
  print(n = 300)

summary2 %>% 
  group_by(year) %>% 
  summarise(mean_duration = mean(duration),
            avg_n = mean(n))

#--- 
# now we come back to herding to divide data into bursts (separated
# by herding events) and remove the herding events

# divide into bursts using data from the herding events

# transform data into ltraj
gps_summer_clean6_aux <- as.data.frame(gps_summer_clean6)
gps_ltraj <- as.ltraj(xy = gps_summer_clean6_aux %>% dplyr::select(x = Longitude, y = Latitude),
                      date = gps_summer_clean6_aux$timestamp, id = gps_summer_clean6_aux$Object_ID_year,
                      infolocs = gps_summer_clean6_aux[c("Collar_ID", "Object_ID", "year", "herding")],
                      proj4string = sp::CRS(paste0("+init=epsg:", crs_to_use)))
gps_ltraj

# function to separate bursts
foo <- function(herding) {
  return(herding == 1)
}

# separate into bursts
gps_summer_clean7 <- cutltraj(gps_ltraj, "foo(herding)", nextr = F) %>% 
  ld()

# remove bursts with too few positions
(summary_bursts1 <- gps_summer_clean7 %>%
    dplyr::group_by(year, id) %>%
    summarise(n = n(),
              n_bursts = length(unique(burst)),
              avg_n = n/n_bursts))

(n_tot_bursts <- sum(summary_bursts1$n_bursts))

(summary_bursts2 <- gps_summer_clean7 %>% 
    dplyr::group_by(year, burst) %>% 
    summarise(start = min(date),
              end = max(date),
              duration = round(difftime(end, start, units = "days")),
              n = n())) %>% 
  print(n = 300)

# remove bursts with less than 7 days
min_days_burst <- 14
fix_rate <- 4
min_points_burst <- 14*24/fix_rate

burst_few_days <- summary_bursts2 %>% 
  dplyr::filter(as.numeric(duration) <= min_days_burst | n <= min_points_burst)

gps_summer_clean8 <- gps_summer_clean7 %>% 
  dplyr::filter(!(burst %in% burst_few_days$burst))

# rename bursts
gps_ltraj_rename <- gps_summer_clean8 %>% 
  dl()

summ_cut <- summary(gps_ltraj_rename) %>% 
  group_by(id) %>% 
  summarize(rows = length(burst))
# function to rename
name_burst <- function(name, n_bursts) paste0(name, "_", 1:n_bursts)
# test
name_burst("maria", 2)
# new names
new_names <- unlist(mapply(name_burst, summ_cut$id, summ_cut$rows))
# rename and remove herding events
gps_summer_clean9 <- gps_ltraj_rename %>% 
  ld() %>% 
  dplyr::mutate(burst = new_names[as.factor(burst)],
                id = as.character(id)) %>% 
  dplyr::as_tibble() %>% 
  dplyr::arrange(year, id, burst) %>% 
  dplyr::select(-herding)

# summarize final number of animals
(summary1 <- gps_summer_clean9 %>% 
    dplyr::group_by(year) %>% 
    summarise(n = n(),
              n_inds = length(unique(id)),
              avg_n = round(n/n_inds)))

(n_tot <- sum(summary1$n_inds))

(summary2 <- gps_summer_clean9 %>% 
    dplyr::group_by(year, id) %>% 
    summarise(start = min(date),
              end = max(date),
              duration_days = round(difftime(end, start, units = "days")),
              n = n())) %>% 
  print(n = 300)

summary3 <- summary2 %>% 
  group_by(year) %>% 
  summarise(mean_duration = mean(duration_days),
            avg_n = mean(n))

summary1 <- summary1 %>% 
  dplyr::left_join(summary3[,1:2], by = "year")

# write summary tables
readr::write_csv(summary1, file = "output/RG_mala/summary_n_per_season_summer.csv")
readr::write_csv(summary2, file = "output/RG_mala/summary_n_per_individual_summer.csv")

#------------------------
# Cleaning for Autumn season

# remove bad quality and duplicates, keep points within Mala, keep only snow-free season
gps_autumn_clean1 <- gps_raw_filter %>%
  # remove bad quality data
  dplyr::filter((DOP < 5 | is.na(DOP)) & (Sats_used >= 4 | is.na(Sats_used))) %>% 
  # remove NA
  dplyr::filter(!is.na(Longitude), !is.na(Latitude)) %>% 
  # remove duplicates
  dplyr::distinct(Object_ID, GMT_date, .keep_all = T) %>% 
  # spatial filter
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  sf::st_transform(crs = crs_to_use) %>%
  sf::st_join(availability_autumn, join = st_intersects) %>%
  dplyr::filter(!is.na(NAMN)) %>%
  dplyr::select(1:ncol(gps_raw_filter)) %>%
  # include positions as columns
  dplyr::bind_cols(as_tibble(sf::st_coordinates(.))) %>% 
  dplyr::rename(Longitude = X, Latitude = Y) %>% 
  sf::st_drop_geometry() %>% 
  # rename timestamp column
  dplyr::rename(timestamp = GMT_date) %>%
  # add column correspondent to the year of the winter (starting in January) and the individual-year
  dplyr::mutate(year = lubridate::year(timestamp),
                Object_ID_year = paste0(Object_ID, "_", year)) %>% 
  # sort by year and collarID
  dplyr::arrange(year, Object_ID) %>% 
  # temporal filter, the logical & or | may change depending on the season
  # this is for the same season/days in every year
  # dplyr::filter(lubridate::yday(GMT_date) >= temp_filter[1] |
  #                 lubridate::yday(GMT_date) <= temp_filter[2]) %>% 
  dplyr::filter(year %in% years_to_study)

# dates for the temporal filter, in case we want different dates per year
# subset data of dates
details_years_to_study <- details_years %>% 
  dplyr::filter(year %in% years_to_study)

# dates for the temporal filter for seasons - autumn - here we need a vector for each data point
temp_filter_begin <- details_years_to_study$date_start_autumn[as.factor(gps_autumn_clean1$year)]
temp_filter_end <- details_years_to_study$date_end_autumn[as.factor(gps_autumn_clean1$year)]

# temporal filter for season, the logical & or | may change depending on the season
# here it is winter
gps_autumn_clean2 <- gps_autumn_clean1 %>%
  dplyr::filter(timestamp >= temp_filter_begin &
                  timestamp < temp_filter_end)

# remove individuals to be removed, and break in the herding events.
gps_autumn_clean3 <- gps_autumn_clean2 %>% 
  # sort by year and collarID
  dplyr::arrange(year, Object_ID) %>% 
  # select only the relevant columns
  dplyr::select(Collar_ID, Object_ID, Object_ID_year, year, Longitude, Latitude, timestamp) %>% 
  # remove duplicated individuals
  dplyr::filter(!(Object_ID_year %in% individuals_to_remove))

#--- 
# first we mark the herding events

# clean herding events and divide them into bursts
herding_events <- herding_events %>% 
  dplyr::select(Object_ID_year, year, timestamp_end, timestamp_begin, code) %>% 
  dplyr::filter(year %in% years_to_study)

# filtering relevant dates when animals where herded
temp_filter_begin_herding <- details_years_to_study$date_start_autumn[as.factor(herding_events$year)]
temp_filter_end_herding <- details_years_to_study$date_end_autumn[as.factor(herding_events$year)]

herding_events_autumn <- herding_events %>%
  dplyr::filter(!(timestamp_begin < temp_filter_begin_herding),
                !(timestamp_end > temp_filter_end_herding)) 
herding_events_autumn

# mark lines corresponding to herding
# here we use the function lines_to_remove
where_herding <- lines_to_remove(dataset = gps_autumn_clean3, when_to_remove = herding_events_autumn)

# to remove
# gps_clean6 <- gps_clean5[-where_herding,]

# to mark and remove later, if we want
gps_autumn_clean4 <- gps_autumn_clean3 %>% 
  dplyr::mutate(herding = 0)
gps_autumn_clean4$herding[where_herding$lines_to_remove] <- 1

#---
# now we remove individuals when the collar was Off or not in an animal
# code == 1
# keep only what correspond to collar off or herding events
collars_out <- collars_out %>% 
  dplyr::select(Object_ID_year, year, timestamp_end, timestamp_begin, code) %>% 
  dplyr::filter(year %in% years_to_study)

# filtering relevant dates when collars where out of animals
temp_filter_begin_colout <- details_years_to_study$date_start_autumn[as.factor(collars_out$year)]
temp_filter_end_colout <- details_years_to_study$date_start_autumn[as.factor(collars_out$year)]

collars_out_autumn <- collars_out %>%
  dplyr::filter(!(timestamp_begin < temp_filter_begin_colout),
                !(timestamp_end > temp_filter_end_colout)) 
collars_out_autumn

# here we use the function lines_to_remove from the aux script file
where_collar_out <- lines_to_remove(dataset = gps_autumn_clean4, when_to_remove = collars_out_autumn)

# first we rename the individuals using the row number identified above
# this must be done before removeing the lines when the collar was removed
gps_autumn_clean5 <- gps_autumn_clean4

where_collar_out$lines_of_new_individuals
gps_autumn_clean5$Object_ID_year[where_collar_out$lines_of_new_individuals]

# make new names for all
# we add _newind to the end of the Object_ID_year_name
new_names <- gps_autumn_clean5$Object_ID_year
stringr::str_sub(new_names, -5, -6) <- "_newind"
new_names

# replace
gps_autumn_clean5$Object_ID_year[where_collar_out$lines_of_new_individuals] <- new_names[where_collar_out$lines_of_new_individuals]

# check
unique(gps_autumn_clean4$Object_ID_year)
unique(gps_autumn_clean5$Object_ID_year)

# remove lines when collar was out
gps_autumn_clean5 <- gps_autumn_clean5[-where_collar_out$lines_to_remove,]

# mark and remove moments when animals were within/around corrals
# considering a radius of 200 m
gps_autumn_clean5_sf <- gps_autumn_clean5 %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), 
               crs = crs_to_use, remove = F)

# check overlap with different facilities
# here we use the function check_within

# check overlap with different facilities
gps_in_facilities <- check_within(gps_locations = gps_autumn_clean5_sf, 
                                  areas = corrals_buffer, feature_name = "corrals", min_points = 1)
when_in_corral <- which(!is.na(gps_in_facilities$corrals_polygon))

gps_autumn_clean5 <- gps_autumn_clean5[-when_in_corral,]

# check the period of monitoring of each individual
(summary1 <- gps_autumn_clean5 %>% 
    dplyr::group_by(year) %>% 
    summarise(n = n(),
              n_inds = length(unique(Object_ID_year)),
              avg_n = n/n_inds))

(n_tot <- sum(summary1$n_inds))

(summary2 <- gps_autumn_clean5 %>% 
    dplyr::group_by(year, Object_ID_year) %>% 
    summarise(start = min(timestamp),
              end = max(timestamp),
              duration = round(difftime(end, start, units = "days")),
              n = n())) %>% 
  print(n = 350)

# remove individuals with less than 14 days of monitoring
min_days <- 14

inds_few_days <- summary2 %>% 
  dplyr::filter(as.numeric(duration) <= min_days)

gps_autumn_clean6 <- gps_autumn_clean5 %>% 
  dplyr::filter(!(Object_ID_year %in% inds_few_days$Object_ID_year))

# check again and summarize
(summary1 <- gps_autumn_clean6 %>% 
    dplyr::group_by(year) %>% 
    summarise(n = n(),
              n_inds = length(unique(Object_ID_year)),
              avg_n = n/n_inds))

(n_tot <- sum(summary1$n_inds))

(summary2 <- gps_autumn_clean6 %>% 
    dplyr::group_by(year, Object_ID_year) %>% 
    summarise(start = min(timestamp),
              end = max(timestamp),
              duration = round(difftime(end, start, units = "days")),
              n = n())) %>% 
  print(n = 300)

summary2 %>% 
  group_by(year) %>% 
  summarise(mean_duration = mean(duration),
            avg_n = mean(n))

#--- 
# now we come back to herding to divide data into bursts (separated
# by herding events) and remove the herding events

# divide into bursts using data from the herding events

# transform data into ltraj
gps_autumn_clean6_aux <- as.data.frame(gps_autumn_clean6)
gps_ltraj <- as.ltraj(xy = gps_autumn_clean6_aux %>% dplyr::select(x = Longitude, y = Latitude),
                      date = gps_autumn_clean6_aux$timestamp, id = gps_autumn_clean6_aux$Object_ID_year,
                      infolocs = gps_autumn_clean6_aux[c("Collar_ID", "Object_ID", "year", "herding")],
                      proj4string = sp::CRS(paste0("+init=epsg:", crs_to_use)))
gps_ltraj

# function to separate bursts
foo <- function(herding) {
  return(herding == 1)
}

# separate into bursts
gps_autumn_clean7 <- cutltraj(gps_ltraj, "foo(herding)", nextr = F) %>% 
  ld()

# remove bursts with too few positions
(summary_bursts1 <- gps_autumn_clean7 %>%
    dplyr::group_by(year, id) %>%
    summarise(n = n(),
              n_bursts = length(unique(burst)),
              avg_n = n/n_bursts))

(n_tot_bursts <- sum(summary_bursts1$n_bursts))

(summary_bursts2 <- gps_autumn_clean7 %>% 
    dplyr::group_by(year, burst) %>% 
    summarise(start = min(date),
              end = max(date),
              duration = round(difftime(end, start, units = "days")),
              n = n())) %>% 
  print(n = 300)

# remove bursts with less than 7 days
min_days_burst <- 14
fix_rate <- 4
min_points_burst <- 14*24/fix_rate

burst_few_days <- summary_bursts2 %>% 
  dplyr::filter(as.numeric(duration) <= min_days_burst | n <= min_points_burst)

gps_autumn_clean8 <- gps_autumn_clean7 %>% 
  dplyr::filter(!(burst %in% burst_few_days$burst))

# rename bursts
gps_ltraj_rename <- gps_autumn_clean8 %>% 
  dl()

summ_cut <- summary(gps_ltraj_rename) %>% 
  group_by(id) %>% 
  summarize(rows = length(burst))
# function to rename
name_burst <- function(name, n_bursts) paste0(name, "_", 1:n_bursts)
# test
name_burst("maria", 2)
# new names
new_names <- unlist(mapply(name_burst, summ_cut$id, summ_cut$rows))
# rename and remove herding events
gps_autumn_clean9 <- gps_ltraj_rename %>% 
  ld() %>% 
  dplyr::mutate(burst = new_names[as.factor(burst)],
                id = as.character(id)) %>% 
  dplyr::as_tibble() %>% 
  dplyr::arrange(year, id, burst) %>% 
  dplyr::select(-herding)

# summarize final number of animals
(summary1 <- gps_autumn_clean9 %>% 
    dplyr::group_by(year) %>% 
    summarise(n = n(),
              n_inds = length(unique(id)),
              avg_n = round(n/n_inds)))

(n_tot <- sum(summary1$n_inds))

(summary2 <- gps_autumn_clean9 %>% 
    dplyr::group_by(year, id) %>% 
    summarise(start = min(date),
              end = max(date),
              duration_days = round(difftime(end, start, units = "days")),
              n = n())) %>% 
  print(n = 300)

summary3 <- summary2 %>% 
  group_by(year) %>% 
  summarise(mean_duration = mean(duration_days),
            avg_n = mean(n))

summary1 <- summary1 %>% 
  dplyr::left_join(summary3[,1:2], by = "year")

# write summary tables
readr::write_csv(summary1, file = "output/RG_mala/summary_n_per_season_autumn.csv")
readr::write_csv(summary2, file = "output/RG_mala/summary_n_per_individual_autumn.csv")

# ##
# Clean spikes
#
# source("code/Outlier_Detection_Bjorneraas_yann.R") # adapted from the using adehabitatLT instead of adehabitat package
# 
# #!!!!!!!!!!!!!!!!!!!!!!!!!
# # did not work
# gps_spikes <- gps_autumn_clean9 %>%
#   dplyr::group_by(id) %>%
#   do(GPS.screening.wrp(id=.$id, burst = .$burst, x=.$x, y=.$y, 
#                        date=.$date, medcrit=100000, meancrit=20000, 
#                        spikesp=2500, spikecos=(-0.97))) %>%
#   ungroup()
# 
# # spikes and errors
# which_spikes <- which(gps_spikes$R1error == TRUE | gps_spikes$R2error == TRUE)
# 
# gps_clean10 <- gps_clean9[-which_spikes,] %>% 
#   dplyr::select(Collar_ID, Object_ID, id, burst, winter, x, y, timestamp = date,
#                 dx:rel.angle)

# final organization
gps_calving_clean10 <- gps_calving_clean9 %>% 
  dplyr::select(Collar_ID, Object_ID, id, burst, year, x, y, timestamp = date,
                dx:rel.angle)

gps_summer_clean10 <- gps_summer_clean9 %>% 
  dplyr::select(Collar_ID, Object_ID, id, burst, year, x, y, timestamp = date,
                dx:rel.angle)

gps_autumn_clean10 <- gps_autumn_clean9 %>% 
  dplyr::select(Collar_ID, Object_ID, id, burst, year, x, y, timestamp = date,
                dx:rel.angle)

#------ save_outputs

#' # 5 Save trajectories to visualize outside R

# folder for cleaned data
folder_clean <- "output/RG_mala/clean_data_step2/"
if(!dir.exists(folder_clean)) dir.create(folder_clean)

#---
# save data as RData and rds

# calving
save(gps_calving_clean10, file = paste0(folder_clean, "data_cleaned_calving_RG.RData"))
saveRDS(gps_calving_clean10, file = paste0(folder_clean, "data_cleaned_calving_RG.rds"))
readr::write_csv(gps_calving_clean10, path = paste0(folder_clean, "data_cleaned_calving_RG.csv"))

# summer
save(gps_summer_clean10, file = paste0(folder_clean, "data_cleaned_summer_RG.RData"))
saveRDS(gps_summer_clean10, file = paste0(folder_clean, "data_cleaned_summer_RG.rds"))
readr::write_csv(gps_summer_clean10, path = paste0(folder_clean, "data_cleaned_summer_RG.csv"))

# autumn
save(gps_autumn_clean10, file = paste0(folder_clean, "data_cleaned_autumn_RG.RData"))
saveRDS(gps_autumn_clean10, file = paste0(folder_clean, "data_cleaned_autumn_RG.rds"))
readr::write_csv(gps_autumn_clean10, path = paste0(folder_clean, "data_cleaned_autumn_RG.csv"))

#---
# transform into geopackage and shapefiles to export

#---
# simplify calving
gps_calving_clean10_vect <- gps_calving_clean10 %>% 
  sf::st_as_sf(coords = c("x", "y"), remove = FALSE, crs = crs_to_use) %>% 
  dplyr::select(Collar_ID:timestamp, geometry) %>% 
  dplyr::mutate_at(c(1:(ncol(.)-1)), as.character)
gps_calving_clean10_vect

# simplify summer
gps_summer_clean10_vect <- gps_summer_clean10 %>% 
  sf::st_as_sf(coords = c("x", "y"), remove = FALSE, crs = crs_to_use) %>% 
  dplyr::select(Collar_ID:timestamp, geometry) %>% 
  dplyr::mutate_at(c(1:(ncol(.)-1)), as.character)
gps_calving_clean10_vect

# simplify calving
gps_autumn_clean10_vect <- gps_autumn_clean10 %>% 
  sf::st_as_sf(coords = c("x", "y"), remove = FALSE, crs = crs_to_use) %>% 
  dplyr::select(Collar_ID:timestamp, geometry) %>% 
  dplyr::mutate_at(c(1:(ncol(.)-1)), as.character)
gps_autumn_clean10_vect

#----------
# folder for vectors
folder <- paste0(folder_clean, "vector/")
if(!dir.exists(folder)) dir.create(folder)

#----------
# gpkg calving
gps_calving_clean10_vect %>% 
  sf::st_write(dsn = paste0(folder, "data_cleaned_calving_RG.gpkg"), 
               delete_dsn = TRUE)

# gpkg summer
gps_summer_clean10_vect %>% 
  sf::st_write(dsn = paste0(folder, "data_cleaned_summer_RG.gpkg"), 
               delete_dsn = TRUE)

# gpkg autumn
gps_autumn_clean10_vect %>% 
  sf::st_write(dsn = paste0(folder, "data_cleaned_autumn_RG.gpkg"), 
               delete_dsn = TRUE)

#----------
# shp - Object_ID_year - calving
folder_shp_calv <- paste0(folder, "shp_calving/")
if(!dir.exists(folder_shp_calv)) dir.create(folder_shp_calv)

# split maps into individual tracks for each id - calving
gps_calving_clean10_vect_split <- gps_calving_clean10_vect %>% 
  split(paste0(.$year, "_", str_sub(.$id, 1, -6)))

# export each - calving
lapply(names(gps_calving_clean10_vect_split), function(x)
  sf::st_write(gps_calving_clean10_vect_split[[x]], 
               dsn = paste0(folder_shp_calv, x, ".shp"), 
               delete_dsn = TRUE))

#---
# shp - Object_ID_year - summer
folder_shp_summ <- paste0(folder, "shp_summer/")
if(!dir.exists(folder_shp_summ)) dir.create(folder_shp_summ)

# split maps into individual tracks for each id - summer
gps_summer_clean10_vect_split <- gps_summer_clean10_vect %>% 
  split(paste0(.$year, "_", str_sub(.$id, 1, -6)))

# export each - summer
lapply(names(gps_summer_clean10_vect_split), function(x)
  sf::st_write(gps_summer_clean10_vect_split[[x]], 
               dsn = paste0(folder_shp_summ, x, ".shp"), 
               delete_dsn = TRUE))

#---
# shp - Object_ID_year - autumn
folder_shp_autumn <- paste0(folder, "shp_autumn/")
if(!dir.exists(folder_shp_autumn)) dir.create(folder_shp_autumn)

# split maps into individual tracks for each id - autumn
gps_autumn_clean10_vect_split <- gps_autumn_clean10_vect %>% 
  split(paste0(.$year, "_", str_sub(.$id, 1, -6)))

# export each - autumn
lapply(names(gps_autumn_clean10_vect_split), function(x)
  sf::st_write(gps_autumn_clean10_vect_split[[x]], 
               dsn = paste0(folder_shp_autumn, x, ".shp"), 
               delete_dsn = TRUE))

#----------
# shp - burst - calving
folder_shp_burst_calving <- paste0(folder, "shp_burst_calving/")
if(!dir.exists(folder_shp_burst_calving)) dir.create(folder_shp_burst_calving)

# split maps into individual tracks for each id
gps_calving_clean10_vect_split_burst <- gps_calving_clean10_vect %>% 
  split(paste0(.$year, "_", .$burst))

# export each
lapply(names(gps_calving_clean10_vect_split_burst), function(x)
  sf::st_write(gps_calving_clean10_vect_split_burst[[x]], 
               dsn = paste0(folder_shp_burst_calving, x, ".shp"), 
               delete_dsn = TRUE))

#---
# shp - burst - summer
folder_shp_burst_summer <- paste0(folder, "shp_burst_summer/")
if(!dir.exists(folder_shp_burst_summer)) dir.create(folder_shp_burst_summer)

# split maps into individual tracks for each id
gps_summer_clean10_vect_split_burst <- gps_summer_clean10_vect %>% 
  split(paste0(.$year, "_", .$burst))

# export each
lapply(names(gps_summer_clean10_vect_split_burst), function(x)
  sf::st_write(gps_summer_clean10_vect_split_burst[[x]], 
               dsn = paste0(folder_shp_burst_summer, x, ".shp"), 
               delete_dsn = TRUE))

#---
# shp - burst - autumn
folder_shp_burst_autumn <- paste0(folder, "shp_burst_autumn/")
if(!dir.exists(folder_shp_burst_autumn)) dir.create(folder_shp_burst_autumn)

# split maps into individual tracks for each id
gps_autumn_clean10_vect_split_burst <- gps_autumn_clean10_vect %>% 
  split(paste0(.$year, "_", .$burst))

# export each
lapply(names(gps_autumn_clean10_vect_split_burst), function(x)
  sf::st_write(gps_autumn_clean10_vect_split_burst[[x]], 
               dsn = paste0(folder_shp_burst_autumn, x, ".shp"), 
               delete_dsn = TRUE))
