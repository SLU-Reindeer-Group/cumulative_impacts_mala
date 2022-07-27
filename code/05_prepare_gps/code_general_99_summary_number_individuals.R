#' ---
#' title: 'Reindeer GPS data general statistics'
#' author: Bernardo Niebuhr
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

#---- load_gps_data

#' # 1 Load GPS data
#'

# connection to the main dataset
con <- dbConnect(odbc(),
                 Driver   = "SQL Server",
                 Server   = "alces.slu.se\\gps",
                 Database = "GPS_Ren")

# get Object_Info table from WRAM, only Collars from the herding district of interest
district.codes <- c("SJ", "SK", "RG") # this could be a vector of several districts also
codes.wrapped <- paste(district.codes, collapse = "|")

object.info <- dbReadTable(con, "Object_Info") %>%
  dplyr::as_tibble() %>%
  dplyr::filter(grepl(codes.wrapped, ADGroup))

# No information on dates the collar were put on or off the animals exist.

# get raw movement data from WRAM, only for this district
# do basic cleaning and remove gross duplicates
gps.raw <- dbReadTable(con, "GPS_Main") %>%
  dplyr::as_tibble() %>%
  # select only validated positions
  dplyr::filter(Valid_pos == 2) %>%  
  # select only reasonable positions, remove 0 and negative coordinates
  dplyr::filter(Locale_N > 0 & Locale_E > 0 & Longitude > 0 & Latitude > 0) %>%  
  # select columns of interest  creates a new column where the time 
  # stamp is rounded, to be able to...
  dplyr::select(GPS_main_ID, Collar_ID, GMT_date, LMT_date, 
                Longitude, Latitude, Locale_N, Locale_E, DOP, 
                Sats_used, SMS_ID, Valid_pos) %>% 
  dplyr::mutate(date_round = round_date(GMT_date, unit="1 minute")) %>% 
  # ...remove rough duplicateswhich does not have the exact same times
  dplyr::distinct(Collar_ID, Longitude, Latitude, date_round, .keep_all=TRUE) %>% 
  # remove this new column
  dplyr::select(-date_round) %>%
  # get only data for the collars used in the herding district of interest
  dplyr::filter(Collar_ID %in% object.info$Collar_ID) 

# get ObjectIDs for the herding district and filter data in time according to ObjectID
gps.raw.filter <- gps.raw %>%
  dplyr::right_join(
    object.info %>%
      dplyr::select(Collar_ID, Object_ID, PubName, Sex, GMT_C_On, GMT_C_Off),
    by = c("Collar_ID")) %>%
  dplyr::filter(GMT_date %>% data.table::between(GMT_C_On, GMT_C_Off)) %>%
  dplyr::arrange(Collar_ID, GMT_date)


# check if the PU Object_IDs are in Mittadalen
gps.sf <- gps.raw.filter %>%
  dplyr::filter(grepl("PU", Object_ID)) %>% 
  dplyr::select(Collar_ID, Object_ID, Longitude, Latitude, GMT_date) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# export to check
gps.sf %>%
  sf::st_write("data/SK_mittadalen/gps_points_checknewcollars.gpkg")
# they really seem to be Mittadalen data! From 2019-2020:

gps.sf %>%
  sf::st_drop_geometry() %>% 
  dplyr::group_by(Object_ID) %>%
  dplyr::summarize(GMT_C_On = min(GMT_date),
                   GMT_C_Off = max(GMT_date)) %>%
  dplyr::arrange(GMT_C_On)

#------
# table of number of collars

# temporal filter
temp.filter.snowfree <- c(lubridate::yday('2019-05-01'), lubridate::yday('2020-10-01')) # snow-free

gps.RG <- gps.raw.filter %>% 
  dplyr::filter(grepl("_RG_", Object_ID)) %>% 
  dplyr::select(Collar_ID, Object_ID, GMT_date, Longitude, Latitude) %>% 
  dplyr::filter(lubridate::yday(GMT_date) >= temp.filter.snowfree[1] &
                  lubridate::yday(GMT_date) <= temp.filter.snowfree[2]) %>% 
  dplyr::mutate(year = lubridate::year(GMT_date))
  
temp.filter.winter <- c(lubridate::yday('2019-09-01'), lubridate::yday('2020-04-01')) # winter
gps.SK.SJ <- gps.raw.filter %>% 
  dplyr::filter(!grepl("_RG_", Object_ID)) %>% 
  dplyr::select(Collar_ID, Object_ID, GMT_date, Longitude, Latitude) %>% 
  dplyr::filter(lubridate::yday(GMT_date) >= temp.filter.winter[1] |
                  lubridate::yday(GMT_date) <= temp.filter.winter[2]) %>% 
  dplyr::mutate(year = lubridate::year(GMT_date - lubridate::days(5*30)))

gps.temp.filter <- dplyr::bind_rows(gps.RG, gps.SK.SJ) %>% 
  dplyr::mutate(sameby = case_when(
    grepl("_RG_", Object_ID) ~ "Malå",
    grepl("_SJ_", Object_ID) ~ "Tåssåsen",
    TRUE ~ "Mittådalen"
  )) %>% 
  filter(year >= 2007 & year < 2020)

n.animals <- gps.temp.filter %>% 
  dplyr::group_by(sameby, year, Object_ID) %>% 
  summarise(
    start.date = min(GMT_date),
    end.date = max(GMT_date), 
    period = difftime(end.date, start.date, units = "days")) %>% 
  dplyr::filter(period > 30) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(sameby, year) %>% 
  summarise(n = length(unique(Object_ID)))

n.animals %>% print(n = 100)

(tot.animals <- n.animals %>% 
    group_by(sameby) %>% 
    summarize(tot = sum(n)))

# export table
n.animals %>% 
  readr::write_csv(path = "data/summary_table_n_individuals.csv")

  # # dplyr::group_by(season, construction) %>% 
  # # summarise(n_years = n(),
  #           total.n = sum(n),
  #           min.n = min(n),
  #           max.n = max(n),
  #           avg.n = mean(n))
