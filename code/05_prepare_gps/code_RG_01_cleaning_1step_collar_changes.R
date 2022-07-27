#' ---
#' title: 'Cleaning GPS reindeer data for Mala herding disctrict - step 1 - identifying herding events'
#' author: Bernardo Niebuhr, based on code by Erik Cronvall, Anna Skarin, Alessia Uboni
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
install.load::install_load("sf", "amt")

# parameters
# crs_to_use <- "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
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

# get raw movement data from WRAM, only for this district
# do basic cleaning and remove gross duplicates
gps_raw_wram <- dbReadTable(con, "GPS_Main") %>%
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
  dplyr::mutate(date_round = round_date(GMT_date, unit="3 minutes")) %>% 
  # ...remove rough duplicateswhich does not have the exact same times
  # we use 3 minutes here because we do not need that precision to look
  # for changes in collars
  dplyr::distinct(Collar_ID, Longitude, Latitude, date_round, .keep_all=TRUE) %>% 
  # remove this new column
  dplyr::select(-date_round) %>%
  # get only data for the collars used in the herding district of interest
  dplyr::filter(Collar_ID %in% object_info$Collar_ID) 

# which are the collars
unique(object_info$Collar_ID) %>% sort
unique(gps_raw_wram$Collar_ID) %>% sort

# These collars are not in the GPS main
(missing_in_wram <- dplyr::setdiff(object_info$Collar_ID, unique(gps_raw_wram$Collar_ID)) %>%
  sort)
# These are the ones we should add to WRAM - 55 collars

# check duration of each collar
gps_wram_summmary <- gps_raw_wram %>%
  dplyr::group_by(Collar_ID) %>%
  dplyr::summarize(start_date_wram = min(GMT_date),
                   end_date_wram = max(GMT_date),
                   n = n()) %>%
  dplyr::arrange(Collar_ID, start_date_wram)

# get old data from Followit that is not in WRAM 
# (but in Anna Skarin's local database)

#---
# old data for only a few collars
# for some reason this data are in local Swedish time (CET/CEST)
# data saved in the object (data.frame) called "data1"
# load("data/RG_mala/missing_mala.RData")
# data1
# nrow(data1)
# unique(data1$collar_id)

# backup
# data1 %>% write.csv(file = "data/missing_data_Mala.csv", row.names = F)

#---
# old data for all collars, but only date (and not date-time)
# mala_old_raw <- readr::read_csv("data/RG_mala/old_mala_raw.csv") %>% 
#   dplyr::select(Collar_ID = 1, PubName = collar_id, Latitude = latitude, Longitude = longitude,
#                 GMT_date = date_time, DOP = dop, Sats_used = position_quality) %>% 
#   dplyr::mutate(Collar_ID = paste0("FIT_", gsub("([0-9]+).*$", "\\1", PubName)),
#                 ADgroup = "RG")
# nrow(mala_old_raw)
# unique(mala_old_raw$Collar_ID)
# 
# gps_old_summmary <- mala_old_raw %>%
#   dplyr::group_by(Collar_ID) %>%
#   dplyr::summarize(start_date_old = min(GMT_date),
#                    end_date_old = max(GMT_date)) %>%
#   dplyr::arrange(Collar_ID, start_date_old)
# View(gps_old_summmary)

#---
# old data for most (not all) collars
# mala_old_raw_missing <- readr::read_csv("data/RG_mala/old_mala_raw_objectid.csv") %>% 
#   dplyr::select(Collar_ID = 1, PubName = collar_id, Latitude = latitude, Longitude = longitude,
#                 GMT_date = date_time, ADgroup = sameby, DOP = dop, Sats_used = position_quality) %>% 
#   dplyr::mutate(Collar_ID = paste0("FIT_", gsub("([0-9]+).*$", "\\1", PubName)))
# 
# nrow(mala_old_raw_missing)
# unique(mala_old_raw_missing$Collar_ID)
# str(mala_old_raw_missing)
# 
# diff_old_old2 <- dplyr::setdiff(unique(mala_old_raw$Collar_ID), unique(mala_old_raw_missing$Collar_ID)) %>%
#   sort
# dplyr::intersect(diff_old_old2, missing_in_wram)
# 
# gps_old2_summmary <- mala_old_raw_missing %>%
#   dplyr::group_by(Collar_ID) %>%
#   dplyr::summarize(start_date_old2 = min(GMT_date),
#                    end_date_old2 = max(GMT_date),
#                    n2 = n()) %>%
#   dplyr::arrange(Collar_ID, start_date_old2)

#---
# old data for natrix collars, directly exported by FIT in Nov 2020
gps_raw_natrix <- read.csv2("data/RG_mala/FIT_export_natrix_Mala_2020_11_d11.csv", header = T,
                            stringsAsFactors = F) %>% 
  dplyr::as_tibble() %>% 
  dplyr::rename(serial_n = Serial.no, PubName = Name, GMT_date = Event.Date,
                Latitude = Lat, Longitude = Lng, DOP = HDOP, Sats_used = Sats, Height = Alt) %>% 
  dplyr::mutate(Collar_ID = #paste0("FIT_", as.character(Collar_ID)),
                  PubName %>% 
                  readr::parse_number() %>% 
                  stringr::str_pad(width = 5, pad = "0") %>% 
                  paste0("FIT_", .),
                GMT_date = lubridate::ymd_hms(GMT_date, tz = "UTC"),
                Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude),
                Locale_N = NA,
                Locale_E = NA,
                LMT_date = NA,
                SMS_ID = NA,
                Valid_pos = NA,
                GPS_main_ID = NA) %>% 
  dplyr::select_at(colnames(gps_raw_wram)) %>% 
  dplyr::arrange(Collar_ID) %>% 
  dplyr::mutate(date_round = round_date(GMT_date, unit="3 minutes")) %>% 
  # ...remove rough duplicateswhich does not have the exact same times
  # we use 3 minutes here because we do not need that precision to look
  # for changes in collars
  dplyr::distinct(Collar_ID, Longitude, Latitude, date_round, .keep_all=TRUE) %>% 
  # remove this new column
  dplyr::select(-date_round)
gps_raw_natrix

# check duration of each collar
gps_natrix_summary <- gps_raw_natrix %>% 
  dplyr::group_by(Collar_ID) %>%
  dplyr::summarize(start_date = min(GMT_date),
                   end_date = max(GMT_date),
                   n2 = n()) %>%
  dplyr::arrange(Collar_ID, start_date)

# merge summaries from wram and natrix to compare
gps_summary <- gps_wram_summmary %>% 
  dplyr::full_join(gps_natrix_summary, by = "Collar_ID") %>% 
  dplyr::arrange(Collar_ID)
View(gps_summary)

# save this summary
folder_before <- "output/RG_mala/before_cleaning/"
if(!dir.exists(folder_before)) dir.create(folder_before)

gps_summary %>% 
  readr::write_csv(paste0(folder_before, "summary_dates_collars_WRAM_natrix.csv"))

# check which ones are missing and which ones to replace
# if n == 0, it is missing
# if n2 > n, replace
# if n2 < n, we keep what we have in wram
collars_natrix_more_data <- gps_summary %>% 
  dplyr::mutate(n = ifelse(is.na(n), 0, n),
                n2 = ifelse(is.na(n2), 0, n2)) %>% 
  dplyr::mutate(missing_wram = ifelse(n == 0, 1, 0),
                replace_wram = ifelse(n2 > n & missing_wram == 0, 1, 0))
View(collars_natrix_more_data)

# plot a few individuals just to check the coordinates are right
ind <- "00091"
gps_raw_natrix %>% 
  dplyr::filter(grepl(ind, Collar_ID)) %>% 
  ggplot() +
  geom_point(aes(Longitude, Latitude, color = "natrix")) + 
  geom_point(aes(Longitude, Latitude, color = "wram"), 
             data = gps_raw_wram %>%
               dplyr::filter(grepl(ind, Collar_ID)))

gps_raw_wram %>% 
  dplyr::filter(grepl(ind, Collar_ID)) %>% 
  ggplot() +
  geom_point(aes(Longitude, Latitude, color = "wram")) + 
  geom_point(aes(Longitude, Latitude, color = "natrix"), 
             data = gps_raw_natrix %>%
               dplyr::filter(grepl(ind, Collar_ID)))

# collars missing in WRAM, to add
collars_to_add <- collars_natrix_more_data %>% 
  dplyr::filter(missing_wram == 1)
sum(collars_to_add$n2)
View(collars_to_add)

data_natrix_to_add <- gps_raw_natrix %>% 
  dplyr::filter(Collar_ID %in% collars_to_add$Collar_ID)

# collars with less data in WRAM than in the natrix db file, to be replaces
collars_replace <- collars_natrix_more_data %>% 
  dplyr::filter(replace_wram == 1)
View(collars_replace)
sum(collars_replace$n2 - collars_replace$n)

data_natrix_replace <- gps_raw_natrix %>% 
  dplyr::filter(Collar_ID %in% collars_replace$Collar_ID)

# replace the natrix collars from WRAM by the ones exported later by Followit in November 2020,
# replacing those with more data and adding those missing
gps_raw_wram_natrix <- gps_raw_wram %>% 
  # move the natrix collars with spikes
  dplyr::filter(!(Collar_ID %in% collars_replace$Collar_ID)) %>% 
  # replace collars from FIT
  dplyr::bind_rows(data_natrix_replace) %>% 
  # add missing collars exported from FIT
  dplyr::bind_rows(data_natrix_to_add) %>% 
  # arrange
  dplyr::arrange(Collar_ID)

gps_raw_wram_natrix
tail(gps_raw_wram_natrix)
# ok, they have the same format!

# check collars still missing in GPS_Main
(miss_FIT <- dplyr::setdiff(object_info$Collar_ID, unique(gps_raw_wram_natrix$Collar_ID)) %>%
  sort) # 0 collars

(miss_object_info <- dplyr::setdiff(unique(gps_raw_wram_natrix$Collar_ID), object_info$Collar_ID) %>%
    sort) # 3 collars not in object_info table
# they may be in WRAM, but we did not check
#### check that later!

# change object_info table for starting in 2008-01-01 and ending 2011-12-31
object_info_expanded <- object_info %>% 
  dplyr::mutate(GMT_C_On = if_else(GMT_C_On == lubridate::ymd_hms("2008-01-01 00:00:00"), lubridate::ymd_hms("2007-01-01 00:00:00"), 
                                   GMT_C_On),
                GMT_C_Off = if_else(GMT_C_Off == lubridate::ymd_hms("2011-12-31 23:59:0"), lubridate::ymd_hms("2012-12-31 23:59:0"), 
                                   GMT_C_On))
# DID NOT WORK
# BUT WE HAVE TO CHECK THAT LATER

# get ObjectIDs for Mala and filter data in time according to ObjectID
gps_raw_wram_natrix_obj <- gps_raw_wram_natrix %>% 
  dplyr::left_join(
    object_info %>% 
      dplyr::select(Collar_ID, Object_ID, PubName, Sex, GMT_C_On, GMT_C_Off),
    by = c("Collar_ID")) %>%
  dplyr::filter(GMT_date %>% data.table::between(GMT_C_On, GMT_C_Off)) %>% 
  dplyr::arrange(Collar_ID, GMT_date) %>% 
  # fill Object_ID for the three collars missing in the object_info table
  dplyr::mutate(Object_ID = ifelse(is.na(Object_ID), 
                                   stringr::str_replace(Collar_ID, "FIT_", "rt_RG_"), 
                                   Object_ID))

# check data
gps_raw_wram_natrix %>% 
  dplyr::group_by(Collar_ID) %>%
  dplyr::summarize(start_date = min(GMT_date),
                   end_date = max(GMT_date),
                   n2 = n()) %>%
  dplyr::arrange(Collar_ID, start_date)

gps_raw_wram_natrix_obj %>% 
  dplyr::group_by(Collar_ID) %>%
  dplyr::summarize(start_date = min(GMT_date),
                   end_date = max(GMT_date),
                   n2 = n()) %>%
  dplyr::arrange(Collar_ID, start_date) %>% 
  View()

##############
# warning
# this command reduced htre dataset from 1.2 M rows to 600k rows. Maybe there is a lot of data
# which is not in the Object_Info table?!?!

gps_raw_wram_natrix_obj %>% 
  dplyr::filter(is.na(Object_ID)) %>% 
  pull(Collar_ID) %>% unique()

# new data from Tracker GPS company
gps_tracker_jan2020 <- readRDS("data/RG_mala/gpx_data_mala_20200117.rds") %>% 
  dplyr::as_tibble() %>% 
  dplyr::rename(PubName = name, GMT_date = date, Longitude = lat, Latitude = long) %>% 
  dplyr::mutate(number = stringr::str_sub(PubName, end = 2),
    Collar_ID = paste0("TRK_000", number),
    Object_ID = paste0("rt_RG_", Collar_ID)) %>% 
  dplyr::select(-geometry, -number) %>% 
  dplyr::mutate(date_round = round_date(GMT_date, unit="3 minutes")) %>% 
  # ...remove rough duplicateswhich does not have the exact same times
  # we use 3 minutes here because we do not need that precision to look
  # for changes in collars
  dplyr::distinct(Collar_ID, Longitude, Latitude, date_round, .keep_all=TRUE) %>% 
  # remove this new column
  dplyr::select(-date_round)

gpx_tracker_files <- list.files("data/RG_mala/not_used/", pattern = "gpx", full.names = T)
gpx_tracker <- list()
for(i in gpx_tracker_files) {
  Tracker_oct_2020 <- plotKML::readGPX(i)$tracks[[1]][[1]]
  name_tracker <- stringr::str_split(i, pattern = "/") %>% 
    last() %>% last() %>% 
    str_replace(".gpx", "")
  Tracker_oct_2020$PubName <- name_tracker
  
  gpx_tracker[[i]] <- Tracker_oct_2020
}
gpx_tracker

gps_tracker_oct2020 <- plyr::ldply(gpx_tracker, data.frame) %>% 
  dplyr::select(-.id) %>% 
  dplyr::as_tibble() %>% 
  dplyr::rename(Latitude = lat, Longitude = lon) %>% 
  dplyr::mutate(GMT_date = lubridate::ymd_hms(time)) %>% 
  dplyr::mutate(date_round = round_date(GMT_date, unit="3 minutes")) %>% 
  # ...remove rough duplicateswhich does not have the exact same times
  # we use 3 minutes here because we do not need that precision to look
  # for changes in collars
  dplyr::distinct(PubName, Longitude, Latitude, date_round, .keep_all=TRUE) %>% 
  # remove this new column
  dplyr::select(-date_round)
gps_tracker_oct2020$PubName %>% unique()

summ_oct_2020 <- gps_tracker_oct2020 %>% 
  dplyr::group_by(PubName) %>%
  dplyr::summarize(start_date = min(GMT_date),
                   end_date = max(GMT_date),
                   n2 = n()) %>%
  dplyr::arrange(PubName, start_date)
View(summ_oct_2020)

summ_jan_2020 <- gps_Tracker_jan2020 %>% 
  dplyr::group_by(PubName) %>%
  dplyr::summarize(start_date = min(GMT_date),
                   end_date = max(GMT_date),
                   n2 = n()) %>%
  dplyr::arrange(PubName, start_date)
View(summ_jan_2020)

# check collars
id <- "02"
gps_tracker_oct2020 %>% 
  dplyr::filter(grepl(id, PubName)) %>% 
  ggplot() +
  geom_point(aes(Longitude, Latitude, color = "Oct2020")) + 
  geom_point(aes(Longitude, Latitude, color = "Jan2020"), 
             data = gps_tracker_jan2020 %>%
               dplyr::filter(grepl("02 ", PubName)))

id <- "05"
gps_tracker_oct2020 %>% filter(grepl(id, PubName)) %>% mutate(GMT_date = GMT_date - lubridate::hours(2)) %>% 
  dplyr::bind_rows(gps_tracker_jan2020 %>% filter(grepl(id, PubName))) %>% 
  dplyr::arrange(GMT_date) %>% 
  # dplyr::distinct(GMT_date) %>%
  dplyr::filter(month(GMT_date) == 1)

test <- gps_tracker_oct2020 %>% filter(grepl(id, PubName)) %>% mutate(GMT_date = GMT_date - lubridate::hours(2)) %>% 
  dplyr::bind_rows(gps_tracker_jan2020 %>% filter(grepl(id, PubName))) %>% 
  dplyr::arrange(GMT_date)
sum(duplicated(test$Latitude))

test <- gps_tracker_oct2020 %>% filter(grepl("u02", PubName)) %>% mutate(GMT_date = GMT_date - lubridate::hours(2)) %>% 
  dplyr::bind_rows(gps_tracker_jan2020 %>% filter(grepl("02", PubName))) %>% 
  dplyr::arrange(GMT_date)
sum(duplicated(test$Latitude))
sum(duplicated(test$Longitude))
sum(duplicated(test$GMT_date))
test[duplicated(test$Latitude),]

# I am not sure about the collar Malatr02 and Malboazu02, and Malatr03 - which one correspond to collar 02 exported in January
# so I remove both and we check that later on
not_use_now <- c("Malatr02", "Malboazu02", "Malatr03") 

# for now I'll consider only the new data from Tracker
# The data from October seems to be in local time (GMT+2), so we remove that.
gps_tracker_jan_oct2020 <- gps_tracker_oct2020 %>% 
  dplyr::filter(!(PubName %in% not_use_now)) %>% 
  dplyr::mutate(Collar_ID = PubName %>% 
                  readr::parse_number() %>% 
                  stringr::str_pad(width = 5, pad = "0") %>% 
                  paste0("TRK_", .),
                Object_ID = paste0("rt_RG_", Collar_ID),
                GMT_date = GMT_date - lubridate::hours(2),
                Sats_used = as.numeric(sat)) %>% 
  dplyr::bind_rows(gps_tracker_jan2020) %>% 
  dplyr::arrange(Collar_ID, Object_ID, PubName, GMT_date) %>% 
  dplyr::select(Collar_ID, Object_ID, PubName, Longitude, Latitude, GMT_date, Sats_used)
  

# check
gps_tracker_jan_oct2020 %>% 
  dplyr::group_by(Collar_ID, PubName) %>% 
  summarize(n = n()) %>% 
  print(n = 100)

duplicated(gps_tracker_jan_oct2020[c("Collar_ID", "GMT_date")]) %>% 
  sum()
# this is different if we use Lon or Lat, there are different numbers of duplicates
# check this later
# for now we remove what is repeated in time

gps_tracker_jan_oct2020_clean <- gps_tracker_jan_oct2020 %>% 
  dplyr::distinct(Collar_ID, GMT_date, .keep_all = TRUE)

# create standardized columns for gps_tracker_oct2020

# check Collar_IDs
(miss_trk <- gps_tracker_jan_oct2020_clean$Collar_ID %>% unique %>% sort) # 41 collars

# new data from Telespor GPS company
gps_raw_telespor <- read.csv2("data/RG_mala/telespor-2_mala.csv") %>% 
  dplyr::as_tibble() %>% 
  dplyr::rename(GMT_date = 2, position_OK = 8) %>% 
  dplyr::mutate(Collar_ID = paste0("TSP_", Telespornr),
                Object_ID = paste0("rt_RG_", Collar_ID),
                PubName = as.character(Individnr),
                GMT_date = lubridate::ymd_hms(GMT_date),
                Longitude = as.numeric(as.character(Lengdegrad)), 
                Latitude = as.numeric(as.character(Breddegrad))) %>% 
  dplyr::filter(position_OK == 1) %>% 
  dplyr::select(Collar_ID, Object_ID, PubName, GMT_date, 
                Longitude, Latitude)

# check Collar_IDs
(miss_tsp <- gps_raw_telespor$Collar_ID %>% unique %>% sort) # 45 collars

# list all collars missing in WRAM and save then in a file
len <- c(length(miss_object_info), 
         length(miss_trk), length(miss_tsp))
tibble::tibble(Collar_ID = c(miss_object_info, miss_trk, miss_tsp),
               company = rep(c("Followit", "Tracker", "Telespor"),
                             times = c(len[1], len[2], len[3])),
               where = rep(c("Natrix_missing_object_info_tab", "TRK", "TSP"),
                           times = c(len[1], len[2], len[3]))) %>% 
  readr::write_csv(path = paste0(folder_before, "missing_collars_WRAM_Mala_20201124.csv"))

#---
#' If the On and Off deployment dates are too broad and imprecise in the Object_Info
#' (e.g. from 2000 to 2050), we can use as the first approximation the earliest and
#' latest dates for each collar in WRAM, to initialize the Object_Info On/Off date 
#' columns for these individuals. This is not the case here, but it is for the
#' collars from Tracker and Telespor.
#' 
#' Then, we'll initialize and create an Object_Info for these collars.

not_in_object_info <- gps_raw_wram_natrix_obj %>% 
  dplyr::filter(Collar_ID %in% miss_object_info) %>% 
  dplyr::select(Collar_ID, Object_ID, GMT_date, PubName)

# all collars
all_collars_new <- dplyr::bind_rows(not_in_object_info, gps_tracker_jan_oct2020_clean, gps_raw_telespor) %>% 
  dplyr::arrange(Collar_ID)
                    
all_collars <- all_collars_new$Collar_ID %>% 
  unique

all_objectids <- all_collars_new$Object_ID %>% 
  unique

# initialize Object_Info table
object_info_new <- object_info %>% 
  dplyr::filter(Collar_ID == "0") %>% # getting only the headers
  bind_rows(as_tibble(matrix(data = NA, nrow = length(all_collars),
                             ncol = ncol(object_info), 
                             dimnames = list(NULL, colnames(object_info)))))

object_info_new$Collar_ID <- all_collars
object_info_new$Object_ID <- all_objectids
object_info_new$PubName <- all_collars_new$PubName[!duplicated(sort(all_collars_new$Collar_ID))]
object_info_new$Adopted <- object_info$Info <- ""

# dates according to the data
# just a first look at the data
gps_raw_summ_new <- all_collars_new %>%
  dplyr::group_by(Collar_ID) %>%
  dplyr::summarize(GMT_C_On = min(GMT_date),
                   GMT_C_Off = max(GMT_date)) %>%
  dplyr::arrange(Collar_ID, GMT_C_On)

gps_raw_summ_new %>%
  print(n = 1000)

object_info_new$GMT_C_On <- gps_raw_summ_new$GMT_C_On
object_info_new$GMT_C_Off <- gps_raw_summ_new$GMT_C_Off

object_info_new$ADGroup <- "RG"

object_info_old_new <- object_info %>% 
  dplyr::bind_rows(object_info_new) %>% 
  dplyr::arrange(Collar_ID, Object_ID)

# save it outside R
object_info_old_new %>% 
  write.csv(file = paste0(folder_before, "object_info_allinds_beforeclean_202011.csv", row.names = F))
#readr::write_csv(path = "data/RG_mala/object_info_newinds_beforeclean_20200504.csv")

# put all the data together
gps_raw_filter <- dplyr::bind_rows(gps_raw_wram_natrix_obj, gps_tracker_jan_oct2020_clean, gps_raw_telespor)

# backup this data

# original data from WRAM
# gps_raw_wram %>%
#   readr::write_csv(path = paste0(folder_before, "gps_raw_wram_2020_11_d20.csv"))
# save(gps_raw_wram, file = paste0(folder_before, "gps_raw_wram_2020_11_d20.RData"))
# saveRDS(gps_raw_wram, file = paste0(folder_before, "gps_raw_wram_2020_11_d20.rds"))

# natrix collars for mittadalen, exported by FIT in 2020-11-16, 
# with column names standardized with WRAM
# gps_raw_natrix   %>%
#   readr::write_csv(path = paste0(folder_before, "gps_raw_natrix_2020_11_d16.csv"))
# save(gps_raw_natrix, file = paste0(folder_before, "gps_raw_natrix_2020_11_d16.RData"))
# saveRDS(gps_raw_natrix, file = paste0(folder_before, "gps_raw_natrix_2020_11_d16.rds"))

# raw data from WRAM plus corrected natrix collars
# gps_raw_wram_natrix_obj %>%
#   readr::write_csv(path = paste0(folder_before, "gps_raw_wram_natrix_corrected_2020_11_d20.csv"))
# save(gps_raw_wram_natrix_obj, file = paste0(folder_before, "gps_raw_wram_natrix_corrected_2020_11_d20.RData"))
# saveRDS(gps_raw_wram_natrix_obj, file = paste0(folder_before, "gps_raw_wram_natrix_corrected_2020_11_d20.rds"))

# raw data from Tracker, missing three collars Malatr02, Malatr03, Malboazu02
# gps_tracker_jan_oct2020 %>%
#   readr::write_csv(path = paste0(folder_before, "gps_tracker_jan_oct2020_2020_11_d20.csv"))
# save(gps_tracker_jan_oct2020, file = paste0(folder_before, "gps_tracker_jan_oct2020_2020_11_d20.RData"))
# saveRDS(gps_tracker_jan_oct2020, file = paste0(folder_before, "gps_tracker_jan_oct2020_2020_11_d20.rds"))

# raw data from Telesport, with column standardized with WRAM
# gps_raw_telespor %>%
#   readr::write_csv(path = paste0(folder_before, "gps_raw_telespor_2020_01_d22.csv"))
# save(gps_raw_telespor, file = paste0(folder_before, "gps_raw_telespor_2020_01_d22.RData"))
# saveRDS(gps_raw_telespor, file = paste0(folder_before, "gps_raw_telespor_2020_01_d22.rds"))

#---- load_env_data

#' # 2 Load ancillary spatial data
#'
#' Now we load spatial data needed for anotating data: the limits of the herding districts, corrals,
#' houses, buildings, and urban areas. We also create buffers around them to be able to use them as spatial
#' filters while accounting for GPS errors.

# folder
spatial_folder <- "spatial_data/"

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

# corrals_after2009 <- grep("corrals_Mala", corrals_files, value = T) %>%
#   grep(pattern = "Anlaggning", value = T) %>% 
#   sf::st_read() %>% 
#   sf::st_transform(crs = "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")

# beteshage
beteshage <- grep("corrals_IRENMARK", spatial_files, value = T) %>%
  sf::st_read() %>% 
  dplyr::filter(SAMEBY1 == "Mala", BESKRIVNIN == "Beteshage") %>% 
  sf::st_transform(crs = crs_to_use)

# houses
houses <- grep("houses", spatial_files, value = T) %>%
  sf::st_read() %>% 
  sf::st_transform(crs = crs_to_use)

# buildings
buildings <- grep("buildings", spatial_files, value = T) %>%
  sf::st_read() %>% 
  sf::st_transform(crs = crs_to_use)

# urban
urban <- grep("urban", spatial_files, value = T) %>%
  sf::st_read() %>% 
  sf::st_transform(crs = crs_to_use)

#---
# buffer for herding district (30 km), corrals (300 m), houses, and buildings (30 m)
# for corrals this may be realted to possible errors in mapping
# for houses and building, this is to account for GPS error

district_names <- c("Mala")
district_buffer <- sameby_limits %>%
  dplyr::filter(NAMN %in% district_names) %>%
  sf::st_buffer(dist = 30000)

corrals_buffer <- corrals %>%
  sf::st_buffer(dist = 300)

# corrals_buffer_after2009 <- corrals_after2009 %>%
#   sf::st_buffer(dist = 300)

beteshage_buffer <- beteshage %>%
  sf::st_buffer(dist = 300)

houses_buffer <- houses %>%
  sf::st_buffer(dist = 50)

buildings_buffer <- buildings %>%
  sf::st_buffer(dist = 50)

# read auxiliry files
details_snow_free <- readr::read_csv("data/RG_mala/details_snow_free_Mala_before_cleaning.csv") %>% 
  dplyr::select(year, date_start_calving, date_end_autumn)

#------ remove_bad_data

#' # 3 Remove bad quality data and perform spatial and temporal filters
#'
#' Now we perform the first steps of cleaning. First we perform a temporal filter, keeping only the data for
#' the season of interest. We remove "bad quality" (DOP > 5 or n. of sattelites for the position < 4) and missing data.
#' Then, perform the first spatial filter: we keep only the data within a given buffer (here, 30 km) from the herding district borders,
#' to remove rough outliers.

# dates for the temporal filter, in case we want general and equivalent dates for all years
# temp_filter <- c(lubridate::yday('2019-04-20'), lubridate::yday('2019-11-30')) # winter

# years we are going to work with
years_to_study <- c(2007:2011, 2015:2020)
# no individuals during the construction period, 2012-2014

# remove bad quality and duplicates, keep points within Mala, keep only snow-free season
gps_clean1 <- gps_raw_filter %>%
  # remove bad quality data
  dplyr::filter((DOP < 5 | is.na(DOP)) & (Sats_used >= 4 | is.na(Sats_used))) %>%
  # remove NA
  dplyr::filter(!is.na(Longitude), !is.na(Latitude)) %>% 
  # remove duplicates
  dplyr::distinct(Object_ID, GMT_date, .keep_all = T) %>% 
  # spatial filter
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  sf::st_transform(crs = crs_to_use) %>%
  sf::st_join(district_buffer, join = st_intersects) %>%
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
  dplyr::arrange(year, Collar_ID) %>% 
  # temporal filter, the logical & or | may change depending on the season
  # dplyr::filter(lubridate::yday(GMT_date) >= temp_filter[1] &
  #                 lubridate::yday(GMT_date) <= temp_filter[2]) %>% 
  # include years to study
  dplyr::filter(year %in% years_to_study) %>% 
  dplyr::select(Collar_ID, Object_ID, Object_ID_year, year, Longitude, Latitude, timestamp)

# dates for the temporal filter, in case we want different dates per year
# subset data of dates
details_snow_free_to_study <- details_snow_free %>% 
  dplyr::filter(year %in% years_to_study)

# dates for the temporal filter for seasons - here we need a vector for each data point
temp_filter_begin <- details_snow_free_to_study$date_start_calving[as.factor(gps_clean1$year)]
temp_filter_end <- details_snow_free_to_study$date_end_autumn[as.factor(gps_clean1$year)]

# temporal filter for season, the logical & or | may change depending on the season
# here it is winter
gps_clean2 <- gps_clean1 %>%
  dplyr::filter(timestamp >= temp_filter_begin &
                  timestamp <= temp_filter_end)

  
#' The next step is to separate tracks for the same object ID into bursts, if there is a long time between two positions.
#' Here we set this time as 14 days. We also remove tracks with less than 50 positions to be able to successfully remove "spikes"
#' in the next step.

# split into bursts if there are longer periods without positions
gps_bursts <- gps_clean2 %>%
  dplyr::group_by(Object_ID) %>%
  dplyr::mutate(time_diff = timestamp - lag(timestamp),
                # creates new id if the time difference between positions is > 14 days
                temp_id = ifelse(time_diff > 3600 * 24 * 14, 1, 0),    
                # replace NA with 0
                temp_id = ifelse(is.na(temp_id), 0, temp_id)) %>%
  # The event == 1L creates a logical index telling us whether the column "event" is an integer 
  # 1 or not (TRUE/FALSE). Then, cumsum(...) sums up starting from 0 for the first 2 rows, 
  # 1 for the next 3, 2 for the next 3 and so on
  dplyr::group_by(Object_ID, idx = cumsum(temp_id == 1L)) %>%    
  dplyr::mutate(counter = sprintf("%05d", row_number())) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Object_ID) %>%
  # add numbers to the bursts within each new_id group
  dplyr::mutate(burst_no = sprintf("%02d", cumsum(c(TRUE, idx[-1] != idx[-n()])))) %>%  
  dplyr::ungroup() %>%
  dplyr::select(-c(time_diff, temp_id, idx)) %>%
  dplyr::mutate(burst_id = paste0(Object_ID, "_", burst_no),
                point_id = paste0(Object_ID, "_", burst_no, "_", counter)) %>%
  dplyr::add_count(burst_id) %>%
  dplyr::filter(n > 50)  # remove burst with less than 50 positions

# folder files
folder_files_step1 <- "output/RG_mala/clean_data_step1/"
if(!dir.exists(folder_files_step1)) dir.create(folder_files_step1)

# save data as RData and rsd
save(gps_bursts, file = paste0(folder_files_step1, "data_cleaned_RG_step1_beforeBjorneraas.RData"))
saveRDS(gps_bursts, file = paste0(folder_files_step1, "data_cleaned_RG_step1_beforeBjorneraas.rds"))

#------ remove_spikes

#' # 4 Remove outliers and spikes
#'
#' Now we romove strange outliers when animals perform unrealistic long displacements or travel with unrealistic
#' fast speeds, using the algorithm from Bjorneraas et al. (2010,
#' **Screening Global Positioning System Location Data for Errors Using Animal Movement Characteristics**). '
#' Here we use the same parameters suggested by the authors for moose data. In the first step, we
#' remove outliers more than 100 km apart from the median position and, after that, we remove those more
#' than 20 km apart from the mean position, in 21-step moving windows.
#' In the second step, we remove spikes characterized by speeds that are too fast (> 2.5 km/h) and follow by very extreme
#' turns (cosine of the angle = -0.97, corresponding to an angle between 166 and 194 degrees).

source("code/Outlier_Detection_Bjorneraas_yann.R") # adapted from the using adehabitatLT instead of adehabitat package

gps_spikes <- gps_bursts %>%
  dplyr::group_by(burst_id) %>%
  do(GPS.screening.wrp(id=.$Object_ID, burst = .$burst_id, x=.$Longitude, y=.$Latitude, 
                       date=.$timestamp, medcrit=100000, meancrit=20000, 
                       spikesp=2500, spikecos=(-0.97))) %>%
  ungroup()

# spikes and errors
which_spikes <- which(gps_spikes$R1error == TRUE | gps_spikes$R2error == TRUE)

gps_clean3 <- gps_bursts[-which_spikes,] %>% 
  dplyr::select(-c(counter, burst_no, point_id, n))

# detach packages from the Bjorneraas scripts that interfere with dplyr functions

detach("package:adehabitatHR", unload=TRUE)
detach("package:adehabitatLT", unload=TRUE)
detach("package:CircStats", unload=TRUE)
detach("package:ade4", unload=TRUE)
detach("package:MASS", unload=TRUE)

#------ idenytify_collar_change_candidates

#' # 5 Identify potential collar changes
#'
#' Here we identify possible changes in collars. To do so, we identify two situations.
#' First, we look at periods of more than 1 day when animal positions were recorded within the same corral, urban area,
#' house or building. The first case refers to when animals are gathered to be marked, weighted or managed in some way, and is
#' one of the moments GPS collars are generally taken out from animals for battery replacement. The other ones
#' correspond to moments when the collars were probably inside the herders houses or in other facilites used by them.
#'
#' Second, we identify excessively fast movements (> 10 km/h) as potential moments when animals were moved by
#' truck or the collar was traveling with a herder instead of an animal, to bu put in another animal.

# first, take the columns of interest and transform the data into an sf object
gps_clean3_sf <- gps_clean3 %>%
  dplyr::select(Collar_ID, Object_ID, Object_ID_year, burst_id, year, Longitude, Latitude, timestamp) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), 
               crs = crs_to_use, remove = F)

# check overlap with different facilities
# here we use the function check within
source("code/code_general_00_aux_functions.R")

# check overlap with different facilities
gps_in_facilities <- check_within(gps_locations = gps_clean3_sf, 
                                  areas = corrals_buffer, feature_name = "corrals") # corrals up to 2009
# gps_in_facilities <- check_within(gps_locations = gps_in_facilities, 
#                                   areas = corrals_buffer_after2009, feature_name = "corrals_after2009") # corrals after 2009
gps_in_facilities <- check_within(gps_locations = gps_in_facilities, 
                                  areas = beteshage_buffer, feature_name = "beteshage") # beteshage
gps_in_facilities <- check_within(gps_locations = gps_in_facilities, 
                                  areas = urban, feature_name = "urban") # urban
gps_in_facilities <- check_within(gps_locations = gps_in_facilities, 
                                  areas = buildings_buffer, feature_name = "buildings") # buildings
# error for houses, thats ok
# gps_in_facilities <- check_within(gps_locations = gps_in_facilities, 
#                                   areas = houses_buffer, feature_name = "houses") # houses

# gps_in_facilities <- gps_in_facilities %>% 
#   dplyr::mutate(corrals_polygon = dplyr::if_else(year_start <= 2009, corrals_upto2009_polygon,
#                                                 corrals_after2009_polygon),
#                 corrals_burst = dplyr::if_else(year_start <= 2009, corrals_upto2009_burst,
#                                                 corrals_after2009_burst)) %>%
#   dplyr::select(-contains("2009"))

# separate tracks according to year
gps_clean_break <- gps_in_facilities %>% 
  dplyr::arrange(year, Collar_ID, Object_ID)

# table with potential changes in GPS collars
potential_changes <- gps_clean_break %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(!is.na(corrals_burst) | !is.na(beteshage_burst) | !is.na(urban_burst) | 
                  # !is.na(houses_burst) | 
                  !is.na(buildings_burst)) %>%
  dplyr::mutate(what = case_when(
    !is.na(corrals_burst) ~ corrals_burst,
    !is.na(beteshage_burst) ~ beteshage_burst,
    !is.na(urban_burst) ~ urban_burst,
    #!is.na(houses_burst) ~ houses_burst,
    TRUE ~ buildings_burst,
  )) %>%
  dplyr::group_by(Collar_ID, Object_ID, Object_ID_year, what, year) %>%
  dplyr::summarise(timestamp_end = min(timestamp),
                   timestamp_begin = max(timestamp)) %>%
  dplyr::arrange(year, Collar_ID, Object_ID, timestamp_end) %>% 
  dplyr::mutate(code = 0,
                reviewed_1_0 = 0,
                observations = NA)

# second, identify excessively fast movements
fast_move <- gps_clean_break %>%
  sf::st_drop_geometry() %>%
  amt::mk_track(.x = Longitude, .y = Latitude, .t = timestamp, Collar_ID, Object_ID, Object_ID_year,
                year, order_by_ts = T) %>%
  dplyr::arrange(Collar_ID, Object_ID, Object_ID_year, t_) %>%
  tidyr::nest_legacy(-Object_ID_year) %>%
  dplyr::mutate(sl_ = map(data, step_lengths),
                t2_ = map(data, function(x) lead(x$t_)),
                dt_ = map(data, function(x) as.numeric(difftime(lead(x$t_), x$t_, 
                                                                units = "secs"))),
                speed = map2(sl_, dt_, function(x,y) 3.6*x/y)) %>%
  dplyr::select(-c(dt_)) %>%
  tidyr::unnest_legacy(data, sl_, speed, t2_)

# table with potential changes in GPS collars due to fast moves
max_speed <- 15 # km/h

potential_changes_move <- fast_move %>%
  dplyr::filter(speed > max_speed & t2_ - t_ > minutes(3)) %>%
  dplyr::mutate(what = paste0("fast_speed=", round(speed, digits = 1), "km/h")) %>%
  dplyr::select(Collar_ID, Object_ID, Object_ID_year, year, what,
                timestamp_end = t_, timestamp_begin = t2_) %>%
  dplyr::arrange(year, Collar_ID, Object_ID, timestamp_end) %>% 
  dplyr::mutate(code = 0,
                reviewed_1_0 = 0,
                observations = NA)

# table with pontetial changes in GPS due to long distances
max_dist <- 50 # km

potential_changes_move_dist <- fast_move %>%
  dplyr::filter(sl_/1000 > max_dist) %>%
  dplyr::mutate(what = paste0("long_dist=", round(sl_/1000, digits = 1), "km")) %>%
  dplyr::select(Collar_ID, Object_ID, Object_ID_year, year, what,
                timestamp_end = t_, timestamp_begin = t2_) %>%
  dplyr::arrange(year, Collar_ID, Object_ID, timestamp_end) %>% 
  dplyr::mutate(code = 0,
                reviewed_1_0 = 0,
                observations = NA)

# merge all potential changes
all_potential_changes <- dplyr::bind_rows(potential_changes, potential_changes_move, potential_changes_move_dist) %>%
  dplyr::arrange(year, Collar_ID, Object_ID, Object_ID_year, timestamp_end)

# check Object_ID_year values that are not present, just to be able to check them and register
# that on the table
no_occurrences_ObjIDyear <- dplyr::setdiff(sort(unique(gps_clean_break$Object_ID_year)), 
                                           sort(unique(all_potential_changes$Object_ID_year))) %>%
  sort

# correspondent Collar_ID and Object_ID
no_occurrences <- tibble::tibble(Object_ID = stringr::str_sub(no_occurrences_ObjIDyear, 1, -6)) %>% 
  dplyr::left_join(object_info) %>% 
  dplyr::select(Object_ID, Collar_ID)

# select the table for these lines
other_individuals <- all_potential_changes[1,] %>% 
  dplyr::ungroup() %>% 
  dplyr::slice(rep(1, each = nrow(no_occurrences))) %>% 
  dplyr::mutate(Collar_ID = no_occurrences$Collar_ID, 
                Object_ID = no_occurrences$Object_ID,
                Object_ID_year = no_occurrences_ObjIDyear,
                what = "-",
                year = as.numeric(stringr::str_sub(Object_ID_year, -4, -1)), 
                timestamp_end = NA, 
                timestamp_begin = NA)

# put all individuals in the same table of potential changes
all_potential_changes <- all_potential_changes %>% 
  dplyr::bind_rows(other_individuals) %>% 
  dplyr::arrange(year, Object_ID, Object_ID_year, timestamp_end)

# save table
all_potential_changes %>%
  readr::write_csv(path = paste0(folder_files_step1, "herding_events_Mala_RG_before_check.csv"))


#------ save_outputs

#' # 5 Save trajectories to visualize outside R
#'
#' Here we save the raw and cleaned GPS data in several forms: geopackage with all individuals pooled,
#' shapefiles for each individual/year, and the data loaded within R as `.RData` and `.rds` files.
#' The files may be used to play/animate positions and understand in more details when there where changes in
#' GPS collars between animals.

# save data as csv, RData and rsd
gps_clean_break %>% 
  sf::st_drop_geometry() %>% 
  readr::write_csv(path = paste0(folder_files_step1, "data_cleaned_RG_step1.csv"))
save(gps_clean_break, file = paste0(folder_files_step1, "data_cleaned_RG_step1.RData"))
saveRDS(gps_clean_break, file = paste0(folder_files_step1, "data_cleaned_RG_step1.rds"))

# save cleaned GPS data as a single GPKG
gps_clean_break_export <- gps_clean_break %>%
  dplyr::select(Collar_ID, Object_ID, burst = Object_ID_year, 
                timestamp, X = Longitude, Y = Latitude, year = year) %>%
  dplyr::mutate_at(c(1:7), as.character)

# folder for vectors
folder <- "output/RG_mala/clean_data_step1/vector/"
if(!dir.exists(folder)) dir.create(folder)

gps_clean_break_export %>%
  sf::st_write(paste0(folder, "all_data_RG_clean_step1.gpkg"))

# split maps into individual tracks in each year and exporting them as shapefile
gps_clean_split <- gps_clean_break_export %>%
  split(paste0(.$year, "_", .$Object_ID))

# export shapefiles for each individual
folder_shp <- "output/RG_mala/clean_data_step1/vector/shp/"
if(!dir.exists(folder_shp)) dir.create(folder_shp)

lapply(names(gps_clean_split), function(x)
  sf::st_write(gps_clean_split[[x]], 
               dsn = paste0(folder_shp, x, ".shp"), 
               delete_dsn = TRUE))

