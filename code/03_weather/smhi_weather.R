library(readr)
library(tidyverse)
library(sf)

# load GPS data for one season 
load("//yksi.srh.slu.se/13_geodata/erik_phd/smhi/data_cleaned_autumn_RG.RData")

# load polygon for Malå sameby
mala <- st_read("//yksi.srh.slu.se/13_geodata/erik_phd/smhi/mala_shape/Mala_sameby.shp")

### Load grid points from SMHI and select only those Within Malå sameby ###

# 2007-2014 grid size 11 x 11km
smhi_mala_1 <- read_delim("//yksi.srh.slu.se/13_geodata/erik_phd/smhi/coordinates1.csv", ";",
  escape_double = FALSE, trim_ws = TRUE, skip = 1, col_types = "iddii"
) %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(3006) %>%
  filter(st_intersects(., mala, sparse = FALSE)[, 1])
# 2015-2020 grid size 2.5 x 2.5km
smhi_mala_2 <- read_delim("//yksi.srh.slu.se/13_geodata/erik_phd/smhi/coordinates2.csv", ";",
  escape_double = FALSE, trim_ws = TRUE, skip = 1, col_types = "iddii"
) %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(3006) %>%
  filter(st_intersects(., mala, sparse = FALSE)[, 1])

# plot(smhi_mala_1[1])


### Read and filter data from SMHI csv-files ###

# # List all SMHI ids within Malå sameby
# ids <- unique(as.integer(c(smhi_mala_1$Nr, smhi_mala_2$Nr)))
# 
# # List years of interest
# years <- c(as.character(rep(2008:2011)),as.character(rep(2015:2018)))
# 
# data_path <- "//yksi.srh.slu.se/13_geodata/erik_phd/smhi/data_final"
# 
# # List all csv-files from SMHI
# files <- list.files(path=data_path, pattern="*.csv", full.names = TRUE)
# # Filter to only include the years of interest
# files_years <- files[files %in% grep(paste(years, collapse="|"), files, value = T)]
# 
# # Create function for reading and filtering data
# read_and_filter <- function(csv) {
#   read_delim(csv, delim = ";", col_types = "Diddddddidid") %>%
#     filter(id %in% ids)
# }
# 
# # combine data from csv-files
# smhi_df <- map_dfr(files_years,
#                    read_and_filter)
# 
# saveRDS(smhi_df, "//yksi.srh.slu.se/13_geodata/erik_phd/smhi/smhi_df.RDS")

smhi_df <- readRDS("//yksi.srh.slu.se/13_geodata/erik_phd/smhi/smhi_df.RDS")


# # tried parallel processing with furrr but it didn't make much difference
# library(furrr)
# smhi_df <- future_map_dfr(files_years,
#                           read_and_filter)


### Filter GPS data and add id for closest smhi grid point ###

# test for 2008-2011
gps_autumn_2008_2011 <- gps_autumn_clean10 %>% 
  filter(year < 2012) %>% 
  st_as_sf(.,coords = c("x","y"), crs = 3006) %>%
  mutate(smhi_nr=smhi_mala_1$Nr[st_nearest_feature(., smhi_mala_1)]) %>% 
  st_drop_geometry() %>% 
  mutate(date=as.Date(timestamp)) %>% 
  left_join(smhi_df, by=c("smhi_nr"="id", "date"="day"))