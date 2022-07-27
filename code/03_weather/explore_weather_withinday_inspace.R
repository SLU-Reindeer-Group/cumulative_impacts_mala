#' ---
#' title: "Weather data in Malå Sami reindeer herding district"
#' author: ""
#' output: 
#'   rmarkdown::github_document
#' ---
#' 
#' Here we explore two sources of weather data in the Malå Sami reindeer herding 
#' district in Northern Sweden. The aim is to choose one of the them to be used for
#' inferring changes in reindeer habitat selection.
#' The first source comes from an interpolated map of SMHI, based on weather station
#' data, and includes spatial variation within the district, but only one value per day 
#' (min temperature, max temperature, min wind speed, max wind speed, accumulated 
#' precipitation).
#' The second source comes from a single weather station in Malå-Brannan. It includes
#' within-day variation in temperature, wind speed and direction, and precipitation,
#' but no spatial variation within the district.

# --------------- label=setup, warning=FALSE, message=FALSE, echo=TRUE

# Load packages
if(!require(install.load)) install.packages("install.load"); library(install.load)

install.load::install_load("knitr", "rprojroot")

# Print options for this document
options(width = 165)
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file()) # root project folder
opts_chunk$set(error = F, message = F, warning = F, cache = F, echo = T, results = T)

# ----------

# packages
library(dplyr)
library(ggplot2)

#' # Temperature

# load weather data in spac
smhi_df <- readRDS("data/weather/smhi_df.RDS")
smhi_df

# summarize variation in space
summ_space_day <- smhi_df %>%
     dplyr::group_by(day) %>%
     dplyr::summarize(mintemp_min = min(mintemp, na.rm = T),
                      mintemp_max = max(mintemp, na.rm = T),
                      mintemp_avg = mean(mintemp, na.rm = T),
                      mintemp_sd = sd(mintemp, na.rm = T),
                      mintemp_range = range(mintemp, na.rm = T),
                      maxtemp_min = min(maxtemp, na.rm = T),
                      maxtemp_max = max(maxtemp, na.rm = T),
                      maxtemp_avg = mean(maxtemp, na.rm = T),
                      maxtemp_sd = sd(maxtemp, na.rm = T),
                      maxtemp_range = range(maxtemp, na.rm = T),
                      rangetemp = range(maxtemp - mintemp, na.rm = T)) %>% 
  dplyr::mutate(year = lubridate::year(day),
                yday = lubridate::yday(day)) 
summ_space_day

summ_space_day_persite <- smhi_df %>%
  dplyr::group_by(day, id) %>%
  dplyr::summarize(mintemp_min = min(mintemp, na.rm = T),
                   mintemp_max = max(mintemp, na.rm = T),
                   mintemp_avg = mean(mintemp, na.rm = T),
                   mintemp_sd = sd(mintemp, na.rm = T),
                   mintemp_range = range(mintemp, na.rm = T),
                   maxtemp_min = min(maxtemp, na.rm = T),
                   maxtemp_max = max(maxtemp, na.rm = T),
                   maxtemp_avg = mean(maxtemp, na.rm = T),
                   maxtemp_sd = sd(maxtemp, na.rm = T),
                   maxtemp_range = range(maxtemp, na.rm = T),
                   rangetemp = range(maxtemp - mintemp, na.rm = T)) %>% 
  dplyr::mutate(year = lubridate::year(day),
                yday = lubridate::yday(day)) 
summ_space_day_persite

# load weather data for one location, with within-day variation
temp_day <- readr::read_csv2("data/weather/temperature_smhi_mala_brannan.csv", skip = 9) %>% 
  dplyr::select(day = Datum, time = 2, temperature = 3) %>% 
  dplyr::mutate(temperature = as.numeric(temperature),
                timestamp = lubridate::ymd_hms(paste(as.character(day), as.character(time))),
                temp_cat = ifelse(temperature <= 0, "negative", "positive"))
temp_day

# summarize variation within a day
summ_within_day <- temp_day %>%
  dplyr::group_by(day) %>%
  dplyr::summarize(temp_min = min(temperature, na.rm = T),
                   temp_max = max(temperature, na.rm = T),
                   temp_avg = mean(temperature, na.rm = T),
                   temp_sd = sd(temperature, na.rm = T),
                   temp_range = range(temperature, na.rm = T)) %>% 
  dplyr::mutate(year = lubridate::year(day),
                yday = lubridate::yday(day)) %>% 
  dplyr::filter(year > 2007 & year < 2021)
summ_within_day

# plot

# range minimum temp
summ_space_day %>% 
  ggplot(aes(yday, abs(mintemp_range))) +
  geom_vline(xintercept = 30*4 + 5, linetype = 2, col = 2) + # May 5
  geom_vline(xintercept = 30*11 - 15, linetype = 2, col = 2) + # Nov 15
  geom_point() +
  geom_smooth() +
  facet_wrap(~year) +
  ylim(0, 30) +
  labs(x = "Day of the year",
       y = "Range of minimum temperature in space")

# range maximum temp
summ_space_day %>% 
  ggplot(aes(yday, abs(maxtemp_range))) +
  geom_vline(xintercept = 30*4 + 5, linetype = 2, col = 2) + # May 5
  geom_vline(xintercept = 30*11 - 15, linetype = 2, col = 2) + # Nov 15
  geom_point() +
  geom_smooth() +
  facet_wrap(~year) +
  ylim(0, 30) +
  labs(x = "Day of the year",
       y = "Range of maximum temperature in space")

# range daily temp
summ_space_day %>% 
  ggplot(aes(yday, abs(rangetemp))) +
  geom_vline(xintercept = 30*4 + 5, linetype = 2, col = 2) + # May 5
  geom_vline(xintercept = 30*11 - 15, linetype = 2, col = 2) + # Nov 15
  geom_point() +
  geom_smooth() +
  facet_wrap(~year) +
  ylim(0, 30) +
  labs(x = "Day of the year",
       y = "Range of daily temperatures in space")

# range temp within a day
summ_within_day %>% 
  ggplot(aes(yday, abs(temp_range))) +
  geom_vline(xintercept = 30*4 + 5, linetype = 2, col = 2) + # May 5
  geom_vline(xintercept = 30*11 - 15, linetype = 2, col = 2) + # Nov 15
  geom_point() +
  geom_smooth() +
  facet_wrap(~year) +
  ylim(0, 30) +
  labs(x = "Day of the year",
       y = "Range of temperatures within a day in a single place")

# sd minimum temp
summ_space_day %>% 
  dplyr::mutate(year = lubridate::year(day),
                yday = lubridate::yday(day)) %>% 
  ggplot(aes(yday, abs(mintemp_sd))) +
  geom_vline(xintercept = 30*4 + 5, linetype = 2, col = 2) + # May 5
  geom_vline(xintercept = 30*11 - 15, linetype = 2, col = 2) + # Nov 15
  geom_point() +
  geom_smooth() +
  facet_wrap(~year) +
  labs(x = "Day of the year",
       y = "SD of minimum temperature in space")

# sd maximum temp
summ_space_day %>% 
  dplyr::mutate(year = lubridate::year(day),
                yday = lubridate::yday(day)) %>% 
  ggplot(aes(yday, abs(maxtemp_sd))) +
  geom_vline(xintercept = 30*4 + 5, linetype = 2, col = 2) + # May 5
  geom_vline(xintercept = 30*11 - 15, linetype = 2, col = 2) + # Nov 15
  geom_point() +
  geom_smooth() +
  facet_wrap(~year) +
  labs(x = "Day of the year",
       y = "SD of maximum temperature in space")

# SD temp within a day
summ_within_day %>% 
  ggplot(aes(yday, abs(temp_sd))) +
  geom_vline(xintercept = 30*4 + 5, linetype = 2, col = 2) + # May 5
  geom_vline(xintercept = 30*11 - 15, linetype = 2, col = 2) + # Nov 15
  geom_point() +
  geom_smooth() +
  facet_wrap(~year) +
  labs(x = "Day of the year",
       y = "SD of temperatures within a day in a single place")

#' Wind speed and direction

# summarize variation in space
summ_space_day_w <- smhi_df %>%
  dplyr::group_by(day) %>%
  dplyr::summarize(minws_min = min(minws, na.rm = T),
                   minws_max = max(minws, na.rm = T),
                   minws_avg = mean(minws, na.rm = T),
                   minws_sd = sd(minws, na.rm = T),
                   minws_range = range(minws, na.rm = T),
                   maxws_min = min(maxws, na.rm = T),
                   maxws_max = max(maxws, na.rm = T),
                   maxws_avg = mean(maxws, na.rm = T),
                   maxws_sd = sd(maxws, na.rm = T),
                   maxws_range = range(maxws, na.rm = T),
                   rangews = range(maxws - minws, na.rm = T)) %>% 
  dplyr::mutate(year = lubridate::year(day),
                yday = lubridate::yday(day)) 
summ_space_day_w

# load weather data for one location, with within-day variation
wind_day <- readr::read_csv2("data/weather/wind_speed_direction_smhi_mala_brannan.csv", skip = 10) %>% 
  dplyr::select(day = Datum, time = 2, wind_direction = 3, wind_speed = 5) %>% 
  dplyr::mutate(wind_speed = as.numeric(wind_speed),
                timestamp = lubridate::ymd_hms(paste(as.character(day), as.character(time))),
                wind_dir_cat = case_when(
                  wind_direction >= 315 | wind_direction < 45 ~ "N",
                  wind_direction >= 45 & wind_direction < 135 ~ "E",
                  wind_direction >= 135 & wind_direction < 225 ~ "S",
                  wind_direction >= 225 & wind_direction < 315 ~ "W",
                )) %>% 
  dplyr::mutate(year = lubridate::year(day),
                yday = lubridate::yday(day)) 
wind_day

# summarize variation within a day
summ_within_day_w <- wind_day %>%
  dplyr::group_by(day) %>%
  dplyr::summarize(ws_min = min(wind_speed, na.rm = T),
                   ws_max = max(wind_speed, na.rm = T),
                   ws_avg = mean(wind_speed, na.rm = T),
                   ws_sd = sd(wind_speed, na.rm = T),
                   ws_range = range(wind_speed, na.rm = T)) %>% 
  dplyr::mutate(year = lubridate::year(day),
                yday = lubridate::yday(day)) %>% 
  dplyr::filter(year > 2007 & year < 2021)
summ_within_day_w

# plot

# range minimum wind speed
summ_space_day_w %>% 
  ggplot(aes(yday, abs(minws_range))) +
  geom_vline(xintercept = 30*4 + 5, linetype = 2, col = 2) + # May 5
  geom_vline(xintercept = 30*11 - 15, linetype = 2, col = 2) + # Nov 15
  geom_point() +
  geom_smooth() +
  facet_wrap(~year) +
  labs(x = "Day of the year",
       y = "Range of minimum wind speed in space")

# range maximum wind speed
summ_space_day_w %>% 
  ggplot(aes(yday, abs(maxws_range))) +
  geom_vline(xintercept = 30*4 + 5, linetype = 2, col = 2) + # May 5
  geom_vline(xintercept = 30*11 - 15, linetype = 2, col = 2) + # Nov 15
  geom_point() +
  geom_smooth() +
  facet_wrap(~year) +
  labs(x = "Day of the year",
       y = "Range of maximum wind speed in space")

# range daily wind speed
summ_space_day_w %>% 
  ggplot(aes(yday, abs(rangews))) +
  geom_vline(xintercept = 30*4 + 5, linetype = 2, col = 2) + # May 5
  geom_vline(xintercept = 30*11 - 15, linetype = 2, col = 2) + # Nov 15
  geom_point() +
  geom_smooth() +
  facet_wrap(~year) +
  labs(x = "Day of the year",
       y = "Range of daily wind speeds in space")

# range wind speed within a day
summ_within_day_w %>% 
  ggplot(aes(yday, abs(ws_range))) +
  geom_vline(xintercept = 30*4 + 5, linetype = 2, col = 2) + # May 5
  geom_vline(xintercept = 30*11 - 15, linetype = 2, col = 2) + # Nov 15
  geom_point() +
  geom_smooth() +
  facet_wrap(~year) +
  labs(x = "Day of the year",
       y = "Range of wind speeds within a day in a single place")

#' Precipitation

