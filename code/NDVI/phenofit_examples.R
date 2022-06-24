library(tidyverse)
library(phenofit)

# load preprocessed data for one pixel of modis data for Mal?
df_mala <- read.csv("code/NDVI/df_mala_example.csv")

#############################
#### using phenofit #########

# set parameters
ymax_min   <- 0.1  # the maximum ymax shoud be greater than `ymax_min` 
rymin_less <- 0.8  # trough < ymin + A*rymin_less
nptperyear <- 23   # How many points for a single year
wFUN       <- wBisquare # Weights updating function, could be one of `wTSM`, 'wBisquare', `wChen` and `wSELF`. 
south      <- FALSE # sp$lat < 0

# add additional one year in head and tail (needed for the curve fitting)
dnew  <- add_HeadTail(df_mala, south, nptperyear = 23)

# check input data, interpolates NA values, remove spikes, and set weights for NA
in_data <- check_input(
  dnew$t,
  dnew$y,
  dnew$w,
  dnew$QC_flag,
  nptperyear,
  south,
  maxgap = nptperyear / 4,
  alpha = 0.02,
  wmin = 0.2
)

# divide growing seasons and plot result 
par(mar = c(3, 2, 2, 1), mgp = c(3, 0.6, 0))

lambda <- init_lambda(in_data$y)

brks <- season_mov(in_data,
                   list(
                     FUN = smooth_wWHIT,
                     wFUN = wFUN,
                     maxExtendMonth = 6,
                     r_min = 0.1
                   ))

plot_season(in_data, brks)


# fit curves using multiple methods and plot them
fit  <- curvefits(
  in_data,
  brks,
  options = list(
    methods = c("AG", "Zhang", "Beck", "Elmore","Gu"),
    wFUN = wFUN,
    nextend = 2,
    maxExtendMonth = 3,
    minExtendMonth = 1,
    minPercValid = 0.2,
    print = F
  )
)

d_fit <- get_fitting(fit)

g <- plot_curvefits(d_fit,
                    brks,
                    NULL,
                    ylab = "NDVI",
                    "Time")
grid::grid.newpage()
grid::grid.draw(g)

# extract and plot phenology
pheno <- get_pheno(fit[2:15], IsPlot = T)


######################################
###### data extracting examples ######

# sum ndvi values for seasons 

# load seasons
seasons <- read.csv("code/NDVI/details_snow_free_mala_clean_MOD.csv") %>%
  mutate_at(3:8, as.Date)

# define seasons
# (in this case using data from the AG curve fitting method and the derivative method for phenology extracting)
s_start_end <- pheno$date$AG %>%
  mutate(year = as.integer(substr(flag, 1, 4))) %>%
  dplyr::select(year, sos = DER.sos, eos = DER.eos) %>%
  left_join(dplyr::select(seasons, year, 3:8), by = "year") %>%
  # set new start date if it's before SOS and new end date if it's after EOS
  mutate(date_start_calving = if_else(sos > date_start_calving, sos, date_start_calving)) %>%
  mutate(date_end_autumn = if_else(eos < date_end_autumn, eos, date_end_autumn))

# get days and corresponding ndvi values from the phenofit output for a single year
day <- fit$`2008_1`$model$AG$tout
ndvi_value <- fit$`2008_1`$model$AG$zs$iter2

# plot extracted days and ndvi_values
s_plot <- data.frame(
  day = as.Date(day - 1, origin = "2000-01-01"),
  value = ndvi_value
)

s <- ggplot(s_plot, aes(x=day, y=value)) +
  geom_line() + 
  xlab("")
s

# sum values for each season
data_ndvi_seasons <- data.frame(day, ndvi_value) %>%
  mutate(date = as.Date(day - 1, origin = "2000-01-01")) %>% # -1 because 2000-01-01 is day
  mutate(year = as.integer(substr(date, 1, 4))) %>%
  left_join(s_start_end, by = "year") %>%
  mutate(season = case_when(
      date >= date_start_calving & date <= date_end_calving ~ "calving",
      date >= date_start_summer & date <= date_end_summer ~ "summer",
      date >= date_start_autumn &
      date <= date_end_autumn ~ "autumn")) %>%
  filter(!is.na(season)) %>%
  group_by(season) %>%
  summarise(total_ndvi = sum(ndvi_value))  #, n_days=n()
  # pivot_wider(names_from = season, values_from = total_ndvi)

# plot sums for seasons
s2 <-ggplot(data=data_ndvi_seasons, aes(x=season, y=total_ndvi)) +
  geom_bar(stat="identity")
s2

###########################

# calculate average ndvi value from Beck and Elmore methods for each day 

# get days and corresponding ndvi values from the phenofit output 
days_beck <- unlist(sapply(fit, function(x) x[["model"]][["Beck"]][["tout"]]))
ndvis_beck <- unlist(sapply(fit, function(x) x[["model"]][["Beck"]][["zs"]][["iter2"]]))
days_elmore <- unlist(sapply(fit, function(x) x[["model"]][["Elmore"]][["tout"]]))
ndvis_elmore <- unlist(sapply(fit, function(x) x[["model"]][["Elmore"]][["zs"]][["iter2"]]))

# create a data frame with all dates (to make sure all vectors have the same length later
# when saving them to an array)
all_days <- data.frame(date = seq(as.Date("2006-01-01"), as.Date("2021-12-31"), by="days"))

ndvi_elmore <- data.frame(days_elmore, ndvis_elmore) %>% 
  mutate(date=as.Date(days_elmore-1, origin = "2000-01-01"))
ndvi_beck <- data.frame(days_beck, ndvis_beck) %>% 
  mutate(date=as.Date(days_beck-1, origin = "2000-01-01"))

data_ndvi_days <- all_days %>% 
  left_join(ndvi_elmore, by = "date") %>%
  left_join(ndvi_beck, by = "date") %>%
# count to see if there's ndvi values for both Elmore and Beck on each row  
  rowwise() %>%
  mutate(ndvi_count=length(which(c_across(any_of(starts_with("ndvis")))>0))) %>% 
  ungroup() %>% 
  mutate_at(c(2:5), ~replace(., is.na(.), 0)) %>% 
# calculate the mean value
  mutate(ndvis=(ndvis_elmore+ndvis_beck)/ndvi_count)

# plot time series
d <- ggplot(data_ndvi_days, aes(x=date, y=ndvis)) +
  geom_line() + 
  xlab("")
d
