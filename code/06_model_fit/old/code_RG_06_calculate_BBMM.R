#' ---
#' title: 'Annotate GPS for habitat selection analysis: Mala'
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
install.load::install_load("sf", "terra", "raster", "sp")
install.load::install_load("amt", "adehabitatLT", "adehabitatHR")

# projection to use
crs_to_use <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"

#-----------
#' # Load movement data

# reference raster
ref.raster <- raster("maps/model_for_bbmm_100m.tif") %>% 
  raster::projectRaster(crs = sp::CRS(crs_to_use))

# remove individuals for calving when they had calved within corrals
(remove_calving <- read_csv("data/individuals_to_remove_calving_Mala_2021_05_d02.csv") %>% 
    pull(Object_ID_year))

# movement data
mov_calving <- readRDS("data/data_cleaned_calving_RG.rds") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(Collar_ID, Object_ID, id, burst, year, x, y, timestamp) %>% 
  dplyr::filter(!(id %in% remove_calving))

mov_summer <- readRDS("data/data_cleaned_summer_RG.rds") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(Collar_ID, Object_ID, id, burst, year, x, y, timestamp)

mov_autumn <- readRDS("data/data_cleaned_autumn_RG.rds") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(Collar_ID, Object_ID, id, burst, year, x, y, timestamp)

#-----------
#' # Resampling for all individuals

# transform into track object - calving
mov_track_calving <- mov_calving %>% 
  amt::mk_track(.x = x, .y = y, .t = timestamp, Collar_ID, Object_ID, id, burst, year, 
                crs = sp::CRS(crs_to_use))

mov_track_summer <- mov_summer %>% 
  amt::mk_track(.x = x, .y = y, .t = timestamp, Collar_ID, Object_ID, id, burst, year, 
                crs = sp::CRS(crs_to_use))

mov_track_autumn <- mov_autumn %>% 
  amt::mk_track(.x = x, .y = y, .t = timestamp, Collar_ID, Object_ID, id, burst, year, 
                crs = sp::CRS(crs_to_use))

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
# View(samp_rate)

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

# calving
mov_track_calving_2h <- mov_track_calving %>% 
  tidyr::nest(data = c(x_, y_, t_, Collar_ID, Object_ID, burst, year)) %>% 
  dplyr::mutate(trk = map(data, function(d, fix_rate_h = 2, tolerance_min = 30) {
    cat(paste(d$Object_ID[1], d$year[1], "\n"))
    # resample
    mov_track_rsp_1 <- d %>% 
      amt::track_resample(rate = hours(fix_rate_h), tolerance = minutes(tolerance_min)) %>% 
      amt::filter_min_n_burst(min_n = 3)
    mov_track_rsp_1
  })) %>% 
  dplyr::select(-data) %>% 
  tidyr::unnest(cols = trk)

# summer
mov_track_summer_2h <- mov_track_summer %>% 
  tidyr::nest(data = c(x_, y_, t_, Collar_ID, Object_ID, burst, year)) %>% 
  dplyr::mutate(trk = map(data, function(d, fix_rate_h = 2, tolerance_min = 30) {
    cat(paste(d$Object_ID[1], d$year[1], "\n"))
    # resample
    mov_track_rsp_1 <- d %>% 
      amt::track_resample(rate = hours(fix_rate_h), tolerance = minutes(tolerance_min)) %>% 
      amt::filter_min_n_burst(min_n = 3)
    mov_track_rsp_1
  })) %>% 
  dplyr::select(-data) %>% 
  tidyr::unnest(cols = trk)

# autumn
mov_track_autumn_2h <- mov_track_autumn %>% 
  tidyr::nest(data = c(x_, y_, t_, Collar_ID, Object_ID, burst, year)) %>% 
  dplyr::mutate(trk = map(data, function(d, fix_rate_h = 2, tolerance_min = 15) {
    cat(paste(d$Object_ID[1], d$year[1], "\n"))
    # resample
    mov_track_rsp_1 <- d %>% 
      amt::track_resample(rate = hours(fix_rate_h), tolerance = minutes(tolerance_min)) %>% 
      amt::filter_min_n_burst(min_n = 3)
    mov_track_rsp_1
  })) %>% 
  dplyr::select(-data) %>% 
  tidyr::unnest(cols = trk)

#-----
# transform dataset into adehabitatLT object

# calving
mov_track_calving_2h <- mov_track_calving_2h %>% 
  dplyr::rename(x = x_, y = y_)
mov_track_calving_2h_ltraj <- as.ltraj(xy = mov_track_calving_2h[c("x", "y")], 
                                       date = mov_track_calving_2h$t_,
                                       id = mov_track_calving_2h$id,
                                       burst = mov_track_calving_2h$burst,
                                       proj4string = sp::CRS(crs_to_use))
  

# plot(ref.raster)
# set raster as a reference for BBMM analysis
grid <- as(ref.raster, "SpatialPixels")
sp::bbox(grid)

# take a look at the cleaned data
mov_track_calving_2h_ltraj

# calculating the parameters for the Brownian Bridge analysis
cat(paste("Calculates sigma1 based on sigma2...\n"))
sig2 <- 20										# calculation error in the GPS
cat(paste("sig2 = ", sig2, "\n", sep=""))

# uses the function liker from adehabitatHR package to find the MLE of the parameter
# sigma1
renLiker <- liker(mov_track_calving_2h_ltraj, sig2, byburst = TRUE, rangesig1 = c(0,20), plotit=F)	# Räknar ut sig1
hist(sapply(renLiker, function(x) x$sig1), ncla=20, col="grey")

# get the mean value to use in the analysis
renMedian <- median(sapply(renLiker, function(x) x$sig1)) 
cat(paste("Parameters for BBMM:\n",
          "Average for sig1 = ", renMedian, "\n", 
          "sig2 = ", sig2, "\n", sep=""))

# calculate BBMM for all individuals
# to make the calculations feasible in a short time, we separated the individuals in 
# multiple R sessions and ran them in parallel. After that, we saved each run as 
# separate RData files, loaded them all, and integrated everything in a single object

# run BBMM
cat(paste("Calculating BBMM homerange. Start time:",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n\n"))
# bbmm_calving_1_50 <- kernelbb(mov_track_calving_2h_ltraj[1:50], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
# bbmm_calving_51_150 <- kernelbb(mov_track_calving_2h_ltraj[51:150], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
# bbmm_calving_151_end <- kernelbb(mov_track_calving_2h_ltraj[151:length(mov_track_calving_2h_ltraj)], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
cat(paste("Calculating BBMM homerange. End time::",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n\n"))

# run BBMM for individuals of 2017-28
rows_2017_18 <- summary(mov_track_calving_2h_ltraj) %>% 
  tibble::rownames_to_column() %>% 
  dplyr::filter(grepl("2017|2018", id)) %>% 
  pull(rowname) %>% 
  as.numeric()
  
cat(paste("Calculating BBMM homerange. Start time:",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n\n"))
# bbmm_calving_1_50 <- kernelbb(mov_track_calving_2h_ltraj[1:50], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
# bbmm_calving_51_150 <- kernelbb(mov_track_calving_2h_ltraj[51:150], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
# bbmm_calving_151_end <- kernelbb(mov_track_calving_2h_ltraj[151:length(mov_track_calving_2h_ltraj)], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
bbmm_calving_2017_18 <- kernelbb(mov_track_calving_2h_ltraj[rows_2017_18], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
cat(paste("Calculating BBMM homerange. End time::",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n\n"))

# save(bbmm_calving_1_50, bbmm_calving_51_150, bbmm_calving_151_end, bbmm_calving_2017_18,
#      file = "output/bbmm_spring.RData")

# load("output/old/bbmm_spring.RData")
save(bbmm_calving_1_50, bbmm_calving_51_150, bbmm_calving_151_end, bbmm_calving_2017_18,
     file = "output/bbmm_spring_2008_2018.RData")

bbmm_calving_all <- c(bbmm_calving_1_50,
                      bbmm_calving_51_150,
                      bbmm_calving_151_end,
                      bbmm_calving_2017_18)
class(bbmm_calving_all) <- "estUDm"

# extract 95% polygons and save as vector files
dir_out <- "results/bbmm_calving"
if(!dir.exists(dir_out)) dir.create(dir_out)

# loop for each individuals
ind_info <- summary(mov_track_calving_2h_ltraj)[rows_2017_18,]
ind_info

inds <- as.character(unique(ind_info$id))

list_bbmm_pols_calving <- list()
for(i in inds) {
  
  print(i)
  # select rows
  rows <- which(ind_info$id == i)
  
  # select UD object
  if(length(rows) == 1) {
    ud <- bbmm_calving_all[[rows]]
  } else {
    ud <- bbmm_calving_all[rows]
    class(ud) <- "estUDm"
  }
  
  # get polygon and transform it into sf
  pol <- tryCatch(
    {
      getverticeshr(ud, percent = 99) %>% 
      as("sf") %>% 
      dplyr::mutate(id = i)
    }, 
    error = function(cond) {
      message("Error message:")
      message(cond)
      # did not work
      # message("Instead, we'll average the uds!")
      # ud_df <- lapply(ud, function(x) as.data.frame(x)[,"ud"]) %>% 
      #   bind_cols()
      # ud_sum <- apply(ud_df, MARGIN = 1, sum)
      # ud_prob <- ud_sum/sum(ud_sum)
      # ud_avg <- ud[[1]]
      # ud_avg$ud <- ud_prob
      # pol_avg <- getverticeshr(ud_avg, percent = 50) %>% 
      #   as("sf") %>% 
      #   dplyr::mutate(id = i)
      # return(pol_avg)
      message("Instead, we'll calculate the 80% HR kernel!")
      return(getverticeshr(ud, percent = 80) %>% 
               as("sf") %>% 
               dplyr::mutate(id = i))
    })
  
  # save in list
  list_bbmm_pols_calving[[i]] <- pol
  
  # save external
  sf::st_write(pol, paste0("results/bbmm_calving/bbmm_calving_", i, ".gpkg"), 
               delete_dsn = T)
  
  # plot
  plot(pol[1], col = 2)
}

list_bbmm_pols_calving

# list_bbmm_pols_calving_2017_18 <- list_bbmm_pols_calving
# load("output/list_bbmm_pols_calving.RData")
# length(list_bbmm_pols_calving_2017_18)
# length(list_bbmm_pols_calving)
# list_bbmm_pols_calving <- c(list_bbmm_pols_calving, list_bbmm_pols_calving_2017_18)

# save
save(list_bbmm_pols_calving, file = "output/list_bbmm_pols_calving.RData")

#-----
# summer

# transform dataset into adehabitatLT object

# summer
mov_track_summer_2h <- mov_track_summer_2h %>% 
  dplyr::rename(x = x_, y = y_)
mov_track_summer_2h_ltraj <- as.ltraj(xy = mov_track_summer_2h[c("x", "y")], 
                                       date = mov_track_summer_2h$t_,
                                       id = mov_track_summer_2h$id,
                                       burst = mov_track_summer_2h$burst,
                                       proj4string = sp::CRS(crs_to_use))


# plot(ref.raster)
# set raster as a reference for BBMM analysis
grid <- as(ref.raster, "SpatialPixels")
sp::bbox(grid)

# take a look at the cleaned data
mov_track_summer_2h_ltraj

# calculating the parameters for the Brownian Bridge analysis
cat(paste("Calculates sigma1 based on sigma2...\n"))
sig2 <- 20										# calculation error in the GPS
cat(paste("sig2 = ", sig2, "\n", sep=""))

# uses the function liker from adehabitatHR package to find the MLE of the parameter
# sigma1
renLiker <- liker(mov_track_summer_2h_ltraj, sig2, byburst = TRUE, rangesig1 = c(0,20), plotit=F)	# Räknar ut sig1
hist(sapply(renLiker, function(x) x$sig1), ncla=20, col="grey")

# get the mean value to use in the analysis
renMedian <- median(sapply(renLiker, function(x) x$sig1)) 
cat(paste("Parameters for BBMM:\n",
          "Average for sig1 = ", renMedian, "\n", 
          "sig2 = ", sig2, "\n", sep=""))

# calculate BBMM for all individuals
# to make the calculations feasible in a short time, we separated the individuals in 
# multiple R sessions and ran them in parallel. After that, we saved each run as 
# separate RData files, loaded them all, and integrated everything in a single object

# run BBMM
cat(paste("Calculating BBMM homerange. Start time:",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n\n"))
# bbmm_summer_1_50 <- kernelbb(mov_track_summer_2h_ltraj[1:50], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
# bbmm_summer_51_150 <- kernelbb(mov_track_summer_2h_ltraj[51:150], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
# bbmm_summer_151_end <- kernelbb(mov_track_summer_2h_ltraj[151:length(mov_track_summer_2h_ltraj)], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
cat(paste("Calculating BBMM homerange. End time::",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n\n"))

# run BBMM for individuals of 2017-28
rows_2017_18 <- summary(mov_track_summer_2h_ltraj) %>% 
  tibble::rownames_to_column() %>% 
  dplyr::filter(grepl("2017|2018", id)) %>% 
  pull(rowname) %>% 
  as.numeric()

cat(paste("Calculating BBMM homerange. Start time:",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n\n"))
bbmm_summer_2017_18 <- kernelbb(mov_track_summer_2h_ltraj[rows_2017_18], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
cat(paste("Calculating BBMM homerange. End time::",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n\n"))

# load("output/old/bbmm_summer.RData")
# save(bbmm_summer_1_50, bbmm_summer_51_150, bbmm_summer_151_end, bbmm_summer_2017_18, file = "output/bbmm_summer.RData")

bbmm_summer_all <- c(bbmm_summer_1_50,
                     bbmm_summer_51_150, 
                     bbmm_summer_151_end,
                     bbmm_summer_2017_18)
# bbmm_summer_all <- bbmm_summer_2017_18
class(bbmm_summer_all) <- "estUDm"

# extract 95% polygons and save as vector files
dir_out <- "results/bbmm_summer"
if(!dir.exists(dir_out)) dir.create(dir_out)

# loop for each individuals
ind_info <- summary(mov_track_summer_2h_ltraj)[rows_2017_18,]
ind_info

inds <- as.character(unique(ind_info$id))

list_bbmm_pols_summer <- list()
for(i in inds) {
  
  print(i)
  # select rows
  rows <- which(ind_info$id == i)
  
  # select UD object
  if(length(rows) == 1) {
    ud <- bbmm_summer_all[[rows]]
  } else {
    ud <- bbmm_summer_all[rows]
    class(ud) <- "estUDm"
  }
  
  # get polygon and transform it into sf
  pol <- getverticeshr(ud, percent = 99) %>% 
    as("sf") %>% 
    dplyr::mutate(id = i)
  
  # save in list
  list_bbmm_pols_summer[[i]] <- pol
  
  # save external
  sf::st_write(pol, paste0("results/bbmm_summer/bbmm_summer_", i, ".gpkg"), 
               delete_dsn = T)
  
  # plot
  plot(pol[1], col = 2)
}

# list_bbmm_pols_summer_2017_18 <- list_bbmm_pols_summer
# load("output/list_bbmm_pols_summer.RData")
# length(list_bbmm_pols_summer_2017_18)
# length(list_bbmm_pols_summer)
# list_bbmm_pols_summer <- c(list_bbmm_pols_summer, list_bbmm_pols_summer_2017_18)

# save
save(list_bbmm_pols_summer, file = "output/list_bbmm_pols_summer.RData")

#-----
# autumn

# transform dataset into adehabitatLT object

# autumn
mov_track_autumn_2h <- mov_track_autumn_2h %>% 
  dplyr::rename(x = x_, y = y_)
mov_track_autumn_2h_ltraj <- as.ltraj(xy = mov_track_autumn_2h[c("x", "y")], 
                                      date = mov_track_autumn_2h$t_,
                                      id = mov_track_autumn_2h$id,
                                      burst = mov_track_autumn_2h$burst,
                                      proj4string = sp::CRS(crs_to_use))


# plot(ref.raster)
# set raster as a reference for BBMM analysis
grid <- as(ref.raster, "SpatialPixels")
sp::bbox(grid)

# take a look at the cleaned data
mov_track_autumn_2h_ltraj

# calculating the parameters for the Brownian Bridge analysis
cat(paste("Calculates sigma1 based on sigma2...\n"))
sig2 <- 20										# calculation error in the GPS
cat(paste("sig2 = ", sig2, "\n", sep=""))

# uses the function liker from adehabitatHR package to find the MLE of the parameter
# sigma1
renLiker <- liker(mov_track_autumn_2h_ltraj, sig2, byburst = TRUE, rangesig1 = c(0,20), plotit=F)	# Räknar ut sig1
hist(sapply(renLiker, function(x) x$sig1), ncla=20, col="grey")

# get the mean value to use in the analysis
renMedian <- median(sapply(renLiker, function(x) x$sig1)) 
cat(paste("Parameters for BBMM:\n",
          "Average for sig1 = ", renMedian, "\n", 
          "sig2 = ", sig2, "\n", sep=""))

# calculate BBMM for all individuals
# to make the calculations feasible in a short time, we separated the individuals in 
# multiple R sessions and ran them in parallel. After that, we saved each run as 
# separate RData files, loaded them all, and integrated everything in a single object

# run BBMM
cat(paste("Calculating BBMM homerange. Start time:",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n\n"))
# bbmm_autumn_1_50 <- kernelbb(mov_track_autumn_2h_ltraj[1:50], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
# bbmm_autumn_51_150 <- kernelbb(mov_track_autumn_2h_ltraj[51:150], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
# bbmm_autumn_151_end <- kernelbb(mov_track_autumn_2h_ltraj[151:length(mov_track_autumn_2h_ltraj)], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
cat(paste("Calculating BBMM homerange. End time::",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n\n"))

# run BBMM for individuals of 2017-28
rows_2017_18 <- summary(mov_track_autumn_2h_ltraj) %>% 
  tibble::rownames_to_column() %>% 
  dplyr::filter(grepl("2017|2018", id)) %>% 
  pull(rowname) %>% 
  as.numeric()

cat(paste("Calculating BBMM homerange. Start time:",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n\n"))
bbmm_autumn_2017_18 <- kernelbb(mov_track_autumn_2h_ltraj[rows_2017_18], sig1 = renMedian, sig2 = 20, byburst = TRUE, grid = grid)
cat(paste("Calculating BBMM homerange. End time::",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n\n"))

# load("output/old/bbmm_autumn.RData")
# save(bbmm_autumn_1_50, bbmm_autumn_51_150, bbmm_autumn_151_end, bbmm_autumn_2017_18, file = "output/bbmm_autumn.RData")

# extract 99% polygons
bbmm_autumn_all <- c(bbmm_autumn_1_50,
                     bbmm_autumn_51_150, 
                     bbmm_autumn_151_end,
                     bbmm_autumn_2017_18)
# bbmm_autumn_all <- bbmm_autumn_2017_18
class(bbmm_autumn_all) <- "estUDm"

# extract 95% polygons and save as vector files
dir_out <- "results/bbmm_autumn"
if(!dir.exists(dir_out)) dir.create(dir_out)

# loop for each individuals
ind_info <- summary(mov_track_autumn_2h_ltraj)[rows_2017_18,]
ind_info

inds <- as.character(unique(ind_info$id))

list_bbmm_pols_autumn <- list()
for(i in inds) {
  
  print(i)
  # select rows
  rows <- which(ind_info$id == i)
  
  # select UD object
  if(length(rows) == 1) {
    ud <- bbmm_autumn_all[[rows]]
  } else {
    ud <- bbmm_autumn_all[rows]
    class(ud) <- "estUDm"
  }
  
  # get polygon and transform it into sf
  pol <- getverticeshr(ud, percent = 99) %>% 
    as("sf") %>% 
    dplyr::mutate(id = i)
  
  # save in list
  list_bbmm_pols_autumn[[i]] <- pol
  
  # save external
  sf::st_write(pol, paste0("results/bbmm_autumn/bbmm_autumn_", i, ".gpkg"), 
               delete_dsn = T)
  
  # plot
  plot(pol[1], col = 2)
}

list_bbmm_pols_autumn

list_bbmm_pols_autumn_2017_18 <- list_bbmm_pols_autumn
load("output/list_bbmm_pols_autumn.RData")
length(list_bbmm_pols_autumn_2017_18)
length(list_bbmm_pols_autumn)
list_bbmm_pols_autumn <- c(list_bbmm_pols_autumn, list_bbmm_pols_autumn_2017_18)

# save
save(list_bbmm_pols_autumn, file = "output/list_bbmm_pols_autumn.RData")
