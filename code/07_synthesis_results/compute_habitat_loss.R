#---------------------
# Computing habitat loss and human footprint for the Mala case

#--------------------
# compute habitat loss / proportional loss in suitability

install.load::install_load("terra", "arrow")
library(tmap)
library(ggplot2)

# buffer around turbines
wt <- terra::vect("maps/shp/wind_turbines.shp")
wt_area <- terra::buffer(wt, width = 200) |> 
  terra::aggregate()
mine <- terra::vect("maps/shp/mine.shp") |> 
  terra::project(wt)
area_calving <- terra::vect("maps/availability_mala_calving_summer.gpkg") |> 
  terra::project(wt)

# buffer
mine_buff_calving <- terra::buffer(mine, width = 10000) |>
  intersect(area_calving)
wt_buff_calving <- terra::buffer(wt, width = 5000) |> 
  terra::aggregate() |> 
  intersect(area_calving)
mine_wt_buff_calving <- terra::union(mine_buff_calving, wt_buff_calving) |> 
  terra::aggregate()

# plot
ggplot() +
  geom_sf(data = sf::st_as_sf(area_calving), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_area), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_calving), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_buff_calving), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_buff_calving), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_calving), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_wt_buff_calving), fill = "red", color = "red")

# read maps
f_m <- list.files("output/rsf_spatial_predictions/", pattern = "calving", full.names = TRUE) |> 
  grep(pattern = ".tif$", value = TRUE) |> 
  grep(pattern = "grouped", value = TRUE) |> 
  grep(pattern = "difference", value = TRUE, invert = TRUE)

#-----------
# calving

# read maps calving
maps_calving <- terra::rast(f_m)

# cap 97.5%
cap975_before_calving <- terra::global(maps_calving[[1]], quantile, probs = 0.975, na.rm = TRUE)[1,1]
maps_calving_cap975 <- terra::ifel(maps_calving > cap975_before_calving, cap975_before_calving, maps_calving)/cap975_before_calving

cell_area <- cellSize(maps_calving[[1]], unit = "km", mask = TRUE)

#-----
# compute habitat suitability reduction

#---
# without cap - whole area
# this does not account for the increased RSF scores far from the turbines
# so it mixes losses and gains

# # prop
# summ_calv <- terra::global(maps_calving*cell_area, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_whole <- 100*(1 - summ_calv$sum[c(2,3)]/summ_calv$sum[1]))
# 
# # area
# (summ_calv$sum[c(2,3)] - summ_calv$sum[1])
# 
# #---
# # without cap - within the ZOI
# # here we compute only in the impacted area, so maybe it makes more sense because
# # we do not mix relative loss and gain in suitability
# maps_calving_zoi <- maps_calving |> 
#   terra::crop(wt_buff, mask = TRUE)
# plot(maps_calving_zoi[[c(1,2)]])
# 
# cell_area_zoi <- cell_area |> 
#   terra::crop(wt_buff, mask = TRUE)
# 
# # prop
# summ_calv_zoi <- terra::global(maps_calving_zoi*cell_area_zoi, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_zoi <- 100*(1 - summ_calv_zoi$sum[c(2,3)]/summ_calv_zoi$sum[1]))
# 
# # area
# (summ_calv_zoi$sum[c(2,3)] - summ_calv_zoi$sum[1])
# 
# #---
# # with a cap, to compute area, whole availability area
# # here we assume everything with suitability above the 97.5% percentile of the baseline/before period is good habitat, 
# # with value 1. This means that suitability cannot be higher than that, we are now accounting
# # only for the lost areas and not for the gains
# cap975_before <- terra::global(maps_calving[[1]], quantile, probs = 0.975, na.rm = TRUE)[1,1]
# maps_calving_cap975 <- terra::ifel(maps_calving > cap975_before, cap975_before, maps_calving)/cap975_before
# 
# summ_calv_cap <- terra::global(maps_calving_cap975*cell_area, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_cap_whole <- 100*(1 - summ_calv_cap$sum[c(2,3)]/summ_calv_cap$sum[1]))
# 
# # area
# (summ_calv_cap$sum[c(2,3)] - summ_calv_cap$sum[1])
# 
# #---
# # with a cap, to compute area, within the ZOI
# maps_calving_zoi_cap <- maps_calving_cap975 |> 
#   terra::crop(wt_buff, mask = TRUE)
# plot(maps_calving_zoi_cap[[c(1,2)]])
# 
# # prop
# summ_calv_cap_zoi <- terra::global(maps_calving_zoi_cap*cell_area_zoi, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_cap_zoi <- 100*(1 - summ_calv_cap_zoi$sum[c(2,3)]/summ_calv_cap_zoi$sum[1]))
# 
# # area
# (summ_calv_cap_zoi$sum[c(2,3)] - summ_calv_cap_zoi$sum[1])

#-----

# input <- maps_calving
input <- maps_calving_cap975

# Separate decreases and increases before summing
sum_r <- function(x) {
  terra::global(x, "sum", na.rm = TRUE)[1, 1]
}

baseline_total <- sum_r(input[[1]] * cell_area)

decreases <- terra::clamp(input[[1]] - input[[c(2,3)]], lower = 0)
increases <- terra::clamp(input[[c(2,3)]] - input[[1]], lower = 0)

names(decreases) <- c("construction", "operation")
plot(decreases)
plot(increases)

decrease_total_during <- sum_r(decreases[[1]] * cell_area)
decrease_total_after <- sum_r(decreases[[2]] * cell_area)
increase_total_during <- sum_r(increases[[1]] * cell_area)
increase_total_after <- sum_r(increases[[2]] * cell_area)

# proportion
100 * c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
) / baseline_total

# area and prop
table_calving <- data.frame(c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
)) |> 
  dplyr::rename(area_km2 = 1) |> 
  dplyr::mutate(prop = 100*area_km2/baseline_total)

# computing values

area_available_calving <- terra::expanse(area_calving, unit = "km")
area_mine_calving <- terra::expanse(mine, unit = "km")
prop_mine_calving <- 100*area_mine/area_available_calving
area_mine_zoi_calving <- terra::expanse(mine_buff_calving, unit = "km")
prop_mine_zoi_calving <- 100*area_mine_zoi_calving/area_available_calving
area_turbines_calving <- terra::expanse(wt_area, unit = "km")
prop_turbines_calving <- 100*area_turbines_calving/area_available_calving
area_zoi_turbines_calving <- terra::expanse(wt_buff_calving, unit = "km")
prop_zoi_turbines_calving <- 100*area_zoi_turbines_calving/area_available_calving
area_zoi_combined_calving <- terra::expanse(mine_wt_buff_calving, unit = "km")
prop_zoi_combined_calving <- 100*area_zoi_combined_calving/area_available_calving


#----------------------------------------------------------
# summer 

# buffer around turbines
area_summer <- terra::vect("maps/availability_mala_late_summer.gpkg") |> 
  terra::project(wt)

# buffer
mine_buff_summer <- terra::buffer(mine, width = 10000) |>
  intersect(area_summer)
wt_buff_summer <- terra::buffer(wt, width = 10000) |> 
  terra::aggregate() |> 
  intersect(area_summer)
mine_wt_buff_summer <- terra::union(mine_buff_summer, wt_buff_summer) |> 
  terra::aggregate()

# plot
ggplot() +
  geom_sf(data = sf::st_as_sf(area_summer), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_area), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_summer), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_buff_summer), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_buff_summer), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_summer), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_wt_buff_summer), fill = "red", color = "red")

# computing values

area_available_summer <- terra::expanse(area_summer, unit = "km")
area_mine_summer <- terra::expanse(mine, unit = "km")
prop_mine_summer <- 100*area_mine/area_available_summer
area_mine_zoi_summer <- terra::expanse(mine_buff_summer, unit = "km")
prop_mine_zoi_summer <- 100*area_mine_zoi_summer/area_available_summer
area_turbines_summer <- terra::expanse(wt_area, unit = "km")
prop_turbines_summer <- 100*area_turbines_summer/area_available_summer
area_zoi_turbines_summer <- NA#terra::expanse(wt_buff_summer, unit = "km")
prop_zoi_turbines_summer <- NA#100*area_zoi_turbines_summer/area_available_summer
area_zoi_combined_summer <- NA
prop_zoi_combined_summer <- NA

#----------------------------------------------------------
# autumn 

# buffer around turbines
area_autumn <- terra::vect("maps/availability_mala_autumn.gpkg") |> 
  terra::project(wt)

# buffer
mine_buff_autumn <- terra::buffer(mine, width = 10000) |>
  intersect(area_autumn)
wt_buff_autumn <- terra::buffer(wt, width = 10000) |> 
  terra::aggregate() |> 
  intersect(area_autumn)
mine_wt_buff_autumn <- terra::union(mine_buff_autumn, wt_buff_autumn) |> 
  terra::aggregate()

# plot
ggplot() +
  geom_sf(data = sf::st_as_sf(area_autumn), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_area), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_autumn), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_buff_autumn), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_buff_autumn), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_autumn), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_wt_buff_autumn), fill = "red", color = "red")

# read maps
f_m <- list.files("output/rsf_spatial_predictions/", pattern = "autumn", full.names = TRUE) |> 
  grep(pattern = ".tif$", value = TRUE) |> 
  grep(pattern = "grouped", value = TRUE) |> 
  grep(pattern = "difference", value = TRUE, invert = TRUE)

# read maps autumn
maps_autumn <- terra::rast(f_m)

# cap 97.5%
cap975_before_autumn <- terra::global(maps_autumn[[1]], quantile, probs = 0.975, na.rm = TRUE)[1,1]
maps_autumn_cap975 <- terra::ifel(maps_autumn > cap975_before_autumn, cap975_before_autumn, maps_autumn)/cap975_before_autumn

cell_area <- cellSize(maps_autumn[[1]], unit = "km", mask = TRUE)

#-----
# compute habitat suitability reduction

# input <- maps_autumn
input <- maps_autumn_cap975

# Separate decreases and increases before summing
sum_r <- function(x) {
  terra::global(x, "sum", na.rm = TRUE)[1, 1]
}

baseline_total <- sum_r(input[[1]] * cell_area)

decreases <- terra::clamp(input[[1]] - input[[c(2,3)]], lower = 0)
increases <- terra::clamp(input[[c(2,3)]] - input[[1]], lower = 0)

plot(decreases)
plot(increases)

decrease_total_during <- sum_r(decreases[[1]] * cell_area)
decrease_total_after <- sum_r(decreases[[2]] * cell_area)
increase_total_during <- sum_r(increases[[1]] * cell_area)
increase_total_after <- sum_r(increases[[2]] * cell_area)

# proportion
100 * c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
) / baseline_total

# area and prop
table_autumn <- data.frame(c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
)) |> 
  dplyr::rename(area_km2 = 1) |> 
  dplyr::mutate(prop = 100*area_km2/baseline_total)

# computing values

area_available_autumn <- terra::expanse(area_autumn, unit = "km")
area_mine_autumn <- terra::expanse(mine, unit = "km")
prop_mine_autumn <- 100*area_mine/area_available_autumn
area_mine_zoi_autumn <- terra::expanse(mine_buff_autumn, unit = "km")
prop_mine_zoi_autumn <- 100*area_mine_zoi_autumn/area_available_autumn
area_turbines_autumn <- terra::expanse(wt_area, unit = "km")
prop_turbines_autumn <- 100*area_turbines_autumn/area_available_autumn
area_zoi_turbines_autumn <- terra::expanse(wt_buff_autumn, unit = "km")
prop_zoi_turbines_autumn <- 100*area_zoi_turbines_autumn/area_available_autumn
area_zoi_combined_autumn <- terra::expanse(mine_wt_buff_autumn, unit = "km")
prop_zoi_combined_autumn <- 100*area_zoi_combined_autumn/area_available_autumn

#-----------------------------
# final table

table_habitat_change <- tibble::tribble(
  ~season, ~area_mining_prop, ~area_mining_zoi_prop, ~area_turbines_200m_prop, ~area_turbines_zoi_prop, ~area_zoi_combined_prop, ~equivalent_area_lost_prop_during, ~equivalent_area_lost_prop_after,
  "Calving", 
  paste0(round(area_mine_calving, digits = 3), " (", round(prop_mine_calving, digits = 3), "%)"), 
  paste0(round(area_mine_zoi_calving, digits = 3), " (", round(prop_mine_zoi_calving, digits = 3), "%)"), 
  paste0(round(area_turbines_calving, digits = 3), " (", round(prop_turbines_calving, digits = 3), "%)"),
  paste0(round(area_zoi_turbines, digits = 3), " (", round(prop_zoi_turbines, digits = 3), "%)"),
  paste0(round(area_zoi_combined, digits = 3), " (", round(prop_zoi_combined, digits = 3), "%)"), 
  paste0(round(table_calving$area_km2[1], digits = 3), " (", round(table_calving$prop[1], digits = 3), "%)"), 
  paste0(round(table_calving$area_km2[4], digits = 3), " (", round(table_calving$prop[4], digits = 3), "%)"),
  "Summer", 
  paste0(round(area_mine_summer, digits = 3), " (", round(prop_mine_summer, digits = 3), "%)"), 
  paste0(round(area_mine_zoi_summer, digits = 3), " (", round(prop_mine_zoi_summer, digits = 3), "%)"), 
  paste0(round(area_turbines_summer, digits = 3), " (", round(prop_turbines_summer, digits = 3), "%)"),
  NA,#paste0(round(area_zoi_turbines_summer, digits = 3), " (", round(prop_zoi_turbines_summer, digits = 3), ")"),
  NA,#paste0(round(area_zoi_combined_summer, digits = 3), " (", round(prop_zoi_combined_summer, digits = 3), ")"), 
  NA, NA,
  "Autumn", 
  paste0(round(area_mine_autumn, digits = 3), " (", round(prop_mine_autumn, digits = 3), "%)"), 
  paste0(round(area_mine_zoi_autumn, digits = 3), " (", round(prop_mine_zoi_autumn, digits = 3), "%)"), 
  paste0(round(area_turbines_autumn, digits = 3), " (", round(prop_turbines_autumn, digits = 3), "%)"),
  paste0(round(area_zoi_turbines_autumn, digits = 3), " (", round(prop_zoi_turbines_autumn, digits = 3), ")"),
  paste0(round(area_zoi_combined_autumn, digits = 3), " (", round(prop_zoi_combined_autumn, digits = 3), ")"),
  paste0(round(table_autumn$area_km2[1], digits = 3), " (", round(table_autumn$prop[1], digits = 3), "%)"), 
  paste0(round(table_autumn$area_km2[4], digits = 3), " (", round(table_autumn$prop[4], digits = 3), "%)"),
)
table_habitat_change

readr::write_csv(table_habitat_change, file = "output/table_habitat_change_all_seasons.csv")


#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# Do the same and use 10%

#--------------------
# compute habitat loss / proportional loss in suitability

install.load::install_load("terra", "arrow")
library(tmap)
library(ggplot2)

# buffer around turbines
wt <- terra::vect("maps/shp/wind_turbines.shp")
wt_area <- terra::buffer(wt, width = 200) |> 
  terra::aggregate()
mine <- terra::vect("maps/shp/mine.shp") |> 
  terra::project(wt)
area_calving <- terra::vect("maps/availability_mala_calving_summer.gpkg") |> 
  terra::project(wt)

# buffer
mine_buff_calving <- terra::buffer(mine, width = 10000) |>
  intersect(area_calving)
wt_buff_calving <- terra::buffer(wt, width = 5000) |> 
  terra::aggregate() |> 
  intersect(area_calving)
mine_wt_buff_calving <- terra::union(mine_buff_calving, wt_buff_calving) |> 
  terra::aggregate()

# plot
ggplot() +
  geom_sf(data = sf::st_as_sf(area_calving), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_area), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_calving), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_buff_calving), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_buff_calving), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_calving), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_wt_buff_calving), fill = "red", color = "red")

# read maps
f_m <- list.files("output/rsf_spatial_predictions/", pattern = "calving", full.names = TRUE) |> 
  grep(pattern = ".tif$", value = TRUE) |> 
  grep(pattern = "grouped", value = TRUE) |> 
  grep(pattern = "difference", value = TRUE, invert = TRUE)

#-----------
# calving

# read maps calving
maps_calving <- terra::rast(f_m)

# cap 9%
cap975_before_calving <- terra::global(maps_calving[[1]], quantile, probs = 0.9, na.rm = TRUE)[1,1]
maps_calving_cap975 <- terra::ifel(maps_calving > cap975_before_calving, cap975_before_calving, maps_calving)/cap975_before_calving

cell_area <- cellSize(maps_calving[[1]], unit = "km", mask = TRUE)

#-----
# compute habitat suitability reduction

#---
# without cap - whole area
# this does not account for the increased RSF scores far from the turbines
# so it mixes losses and gains

# # prop
# summ_calv <- terra::global(maps_calving*cell_area, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_whole <- 100*(1 - summ_calv$sum[c(2,3)]/summ_calv$sum[1]))
# 
# # area
# (summ_calv$sum[c(2,3)] - summ_calv$sum[1])
# 
# #---
# # without cap - within the ZOI
# # here we compute only in the impacted area, so maybe it makes more sense because
# # we do not mix relative loss and gain in suitability
# maps_calving_zoi <- maps_calving |> 
#   terra::crop(wt_buff, mask = TRUE)
# plot(maps_calving_zoi[[c(1,2)]])
# 
# cell_area_zoi <- cell_area |> 
#   terra::crop(wt_buff, mask = TRUE)
# 
# # prop
# summ_calv_zoi <- terra::global(maps_calving_zoi*cell_area_zoi, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_zoi <- 100*(1 - summ_calv_zoi$sum[c(2,3)]/summ_calv_zoi$sum[1]))
# 
# # area
# (summ_calv_zoi$sum[c(2,3)] - summ_calv_zoi$sum[1])
# 
# #---
# # with a cap, to compute area, whole availability area
# # here we assume everything with suitability above the 97.5% percentile of the baseline/before period is good habitat, 
# # with value 1. This means that suitability cannot be higher than that, we are now accounting
# # only for the lost areas and not for the gains
# cap975_before <- terra::global(maps_calving[[1]], quantile, probs = 0.975, na.rm = TRUE)[1,1]
# maps_calving_cap975 <- terra::ifel(maps_calving > cap975_before, cap975_before, maps_calving)/cap975_before
# 
# summ_calv_cap <- terra::global(maps_calving_cap975*cell_area, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_cap_whole <- 100*(1 - summ_calv_cap$sum[c(2,3)]/summ_calv_cap$sum[1]))
# 
# # area
# (summ_calv_cap$sum[c(2,3)] - summ_calv_cap$sum[1])
# 
# #---
# # with a cap, to compute area, within the ZOI
# maps_calving_zoi_cap <- maps_calving_cap975 |> 
#   terra::crop(wt_buff, mask = TRUE)
# plot(maps_calving_zoi_cap[[c(1,2)]])
# 
# # prop
# summ_calv_cap_zoi <- terra::global(maps_calving_zoi_cap*cell_area_zoi, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_cap_zoi <- 100*(1 - summ_calv_cap_zoi$sum[c(2,3)]/summ_calv_cap_zoi$sum[1]))
# 
# # area
# (summ_calv_cap_zoi$sum[c(2,3)] - summ_calv_cap_zoi$sum[1])

#-----

# input <- maps_calving
input <- maps_calving_cap975

# Separate decreases and increases before summing
sum_r <- function(x) {
  terra::global(x, "sum", na.rm = TRUE)[1, 1]
}

baseline_total <- sum_r(input[[1]] * cell_area)

decreases <- terra::clamp(input[[1]] - input[[c(2,3)]], lower = 0)
increases <- terra::clamp(input[[c(2,3)]] - input[[1]], lower = 0)

names(decreases) <- c("construction", "operation")
plot(decreases)
plot(increases)

decrease_total_during <- sum_r(decreases[[1]] * cell_area)
decrease_total_after <- sum_r(decreases[[2]] * cell_area)
increase_total_during <- sum_r(increases[[1]] * cell_area)
increase_total_after <- sum_r(increases[[2]] * cell_area)

# proportion
100 * c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
) / baseline_total

# area and prop
table_calving <- data.frame(c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
)) |> 
  dplyr::rename(area_km2 = 1) |> 
  dplyr::mutate(prop = 100*area_km2/baseline_total)

# computing values

area_available_calving <- terra::expanse(area_calving, unit = "km")
area_mine_calving <- terra::expanse(mine, unit = "km")
prop_mine_calving <- 100*#--------------------
# compute habitat loss / proportional loss in suitability

install.load::install_load("terra", "arrow")
library(tmap)
library(ggplot2)

# buffer around turbines
wt <- terra::vect("maps/shp/wind_turbines.shp")
wt_area <- terra::buffer(wt, width = 200) |> 
  terra::aggregate()
mine <- terra::vect("maps/shp/mine.shp") |> 
  terra::project(wt)
area_calving <- terra::vect("maps/availability_mala_calving_summer.gpkg") |> 
  terra::project(wt)

# buffer
mine_buff_calving <- terra::buffer(mine, width = 10000) |>
  intersect(area_calving)
wt_buff_calving <- terra::buffer(wt, width = 5000) |> 
  terra::aggregate() |> 
  intersect(area_calving)
mine_wt_buff_calving <- terra::union(mine_buff_calving, wt_buff_calving) |> 
  terra::aggregate()

# plot
ggplot() +
  geom_sf(data = sf::st_as_sf(area_calving), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_area), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_calving), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_buff_calving), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_buff_calving), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_calving), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_wt_buff_calving), fill = "red", color = "red")

# read maps
f_m <- list.files("output/rsf_spatial_predictions/", pattern = "calving", full.names = TRUE) |> 
  grep(pattern = ".tif$", value = TRUE) |> 
  grep(pattern = "grouped", value = TRUE) |> 
  grep(pattern = "difference", value = TRUE, invert = TRUE)

#-----------
# calving

# read maps calving
maps_calving <- terra::rast(f_m)

# cap 97.5%
cap975_before_calving <- terra::global(maps_calving[[1]], quantile, probs = 0.975, na.rm = TRUE)[1,1]
maps_calving_cap975 <- terra::ifel(maps_calving > cap975_before_calving, cap975_before_calving, maps_calving)/cap975_before_calving

cell_area <- cellSize(maps_calving[[1]], unit = "km", mask = TRUE)

#-----
# compute habitat suitability reduction

#---
# without cap - whole area
# this does not account for the increased RSF scores far from the turbines
# so it mixes losses and gains

# # prop
# summ_calv <- terra::global(maps_calving*cell_area, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_whole <- 100*(1 - summ_calv$sum[c(2,3)]/summ_calv$sum[1]))
# 
# # area
# (summ_calv$sum[c(2,3)] - summ_calv$sum[1])
# 
# #---
# # without cap - within the ZOI
# # here we compute only in the impacted area, so maybe it makes more sense because
# # we do not mix relative loss and gain in suitability
# maps_calving_zoi <- maps_calving |> 
#   terra::crop(wt_buff, mask = TRUE)
# plot(maps_calving_zoi[[c(1,2)]])
# 
# cell_area_zoi <- cell_area |> 
#   terra::crop(wt_buff, mask = TRUE)
# 
# # prop
# summ_calv_zoi <- terra::global(maps_calving_zoi*cell_area_zoi, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_zoi <- 100*(1 - summ_calv_zoi$sum[c(2,3)]/summ_calv_zoi$sum[1]))
# 
# # area
# (summ_calv_zoi$sum[c(2,3)] - summ_calv_zoi$sum[1])
# 
# #---
# # with a cap, to compute area, whole availability area
# # here we assume everything with suitability above the 97.5% percentile of the baseline/before period is good habitat, 
# # with value 1. This means that suitability cannot be higher than that, we are now accounting
# # only for the lost areas and not for the gains
# cap975_before <- terra::global(maps_calving[[1]], quantile, probs = 0.975, na.rm = TRUE)[1,1]
# maps_calving_cap975 <- terra::ifel(maps_calving > cap975_before, cap975_before, maps_calving)/cap975_before
# 
# summ_calv_cap <- terra::global(maps_calving_cap975*cell_area, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_cap_whole <- 100*(1 - summ_calv_cap$sum[c(2,3)]/summ_calv_cap$sum[1]))
# 
# # area
# (summ_calv_cap$sum[c(2,3)] - summ_calv_cap$sum[1])
# 
# #---
# # with a cap, to compute area, within the ZOI
# maps_calving_zoi_cap <- maps_calving_cap975 |> 
#   terra::crop(wt_buff, mask = TRUE)
# plot(maps_calving_zoi_cap[[c(1,2)]])
# 
# # prop
# summ_calv_cap_zoi <- terra::global(maps_calving_zoi_cap*cell_area_zoi, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_cap_zoi <- 100*(1 - summ_calv_cap_zoi$sum[c(2,3)]/summ_calv_cap_zoi$sum[1]))
# 
# # area
# (summ_calv_cap_zoi$sum[c(2,3)] - summ_calv_cap_zoi$sum[1])

#-----

# input <- maps_calving
input <- maps_calving_cap975

# Separate decreases and increases before summing
sum_r <- function(x) {
  terra::global(x, "sum", na.rm = TRUE)[1, 1]
}

baseline_total <- sum_r(input[[1]] * cell_area)

decreases <- terra::clamp(input[[1]] - input[[c(2,3)]], lower = 0)
increases <- terra::clamp(input[[c(2,3)]] - input[[1]], lower = 0)

plot(decreases)
plot(increases)

decrease_total_during <- sum_r(decreases[[1]] * cell_area)
decrease_total_after <- sum_r(decreases[[2]] * cell_area)
increase_total_during <- sum_r(increases[[1]] * cell_area)
increase_total_after <- sum_r(increases[[2]] * cell_area)

# proportion
100 * c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
) / baseline_total

# area and prop
table_calving <- data.frame(c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
)) |> 
  dplyr::rename(area_km2 = 1) |> 
  dplyr::mutate(prop = 100*area_km2/baseline_total)

# computing values

area_available_calving <- terra::expanse(area_calving, unit = "km")
area_mine_calving <- terra::expanse(mine, unit = "km")
prop_mine_calving <- 100*area_mine_calving/area_available_calving
area_mine_zoi_calving <- terra::expanse(mine_buff_calving, unit = "km")
prop_mine_zoi_calving <- 100*area_mine_zoi_calving/area_available_calving
area_turbines_calving <- terra::expanse(wt_area, unit = "km")
prop_turbines_calving <- 100*area_turbines_calving/area_available_calving
area_zoi_turbines_calving <- terra::expanse(wt_buff_calving, unit = "km")
prop_zoi_turbines_calving <- 100*area_zoi_turbines_calving/area_available_calving
area_zoi_combined_calving <- terra::expanse(mine_wt_buff_calving, unit = "km")
prop_zoi_combined_calving <- 100*area_zoi_combined_calving/area_available_calving


#----------------------------------------------------------
# summer 

# buffer around turbines
area_summer <- terra::vect("maps/availability_mala_late_summer.gpkg") |> 
  terra::project(wt)

# buffer
mine_buff_summer <- terra::buffer(mine, width = 10000) |>
  intersect(area_summer)
wt_buff_summer <- terra::buffer(wt, width = 10000) |> 
  terra::aggregate() |> 
  intersect(area_summer)
mine_wt_buff_summer <- terra::union(mine_buff_summer, wt_buff_summer) |> 
  terra::aggregate()

# plot
ggplot() +
  geom_sf(data = sf::st_as_sf(area_summer), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_area), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_summer), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_buff_summer), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_buff_summer), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_summer), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_wt_buff_summer), fill = "red", color = "red")

# computing values

area_available_summer <- terra::expanse(area_summer, unit = "km")
area_mine_summer <- terra::expanse(mine, unit = "km")
prop_mine_summer <- 100*area_mine_summer/area_available_summer
area_mine_zoi_summer <- terra::expanse(mine_buff_summer, unit = "km")
prop_mine_zoi_summer <- 100*area_mine_zoi_summer/area_available_summer
area_turbines_summer <- terra::expanse(wt_area, unit = "km")
prop_turbines_summer <- 100*area_turbines_summer/area_available_summer
area_zoi_turbines_summer <- NA#terra::expanse(wt_buff_summer, unit = "km")
prop_zoi_turbines_summer <- NA#100*area_zoi_turbines_summer/area_available_summer
area_zoi_combined_summer <- NA
prop_zoi_combined_summer <- NA

#----------------------------------------------------------
# autumn 

# buffer around turbines
area_autumn <- terra::vect("maps/availability_mala_autumn.gpkg") |> 
  terra::project(wt)

# buffer
mine_buff_autumn <- terra::buffer(mine, width = 10000) |>
  intersect(area_autumn)
wt_buff_autumn <- terra::buffer(wt, width = 10000) |> 
  terra::aggregate() |> 
  intersect(area_autumn)
mine_wt_buff_autumn <- terra::union(mine_buff_autumn, wt_buff_autumn) |> 
  terra::aggregate()

# plot
ggplot() +
  geom_sf(data = sf::st_as_sf(area_autumn), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_area), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_autumn), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_buff_autumn), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_buff_autumn), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_autumn), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_wt_buff_autumn), fill = "red", color = "red")

# read maps
f_m <- list.files("output/rsf_spatial_predictions/", pattern = "autumn", full.names = TRUE) |> 
  grep(pattern = ".tif$", value = TRUE) |> 
  grep(pattern = "grouped", value = TRUE) |> 
  grep(pattern = "difference", value = TRUE, invert = TRUE)

# read maps autumn
maps_autumn <- terra::rast(f_m)

# cap 9%
cap975_before_autumn <- terra::global(maps_autumn[[1]], quantile, probs = 0.9, na.rm = TRUE)[1,1]
maps_autumn_cap975 <- terra::ifel(maps_autumn > cap975_before_autumn, cap975_before_autumn, maps_autumn)/cap975_before_autumn

cell_area <- cellSize(maps_autumn[[1]], unit = "km", mask = TRUE)

#-----
# compute habitat suitability reduction

# input <- maps_autumn
input <- maps_autumn_cap975

# Separate decreases and increases before summing
sum_r <- function(x) {
  terra::global(x, "sum", na.rm = TRUE)[1, 1]
}

baseline_total <- sum_r(input[[1]] * cell_area)

decreases <- terra::clamp(input[[1]] - input[[c(2,3)]], lower = 0)
increases <- terra::clamp(input[[c(2,3)]] - input[[1]], lower = 0)

plot(decreases)
plot(increases)

decrease_total_during <- sum_r(decreases[[1]] * cell_area)
decrease_total_after <- sum_r(decreases[[2]] * cell_area)
increase_total_during <- sum_r(increases[[1]] * cell_area)
increase_total_after <- sum_r(increases[[2]] * cell_area)

# proportion
100 * c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
) / baseline_total

# area and prop
table_autumn <- data.frame(c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
)) |> 
  dplyr::rename(area_km2 = 1) |> 
  dplyr::mutate(prop = 100*area_km2/baseline_total)

# computing values

area_available_autumn <- terra::expanse(area_autumn, unit = "km")
area_mine_autumn <- terra::expanse(mine, unit = "km")
prop_mine_autumn <- 100*area_mine_autumn/area_available_autumn
area_mine_zoi_autumn <- terra::expanse(mine_buff_autumn, unit = "km")
prop_mine_zoi_autumn <- 100*area_mine_zoi_autumn/area_available_autumn
area_turbines_autumn <- terra::expanse(wt_area, unit = "km")
prop_turbines_autumn <- 100*area_turbines_autumn/area_available_autumn
area_zoi_turbines_autumn <- terra::expanse(wt_buff_autumn, unit = "km")
prop_zoi_turbines_autumn <- 100*area_zoi_turbines_autumn/area_available_autumn
area_zoi_combined_autumn <- terra::expanse(mine_wt_buff_autumn, unit = "km")
prop_zoi_combined_autumn <- 100*area_zoi_combined_autumn/area_available_autumn

#-----------------------------
# final table

table_habitat_change <- tibble::tribble(
  ~season, ~area_mining_prop, ~area_mining_zoi_prop, ~area_turbines_200m_prop, ~area_turbines_zoi_prop, ~area_zoi_combined_prop, ~equivalent_area_lost_prop_during, ~equivalent_area_lost_prop_after,
  "Calving", 
  paste0(round(area_mine_calving, digits = 3), " (", round(prop_mine_calving, digits = 3), "%)"), 
  paste0(round(area_mine_zoi_calving, digits = 3), " (", round(prop_mine_zoi_calving, digits = 3), "%)"), 
  paste0(round(area_turbines_calving, digits = 3), " (", round(prop_turbines_calving, digits = 3), "%)"),
  paste0(round(area_zoi_turbines_calving, digits = 3), " (", round(prop_zoi_turbines_calving, digits = 3), "%)"),
  paste0(round(area_zoi_combined_calving, digits = 3), " (", round(prop_zoi_combined_calving, digits = 3), "%)"), 
  paste0(round(table_calving$area_km2[1], digits = 3), " (", round(table_calving$prop[1], digits = 3), "%)"), 
  paste0(round(table_calving$area_km2[4], digits = 3), " (", round(table_calving$prop[4], digits = 3), "%)"),
  "Summer", 
  paste0(round(area_mine_summer, digits = 3), " (", round(prop_mine_summer, digits = 3), "%)"), 
  paste0(round(area_mine_zoi_summer, digits = 3), " (", round(prop_mine_zoi_summer, digits = 3), "%)"), 
  paste0(round(area_turbines_summer, digits = 3), " (", round(prop_turbines_summer, digits = 3), "%)"),
  NA,#paste0(round(area_zoi_turbines_summer, digits = 3), " (", round(prop_zoi_turbines_summer, digits = 3), ")"),
  NA,#paste0(round(area_zoi_combined_summer, digits = 3), " (", round(prop_zoi_combined_summer, digits = 3), ")"), 
  NA, NA,
  "Autumn", 
  paste0(round(area_mine_autumn, digits = 3), " (", round(prop_mine_autumn, digits = 3), "%)"), 
  paste0(round(area_mine_zoi_autumn, digits = 3), " (", round(prop_mine_zoi_autumn, digits = 3), "%)"), 
  paste0(round(area_turbines_autumn, digits = 3), " (", round(prop_turbines_autumn, digits = 3), "%)"),
  paste0(round(area_zoi_turbines_autumn, digits = 3), " (", round(prop_zoi_turbines_autumn, digits = 3), ")"),
  paste0(round(area_zoi_combined_autumn, digits = 3), " (", round(prop_zoi_combined_autumn, digits = 3), ")"),
  paste0(round(table_autumn$area_km2[1], digits = 3), " (", round(table_autumn$prop[1], digits = 3), "%)"), 
  paste0(round(table_autumn$area_km2[4], digits = 3), " (", round(table_autumn$prop[4], digits = 3), "%)"),
)
table_habitat_change

readr::write_csv(table_habitat_change, file = "output/table_habitat_change_all_seasons_top10pct.csv")

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# Do the same and use 50%

#--------------------
# compute habitat loss / proportional loss in suitability

rm(list = ls())

install.load::install_load("terra", "arrow")
library(tmap)
library(ggplot2)

# buffer around turbines
wt <- terra::vect("maps/shp/wind_turbines.shp")
wt_area <- terra::buffer(wt, width = 200) |> 
  terra::aggregate()
mine <- terra::vect("maps/shp/mine.shp") |> 
  terra::project(wt)
area_calving <- terra::vect("maps/availability_mala_calving_summer.gpkg") |> 
  terra::project(wt)

# buffer
mine_buff_calving <- terra::buffer(mine, width = 10000) |>
  intersect(area_calving)
wt_buff_calving <- terra::buffer(wt, width = 5000) |> 
  terra::aggregate() |> 
  intersect(area_calving)
mine_wt_buff_calving <- terra::union(mine_buff_calving, wt_buff_calving) |> 
  terra::aggregate()

# plot
ggplot() +
  geom_sf(data = sf::st_as_sf(area_calving), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_area), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_calving), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_buff_calving), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_buff_calving), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_calving), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_wt_buff_calving), fill = "red", color = "red")

# read maps
f_m <- list.files("output/rsf_spatial_predictions/", pattern = "calving", full.names = TRUE) |> 
  grep(pattern = ".tif$", value = TRUE) |> 
  grep(pattern = "grouped", value = TRUE) |> 
  grep(pattern = "difference", value = TRUE, invert = TRUE)

#-----------
# calving

# read maps calving
maps_calving <- terra::rast(f_m)

# cap 50%
cap975_before_calving <- terra::global(maps_calving[[1]], quantile, probs = 0.5, na.rm = TRUE)[1,1]
maps_calving_cap975 <- terra::ifel(maps_calving > cap975_before_calving, cap975_before_calving, maps_calving)/cap975_before_calving

cell_area <- cellSize(maps_calving[[1]], unit = "km", mask = TRUE)

#-----
# compute habitat suitability reduction

#---
# without cap - whole area
# this does not account for the increased RSF scores far from the turbines
# so it mixes losses and gains

# # prop
# summ_calv <- terra::global(maps_calving*cell_area, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_whole <- 100*(1 - summ_calv$sum[c(2,3)]/summ_calv$sum[1]))
# 
# # area
# (summ_calv$sum[c(2,3)] - summ_calv$sum[1])
# 
# #---
# # without cap - within the ZOI
# # here we compute only in the impacted area, so maybe it makes more sense because
# # we do not mix relative loss and gain in suitability
# maps_calving_zoi <- maps_calving |> 
#   terra::crop(wt_buff, mask = TRUE)
# plot(maps_calving_zoi[[c(1,2)]])
# 
# cell_area_zoi <- cell_area |> 
#   terra::crop(wt_buff, mask = TRUE)
# 
# # prop
# summ_calv_zoi <- terra::global(maps_calving_zoi*cell_area_zoi, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_zoi <- 100*(1 - summ_calv_zoi$sum[c(2,3)]/summ_calv_zoi$sum[1]))
# 
# # area
# (summ_calv_zoi$sum[c(2,3)] - summ_calv_zoi$sum[1])
# 
# #---
# # with a cap, to compute area, whole availability area
# # here we assume everything with suitability above the 97.5% percentile of the baseline/before period is good habitat, 
# # with value 1. This means that suitability cannot be higher than that, we are now accounting
# # only for the lost areas and not for the gains
# cap975_before <- terra::global(maps_calving[[1]], quantile, probs = 0.975, na.rm = TRUE)[1,1]
# maps_calving_cap975 <- terra::ifel(maps_calving > cap975_before, cap975_before, maps_calving)/cap975_before
# 
# summ_calv_cap <- terra::global(maps_calving_cap975*cell_area, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_cap_whole <- 100*(1 - summ_calv_cap$sum[c(2,3)]/summ_calv_cap$sum[1]))
# 
# # area
# (summ_calv_cap$sum[c(2,3)] - summ_calv_cap$sum[1])
# 
# #---
# # with a cap, to compute area, within the ZOI
# maps_calving_zoi_cap <- maps_calving_cap975 |> 
#   terra::crop(wt_buff, mask = TRUE)
# plot(maps_calving_zoi_cap[[c(1,2)]])
# 
# # prop
# summ_calv_cap_zoi <- terra::global(maps_calving_zoi_cap*cell_area_zoi, fun = "sum", na.rm = TRUE)
# (calc_reduction_prop_cap_zoi <- 100*(1 - summ_calv_cap_zoi$sum[c(2,3)]/summ_calv_cap_zoi$sum[1]))
# 
# # area
# (summ_calv_cap_zoi$sum[c(2,3)] - summ_calv_cap_zoi$sum[1])

#-----

# input <- maps_calving
input <- maps_calving_cap975

# Separate decreases and increases before summing
sum_r <- function(x) {
  terra::global(x, "sum", na.rm = TRUE)[1, 1]
}

baseline_total <- sum_r(input[[1]] * cell_area)

decreases <- terra::clamp(input[[1]] - input[[c(2,3)]], lower = 0)
increases <- terra::clamp(input[[c(2,3)]] - input[[1]], lower = 0)

names(decreases) <- c("construction", "operation")
plot(decreases)
plot(increases)

decrease_total_during <- sum_r(decreases[[1]] * cell_area)
decrease_total_after <- sum_r(decreases[[2]] * cell_area)
increase_total_during <- sum_r(increases[[1]] * cell_area)
increase_total_after <- sum_r(increases[[2]] * cell_area)

# proportion
100 * c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
) / baseline_total

# area and prop
table_calving <- data.frame(c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
)) |> 
  dplyr::rename(area_km2 = 1) |> 
  dplyr::mutate(prop = 100*area_km2/baseline_total)

# computing values
area_available_calving <- terra::expanse(area_calving, unit = "km")
area_mine_calving <- terra::expanse(mine, unit = "km")
prop_mine_calving <- 100*area_mine_calving/area_available_calving
area_mine_zoi_calving <- terra::expanse(mine_buff_calving, unit = "km")
prop_mine_zoi_calving <- 100*area_mine_zoi_calving/area_available_calving
area_turbines_calving <- terra::expanse(wt_area, unit = "km")
prop_turbines_calving <- 100*area_turbines_calving/area_available_calving
area_zoi_turbines_calving <- terra::expanse(wt_buff_calving, unit = "km")
prop_zoi_turbines_calving <- 100*area_zoi_turbines_calving/area_available_calving
area_zoi_combined_calving <- terra::expanse(mine_wt_buff_calving, unit = "km")
prop_zoi_combined_calving <- 100*area_zoi_combined_calving/area_available_calving


#----------------------------------------------------------
# summer 

# buffer around turbines
area_summer <- terra::vect("maps/availability_mala_late_summer.gpkg") |> 
  terra::project(wt)

# buffer
mine_buff_summer <- terra::buffer(mine, width = 10000) |>
  intersect(area_summer)
wt_buff_summer <- terra::buffer(wt, width = 10000) |> 
  terra::aggregate() |> 
  intersect(area_summer)
mine_wt_buff_summer <- terra::union(mine_buff_summer, wt_buff_summer) |> 
  terra::aggregate()

# plot
ggplot() +
  geom_sf(data = sf::st_as_sf(area_summer), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_area), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_summer), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_buff_summer), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_buff_summer), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_summer), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_wt_buff_summer), fill = "red", color = "red")

# computing values

area_available_summer <- terra::expanse(area_summer, unit = "km")
area_mine_summer <- terra::expanse(mine, unit = "km")
prop_mine_summer <- 100*area_mine_summer/area_available_summer
area_mine_zoi_summer <- terra::expanse(mine_buff_summer, unit = "km")
prop_mine_zoi_summer <- 100*area_mine_zoi_summer/area_available_summer
area_turbines_summer <- terra::expanse(wt_area, unit = "km")
prop_turbines_summer <- 100*area_turbines_summer/area_available_summer
area_zoi_turbines_summer <- NA#terra::expanse(wt_buff_summer, unit = "km")
prop_zoi_turbines_summer <- NA#100*area_zoi_turbines_summer/area_available_summer
area_zoi_combined_summer <- NA
prop_zoi_combined_summer <- NA

#----------------------------------------------------------
# autumn 

# buffer around turbines
area_autumn <- terra::vect("maps/availability_mala_autumn.gpkg") |> 
  terra::project(wt)

# buffer
mine_buff_autumn <- terra::buffer(mine, width = 10000) |>
  intersect(area_autumn)
wt_buff_autumn <- terra::buffer(wt, width = 10000) |> 
  terra::aggregate() |> 
  intersect(area_autumn)
mine_wt_buff_autumn <- terra::union(mine_buff_autumn, wt_buff_autumn) |> 
  terra::aggregate()

# plot
ggplot() +
  geom_sf(data = sf::st_as_sf(area_autumn), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_area), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_autumn), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_buff_autumn), fill = "red", color = "red") +
  geom_sf(data = sf::st_as_sf(wt_buff_autumn), fill = "black", color = "black")

ggplot() +
  geom_sf(data = sf::st_as_sf(area_autumn), fill = "grey", color = "grey") +
  geom_sf(data = sf::st_as_sf(mine_wt_buff_autumn), fill = "red", color = "red")

# read maps
f_m <- list.files("output/rsf_spatial_predictions/", pattern = "autumn", full.names = TRUE) |> 
  grep(pattern = ".tif$", value = TRUE) |> 
  grep(pattern = "grouped", value = TRUE) |> 
  grep(pattern = "difference", value = TRUE, invert = TRUE)

# read maps autumn
maps_autumn <- terra::rast(f_m)

# cap 9%
cap975_before_autumn <- terra::global(maps_autumn[[1]], quantile, probs = 0.5, na.rm = TRUE)[1,1]
maps_autumn_cap975 <- terra::ifel(maps_autumn > cap975_before_autumn, cap975_before_autumn, maps_autumn)/cap975_before_autumn

cell_area <- cellSize(maps_autumn[[1]], unit = "km", mask = TRUE)

#-----
# compute habitat suitability reduction

# input <- maps_autumn
input <- maps_autumn_cap975

# Separate decreases and increases before summing
sum_r <- function(x) {
  terra::global(x, "sum", na.rm = TRUE)[1, 1]
}

baseline_total <- sum_r(input[[1]] * cell_area)

decreases <- terra::clamp(input[[1]] - input[[c(2,3)]], lower = 0)
increases <- terra::clamp(input[[c(2,3)]] - input[[1]], lower = 0)

plot(decreases)
plot(increases)

decrease_total_during <- sum_r(decreases[[1]] * cell_area)
decrease_total_after <- sum_r(decreases[[2]] * cell_area)
increase_total_during <- sum_r(increases[[1]] * cell_area)
increase_total_after <- sum_r(increases[[2]] * cell_area)

# proportion
100 * c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
) / baseline_total

# area and prop
table_autumn <- data.frame(c(
  gross_decrease_pct_during = decrease_total_during,
  gross_increase_pct_during = increase_total_during,
  net_decrease_pct_during   = decrease_total_during - increase_total_during,
  gross_decrease_pct_after = decrease_total_after,
  gross_increase_pct_after = increase_total_after,
  net_decrease_pct_after   = decrease_total_after - increase_total_after
)) |> 
  dplyr::rename(area_km2 = 1) |> 
  dplyr::mutate(prop = 100*area_km2/baseline_total)

# computing values

area_available_autumn <- terra::expanse(area_autumn, unit = "km")
area_mine_autumn <- terra::expanse(mine, unit = "km")
prop_mine_autumn <- 100*area_mine_autumn/area_available_autumn
area_mine_zoi_autumn <- terra::expanse(mine_buff_autumn, unit = "km")
prop_mine_zoi_autumn <- 100*area_mine_zoi_autumn/area_available_autumn
area_turbines_autumn <- terra::expanse(wt_area, unit = "km")
prop_turbines_autumn <- 100*area_turbines_autumn/area_available_autumn
area_zoi_turbines_autumn <- terra::expanse(wt_buff_autumn, unit = "km")
prop_zoi_turbines_autumn <- 100*area_zoi_turbines_autumn/area_available_autumn
area_zoi_combined_autumn <- terra::expanse(mine_wt_buff_autumn, unit = "km")
prop_zoi_combined_autumn <- 100*area_zoi_combined_autumn/area_available_autumn

#-----------------------------
# final table

table_habitat_change <- tibble::tribble(
  ~season, ~area_mining_prop, ~area_mining_zoi_prop, ~area_turbines_200m_prop, ~area_turbines_zoi_prop, ~area_zoi_combined_prop, ~equivalent_area_lost_prop_during, ~equivalent_area_lost_prop_after,
  "Calving", 
  paste0(round(area_mine_calving, digits = 3), " (", round(prop_mine_calving, digits = 3), "%)"), 
  paste0(round(area_mine_zoi_calving, digits = 3), " (", round(prop_mine_zoi_calving, digits = 3), "%)"), 
  paste0(round(area_turbines_calving, digits = 3), " (", round(prop_turbines_calving, digits = 3), "%)"),
  paste0(round(area_zoi_turbines_calving, digits = 3), " (", round(prop_zoi_turbines_calving, digits = 3), "%)"),
  paste0(round(area_zoi_combined_calving, digits = 3), " (", round(prop_zoi_combined_calving, digits = 3), "%)"), 
  paste0(round(table_calving$area_km2[1], digits = 3), " (", round(table_calving$prop[1], digits = 3), "%)"), 
  paste0(round(table_calving$area_km2[4], digits = 3), " (", round(table_calving$prop[4], digits = 3), "%)"),
  "Summer", 
  paste0(round(area_mine_summer, digits = 3), " (", round(prop_mine_summer, digits = 3), "%)"), 
  paste0(round(area_mine_zoi_summer, digits = 3), " (", round(prop_mine_zoi_summer, digits = 3), "%)"), 
  paste0(round(area_turbines_summer, digits = 3), " (", round(prop_turbines_summer, digits = 3), "%)"),
  NA,#paste0(round(area_zoi_turbines_summer, digits = 3), " (", round(prop_zoi_turbines_summer, digits = 3), ")"),
  NA,#paste0(round(area_zoi_combined_summer, digits = 3), " (", round(prop_zoi_combined_summer, digits = 3), ")"), 
  NA, NA,
  "Autumn", 
  paste0(round(area_mine_autumn, digits = 3), " (", round(prop_mine_autumn, digits = 3), "%)"), 
  paste0(round(area_mine_zoi_autumn, digits = 3), " (", round(prop_mine_zoi_autumn, digits = 3), "%)"), 
  paste0(round(area_turbines_autumn, digits = 3), " (", round(prop_turbines_autumn, digits = 3), "%)"),
  paste0(round(area_zoi_turbines_autumn, digits = 3), " (", round(prop_zoi_turbines_autumn, digits = 3), ")"),
  paste0(round(area_zoi_combined_autumn, digits = 3), " (", round(prop_zoi_combined_autumn, digits = 3), ")"),
  paste0(round(table_autumn$area_km2[1], digits = 3), " (", round(table_autumn$prop[1], digits = 3), "%)"), 
  paste0(round(table_autumn$area_km2[4], digits = 3), " (", round(table_autumn$prop[4], digits = 3), "%)"),
)
table_habitat_change

readr::write_csv(table_habitat_change, file = "output/table_habitat_change_all_seasons_top50pct.csv")
