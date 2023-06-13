library(tidyverse)

# spatial prediction
install.load::install_load("raster", "rasterVis", "terra", "stars")
# 
# vindval data
load(file = "data/01_GPS/05_annotated_data/data_annotated_calving_ready_2h_rsf.RData")
# 
# # model comparison
# load("output/rsf_calving_multi_zoi_table.rda")
# 
# # compare models
# tab_models
# #---------------
# # Re-run best model
# form <- tab_models$definition[1]
# best_fit <- glm(form, 
#                 data = data_ready_calving_rsf_2h, #weights = w_obs,
#                 family = binomial(link = "logit"))

#-----------
#' # Load maps

# vector of wind turbines
# turbines <- sf::st_read("maps/wind_turbines_Malå.shp")

# read maps
plot_maps <- TRUE

# dem
dem <- terra::rast("maps/dem_10m.tif")
names(dem) <- "dem"
if(plot_maps) plot(dem)

# slope
slope <- terra::rast("maps/dem_slope.tif")
names(slope) <- "slope"
if(plot_maps) plot(slope)

# aspect
aspect <- terra::rast("maps/dem_aspect.tif")
cos_aspect <- cos(aspect)
names(cos_aspect) <- "cos_aspect"

# tpi 150m
tpi_s150m <- terra::rast("maps/tpi_s150m.tif")
tpi_cat_s150m <- tpi_s150m
tpi_s150m_vals <- terra::values(tpi_s150m)[,1]
SD <- sd(tpi_s150m_vals, na.rm = T)
tpi_vals <- case_when(
  tpi_s150m_vals <= -SD ~ 1,
  tpi_s150m_vals > -SD & tpi_s150m_vals <= -SD/2  ~ 2,
  tpi_s150m_vals > -SD/2 & tpi_s150m_vals <= 0  ~ 3,
  tpi_s150m_vals > 0 & tpi_s150m_vals <= SD/2  ~ 4,
  tpi_s150m_vals > SD/2 & tpi_s150m_vals <= SD  ~ 5,
  tpi_s150m_vals > SD ~ 6
)
values(tpi_cat_s150m) <- tpi_vals
tpi_levels <- data.frame(id = 1:6, tpi_cat_s150m = c("valley", "lower slope", "flat area", "medium slope", "upper slope", "ridge"))
levels(tpi_cat_s150m) <- tpi_levels
if(plot_maps) plot(tpi_cat_s150m)
is.factor(tpi_cat_s150m)

# tpi 250m
tpi_s250m <- terra::rast("maps/tpi_s250m.tif")
tpi_cat_s250m <- tpi_s250m
tpi_s250m_vals <- terra::values(tpi_s250m)[,1]
SD <- sd(tpi_s250m_vals, na.rm = T)
tpi_vals <- case_when(
  tpi_s250m_vals <= -SD ~ 1,
  tpi_s250m_vals > -SD & tpi_s250m_vals <= -SD/2  ~ 2,
  tpi_s250m_vals > -SD/2 & tpi_s250m_vals <= 0  ~ 3,
  tpi_s250m_vals > 0 & tpi_s250m_vals <= SD/2  ~ 4,
  tpi_s250m_vals > SD/2 & tpi_s250m_vals <= SD  ~ 5,
  tpi_s250m_vals > SD ~ 6
)
values(tpi_cat_s250m) <- tpi_vals
tpi_levels <- data.frame(id = 1:6, tpi_cat_s250m = c("valley", "lower slope", "flat area", "medium slope", "upper slope", "ridge"))
levels(tpi_cat_s250m) <- tpi_levels
if(plot_maps) plot(tpi_cat_s250m)
is.factor(tpi_cat_s250m)

# tpi 510m
tpi_s510m <- terra::rast("maps/tpi_s510m.tif")
tpi_cat_s510m <- tpi_s510m
tpi_s510m_vals <- terra::values(tpi_s510m)[,1]
SD <- sd(tpi_s510m_vals, na.rm = T)
tpi_vals <- case_when(
  tpi_s510m_vals <= -SD ~ 1,
  tpi_s510m_vals > -SD & tpi_s510m_vals <= -SD/2  ~ 2,
  tpi_s510m_vals > -SD/2 & tpi_s510m_vals <= 0  ~ 3,
  tpi_s510m_vals > 0 & tpi_s510m_vals <= SD/2  ~ 4,
  tpi_s510m_vals > SD/2 & tpi_s510m_vals <= SD  ~ 5,
  tpi_s510m_vals > SD ~ 6
)
values(tpi_cat_s510m) <- tpi_vals
tpi_levels <- data.frame(id = 1:6, tpi_cat_s510m = c("valley", "lower slope", "flat area", "medium slope", "upper slope", "ridge"))
levels(tpi_cat_s510m) <- tpi_levels
if(plot_maps) plot(tpi_cat_s510m)
is.factor(tpi_cat_s510m)

# phase
phase <- dem
phase <- !is.na(phase)
terra::values(phase) <- ifelse(terra::values(phase) == 1, 3, NA)
levels(phase) <- data.frame(id = 1:3, phase = levels(data_ready_calving_rsf_2h$phase))
if(plot_maps) plot(phase)
is.factor(phase)

# land cover (before clear cuts)
landcover <- terra::rast("maps/landcover_nmd_mod.tif")
lc_class <- read.csv("maps/nmd_classes_eng_reclassified.csv", header = F, 
                     col.names = c("id", "landcover"), stringsAsFactors = F) |> 
  dplyr::arrange(id)
levels(landcover) <- lc_class
landcover
if(plot_maps) plot(landcover)
#names(landcover) <- "landcover"
# landcover[!is.na(landcover)][1:10]

# clear cuts
clear_cuts <- terra::rast("maps/clear_cuts_2020.tif")
if(plot_maps) plot(clear_cuts)

# landcover and clear cuts
year <- 2015
lc_class
# landcover_cc <- terra::ifel(clear_cuts > year, "clear cuts", landcover)
landcover_cc <- terra::ifel(clear_cuts > year, 118, landcover)
lc_class2 <- lc_class
lc_class2$landcover[lc_class2$id %in% c(111, 112)] <- "coniferous forests"
lc_class2$landcover[lc_class2$id %in% c(2, 202)] <- "mires"
lc_class2$landcover[lc_class2$id %in% c(3, 41, 201)] <- "open lands"
levels(landcover_cc) <- lc_class2
landcover_cc <- terra::ifel(landcover_cc == 61, NA, landcover_cc)
names(landcover_cc) <- "landcover_cc"
if(plot_maps) plot(landcover_cc) 

# maps_after_df$landcover_cc <- forcats::fct_collapse(maps_after_df$landcover_cc,
#                                                    "open lands" = c("arable lands", "tundra/rocks", "other open lands")) |> 
#   forcats::fct_relevel("clear cuts", after = 0)

# wind distance
#wind_dist <- terra::rast("maps/wind_dist.tif")
#if(plot_maps) plot(wind_dist)

# wind distance jokk
wind_dist_jokk <- terra::rast("maps/wind_turbines_dist_jokk.tif")
if(plot_maps) plot(wind_dist_jokk)

# wind distance stor
wind_dist_stor <- terra::rast("maps/wind_turbines_dist_stor.tif")
if(plot_maps) plot(wind_dist_stor)

# wind distance åm
wind_dist_aml <- terra::rast("maps/wind_turbines_dist_aml.tif")
if(plot_maps) plot(wind_dist_aml)

# wind distance ytter
wind_dist_ytte <- terra::rast("maps/wind_turbines_dist_ytte.tif")
if(plot_maps) plot(wind_dist_ytte)

# wind distance jokk - stor - ytter
wind_dist_jsy <- terra::rast("maps/wind_turbines_dist_jsy.tif")
if(plot_maps) plot(wind_dist_jsy)

# wind distance combination jsy + aml
wind_dist_comb <- wind_dist_jsy
values(wind_dist_comb) <- pmin(values(wind_dist_jsy), values(wind_dist_aml))
if(plot_maps) plot(wind_dist_comb)
names(wind_dist_comb) <- "wind_turbines_dist_comb"

# wind_turbines_nearest10000
wind_turbines_nearest10000 <- terra::rast("maps/wind_turbines_bin_zoi_nearest_exp_decay10000.tif")
names(wind_turbines_nearest10000) <- "wind_turbines_nearest10000"
if(plot_maps) plot(wind_turbines_nearest10000)

# wf name
wf_name <- wind_dist_jsy
vals <- apply(cbind(values(wind_dist_jokk), values(wind_dist_stor), values(wind_dist_ytte), values(wind_dist_aml)), 1, which.min)
vals2 <- unlist(vals)
values(wf_name)[!is.na(values(wf_name))] <- vals2
wfs <- data.frame(id = 1:4, wf_name = c("jokkmokksliden", "storliden", "ytteberg", "amliden"))
levels(wf_name) <- wfs
if(plot_maps) plot(wf_name)

# house dist
house_dist <- terra::rast("maps/house_dist.tif")
if(plot_maps) plot(house_dist)

# large road  dist
public_road_dist <- terra::rast("maps/public_road_dist.tif")
if(plot_maps) plot(public_road_dist)

# pl dist
pl_dist <- terra::rast("maps/power_lines_dist.tif")
names(pl_dist) <- "power_lines_dist"
if(plot_maps) plot(pl_dist)


# mining dist
mining_dist <- terra::rast("maps/mining_Krist_dist.tif")
names(mining_dist) <- "mining_Krist_dist"
if(plot_maps) plot(mining_dist)

mining_Krist_nearest10000 <- terra::rast("maps/mining_Krist_bin_zoi_nearest_exp_decay10000.tif")
names(mining_Krist_nearest10000) <- "mining_Krist_nearest10000"
if(plot_maps) plot(mining_Krist_nearest10000)

# small roads
priv_road_dist <- terra::rast("maps/priv_road_dist.tif")
if(plot_maps) plot(priv_road_dist)

# trails dist
trail_dist <- terra::rast("maps/trails_dist.tif")
if(plot_maps) plot(trail_dist)

# sos
sos_beck <- stars::read_stars(.x = "maps/ndvi/ok/sos_beck.tif")
sos_elmore <- stars::read_stars(.x = "maps/ndvi/ok/sos_elmore.tif")

# monthday
md <- "0501"

# merge start of season maps
years_data <- attr(sos_beck, "dimensions")$band$values
#sos_maps <- mean(sos_beck, sos_elmore)
sos_maps <- c(sos_beck[,,,years_data], sos_elmore[,,,years_data]) |>
  dplyr::mutate(sos = (sos_beck.tif + sos_elmore.tif)/2) |>
  dplyr::select(sos) |>
  dplyr::filter(band >= 2008, band <= 2018)
#sos_maps <- stars::st_set_dimensions(sos_maps, 3, values = years_data, names = "time")
#sos_maps <- stars::st_set_dimensions(sos_maps, 3, values = lubridate::ymd(paste0(years_data, md)), names = "time")

# transform to terra and reproject
sos_all <- terra::rast(sos_maps) |>
  terra::project(dem)
names(sos_all) <- paste0("sos_", names(sos_all))

# sos - after
sos_after <- terra::app(sos_all[[grepl("2015|2016|2017|2018", names(sos_all))]], mean)
names(sos_after) <- "sos_after"

sos_during <- terra::app(sos_all[[grepl("2010|2011", names(sos_all))]], mean)
names(sos_during) <- "sos_during"

sos_before <- terra::app(sos_all[[grepl("2008|2009", names(sos_all))]], mean)
names(sos_before) <- "sos_before"

# sos
sos_beck <- stars::read_stars(.x = "maps/ndvi/ok/sos_beck.tif")
sos_elmore <- stars::read_stars(.x = "maps/ndvi/ok/sos_elmore.tif")

# monthday
md <- "0501"


# eos
eos_beck <- stars::read_stars(.x = "maps/ndvi/ok/eos_beck.tif")
eos_elmore <- stars::read_stars(.x = "maps/ndvi/ok/eos_elmore.tif")

# end of season maps
eos_data <- attr(eos_beck, "dimensions")$band$values
eos_maps <- c(eos_beck[,,,eos_data], eos_elmore[,,,eos_data]) |>
  dplyr::mutate(eos = (eos_beck.tif + eos_elmore.tif)/2) |>
  dplyr::select(eos) |>
  dplyr::filter(band >= 2008, band <= 2018)

# transform to terra and reproject
eos_all <- terra::rast(eos_maps) |>
  terra::project(dem)

# select years eos
eos_after <- terra::app(eos_all[[grepl("2015|2016|2017|2018", names(eos_all))]], mean)
names(eos_after) <- "eos_after"

eos_during <- terra::app(eos_all[[grepl("2010|2011", names(eos_all))]], mean)
names(eos_during) <- "eos_during"

eos_before <- terra::app(eos_all[[grepl("2008|2009", names(eos_all))]], mean)
names(eos_before) <- "eos_before"



# viewshed
viewshed_jsy15km <- terra::rast("maps/viewshed_jsy15km.tif") |>
  terra::project(dem)
if(plot_maps) plot(viewshed_jsy15km)

# viewshed_lc
# in sight open(1): in sight with open wetlands, arable lands, 
#    other open lands, anthropogenic, water, heath, tundra
# in sight cover(2): in sight with pine forests, other coniferous, 
#    deciduous forests, forested wetlands
# out of sight(0): out of sight
viewshed_lc <- terra::ifel(viewshed_jsy15km == 1 & landcover_cc %in% c(111, 112, 114, 121),
                           2, viewshed_jsy15km)
labs_view <- data.frame(id = c(0,1,2), viewshed_lc = c("out of sight", "in sight open", "in sight cover"))
levels(viewshed_lc) <- labs_view
if(plot_maps) terra::plot(viewshed_lc, legend = "topright")

# terra::writeRaster(viewshed_lc, filename = "viewshed_lc.tif", gdal = "COMPRESS=DEFLATE",
# overwrite = T)

#--------
# prepare maps 

maps_predictors <-
  c(
    phase,
    dem,
    slope,
    cos_aspect,
    tpi_cat_s150m,
    tpi_cat_s250m,
    tpi_cat_s510m,
    # sos_all,
    sos_after,
    sos_during,
    sos_before,
    eos_after,
    eos_during,
    eos_before,
    landcover,
    landcover_cc,
    clear_cuts,
    viewshed_jsy15km,
    viewshed_lc,
    public_road_dist,
    priv_road_dist,
    pl_dist,
    trail_dist,
    house_dist,
    mining_dist,
    mining_Krist_nearest10000,
    wind_dist_comb,
    wind_turbines_nearest10000,
    wf_name
  )

# add relevant ZOI variables
terra::writeRaster(maps_predictors, filename = "maps/for_prediction/predictors.tif", 
                   overwrite = TRUE)
