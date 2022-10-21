#' ---
#' title: "Calculating Zone of Influence variables"
#' author: Bernardo Niebuhr
#' output:
#'   rmarkdown::github_document
#' ---

#------
# Create GRASS location and mapset

# packages
library(oneimpact)
library(terra)
library(rgrass7)
library(dplyr)

# read one of the maps to use as a reference for the GRASSdb
template <- terra::rast("maps/report/wind_dist.tif")
plot(template)

# create GRASS location and mapset
# connect to grass gis 7.8 and create grass location
grassdir <- system("grass78 --config path", intern = T)
gisDB <- "maps/00_grassdb" # create location and mapset in the working directory
loc <- "sweden_ren_db" # name of the location
ms <- "availability_mala" # name of the mapset
rgrass7::initGRASS(gisBase = grassdir,
                   SG = template, # use map to define location projection
                   home = tempdir(),
                   override = T,
                   gisDbase = gisDB,
                   location = loc,
                   mapset = ms)
# check
rgrass7::gmeta()

#------
# Import vector to be used as mask

mask <-  "maps/report/availability_mala_autumn.gpkg"
rgrass7::execGRASS("v.in.ogr", input = mask, output = "mask", flags = c("o", "t"))

#------
# Import rasters

# List raw maps
raw_maps <- list.files(path = "maps/report/", pattern = "raw.tif", full.names = T)
raw_maps_names <- list.files(path = "maps/report/", pattern = "raw.tif") |>
  strsplit(split = "_raw") |>
  sapply(FUN = first)

# Import all maps to GRASS
for(i in seq_along(raw_maps)) {
  rgrass7::execGRASS("r.in.gdal", input = raw_maps[i],
                     output = raw_maps_names[i], flags = 'o')
}

# transform maps into binary
names_bin <- sapply(raw_maps_names, function(x) gsub("1null", "bin", x))
for(i in seq_along(raw_maps_names)) {
  oneimpact::grass_binarize(raw_maps_names[i],
                            output = names_bin[i],
                            breaks = 0.5, null = 0, overwrite = TRUE)
}

#------
# calculate distance to the nearest feature for trails (not computed earlier) 
# using 10m resolution

# remove mask, if present
rgrass7::execGRASS("r.mask", flags = "r")
# define region in GRASS GIS
rgrass7::execGRASS("g.region", raster = raw_maps_names[1],
                   flags = "p")

# compute distance
inp <- raw_maps_names |> grep(pattern = "trails", value = TRUE)
trails_nearest_name <- oneimpact::calc_zoi_nearest(x = inp,
                                                   type = "euclidean",
                                                   where = "GRASS",
                                                   g_overwrite = TRUE, 
                                                   verbose = TRUE)
# export
# region
rgrass7::execGRASS("g.region", raster = raw_maps_names[1],
                   flags = "p")
# mask
rgrass7::execGRASS("r.mask", vector = "mask", flags = "overwrite")
# export
rgrass7::execGRASS("r.out.gdal", input = trails_nearest_name, 
                   output = "maps/report/trail_dist.tif", 
                   createopt = "COMPRESS=DEFLATE", flags = "overwrite")
# remove mask
rgrass7::execGRASS("r.mask", flags = "r")

#------
# calculate cumulative ZoI up to 500m using 10m resolution

# redefine maps with region around, since there are edge effects when
# running r.mfilter

# remove mask, if present
rgrass7::execGRASS("r.mask", flags = "r")
# define region in GRASS GIS
rgrass7::execGRASS("g.region", raster = raw_maps_names[1],
                   flags = "p")
# expand input raster
rgrass7::execGRASS("g.region", n = "n+15000", s = "s-15000", e = "e+15000",
                   w = "w-15000", flags = "p")

# recalculate considering 0 ourside the sameby
names_expanded <- paste0(names_bin, "_expanded15km")
for(i in seq_along(names_expanded)) {
  exp <- paste0(names_expanded[i], " = if(isnull(", names_bin[i], "), 0, ",
                names_bin[i], ")")
  rgrass7::execGRASS("r.mapcalc",
                     expression = exp,
                     flags = "overwrite")
}

# Exponential decay ZoI up to 500
# example for wind turbines
inp <- names_expanded |> grep(pattern = "wind", value = TRUE)
radii <- c(100, 250, 500)
exp_name <- oneimpact::calc_zoi_cumulative(x = inp,
                                           radius = radii, 
                                           zoi_limit = 0.05,
                                           type = "exp_decay", 
                                           g_module = "r.mfilter",
                                           output_type = "cumulative_zoi",
                                           where = "GRASS",
                                           g_overwrite = TRUE, 
                                           verbose = TRUE)

# Exponential decay ZoI up to 500
# for all other infrastructure
radii <- c(100, 250, 500)
for(inp in names_expanded[1:10]) {
  exp_name <- oneimpact::calc_zoi_cumulative(x = inp,
                                             radius = radii, 
                                             zoi_limit = 0.05,
                                             type = "exp_decay", 
                                             g_module = "r.mfilter",
                                             output_type = "cumulative_zoi",
                                             where = "GRASS",
                                             g_overwrite = TRUE, 
                                             verbose = TRUE)
}

# mask the maps to the sameby area

# list maps
maps_zoi_ext <- rgrass7::execGRASS("g.list", type = "raster", 
                                   pattern = "*expanded15km_zoi*", intern = TRUE) |> 
  grep(pattern = "100$|250$|decay500$", value = TRUE)
maps_zoi <- gsub("_expanded15km", "", maps_zoi_ext)

# compute
for(i in seq_along(maps_zoi_ext)) {
  # region
  rgrass7::execGRASS("g.region", raster = raw_maps_names[1], flags = "p")
  # region
  rgrass7::execGRASS("r.mask", vector = "mask", flags = "overwrite")
  # interpolate to 10m
  exp <- paste0(maps_zoi[i], " = ", maps_zoi_ext[i])
  rgrass7::execGRASS("r.mapcalc", 
                     expression = exp,
                     flags = "overwrite")
}

# remove mask
rgrass7::execGRASS("r.mask", flags = "r")

#------
# calculate cumulative ZoI up from 1000m to 10km using 50m resolution

names_50m <- paste0(names_expanded, "_res50")

# resample to 50m
for(i in seq_along(names_expanded)) {
  # region of the input map
  rgrass7::execGRASS("g.region", raster = names_expanded[i], flags = c("a", "p"))
  # change to new resolution
  rgrass7::execGRASS("g.region", res = '50', flags = c("a", "p"))
  # resample
  rgrass7::execGRASS("r.resamp.stats",
                     input = names_expanded[i],
                     output = names_50m[i],
                     method = "maximum",
                     flags = "overwrite")
}

# Exponential decay ZoI from 1000m to 10km
# for all infrastructure types
radii <- c(1000, 2500, 5000, 10000)
for(inp in names_50m[9]) {
  exp_name <- oneimpact::calc_zoi_cumulative(x = inp,
                                             radius = radii, 
                                             zoi_limit = 0.05,
                                             type = "exp_decay", 
                                             g_module = "r.mfilter",
                                             output_type = "cumulative_zoi",
                                             where = "GRASS",
                                             g_overwrite = TRUE, 
                                             verbose = TRUE)
}

# interpolate back to 10m
maps_zoi_50 <- rgrass7::execGRASS("g.list", type = "raster", pattern = "*res50_zoi*", intern = TRUE)
maps_zoi_interp10 <- gsub("_expanded15km_res50", "", maps_zoi_50)

for(i in seq_along(maps_zoi_50)) {
  # region
  rgrass7::execGRASS("g.region", raster = raw_maps_names[1], flags = "p")
  # region
  rgrass7::execGRASS("r.mask", vector = "mask", flags = "overwrite")
  # interpolate to 10m
  rgrass7::execGRASS("r.resamp.interp", input = maps_zoi_50[i],
                     output =  maps_zoi_interp10[i],
                     method = "bilinear", flags = "overwrite")
}

# remove mask
rgrass7::execGRASS("r.mask", flags = "r")

#---------
# export
maps_zoi_final <- rgrass7::execGRASS("g.list", type = "raster", pattern = "*bin_zoi*", intern = TRUE)

for(i in maps_zoi_final) {
  # region
  rgrass7::execGRASS("g.region", raster = raw_maps_names[1], flags = c("a", "p"))
  # mask
  rgrass7::execGRASS("r.mask", vector = "mask", flags = "overwrite")
  # export
  rgrass7::execGRASS("r.out.gdal", input = i, output = paste0("maps/", i, ".tif"),
                     createopt = "COMPRESS=DEFLATE", flags = "overwrite")
  # remove mask
  rgrass7::execGRASS("r.mask", flags = "r")
}

#----------------
# Computing ZoI of the nearest feature

# For these computations, we use the already computed Euclidean distance to the nearest 
# feature for the different types of infrastructure.
# We then transform then into exponential decay ZoI within R directly.

# Read maps of Euclidean distance to the nearest feature

# List raw maps
raw_maps_dist <- list.files(path = "maps/report/", pattern = "dist", full.names = TRUE) |> 
  grep(pattern = ".tif$", value = TRUE) |> 
  grep(pattern = "snowmob|jokk|aml|jsy|stor|ytte|wrong", value = TRUE, invert = TRUE) %>%
  .[c(1,2,4,3,5:11)]
raw_maps_dist_names <- list.files(path = "maps/report/", pattern = "raw.tif") |>
  strsplit(split = "_1null_raw") |>
  sapply(FUN = first)

# read
dist_rasts <- sapply(raw_maps_dist, terra::rast)
names(dist_rasts) <- raw_maps_dist_names

# transform into exponential decay ZoI
radii <- c(100, 250, 500, 1000, 2500, 5000, 10000)
zoi_nearest_rasts <- list()
for(i in seq_along(dist_rasts)) {
  print(raw_maps_dist_names[i])
  zoi_nearest_rasts[[i]] <- oneimpact::exp_decay(dist_rasts[[i]], radius = radii)
  names(zoi_nearest_rasts[[i]]) <- radii
}
names(zoi_nearest_rasts) <- raw_maps_dist_names

# check
plot(zoi_nearest_rasts[[9]][[c(3,6)]])

# export
for(i in seq_along(zoi_nearest_rasts)) {
  
  # prefix
  prefix <- raw_maps_dist_names[i]
  
  for(j in 1:nlyr(zoi_nearest_rasts[[i]])) {
    
    # map name
    rad <- radii[j]
    name <- paste0("maps/", prefix, "_bin_zoi_nearest_exp_decay", rad, ".tif")
    print(name)
    
    # export
    terra::writeRaster(zoi_nearest_rasts[[i]][[j]], filename = name, 
                       gdal=c("COMPRESS=DEFLATE"), overwrite = TRUE)
    
  }
}

#---------
# Select other relevant layers

raw_maps_other <- list.files(path = "maps/report/", pattern = ".tif$", full.names = T) |> 
  grep(pattern = "bear|clear_cuts|dem|landcover|sound|tpi|viewshed|wind_dist_|wolf|wolverine",
       value = TRUE)

# copy to final "maps" folder
file.copy(raw_maps_other, "maps/")
