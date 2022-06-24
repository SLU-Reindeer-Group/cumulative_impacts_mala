library(rsat)
library(sf)
library(terra)

# "username" and "password"
set_credentials("Rrangifer", "RsatTarandus123")

mala <- st_read("C:/Users/ercr/Dropbox/R/rgee/data/mala_availibility.shp") %>%
  st_transform(4326) %>% 
  st_buffer(10000)

roi <- mala
toi <- seq(as.Date("2008-01-01"),as.Date("2020-12-31"),1)

db.path <- "//yksi.srh.slu.se/13_Geodata/erik_phd/ndvi/mala_new/db"
ds.path <- "//yksi.srh.slu.se/13_Geodata/erik_phd/ndvi/mala_new/ds"
dir.create(db.path)
dir.create(ds.path)

mala_ndvi <- new_rtoi(name = "mal_ndvi",
                     region = roi,
                     db_path = db.path,
                     rtoi_path = ds.path)

rsat_search(region = roi, product = c("MOD13Q1"), dates = toi)

rsat_download(mala_ndvi)

################


files = list.files(path = "//yksi.srh.slu.se/13_Geodata/erik_phd/ndvi/mala_new/db/Modis/MOD13Q1", pattern = "\\.hdf$", full.names = T)
# dates <- seq(as.Date("2008-01-01"),as.Date("2020-12-31"),16)

test <- rast(files[8])

plot(test)
plot(mala)

test_mala <- vect(mala)
test_wgs <- project(test, "epsg:4326")

names(test)
crs(test)

test_crop <- crop(test, mala)

st_write(mala, "C:/TEMP/mala_availibility_1km_buff.shp")