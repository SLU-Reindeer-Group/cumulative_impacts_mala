library(rgee)
library(sf)

ee_Initialize(
  drive = TRUE,
  gcs = FALSE,
  display = TRUE
)

# select area of interest, in this case Malå
region <- ee$FeatureCollection("users/erik_gee/mala")$geometry()

mala_bbox <- st_read("E:/Dropbox/R/RenGPS/GIS/mala.shp") %>%
  st_transform(4326) %>%
  st_bbox()

roi <- ee$Geometry$Rectangle(
  coords = mala_bbox,
  proj = "EPSG:4326",
  geodesic = FALSE
)

# MODIS ImageCollection (I downloaded each band separately but you can select all at once)
collection <- ee$ImageCollection("MODIS/006/MOD13Q1")$
  filterBounds(roi)$
  filterDate("2008-01-01", "2020-12-31")$
  select("NDVI")
  # select("DayOfYear")
  # select("SummaryQA")

count <- collection$size()$getInfo()

img_lst <- collection$toList(count)

modis_time <- list()

for (index in seq_len(count)) {
  image <- ee$Image(img_lst$get(index-1))
  name <- image$get('system:index')$getInfo()
  filename <- paste0("mala/ndvi/", name,".tif")
  print(filename)
  modis <- ee_as_raster(           # you can also use ee_as_stars
    image = image,
    region = region,
    scale = 250,
    container = "ndvi_mala",
    dsn = filename,
  ) 
  names(modis) <- name
  modis[modis==0] <- NA
  modis_time[[index]] <- modis
}
