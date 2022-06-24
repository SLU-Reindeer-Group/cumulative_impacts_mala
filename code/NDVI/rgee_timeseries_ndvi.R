# code based on:
# https://www.researchgate.net/publication/355348332_Enhancing_Animal_Movement_Analyses_Spatiotemporal_Matching_of_Animal_Positions_with_Remotely_Sensed_Data_Using_Google_Earth_Engine_and_R
# https://smithsonian.github.io/SpatiotemporalMatchingOfAnimalPositionsWithRemotelySensedDataUsingGoogleEarthEngineAndR/

library(tidyverse)
library(sf)
library(rgee)

ee_Initialize()
ee_check()

gps_data <- readRDS("//yksi.srh.slu.se/13_Geodata/erik_phd/ndvi/data_cleaned_summer_RG.rds") %>% 
  st_as_sf(., coords = c("x", "y"), crs = 3006) %>% 
  st_transform(4326) %>% 
  mutate(Date=sub(" ", "T", timestamp)) %>% 
  transform(timestamp=as.character(timestamp)) %>% 
  select(Collar_ID, Date, timestamp, geometry)
#  slice(which(row_number() %% 500 == 1)) # select every 500th row to create a small dataset for testing
  
# Function to add property with time in milliseconds
add_date<-function(feature) {
  date <- ee$Date(ee$String(feature$get("Date")))$millis()
  feature$set(list(date_millis=date))
}

# Join Image and Points based on a maxDifference Filter within a temporal window

# Set temporal window in days for filter. This will depend on the remote sensing data used.
tempwin <- 16 

# Set the filter
maxDiffFilter<-ee$Filter$maxDifference(
  difference=tempwin*24*60*60*1000, #days * hr * min * sec * milliseconds
  leftField= "date_millis", #Timestamp of the telemetry data
  rightField="system:time_start" #Image date
)

# Define the join. We implement the saveBest function for the join, which finds the image that best
# matches the filter (i.e., the image closest in time to the particular GPS fix location). 
saveBestJoin<-ee$Join$saveBest(
  matchKey="bestImage",
  measureKey="timeDiff"
)

# Function to add property with raster pixel value from the matched image
add_value<-function(feature){
  #Get the image selected by the join
  img1<-ee$Image(feature$get("bestImage"))$select(band)
  #Extract geometry from the feature
  point<-feature$geometry()
  #Get pixel value for each point at the desired spatial resolution (argument scale)
  pixel_value<-img1$sample(region=point, scale=250, tileScale = 16, dropNulls = F) 
  #Return the data containing pixel value and image date.
  feature$setMulti(list(PixelVal = pixel_value$first()$get(band), DateTimeImage = img1$get('system:index')))
}

# Function to remove image property from features
removeProperty<- function(feature) {
  #Get the properties of the data
  properties = feature$propertyNames()
  #Select all items except images
  selectProperties = properties$filter(ee$Filter$neq("item", "bestImage"))
  #Return selected features
  feature$select(selectProperties)
}

# get start and end date from dataset 
date_min_max <- gps_data %>%
  st_drop_geometry() %>% 
  summarize(min_date=min(substr(Date,1,10)), max_date=max(substr(Date,1,10)))

start <- date_min_max$min_date
end <- date_min_max$max_date
imagecoll<-ee$ImageCollection('MODIS/006/MOD13Q1')$filterDate(start,end)
band <- "NDVI" #Name of the band to use.

# This is for up to 1 million points. To increase the max number of points, increase the value for max repetitions.
# To change the number of points to run per time, change the value in the argument each (up to 5000).

gps_data$uniq <- rep(1:1000, each=1000)[1:nrow(gps_data)]

dataoutput_ndvi <- data.frame()
for(x in unique(gps_data$uniq)){
  data1 <- gps_data %>% filter(uniq == x) 
  data <- sf_as_ee(data1) # Send sf to GEE
  data <- data$map(add_date) # Transform day into milliseconds
  Data_match <- saveBestJoin$apply(data, imagecoll, maxDiffFilter) # Apply the join
  DataFinal <- Data_match$map(add_value) # Add pixel value to the data
  DataFinal <- DataFinal$map(removeProperty) # Remove image property from the data
  temp <- ee_as_sf(DataFinal, via = 'getInfo') # Move GEE object into R
  dataoutput_ndvi <- rbind(dataoutput_ndvi, temp) # Append
}

band <- "DayOfYear" # actual acquisition date  
dataoutput_doy <- data.frame()
for(x in unique(gps_data$uniq)){
  data1 <- gps_data %>% filter(uniq == x)
  data <- sf_as_ee(data1)
  data <- data$map(add_date)
  Data_match <- saveBestJoin$apply(data, imagecoll, maxDiffFilter)
  DataFinal <- Data_match$map(add_value)
  DataFinal <- DataFinal$map(removeProperty)
  temp <- ee_as_sf(DataFinal, via = 'getInfo') %>% 
    st_drop_geometry()
  dataoutput_doy <- rbind(dataoutput_doy, temp)
} 

band <- "SummaryQA"
# 0 Good Data. Use with confidence
# 1 Marginal data. Useful, but look at other QA information
# 2 Snow/Ice. Target covered with snow/ice
# 3 Cloudy. Target not visible, covered with cloud

dataoutput_qa <- data.frame()
for(x in unique(gps_data$uniq)){
  data1 <- gps_data %>% filter(uniq == x)
  data <- sf_as_ee(data1)
  data <- data$map(add_date)
  Data_match <- saveBestJoin$apply(data, imagecoll, maxDiffFilter)
  DataFinal <- Data_match$map(add_value)
  DataFinal <- DataFinal$map(removeProperty)
  temp <- ee_as_sf(DataFinal, via = 'getInfo') %>% 
    st_drop_geometry()
  dataoutput_qa <- rbind(dataoutput_qa, temp)
} 

ndvi_with_qa <- dataoutput_ndvi %>% 
  rename(ndvi_PixelVal=PixelVal) %>% 
  left_join(select(dataoutput_doy, Collar_ID, timestamp, doy_Pixelval=PixelVal), by=c("Collar_ID", "timestamp")) %>% 
  left_join(select(dataoutput_qa, Collar_ID, timestamp, qa_Pixelval=PixelVal), by=c("Collar_ID", "timestamp")) %>% 
  mutate(doy_Date=as.Date(doy_Pixelval+1, origin = paste0(substr(timestamp,1,4),"-01-01"))) # change day of year into date format (yyyy-mm-dd)

saveRDS(ndvi_with_qa, "//yksi.srh.slu.se/13_Geodata/erik_phd/ndvi/data_cleaned_summer_RG_ndvi.rds")