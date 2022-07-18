library(sf)
library(dplyr)
library(stars)
library(terra)

files = list.files(path = "//yksi.srh.slu.se/13_geodata/erik_phd/ndvi/mala/ndvi", pattern = "\\.tif$", full.names = T)
dates <- as.Date(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(files)), format="%Y_%m_%d")
files2 = list.files(path = "//yksi.srh.slu.se/13_geodata/erik_phd/ndvi/mala/summaryqa", pattern = "\\.tif$", full.names = T)
dates2 <- as.Date(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(files)), format="%Y_%m_%d")


# read images and add time dimension with stars
ndvi <- read_stars(files, along = list(time = dates))
names(ndvi) <- "NDVI"
qa <- read_stars(files2, along = list(time = dates2))
names(qa) <- "SummaryQA"

library(rTIMESAT)
nptperyear = 23
jobname <- "Mala"

# see https://web.nateko.lu.se/timesat/docs/TIMESAT33_SoftwareManual.pdf page 29
options <- list(
  ylu                 = c(0, 9999),     # Valid data range (lower upper)
  qc_1                = c(0, 0, 1),     # Quality range 1 and weight
  qc_2                = c(1, 1, 0.5),   # Quality range 2 and weight
  qc_3                = c(2, 3, 0.1),   # Quality range 3 and weight
  A                   = 0.1,            # Amplitude cutoff value
  output_type         = c(1, 1, 0),     # Output files (1/0 1/0 1/0), 1: seasonality data; 2: smoothed time-series; 3: original time-series
  seasonpar           = 1.0,            # Seasonality parameter (0-1)
  iters               = 2,              # No. of envelope iterations (3/2/1)
  FUN                 = 2,              # Fitting method (1/2/3): (SG/AG/DL)
  half_win            = 7,              # half Window size for Sav-Gol.
  meth_pheno          = 1,              # (1: seasonal amplitude, 2: absolute value, 3: relative amplitude, 4: STL trend)
  trs                 = c(0.5, 0.5)     # Season start / end values
)


xl <- dim(ndvi[[1]])[1]
yl <- dim(ndvi[[1]])[2]

time_peak <- array(NA, dim = c(xl, yl, 13))
val_peak <- array(NA, dim = c(xl, yl, 13))
len <- array(NA, dim = c(xl, yl, 13))
ampl <- array(NA, dim = c(xl, yl, 13))
integ_large <- array(NA, dim = c(xl, yl, 13))
integ_small <- array(NA, dim = c(xl, yl, 13))

for(i in 1:xl)
{
  for(j in 1:yl)
  {
    print (i)
    print (j)
    # 
    # as.vector(ndvi["NDVI",i,j,][["NDVI"]])/1e4
    # 
    r <- try(TSF_main(y = as.vector(ndvi["NDVI",i,j,][["NDVI"]])/1e4, qc = as.vector(qa["SummaryQA",i,j,][["SummaryQA"]]), nptperyear = nptperyear, jobname = jobname, options = options, cache = FALSE))
    
    if (class(r) == "try-error") {
      for(k in 1:seasons)  
      {  
        time_peak[i,j,k] <- NA
        val_peak[i,j,k] <- NA
        len[i,j,k] <- NA
        ampl[i,j,k] <- NA
        integ_large[i,j,k] <- NA
        integ_small[i,j,k] <- NA
      }
      
    } else {
      for(k in 1:seasons)  
      {  
        time_peak[i,j,k] <- r$pheno$time_peak[k]
        val_peak[i,j,k] <- r$pheno$val_peak[k]
        len[i,j,k] <- r$pheno$len[k]
        ampl[i,j,k] <- r$pheno$ampl[k]
        integ_large[i,j,k] <- r$pheno$integ_large[k]
        integ_small[i,j,k] <- r$pheno$integ_small[k]
      }
    }
  }
}

saveRDS(time_peak, "//yksi.srh.slu.se/13_geodata/erik_phd/ndvi/timesat_integer_small.RDS")
saveRDS(val_peak, "//yksi.srh.slu.se/13_geodata/erik_phd/ndvi/timesat_integer_small.RDS")
saveRDS(len, "//yksi.srh.slu.se/13_geodata/erik_phd/ndvi/timesat_integer_small.RDS")
saveRDS(ampl, "//yksi.srh.slu.se/13_geodata/erik_phd/ndvi/timesat_integer_small.RDS")
saveRDS(integ_large, "//yksi.srh.slu.se/13_geodata/erik_phd/ndvi/timesat_integer_small.RDS")
saveRDS(integ_small, "//yksi.srh.slu.se/13_geodata/erik_phd/ndvi/timesat_integer_small.RDS")

test <- readRDS("//yksi.srh.slu.se/13_geodata/erik_phd/ndvi/timesat_integer_small.RDS")

test_rast <- rast(test)
test_stars <- st_as_stars(test)

# st_dimensions(test_stars)

plot(test_rast)
plot(test_stars)


