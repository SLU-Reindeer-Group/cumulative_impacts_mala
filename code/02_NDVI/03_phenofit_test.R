# Phenofit example
# based on earlier version of code from: https://cran.r-project.org/web/packages/phenofit/vignettes/phenofit_CA-NS6.html

library(tidyverse)
library(stars)
library(phenofit)
library(lubridate)

files = list.files(path = "//yksi.srh.slu.se/13_Geodata/erik_phd/ndvi/mala/ndvi", pattern = "\\.tif$", full.names = T)
files2 = list.files(path = "//yksi.srh.slu.se/13_Geodata/erik_phd/ndvi/mala/summaryqa", pattern = "\\.tif$", full.names = T)
files3 = list.files(path = "//yksi.srh.slu.se/13_Geodata/erik_phd/ndvi/mala/doy", pattern = "\\.tif$", full.names = T)
dates <- as.Date(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(files)), format="%Y_%m_%d")

# read images and add time dimension with stars
ndvi <- read_stars(files, along = list(time = dates))
names(ndvi) <- "NDVI"
qa <- read_stars(files2, along = list(time = dates))
names(qa) <- "SummaryQA"
doy <- read_stars(files3, along = list(time = dates))
names(doy) <- "DayOfYear"

# create dataframe from single pixel for test
mala_data <- data.frame(NDVI = as.vector(ndvi["NDVI",2,2,][["NDVI"]]),
                        SummaryQA = as.vector(qa["SummaryQA",2,2,][["SummaryQA"]]),
                        DayOfYear = as.vector(doy["DayOfYear",2,2,][["DayOfYear"]]),
                        date = dates,
                        stringsAsFactors=FALSE)

df_mala <- mala_data %>% 
  mutate(site="Mala") %>% 
  mutate(year=year(date), doy = as.integer(yday(date))) %>% 
  mutate(DayOfYear=ifelse(is.na(DayOfYear), doy, DayOfYear)) %>%
  mutate(DayOfYear=ifelse(DayOfYear==0, doy, DayOfYear)) %>%
  mutate(t=if_else(DayOfYear - doy >= 300,
                   as.Date(sprintf("%d-%03d", year+1, DayOfYear), "%Y-%j"),
                   as.Date(sprintf("%d-%03d", year, DayOfYear), "%Y-%j"))) %>%
  mutate(QC_flag=unlist(qc_summary(SummaryQA)[1])) %>% 
  mutate(w=unlist(qc_summary(SummaryQA)[2])) %>% 
  mutate(y=NDVI/1e4) %>% 
  select(site, y, t, date, w, QC_flag)

#--- set parameters

ymax_min   <- 0.1  # the maximum ymax shoud be greater than `ymax_min` 
rymin_less <- 0.8  # trough < ymin + A*rymin_less
nptperyear <- 23   # How many points for a single year
wFUN       <- wBisquare # Weights updating function, could be one of `wTSM`, 'wBisquare', `wChen` and `wSELF`. 

sites      <- unique(df_mala$site)
sitename   <- "Mala"
d          <- df_mala %>% filter(site == sitename) # get the first site data
sp         <- df_mala %>% filter(site == sitename)
south      <- FALSE # sp$lat < 0
print      <- FALSE # whether print progress
IsPlot     <- TRUE  # for brks

# -----------------


dnew  <- add_HeadTail(d, south, nptperyear = 23) # add additional one year in head and tail
INPUT <- check_input(dnew$t, dnew$y, dnew$w, dnew$QC_flag,
                     nptperyear, south, 
                     maxgap = nptperyear/4, alpha = 0.02, wmin = 0.2)


par(mar = c(3, 2, 2, 1), mgp = c(3, 0.6, 0))

lambda <- init_lambda(INPUT$y)

brks2 <- season_mov(INPUT, 
                    FUN = smooth_wWHIT, wFUN = wFUN,
                    maxExtendMonth = 6, r_min = 0.1,
                    IsPlot = IsPlot, IsPlot.OnlyBad = FALSE, print = print)

# curve fitting
fit  <- curvefits(INPUT, brks2,
                  options = list(
                  methods = c("AG"), #, "Zhang", "Beck", "Elmore", "Klos", "Gu"),
                  wFUN = wFUN,
                  nextend = 2, maxExtendMonth = 3, minExtendMonth = 1, minPercValid = 0.2
                  ))

## check the curve fitting parameters
l_param <- get_param(fit)
print(str(l_param, 1))
print(l_param$AG)

d_fit <- get_fitting(fit)
print(d_fit)
test <- as.data.frame(d_fit)

# Get GOF information
d_gof <- get_GOF(fit)
# fit$stat <- stat
print(head(d_gof))

print(fit$`2008_1`$model$Zhang$zs$iter2)
plot(fit$`2008_1`$model$Zhang$zs$iter2)

## visualization
g <- plot_curvefits(d_fit, brks2, NULL, ylab = "NDVI", "Time",
                   theme = coord_cartesian(xlim = c(ymd("2008-04-01"), ymd("2018-10-31"))))
grid::grid.newpage(); grid::grid.draw(g)

get_fitting(fit)

### EXTRACT PHENOLOGY
l_pheno <- get_pheno(fit, IsPlot = F) #%>% map(~melt_list(., "meth"))
pheno <- get_pheno(fit[1:6], "Zhang", IsPlot = T)
head(l_pheno$doy$AG)

########

day <- fit$`2010_1`$tout
ndvi <- fit$`2010_1`$model$AG$zs$iter2

data_ndvi <- data.frame(day, ndvi)
