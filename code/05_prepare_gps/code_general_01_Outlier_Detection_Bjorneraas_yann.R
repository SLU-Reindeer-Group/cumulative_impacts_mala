##################################################################################################
#       CODE PROVIDED BY Bjorneraas K.ET AL (JWM) Screening GPS Location Data for Errors using Animal Movement Characteristics
##################################################################################################

#######################################################################

library(adehabitatMA)
library(adehabitatLT)
library(adehabitatHR)


###STEP 1: Run the function GPS.screening

GPS.screening <- function(x, medcrit, meancrit, spikesp, spikecos){
  ###round one:
  #############
  ###remove points far off:
  RoundOne <- function(x, df, win, MM){
    if (x<=win){
      dfT <- df[c(1:2*win),]
    }
    if (x>(nrow(df)-win)){
      dfT <- df[c((nrow(df)-2*win):(nrow(df))),]
    }
    if (x>win & x<=(nrow(df)-win)){
      dfT <- df[c((x-win):(x+win)),]
    }
    if (MM=="mean"){
      temp <- sqrt((df$x[x]-mean(dfT$x, na.rm=T))^2+(df$y[x]-mean(dfT$y, na.rm=T))^2)
      #calculate the distance between the mean and the focal point
    }
    if (MM=="median"){
      temp <- sqrt((df$x[x]-median(dfT$x, na.rm=T))^2+(df$y[x]-median(dfT$y, na.rm=T))^2)
      #calculate the distance between the median and the focal point
    }
    return(temp)
  }
  
  ##get the points that are really far off, by using the median
  x$R1dmed <- unlist(lapply(c(1:nrow(x)), RoundOne, df=x, win=10, MM="median"))
  
  ##get the points that are rather far off, by using the mean
  x$R1dmean <- meancrit+1
  x$R1dmean[x$R1dmed<medcrit] <- unlist(lapply(c(1:nrow(x[x$R1dmed<medcrit,])),  RoundOne, df=x[x$R1dmed<medcrit,], win=10, MM="mean"))
  x$R1error <- ifelse(x$R1dmean>meancrit, TRUE, FALSE)
  
  ###round two:
  #############
  ##find spikes
  xT <- x[x$R1error==FALSE,]
  
  xT <- as.ltraj(xT[,c("x","y")], date=xT$date, id=xT$id)[[1]]
  
  x$R2error <- NA
  x$R2error[x$R1error==FALSE] <- ((xT$dist/xT$dt*3600)>spikesp & c(NA, (xT$dist/xT$dt*3600)[-nrow(xT)])>spikesp & cos(xT$rel.angle)<(spikecos))
  return(x)
}

###########################################################################

###STEP 2: Run the function GPS.screening.wrp

GPS.screening.wrp <- function(id, burst, x, y, date, medcrit, meancrit, spikesp, spikecos){
  mydata <- ld(as.ltraj(id = id, burst=burst, xy = data.frame(x=x,y=y), date = date))
  mydata <- split(mydata, mydata$id)
  mydata <- lapply(mydata, GPS.screening, medcrit=medcrit, meancrit=meancrit, spikesp=spikesp, spikecos=spikecos)
  mydata <- do.call("rbind", mydata)
  return(data.frame(id=mydata$id, burst=mydata$burst, x=mydata$x, y=mydata$y, date=mydata$date, R1dmed=mydata$R1dmed, R1dmean=mydata$R1dmean, R1error=mydata$R1error, R2error=mydata$R2error))
}
