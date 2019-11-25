# Code to time-match prediction values from blue whale distribution model and generate time-use metrics
# Sample of VMS tracks is from 2015-2017

#0. Setup -----------------> ####

library(foreign) #for reading in .dbf file 
library(raster) #for reading in rasters
library(adehabitatLT) #for calculating time-use metrics
library(Hmisc) #for plotting CIs in ggplot
library(dplyr) #for data manipulation
library(gridExtra) # for plotting side by side ggplots
library(ggpubr) # for plotting side by side ggplots with shared legend


rm(list=ls()) #clean workspace
WEARfolder <- "~/Dropbox/NOAA/NOAA Projects/WEAR/"
VMSfolder <- paste0(WEARfolder, "VMS data/")


#1. Load data -----------------> ####

## VMS data
# VMSdat <- read.dbf(paste0(VMSfolder, "vms_2017_10d_targets_all_attributes_geo.dbf"))
# VMSdat <- subset(VMSdat, TARGET_MAX=="DCRB" & STATE=="CA") # & DEPTH_CATM == "0-100m" | DEPTH_CATM == "100-150m"

VMS_2015_2016 <- read.csv(paste0(VMSfolder, "VMS_Outputs_wTARGET_10d_lookback_crabseason_2015-2016_final_cleaned_interpolated_regular_dcrb.csv"))
VMS_2016_2017 <- read.csv(paste0(VMSfolder, "VMS_Outputs_wTARGET_10d_lookback_crabseason_2016-2017_final_cleaned_interpolated_regular_dcrb.csv"))
VMS_2017_2018 <- read.csv(paste0(VMSfolder, "VMS_Outputs_wTARGET_10d_lookback_crabseason_2017-2018_final_cleaned_interpolated_regular_dcrb.csv"))


## Blue whale predictions
# BLWHfiles <- list.files(paste0(WEARfolder, "2017_BLWH_predictions"), pattern = "\\.gri$", full.names = T)
# BLWHlayers<-stack(BLWHfiles)

BLWHfiles_2015_2016 <- list.files(paste0(WEARfolder, "2015-2016_crabseason_BLWH_predictions"), pattern = "\\.gri$", full.names = T)
BLWHlayers_2015_2016<-stack(BLWHfiles_2015_2016)

BLWHfiles_2016_2017 <- list.files(paste0(WEARfolder, "2016-2017_crabseason_BLWH_predictions"), pattern = "\\.gri$", full.names = T)
BLWHlayers_2016_2017<-stack(BLWHfiles_2016_2017)

BLWHfiles_2017_2018 <- list.files(paste0(WEARfolder, "2017-2018_crabseason_BLWH_predictions"), pattern = "\\.gri$", full.names = T)
BLWHlayers_2017_2018<-stack(BLWHfiles_2017_2018)

#2. Extract time-matched prediction values -----------------> ####

#Add dates to each blue whale raster layer
#dates <- as.Date(substr(BLWHfiles,76,85)) #change depending on filepath to extract YYYY-MM-DD from filename
#BLWHlayers<-setZ(BLWHlayers, dates) #assign dates to each layer 

dates_2015_2016 <- as.Date(substr(BLWHfiles_2015_2016,92,101)) #change depending on filepath to extract YYYY-MM-DD from filename
BLWHlayers_2015_2016<-setZ(BLWHlayers_2015_2016, dates_2015_2016) #assign dates to each layer 

dates_2016_2017 <- as.Date(substr(BLWHfiles_2016_2017,92,101)) #change depending on filepath to extract YYYY-MM-DD from filename
BLWHlayers_2016_2017<-setZ(BLWHlayers_2016_2017, dates_2016_2017) #assign dates to each layer 

dates_2017_2018 <- as.Date(substr(BLWHfiles_2017_2018,92,101)) #change depending on filepath to extract YYYY-MM-DD from filename
BLWHlayers_2017_2018<-setZ(BLWHlayers_2017_2018, dates_2017_2018) #assign dates to each layer 

#Make index in VMS dataframe to match VMS dates with BLWHlayers dates
# names(BLWHlayers) = paste0("layer.", getZ(BLWHlayers)) #assign unique name to each BLWHlayer based on date
# VMSdat$dt <- as.Date(VMSdat$UTCDATETIM, format="%m/%d/%y %H:%M")
# VMSdat$layername = paste("layer",substr(VMSdat$dt,1,4),substr(VMSdat$dt,6,7), substr(VMSdat$dt,9,10),sep=".")  #assign same name to each VMS point based on date
# VMSdat$layerindex = match(VMSdat$layername, names(BLWHlayers))

###############
## 2015-2016 ##
###############
names(BLWHlayers_2015_2016) = paste0("layer.", getZ(BLWHlayers_2015_2016)) #assign unique name to each BLWHlayer based on date
VMS_2015_2016$layername = paste("layer",substr(VMS_2015_2016$westcoastdate,1,4),substr(VMS_2015_2016$westcoastdate,6,7), substr(VMS_2015_2016$westcoastdate,9,10),sep=".")  #assign same name to each VMS point based on date
VMS_2015_2016$layerindex = match(VMS_2015_2016$layername, names(BLWHlayers_2015_2016))
VMS_2015_2016 <- VMS_2015_2016[complete.cases(VMS_2015_2016$layerindex),] #remove points with layerindex=NA (not in BLWH pred range)

#Extract based on layer indices
VMS_2015_2016$BLWH_value <- sapply(1:nrow(VMS_2015_2016), function(i){raster::extract(BLWHlayers_2015_2016, VMS_2015_2016[i,c("LONGITUDE","LATITUDE")], layer=VMS_2015_2016$layerindex[i], nl=1)})

#Check, clean up, and save
head(VMS_2015_2016) 
VMS_2015_2016 <- subset(VMS_2015_2016, select=-c(layername,layerindex)) #remove unnecessary layer indices columns
saveRDS(VMS_2015_2016, paste0(VMSfolder, "vms_crabseason_2015-2016_final_cleaned_interpolated_regular_dcrb_BLWH.RDS"))

###############
## 2016-2017 ##
###############
names(BLWHlayers_2016_2017) = paste0("layer.", getZ(BLWHlayers_2016_2017)) #assign unique name to each BLWHlayer based on date
VMS_2016_2017$layername = paste("layer",substr(VMS_2016_2017$westcoastdate,1,4),substr(VMS_2016_2017$westcoastdate,6,7), substr(VMS_2016_2017$westcoastdate,9,10),sep=".")  #assign same name to each VMS point based on date
VMS_2016_2017$layerindex = match(VMS_2016_2017$layername, names(BLWHlayers_2016_2017))
VMS_2016_2017 <- VMS_2016_2017[complete.cases(VMS_2016_2017$layerindex),] #remove points with layerindex=NA (not in BLWH pred range)

#Extract based on layer indices
VMS_2016_2017$BLWH_value <- sapply(1:nrow(VMS_2016_2017), function(i){raster::extract(BLWHlayers_2016_2017, VMS_2016_2017[i,c("LONGITUDE","LATITUDE")], layer=VMS_2016_2017$layerindex[i], nl=1)})

#Check, clean up, and save
head(VMS_2016_2017) 
VMS_2016_2017 <- subset(VMS_2016_2017, select=-c(layername,layerindex)) #remove unnecessary layer indices columns
saveRDS(VMS_2016_2017, paste0(VMSfolder, "vms_crabseason_2016-2017_final_cleaned_interpolated_regular_dcrb_BLWH.RDS"))

###############
## 2017-2018 ##
###############
names(BLWHlayers_2017_2018) = paste0("layer.", getZ(BLWHlayers_2017_2018)) #assign unique name to each BLWHlayer based on date
VMS_2017_2018$layername = paste("layer",substr(VMS_2017_2018$westcoastdate,1,4),substr(VMS_2017_2018$westcoastdate,6,7), substr(VMS_2017_2018$westcoastdate,9,10),sep=".")  #assign same name to each VMS point based on date
VMS_2017_2018$layerindex = match(VMS_2017_2018$layername, names(BLWHlayers_2017_2018))
VMS_2017_2018 <- VMS_2017_2018[complete.cases(VMS_2017_2018$layerindex),] #remove points with layerindex=NA (not in BLWH pred range)

#Extract based on layer indices
VMS_2017_2018$BLWH_value <- sapply(1:nrow(VMS_2017_2018), function(i){raster::extract(BLWHlayers_2017_2018, VMS_2017_2018[i,c("LONGITUDE","LATITUDE")], layer=VMS_2017_2018$layerindex[i], nl=1)})

#Check, clean up, and save
head(VMS_2017_2018) 
VMS_2017_2018 <- subset(VMS_2017_2018, select=-c(layername,layerindex)) #remove unnecessary layer indices columns
saveRDS(VMS_2017_2018, paste0(VMSfolder, "vms_crabseason_2017-2018_final_cleaned_interpolated_regular_dcrb_BLWH.RDS"))


#3. Plot blue whale prediction values in VMS locations  -----------------> ####

### Histogram of VMS locations per month
ggplot(VMSdat) + geom_histogram(aes(x=month(VMSdat$dt)))

ggplot(VMS_2015_2016) + geom_histogram(aes(x=month(VMS_2015_2016$UTCDATETIM)))
ggplot(VMS_2016_2017) + geom_histogram(aes(x=month(VMS_2016_2017$UTCDATETIM)))
ggplot(VMS_2017_2018) + geom_histogram(aes(x=month(VMS_2017_2018$UTCDATETIM)))

### Histogram of blue whale pred values for DCRB VMS locations
# ggplot(VMSdat) + geom_histogram(aes(x=BLWH_value)) +
#   theme_bw() + xlab("Blue whale prediction value") +
#   ylab("# VMS locations") +
#   ggtitle("2017 DCRB VMS locations in CA")

a <- ggplot(VMS_2015_2016) + geom_histogram(aes(x=BLWH_value), fill="gray", color="black") +
  theme_bw() + xlab("Blue whale prediction value") +
  ylab("# DCRB VMS locations") +
  ggtitle("2015-16 (coastwide)")

b <- ggplot(VMS_2016_2017) + geom_histogram(aes(x=BLWH_value), fill="gray", color="black") +
  theme_bw() + xlab("Blue whale prediction value") +
  ylab("# DCRB VMS locations") +
  ggtitle("2016-17 (coastwide)")

c <- ggplot(VMS_2017_2018) + geom_histogram(aes(x=BLWH_value), fill="gray", color="black") +
  theme_bw() + xlab("Blue whale prediction value") +
  ylab("# DCRB VMS locations") +
  ggtitle("2017-18 (coastwide)")

ggarrange(a, b, c, nrow=1, common.legend = T, legend = "right")


a <- ggplot(filter(VMS_2015_2016, LATITUDE < 42)) + geom_histogram(aes(x=BLWH_value), fill="gray", color="black") +
  theme_bw() + xlab("Blue whale prediction value") +
  ylab("# DCRB VMS locations") +
  ggtitle("2015-16 (CA)")

b <- ggplot(filter(VMS_2016_2017, LATITUDE < 42)) + geom_histogram(aes(x=BLWH_value), fill="gray", color="black") +
  theme_bw() + xlab("Blue whale prediction value") +
  ylab("# DCRB VMS locations") +
  ggtitle("2016-17 (CA)")

c <- ggplot(filter(VMS_2017_2018, LATITUDE < 42)) + geom_histogram(aes(x=BLWH_value), fill="gray", color="black") +
  theme_bw() + xlab("Blue whale prediction value") +
  ylab("# DCRB VMS locations") +
  ggtitle("2017-18 (CA)")

ggarrange(a, b, c, nrow=1, common.legend = T, legend = "right")

### Time series of blue whale pred values for DCRB VMS locations - Mean curves

#first add in NA values for missing dates
# ts <- seq.POSIXt(as.POSIXct("2017-08-01"), as.POSIXct("2017-11-01"), by="day")
# df <- data.frame(dt=as.Date(ts))
# newdat <- full_join(df,VMSdat)  
# newdat <- newdat[order(newdat$DOCNUM, newdat$dt),]
# meandat <- aggregate(newdat$BLWH_value, by=list(newdat$dt), FUN=mean, na.rm=T)
# CIdat <- aggregate(newdat$BLWH_value, by=list(newdat$dt), FUN=smean.cl.boot)
# 
# ggplot() +
#   geom_ribbon(data=CIdat, aes(x=Group.1, ymin = x[,2], ymax = x[,3], fill="95% confidence interval")) +
#   geom_line(data=meandat, aes(x=Group.1, y=x), color="grey80") +
#   theme_classic() + xlab("time") +
#   ylab("Average blue whale prediction value") +
#   ggtitle("2017 DCRB VMS locations in CA") +
#   scale_fill_manual("",values="lightblue") +
#   theme(legend.position=c(0.2, 0.9))

###############
## 2015-2016 coastwide ##
###############
ts <- seq.POSIXt(as.POSIXct("2015-11-21"), as.POSIXct("2016-11-10"), by="day")
VMS_2015_2016$date <- as.Date(VMS_2015_2016$westcoastdate)
df <- data.frame(date=as.Date(ts))
newdat <- full_join(df,VMS_2015_2016)  
newdat <- newdat[order(newdat$DOCNUM, newdat$date),]
meandat <- aggregate(newdat$BLWH_value, by=list(newdat$date), FUN=mean, na.rm=T)
CIdat <- aggregate(newdat$BLWH_value, by=list(newdat$date), FUN=smean.cl.boot, na.rm=T)

a <- ggplot() +
  geom_ribbon(data=CIdat, aes(x=Group.1, ymin = x[,2], ymax = x[,3], fill="95% confidence interval")) +
  geom_line(data=meandat, aes(x=Group.1, y=x)) +
  theme_classic() + xlab(NULL) +
  ylab(expression(atop("mean blue whale prediction value", "in DCRB VMS locations"))) +
  ggtitle("2015-16 (coastwide)") +
  scale_fill_manual("",values="lightblue") +
  theme(legend.position=c(0.2, 0.9))

###############
## 2016-2017 coastwide##
###############
ts <- seq.POSIXt(as.POSIXct("2016-11-15"), as.POSIXct("2017-11-11"), by="day")
VMS_2016_2017$date <- as.Date(VMS_2016_2017$westcoastdate)
df <- data.frame(date=as.Date(ts))
newdat <- full_join(df,VMS_2016_2017)  
newdat <- newdat[order(newdat$DOCNUM, newdat$date),]
meandat <- aggregate(newdat$BLWH_value, by=list(newdat$date), FUN=mean, na.rm=T)
CIdat <- aggregate(newdat$BLWH_value, by=list(newdat$date), FUN=smean.cl.boot, na.rm=T)

b <- ggplot() +
  geom_ribbon(data=CIdat, aes(x=Group.1, ymin = x[,2], ymax = x[,3], fill="95% confidence interval")) +
  geom_line(data=meandat, aes(x=Group.1, y=x)) +
  theme_classic() + xlab(NULL) +
  ylab(expression(atop("mean blue whale prediction value", "in DCRB VMS locations"))) +
  ggtitle("2016-17 (coastwide)") +
  scale_fill_manual("",values="lightblue") +
  theme(legend.position=c(0.2, 0.9))

###############
## 2017-2018 coastwide##
###############
ts <- seq.POSIXt(as.POSIXct("2017-11-15"), as.POSIXct("2018-11-11"), by="day")
VMS_2017_2018$date <- as.Date(VMS_2017_2018$westcoastdate)
df <- data.frame(date=as.Date(ts))
newdat <- full_join(df,VMS_2017_2018)  
newdat <- newdat[order(newdat$DOCNUM, newdat$date),]
meandat <- aggregate(newdat$BLWH_value, by=list(newdat$date), FUN=mean, na.rm=T)
CIdat <- aggregate(newdat$BLWH_value, by=list(newdat$date), FUN=smean.cl.boot, na.rm=T)

c <- ggplot() +
  geom_ribbon(data=CIdat, aes(x=Group.1, ymin = x[,2], ymax = x[,3], fill="95% confidence interval")) +
  geom_line(data=meandat, aes(x=Group.1, y=x)) +
  theme_classic() + xlab(NULL) +
  ylab(expression(atop("mean blue whale prediction value", "in DCRB VMS locations"))) +
  ggtitle("2017-18 (coastwide)") +
  scale_fill_manual("",values="lightblue") +
  theme(legend.position=c(0.2, 0.9))

ggarrange(a, b, c, nrow=1, common.legend = T, legend = "bottom")


###############
## 2015-2016 CA ##
###############
ts <- seq.POSIXt(as.POSIXct("2015-11-21"), as.POSIXct("2016-11-10"), by="day")
VMS_2015_2016$date <- as.Date(VMS_2015_2016$westcoastdate)
df <- data.frame(date=as.Date(ts))
newdat <- full_join(df,VMS_2015_2016)  
newdat <- subset(newdat, LATITUDE < 42)
newdat <- newdat[order(newdat$DOCNUM, newdat$date),]
meandat <- aggregate(newdat$BLWH_value, by=list(newdat$date), FUN=mean, na.rm=T)
CIdat <- aggregate(newdat$BLWH_value, by=list(newdat$date), FUN=smean.cl.boot, na.rm=T)

a <- ggplot() +
  geom_ribbon(data=CIdat, aes(x=Group.1, ymin = x[,2], ymax = x[,3], fill="95% confidence interval")) +
  geom_line(data=meandat, aes(x=Group.1, y=x)) +
  theme_classic() + xlab(NULL) +
  ylab(expression(atop("mean blue whale prediction value", "in DCRB VMS locations"))) +
  ggtitle("2015-16 (CA)") +
  scale_fill_manual("",values="lightblue") +
  theme(legend.position=c(0.2, 0.9))

###############
## 2016-2017 CA ##
###############
ts <- seq.POSIXt(as.POSIXct("2016-11-15"), as.POSIXct("2017-11-11"), by="day")
VMS_2016_2017$date <- as.Date(VMS_2016_2017$westcoastdate)
df <- data.frame(date=as.Date(ts))
newdat <- full_join(df,VMS_2016_2017)  
newdat <- subset(newdat, LATITUDE < 42)
newdat <- newdat[order(newdat$DOCNUM, newdat$date),]
meandat <- aggregate(newdat$BLWH_value, by=list(newdat$date), FUN=mean, na.rm=T)
CIdat <- aggregate(newdat$BLWH_value, by=list(newdat$date), FUN=smean.cl.boot, na.rm=T)

b <- ggplot() +
  geom_ribbon(data=CIdat, aes(x=Group.1, ymin = x[,2], ymax = x[,3], fill="95% confidence interval")) +
  geom_line(data=meandat, aes(x=Group.1, y=x)) +
  theme_classic() + xlab(NULL) +
  ylab(expression(atop("mean blue whale prediction value", "in DCRB VMS locations"))) +
  ggtitle("2016-17 (CA)") +
  scale_fill_manual("",values="lightblue") +
  theme(legend.position=c(0.2, 0.9))

###############
## 2017-2018 CA ##
###############
ts <- seq.POSIXt(as.POSIXct("2017-11-15"), as.POSIXct("2018-11-11"), by="day")
VMS_2017_2018$date <- as.Date(VMS_2017_2018$westcoastdate)
df <- data.frame(date=as.Date(ts))
newdat <- full_join(df,VMS_2017_2018)  
newdat <- subset(newdat, LATITUDE < 42)
newdat <- newdat[order(newdat$DOCNUM, newdat$date),]
meandat <- aggregate(newdat$BLWH_value, by=list(newdat$date), FUN=mean, na.rm=T)
CIdat <- aggregate(newdat$BLWH_value, by=list(newdat$date), FUN=smean.cl.boot, na.rm=T)

c <- ggplot() +
  geom_ribbon(data=CIdat, aes(x=Group.1, ymin = x[,2], ymax = x[,3], fill="95% confidence interval")) +
  geom_line(data=meandat, aes(x=Group.1, y=x)) +
  theme_classic() + xlab(NULL) +
  ylab(expression(atop("mean blue whale prediction value", "in DCRB VMS locations"))) +
  ggtitle("2017-18 (CA)") +
  scale_fill_manual("",values="lightblue") +
  theme(legend.position=c(0.2, 0.9))

ggarrange(a, b, c, nrow=1, common.legend = T, legend = "bottom")

###Time series of blue whale pred values for 2017 CA DCRB VMS locations - raw points
# ggplot() +
#   geom_point(data=VMSdat, aes(x=dt, y=BLWH_value))+
#   theme_classic() + xlab("time") +
#   ylab("Blue whale prediction value") +
#   ggtitle("2017 DCRB VMS locations in CA") +
#   scale_fill_manual("",values="lightblue") +
#   theme(legend.position=c(0.2, 0.9))
  

### separate plots for northern vs. southern mgmt area divided by lat 38.769

##### southern area season is Nov 15-Jun 30

# southerndat <- subset(newdat, LATITUDE<=38.769)
# southernmeandat <- aggregate(southerndat$BLWH_value, by=list(southerndat$dt), FUN=mean, na.rm=T)
# southernCIdat <- aggregate(southerndat$BLWH_value, by=list(southerndat$dt), FUN=smean.cl.boot)
# 
# ggplot() +
#   geom_ribbon(data=southernCIdat, aes(x=Group.1, ymin = x[,2], ymax = x[,3], fill="95% confidence interval")) +
#   geom_line(data=southernmeandat, aes(x=Group.1, y=x)) +
#   theme_classic() + xlab("time") +
#   ylab("Average blue whale prediction value") +
#   ggtitle("2017 DCRB VMS locations in southern CA area") +
#   scale_fill_manual("",values="lightblue") +
#   theme(legend.position=c(0.2, 0.9))
# 
# ##### northern area season Dec 1-July 15
# 
# northerndat <- subset(newdat, LATITUDE>38.769)
# northernmeandat <- aggregate(northerndat$BLWH_value, by=list(northerndat$dt), FUN=mean, na.rm=T)
# northernCIdat <- aggregate(northerndat$BLWH_value, by=list(northerndat$dt), FUN=smean.cl.boot)
# 
# ggplot() +
#   geom_ribbon(data=northernCIdat, aes(x=Group.1, ymin = x[,2], ymax = x[,3], fill="95% confidence interval")) +
#   geom_line(data=northernmeandat, aes(x=Group.1, y=x)) +
#   theme_classic() + xlab("time") +
#   ylab("Average blue whale prediction value") +
#   ggtitle("2017 DCRB VMS locations in Northern CA area") +
#   scale_fill_manual("",values="lightblue") +
#   theme(legend.position=c(0.2, 0.9))


### Time series of blue whale pred values by habitat quantiles

#Add quantiles
# VMSdat$quantile <- ifelse(VMSdat$BLWH_value > 0.75, 1, 
#                           ifelse(VMSdat$BLWH_value > 0.5, 2,
#                                  ifelse(VMSdat$BLWH_value > 0.25, 3, 4)))
# 
# ggplot(VMSdat[complete.cases(VMSdat$quantile),], aes(x=dt, color=as.factor(quantile))) + 
#   geom_freqpoly(size=1.1) +
#   theme_classic() + xlab("time") +
#   ylab("# VMS locations") +
#   ggtitle("2017 DCRB VMS locations in CA") +
#   scale_color_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%")) +
#   theme(legend.position="right")

###############
## 2015-2016 ##
###############
VMS_2015_2016$quantile <- ifelse(VMS_2015_2016$BLWH_value > 0.75, 1, 
                          ifelse(VMS_2015_2016$BLWH_value > 0.5, 2,
                                 ifelse(VMS_2015_2016$BLWH_value > 0.25, 3, 4)))

a <- ggplot(VMS_2015_2016[complete.cases(VMS_2015_2016$quantile),], aes(x=as.Date(westcoastdate), color=as.factor(quantile))) + 
  geom_freqpoly(size=1.1) +
  theme_classic() + xlab(NULL) +
  ylab("# VMS locations") +
  ggtitle("2015-16 (coastwide)") +
  scale_color_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%")) +
  theme(legend.position="right")

###############
## 2016-2017 ##
###############
VMS_2016_2017$quantile <- ifelse(VMS_2016_2017$BLWH_value > 0.75, 1, 
                                 ifelse(VMS_2016_2017$BLWH_value > 0.5, 2,
                                        ifelse(VMS_2016_2017$BLWH_value > 0.25, 3, 4)))

b <- ggplot(VMS_2016_2017[complete.cases(VMS_2016_2017$quantile),], aes(x=as.Date(westcoastdate), color=as.factor(quantile))) + 
  geom_freqpoly(size=1.1) +
  theme_classic() + xlab(NULL) +
  ylab("# VMS locations") +
  ggtitle("2016-17 (coastwide)") +
  scale_color_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%")) +
  theme(legend.position="right")

###############
## 2017-2018 ##
###############
VMS_2017_2018$quantile <- ifelse(VMS_2017_2018$BLWH_value > 0.75, 1, 
                                 ifelse(VMS_2017_2018$BLWH_value > 0.5, 2,
                                        ifelse(VMS_2017_2018$BLWH_value > 0.25, 3, 4)))

c <- ggplot(VMS_2017_2018[complete.cases(VMS_2017_2018$quantile),], aes(x=as.Date(westcoastdate), color=as.factor(quantile))) + 
  geom_freqpoly(size=1.1) +
  theme_classic() + xlab(NULL) +
  ylab("# VMS locations") +
  ggtitle("2017-18 (coastwide)") +
  scale_color_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%")) +
  theme(legend.position="right")

ggarrange(a, b, c, nrow=1, common.legend = T, legend = "right")

###############
## 2015-2016 CA ##
###############
VMS_2015_2016$quantile <- ifelse(VMS_2015_2016$BLWH_value > 0.75, 1, 
                                 ifelse(VMS_2015_2016$BLWH_value > 0.5, 2,
                                        ifelse(VMS_2015_2016$BLWH_value > 0.25, 3, 4)))

a <- ggplot(VMS_2015_2016[complete.cases(VMS_2015_2016$quantile) & VMS_2015_2016$LATITUDE < 42,], aes(x=as.Date(westcoastdate), color=as.factor(quantile))) + 
  geom_freqpoly(size=1.1) +
  theme_classic() + xlab(NULL) +
  ylab("# VMS locations") +
  ggtitle("2015-16 (CA)") +
  scale_color_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%")) +
  theme(legend.position="right")

###############
## 2016-2017 CA ##
###############
VMS_2016_2017$quantile <- ifelse(VMS_2016_2017$BLWH_value > 0.75, 1, 
                                 ifelse(VMS_2016_2017$BLWH_value > 0.5, 2,
                                        ifelse(VMS_2016_2017$BLWH_value > 0.25, 3, 4)))

b <- ggplot(VMS_2016_2017[complete.cases(VMS_2016_2017$quantile) & VMS_2016_2017$LATITUDE < 42,], aes(x=as.Date(westcoastdate), color=as.factor(quantile))) + 
  geom_freqpoly(size=1.1) +
  theme_classic() + xlab(NULL) +
  ylab("# VMS locations") +
  ggtitle("2016-17 (CA)") +
  scale_color_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%")) +
  theme(legend.position="right")

###############
## 2017-2018 CA ##
###############
VMS_2017_2018$quantile <- ifelse(VMS_2017_2018$BLWH_value > 0.75, 1, 
                                 ifelse(VMS_2017_2018$BLWH_value > 0.5, 2,
                                        ifelse(VMS_2017_2018$BLWH_value > 0.25, 3, 4)))

c <- ggplot(VMS_2017_2018[complete.cases(VMS_2017_2018$quantile) & VMS_2017_2018$LATITUDE < 42,], aes(x=as.Date(westcoastdate), color=as.factor(quantile))) + 
  geom_freqpoly(size=1.1) +
  theme_classic() + xlab(NULL) +
  ylab("# VMS locations") +
  ggtitle("2017-18 (CA)") +
  scale_color_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%")) +
  theme(legend.position="right")

ggarrange(a, b, c, nrow=1, common.legend = T, legend = "right")

### Monthly proportion of 2017 CA DCRB VMS locations by habitat quartiles

# ggplot(VMSdat[complete.cases(VMSdat$quantile),], aes(x=as.factor(month(dt)), fill = as.factor(quantile))) +
#   geom_bar(position = "fill") +
#   theme_classic() + xlab("month") +
#   ylab("proportion of VMS locations") +
#   ggtitle("2017 DCRB VMS locations in CA") +
#   scale_fill_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%"))

a <- ggplot(VMS_2015_2016[complete.cases(VMS_2015_2016$quantile),], aes(x=as.factor(month(westcoastdate)), fill = as.factor(quantile))) +
  geom_bar(position = "fill") +
  theme_classic() + xlab("month") +
  ylab("proportion of VMS locations") +
  ggtitle("2015-16 (coastwide)") +
  scale_fill_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%"))

b <- ggplot(VMS_2016_2017[complete.cases(VMS_2016_2017$quantile),], aes(x=as.factor(month(westcoastdate)), fill = as.factor(quantile))) +
  geom_bar(position = "fill") +
  theme_classic() + xlab("month") +
  ylab("proportion of VMS locations") +
  ggtitle("2016-17 (coastwide)") +
  scale_fill_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%"))

c <- ggplot(VMS_2017_2018[complete.cases(VMS_2017_2018$quantile),], aes(x=as.factor(month(westcoastdate)), fill = as.factor(quantile))) +
  geom_bar(position = "fill") +
  theme_classic() + xlab("month") +
  ylab("proportion of VMS locations") +
  ggtitle("2017-18 (coastwide)") +
  scale_fill_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%"))

ggarrange(a, b, c, nrow=1, common.legend = T, legend = "right")

a <- ggplot(VMS_2015_2016[complete.cases(VMS_2015_2016$quantile) & VMS_2015_2016$LATITUDE < 42,], aes(x=as.factor(month(westcoastdate)), fill = as.factor(quantile))) +
  geom_bar(position = "fill") +
  theme_classic() + xlab("month") +
  ylab("proportion of VMS locations") +
  ggtitle("2015-16 (CA)") +
  scale_fill_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%"))

b <- ggplot(VMS_2016_2017[complete.cases(VMS_2016_2017$quantile) & VMS_2016_2017$LATITUDE < 42,], aes(x=as.factor(month(westcoastdate)), fill = as.factor(quantile))) +
  geom_bar(position = "fill") +
  theme_classic() + xlab("month") +
  ylab("proportion of VMS locations") +
  ggtitle("2016-17 (CA)") +
  scale_fill_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%"))

c <- ggplot(VMS_2017_2018[complete.cases(VMS_2017_2018$quantile) & VMS_2017_2018$LATITUDE < 42,], aes(x=as.factor(month(westcoastdate)), fill = as.factor(quantile))) +
  geom_bar(position = "fill") +
  theme_classic() + xlab("month") +
  ylab("proportion of VMS locations") +
  ggtitle("2017-18 (CA)") +
  scale_fill_discrete(name = "quantile (blue whale \nprobability values)", labels = c("76-100%", "51-75%", "26-50%", "0-25%"))

ggarrange(a, b, c, nrow=1, common.legend = T, legend = "right")


#4. Time-use metrics -----------------> ####

# Time to Return (T2R) measured as the number of hours spent beyond a specified cut-off time before return to a 
# circle of a given radius centered on each location.
# Residence Time (RT) measured as the number of hours spent inside a circle of a given radius centered on each 
# location without leaving the radius for more than a specified cut-off time.

# Convert VMS file to list of ltraj objects from adehabitatLT package

VMSdat$DOCNUM <- droplevels(VMSdat$DOCNUM) #drop unused id levels
VMSdat <- VMSdat[!duplicated(VMSdat[,c("DOCNUM","UTCDATETIM")]),] #get rid of duplicate points

ltraj<-as.ltraj(xy=VMSdat[,c("LONGITUDE", "LATITUDE")], 
                date=as.POSIXct(VMSdat$UTCDATETIM, format="%m/%d/%y %H:%M", tz="UTC"), 
                id=VMSdat$DOCNUM,
                infolocs=VMSdat[,c(1,2,7:32)])

# Function to calculate RT and T2R
RTandT2R <- function(x, radius, maxt, units="hour", addinfo = F){
  fR <- function(x, dframe, radius, maxt, units=units){
    tmp <- dframe[c(x:nrow(dframe)),]
    dists <- sqrt((tmp$x - tmp$x[1])^2 + (tmp$y - tmp$y[1])^2)
    dists <- as.numeric(dists<=radius)
    ext <- which(dists[-length(dists)] > dists[-1])+1
    entr <-  which(dists[-length(dists)] < dists[-1])+1
    bts <- difftime(tmp$date[entr], tmp$date[ext[c(1:length(entr))]], units=units)    
    tmp1 <- as.numeric(difftime(tmp$date[ext[(as.numeric(bts)>maxt)][1]], tmp$date[1], units=units)) #first exit
    if (is.na(tmp1) & length(ext)>0) tmp1 <- as.numeric(difftime(tmp$date[ext[length(ext)]], tmp$date[1], units=units))  
    tmp2 <- as.numeric(difftime(tmp$date[entr[(as.numeric(bts)>maxt)][1]], tmp$date[1], units=units)) #first re-entry
    return(c(tmp1, tmp2))
  } 
  res <- data.frame(do.call(rbind, lapply(c(1:nrow(x)), fR, dframe=x, radius=radius, maxt=maxt, units=units)))
  names(res) <- c(paste("RT", radius, maxt, sep="_"), paste("T2R", radius, maxt, sep="_"))
  
  if (!addinfo) return(res)
  if (addinfo) {
    attributes(x)$infolocs <- cbind(attributes(x)$infolocs, res)
    return(x) 
  }
}

# create for-loop for calculating RT and T2R for multiple vessels
# Run for radius rads = 0.1 deg, cut-off time maxts = 12hr

lres <- list()
for (j in 1:length(ltraj)){
  res <- ltraj[[j]][,c("x","y","date")]
  # for mean step length: meanDist<- mean(ltraj[[j]][1:nrow(ltraj[[j]])-1,"dist"], na.rm=T) 
  # rads <- c(meanDist) 
  rads <- 0.1
  maxts <- c(12) 
  params <- expand.grid(rads=rads, maxts=maxts)
  for (i in 1:nrow(params)){
    nams <- names(res)
    tmp <- RTandT2R(ltraj[[j]], radius = params$rads[i], maxt=params$maxts[i], units="hour", addinfo = F)
    res <- cbind(res, tmp)
    names(res) <- c(nams, paste("RT", params$rads[i], params$maxts[i], sep="_"), paste("T2R", params$rads[i], params$maxts[i], sep="_"))
  }
  lres[[j]] <- res
}

#Join lres to ltraj object
unlist_ltraj<-adehabitatLT::ld(ltraj) #unlist ltraj object
unlist_lres <- do.call("rbind", lres) #unlist lres
VMSdat_metrics <- merge(unlist_ltraj, unlist_lres, by=c("x","y","date")) #merge
  
#Clean up
VMSdat_metrics$DOCNUM <- VMSdat_metrics$id
VMSdat_metrics$UTCDATETIM <- VMSdat_metrics$date
VMSdat_metrics$LATITUDE <- VMSdat_metrics$y
VMSdat_metrics$LONGITUDE <- VMSdat_metrics$x
colnames(VMSdat_metrics)[39] <- "DATE"

#Check and save
head(VMSdat_metrics) 
saveRDS(VMSdat_metrics, paste0(WEARfolder,"vms_2017_10d_targets_all_attributes_geo_metrics.RDS"))


#5. Plot blue whale prediction values & VMS time-use metrics -----------------> ####

### Scatterplot of Time2Return vs. Residence Time of VMS locations

ggplot(VMSdat_metrics) + geom_point(aes(x=RT_0.1_12, y=T2R_0.1_12)) +
  xlab("Residence Time (hours)") + ylab("Time to Return (hours)") + 
  theme_bw()

### Time series of Time2Return and Residence Times of VMS locations

q <- ggplot() + 
  geom_line(data = VMSdat_metrics %>% group_by(DATE) %>% summarise(RT=mean(RT_0.1_12, na.rm=T)), aes(x = DATE, y = RT), color="darkgoldenrod1", size=1.1) +
  xlab("time") + ylab("Average residence time (hours)") + 
  theme_bw()

p <- ggplot() + 
  geom_line(data = VMSdat_metrics %>% group_by(DATE) %>% summarise(T2R=mean(T2R_0.1_12, na.rm=T)), aes(x = DATE, y = T2R), color="slateblue1", size=1.1) +
  xlab("time") + ylab("Average time to return (hours)") + 
  theme_bw()

grid.arrange(p,q)

## % difference between the 3 years (and just in june-july of those 3 years) in the # vms locations in the upper quartile of blwh habitat
VMS_2015_2016 %>% 
  filter(month(date)==6 | month(date)==7) %>% 
  count(quantile) %>%
  transmute(quantile, Percentage=n/sum(n)*100)

VMS_2016_2017 %>% 
  filter(month(date)==6 | month(date)==7) %>% 
  count(quantile) %>%
  transmute(quantile, Percentage=n/sum(n)*100)

VMS_2017_2018 %>% 
  filter(month(date)==6 | month(date)==7) %>% 
  count(quantile) %>%
  transmute(quantile, Percentage=n/sum(n)*100)

###################################
## bind dfs together and save #####

VMS_2016_2017 <- VMS_2016_2017[,-c(17,18)] #drop "dx", "dt"
VMS_2017_2018 <- VMS_2017_2018[,-c(17,18)] #drop "dx", "dt"
VMS_2015_2016 <- VMS_2015_2016[, c(1:12,14:17,13,18:23)] #move "avg_speed_recalc" to match other dfs

df <- rbind(VMS_2015_2016, VMS_2016_2017)
df <- rbind(df, VMS_2017_2018)

saveRDS(df, paste0(VMSfolder, 'vms_crabseasons_2015-2018_final_cleaned_interpolated_regular_dcrb_BLWH.RDS'))

ggplot(df[complete.cases(df$quantile) & df$quantile==1,], aes(x=as.Date(westcoastdate))) + 
  geom_freqpoly(size=1.1) +
  theme_classic() + xlab(NULL) +
  ylab("# VMS locations") +
  theme(legend.position="right")
