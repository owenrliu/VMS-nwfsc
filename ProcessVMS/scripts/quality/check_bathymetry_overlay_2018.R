setwd("D:/VMS-repo")

library(readr)
library(foreign)
library(lubridate)
library(dplyr)
library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(maps)
library(ggplot2)

year = 2018
mydat <- read.dbf("Input_Data/fromOLE_cleaned/VMS_all_data_2018_edited.dbf")
bdat <- read.dbf("Input_Data/fromBlakeNov2018/bath_region_all_data_to_june_2018_bath_lamb.dbf")

mydat_thin <- dplyr::select(mydat, UTCDATETIM, LATITUDE,LONGITUDE,DOCNUM,NGDC_M)
colnames(mydat_thin)[5] <- "NGDC_r"
bdat_thin <- dplyr::select(mydat, UTCDATETIM, LATITUDE,LONGITUDE,DOCNUM,NGDC_M)
colnames(bdat_thin)[5] <- "NGDC_gis"

mydat_thin$UTCDATETIM <- parse_date_time(mydat_thin$UTCDATETIM, orders=c("Ymd_HMS"))
bdat_thin$UTCDATETIM <- parse_date_time(bdat_thin$UTCDATETIM, orders=c("Ymd_HMS"))

compare_dat <- left_join(mydat_thin, bdat_thin, by=c("UTCDATETIM", "LATITUDE", "LONGITUDE", "DOCNUM"))
compare_dat <- compare_dat %>%
  mutate(diff_ngdc = ifelse(!is.na(NGDC_gis), NGDC_r-NGDC_gis, NA))
ggplot(compare_dat, aes(y=diff_ngdc)) + geom_boxplot()
sum(is.na(compare_dat$diff_ngdc)) # 28915 out of 2673462
sum(compare_dat$diff_ngdc != 0) # 0
length(compare_dat$diff_ngdc)



