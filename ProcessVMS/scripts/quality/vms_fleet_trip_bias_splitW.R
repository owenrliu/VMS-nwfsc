######### Bias in Dungeness crab VMS data #########
#
# 7/11/2019 - M. Fisher
#
###################################################
rm(list=ls())

library(dplyr)
library(readr)
library(reshape2)
library(ggplot2)
library(tidyr)


setwd("C:/Users/Mary.Fisher/Documents/VMS-repo/ProcessVMS")
source("scripts/quality/vms_fleet_trip_bias_FUNCTIONS.r")


# Read in data ------------------------------------------------------------
years <- seq(2009,2017)
states <- c("W") #C/O/W
state <- "Washington"
splitW <- FALSE


# fish tickets (by calendar year)
for(y in years){
  tmptix <- read.csv(paste0("../Input_Data/processed_multisp/fish tickets ",y," processed_multispecies03.csv"))
  if(y==years[1]){
    tickets <- tmptix
  } else{
    tmptix <- dplyr::select(tmptix, colnames(tickets))
    tickets <- rbind(tickets,tmptix)
  }
}

# new crab year length data
for(y in years[1:(length(years)-1)]){
  tmpvl <- read.csv(paste0("../Input_Data/length_keys/dcrb_vessel_length_key_crab",y,"_all.csv"))
  tmpvl <- mutate(tmpvl, crab_year=y)
  if(y==years[1]){
    lkey <- tmpvl
  } else{
    lkey <- rbind(lkey,tmpvl)
  }
}

if(dim(filter(tickets,year==2017))[1] < 1){
  tickets <- rbind(tickets,tmptix)
}
head(tickets)
head(lkey)



# Edit ticket data  -------------------------------------------------------------
# add crab year to tickets
add_crab_year <- function(weeks,years){
  crab_years <- c()
  for(i in seq(1,length(weeks))){
    tmpw <- weeks[i]
    tmpyr <- years[i]
    if(tmpw >45){
      crab_years[i] <- tmpyr
    } else{
      crab_years[i] <- tmpyr-1
    }
  }
  return(crab_years)
}

tickets <- mutate(tickets, crab_year = add_crab_year(weeks=Week,years=year))

# change month names
tickets$month <- factor(tickets$month, levels=c("November","December","January","February",
                                              "March","April","May","June","July","August",
                                              "September","October"))
tickets$month <- recode(tickets$month, "November"="Nov","December"="Dec","January"="Jan",
                       "February"="Feb","March"="Mar","April"="Apr","May"="May","June"="Jun",
                       "July"="Jul","August"="Aug","September"="Sep","October"="Oct")
tickets$month <- as.character(tickets$month)

# filter tickets
tickets_filtered <- tickets %>%
  filter(removal_type_code %in% c("C", "D", "U") & agency_code %in% states) %>%
  mutate(region=ifelse(port_group_code %in% c("NPS","SPS"), "Puget Sound","Coastal/Other"))
lkey_filtered <- filter(lkey,agency_code %in% states)



# Count dcrb vms records / vms-covered trips per vessel  -------------------------------------------------------------
# matched, unfiltered
for(y in years[2:length(years)]){
  tmpvms1 <- read.csv(paste0("R_Output/match/unfiltered/VMS_Outputs_wTARGET_10d_lookback_",y,"_all.csv"))
  tmpvms2 <- read.csv(paste0("R_Output/match/unfiltered/VMS_Outputs_wTARGET_10d_lookback_",y+1,"_all.csv"))
  tmpvms2 <- tmpvms2[,colnames(tmpvms1)]
  tmpvms <- rbind(tmpvms1,tmpvms2);rm(tmpvms1,tmpvms2)
  tmpdat <- vessel_representation_processed(tickets=tickets_filtered, vms=tmpvms, length_data=lkey_filtered, y=y, region=FALSE)
  tmpdat$crab_year[which(is.na(tmpdat$crab_year))] <- y
  if(y==years[2]){
    matchdat <- tmpdat
  } else{
    matchdat <- rbind(matchdat,tmpdat)
  }
  cat("done with data for ", y,".\n")
}
rm(tmpvms)
## (remove 2017 data for which I don't have vessel lengths)
matchdat <- filter(matchdat, crab_year != 2017)
matchdat$n.dcrb.trips.vms[which(is.na(matchdat$n.dcrb.trips.vms))]<- 0
matchdat$n.dcrb.records[which(is.na(matchdat$n.dcrb.records))]<- 0
matchdat <- mutate(matchdat, VMS = ifelse(n.dcrb.trips.vms > 0, "Yes","No"))
if(splitW){
  write.csv(matchdat,paste0("R_Output/quality/VMS_coverage_matchedVMS_yearly_",states[1],"_byregion.csv"))
} else{
  write.csv(matchdat,paste0("R_Output/quality/VMS_coverage_matchedVMS_yearly_",states[1],".csv"))
}


# matched, final filtered
for(y in years[2:length(years)]){
  tmpvms1 <- read.csv(paste0("R_Output/interpolation/VMS_Outputs_wTARGET_10d_lookback_",y,"_final_cleaned.csv"))
  tmpvms2 <- read.csv(paste0("R_Output/interpolation/VMS_Outputs_wTARGET_10d_lookback_",y+1,"_final_cleaned.csv"))
  tmpvms2 <- tmpvms2[,colnames(tmpvms1)]
  tmpvms <- rbind(tmpvms1,tmpvms2);rm(tmpvms1,tmpvms2)
  tmpdat <- vessel_representation_processed(tickets=tickets_filtered, vms=tmpvms, length_data=lkey_filtered, year=y)
  tmpdat$crab_year[which(is.na(tmpdat$crab_year))] <- y
  if(y==years[2]){
    finaldat <- tmpdat
  } else{
    finaldat <- rbind(finaldat,tmpdat)
  }
  cat("done with data for ", y,".\n")
}
rm(tmpvms)
## (remove 2017 data for which I don't have vessel lengths)
finaldat <- filter(finaldat, crab_year != 2017)
finaldat$n.dcrb.trips.vms[which(is.na(finaldat$n.dcrb.trips.vms))]<- 0
finaldat$n.dcrb.records[which(is.na(finaldat$n.dcrb.records))]<- 0
finaldat <- mutate(finaldat, VMS = ifelse(n.dcrb.trips.vms > 0, "Yes","No"))
write.csv(finaldat,paste0("R_Output/quality/VMS_coverage_finalVMS_yearly_",states[1],".csv"))




# Size Distribution -------------------------------------------------------
myplot <- ggplot(data=matchdat, aes(x=mean_length, fill=VMS,alpha=VMS)) +
  geom_histogram() +
  facet_grid(cols=vars(region),row=vars(crab_year)) +
  geom_vline(xintercept=40,col="black") +
  ylab("Number of vessels") + xlab("Vessel Length (ft)") + 
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched VMS")) +
  scale_alpha_manual(values=c(1,0.5)) + scale_fill_manual(values=c("coral2","cyan3")) +
  theme_bw() + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                     axis.text=element_text(size=12),axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_text(size=14))
png(paste0("R_Output/quality/vms_coverage/dcrb_vessel_size_matchedVMS_yearly_",states[1],"_byregion.png"))
myplot
dev.off()

myplot <- ggplot(finaldat, aes(x=mean_length, fill=VMS,alpha=VMS)) +
  geom_histogram() +
  facet_wrap(~crab_year) +
  ylab("Number of vessels") + xlab("Vessel Length (ft)") + 
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched, Filtered VMS")) +
  scale_alpha_manual(values=c(1,0.5)) + scale_fill_manual(values=c("coral2","cyan3")) +
  theme_bw() + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                     axis.text=element_text(size=12),axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_text(size=14))
png(paste0("R_Output/quality/vms_coverage/dcrb_vessel_size_finalVMS_yearly_",states[1],".png"))
myplot
dev.off()


# Proportion of Vessels -------------------------------------------
mytab=with(matchdat,table(crab_year,region,vessel_cat,VMS))
n.vessels.vms <- as.data.frame(mytab)
n.vessels.vms <- n.vessels.vms %>%
  spread(key=VMS,value=Freq) %>%
  mutate(percent.wVMS = Yes/(Yes+No))


myplot <- ggplot(data=filter(n.vessels.vms, vessel_cat!="unknown"),aes(x=crab_year,y=percent.wVMS, fill=vessel_cat)) +
  geom_col(position="dodge") +
  facet_wrap(~region) +
  xlab("Crab Year") + ylab("D. crab Vessels with VMS") + 
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched VMS")) +
  scale_y_continuous(breaks=seq(0,0.75,by=0.1),labels=seq(0,0.75,by=0.1),limits=c(0,0.75),expand=c(0,0)) +
  theme_classic() + theme(axis.text=element_text(size=12),axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_blank())
png(paste0("R_Output/quality/vms_coverage/dcrb_aloVMS_bycat_matchVMS_yearly_",states[1],"_byregion.png"))
myplot
dev.off()


mytab=with(finaldat,table(crab_year,vessel_cat,VMS))
n.vessels.vms <- as.data.frame(mytab)
n.vessels.vms <- n.vessels.vms %>%
  spread(key=VMS,value=Freq) %>%
  mutate(percent.wVMS = Yes/(Yes+No))


myplot <- ggplot(n.vessels.vms,aes(x=crab_year,y=percent.wVMS, fill=vessel_cat)) +
  geom_col(position="dodge") +
  xlab("Crab Year") + ylab("D. crab Vessels with VMS") + 
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched, Filtered VMS")) +
  scale_y_continuous(breaks=seq(0,0.75,by=0.1),labels=seq(0,0.75,by=0.1),limits=c(0,0.75),expand=c(0,0)) +
  theme_classic() + theme(axis.text=element_text(size=12),axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_blank())
png(paste0("R_Output/quality/vms_coverage/dcrb_aloVMS_bycat_finalVMS_yearly_",states[1],".png"))
myplot
dev.off()




# Proportion of Trips -----------------------------------------------------
n.trips.vms <- matchdat %>%
  group_by(crab_year,vessel_cat,region) %>%
  summarise(total.dcrb.trips = sum(n.dcrb.trips),trips.wVMS=sum(n.dcrb.trips.vms)) %>%
  mutate(percent.wVMS=trips.wVMS/total.dcrb.trips)

myplot <- ggplot(data=filter(n.trips.vms,vessel_cat!="unknown"),aes(x=crab_year,y=percent.wVMS, fill=vessel_cat)) +
  geom_col(position="dodge") +
  facet_wrap(~region) +
  xlab("Crab Year") + ylab("D. crab Trips Covered by VMS") + 
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched VMS")) +
  scale_y_continuous(breaks=seq(0,0.3,by=0.1),labels=seq(0,0.3,by=0.1),limits=c(0,0.3),expand=c(0,0)) +
  theme_classic() + theme(axis.text=element_text(size=12),axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_blank(),
                     strip.text=element_text(size=12))
png(paste0("R_Output/quality/vms_coverage/dcrb_trips_wVMS_bycat_matchVMS_yearly_",states[1],"_byregion.png"))
myplot
dev.off()


n.trips.vms <- finaldat %>%
  group_by(crab_year,vessel_cat) %>%
  summarise(total.dcrb.trips = sum(n.dcrb.trips),trips.wVMS=sum(n.dcrb.trips.vms)) %>%
  mutate(percent.wVMS=trips.wVMS/total.dcrb.trips)

myplot <- ggplot(n.trips.vms,aes(x=crab_year,y=percent.wVMS, fill=vessel_cat)) +
  geom_col(position="dodge") +
  xlab("Crab Year") + ylab("D. crab Trips Covered by VMS") + 
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched, Filtered VMS")) +
  scale_y_continuous(breaks=seq(0,0.3,by=0.1),labels=seq(0,0.3,by=0.1),limits=c(0,0.3),expand=c(0,0)) +
  theme_classic() + theme(axis.text=element_text(size=12),axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_blank())
png(paste0("R_Output/quality/vms_coverage/dcrb_trips_wVMS_bycat_finalVMS_yearly_",states[1],".png"))
myplot
dev.off()




# Count dcrb pounds / revenue per vessel per month  -------------------------------------------------------------
# matched, unfiltered
for(y in years[2:length(years)]){
  tmpvms1 <- read.csv(paste0("R_Output/match/unfiltered/VMS_Outputs_wTARGET_10d_lookback_",y,"_all.csv"))
  tmpvms2 <- read.csv(paste0("R_Output/match/unfiltered/VMS_Outputs_wTARGET_10d_lookback_",y+1,"_all.csv"))
  tmpvms2 <- tmpvms2[,colnames(tmpvms1)]
  tmpvms <- rbind(tmpvms1,tmpvms2);rm(tmpvms1,tmpvms2)
  tmpdat <- trip_representation_monthly(tickets=tickets_filtered, vms=tmpvms, length_data=lkey_filtered, y=y)
  sumdat <- tmpdat %>%
    mutate(VMS=ifelse(n.records > 0, "Yes","No")) %>%
    group_by(drvid,crab_year,agency_code,mean_length,month,VMS) %>%
    summarise(total.lbs=sum(DCRB..lbs.),total.rev=sum(DCRB....))
  if(y==years[2]){
    matchdat.trips <- sumdat
  } else{
    matchdat.trips <- rbind(matchdat.trips,sumdat)
  }
  cat("done with data for ", y,".\n")
}
rm(tmpvms)
## (remove 2017 data for which I don't have vessel lengths)
matchdat.trips <- filter(matchdat.trips, crab_year != 2017)
write.csv(matchdat.trips,paste0("R_Output/quality/VMS_poundsrev_coverage_matchedVMS_yearly_",states[1],".csv"))


# matched, final filtered







# Number of Pounds / Revenue per month ----------------------------------------------------
sumdat <- matchdat.trips %>% 
  mutate(vessel_cat=ifelse(mean_length >=40,"Large","Small")) %>%
  group_by(crab_year,month,vessel_cat,VMS) %>%
  summarise(dcrb.lbs=sum(total.lbs),dcrb.rev=sum(total.rev))
sumdat$month <- factor(sumdat$month, levels=c("Nov","Dec","Jan","Feb",
                                                "Mar","Apr","May","Jun","Jul","Aug",
                                                "Sep","Oct"))

#lbs
myplot <- ggplot(data=filter(sumdat, !is.na(vessel_cat)),aes(x=month,y=dcrb.lbs/100000,fill=VMS)) +
  geom_col() +
  facet_grid(cols=vars(crab_year),rows=vars(vessel_cat),scales="free_y") +
  xlab("Month") + ylab("Landed Lbs of D. Crab (x100k)") +
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched VMS")) +
  scale_fill_manual(values=c("coral2","cyan3")) +
  scale_x_discrete(name="Month",labels=c("","Dec","","Feb","","Apr","","Jun","","Aug","","Oct")) +
  theme_bw() + theme(panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank(),
                     axis.text.x=element_text(angle=90,hjust=1,vjust=0.2,size=12),
                     axis.text.y=element_text(size=12), strip.text=element_text(size=14),
                     axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_text(size=14))
png(paste0("R_Output/quality/vms_coverage/dcrb_lbs_wVMS_bycat_matchVMS_yearly_",states[1],".png"),width=720,height=400)
myplot
dev.off()

myplot <- ggplot(data=sumdat,aes(x=month,y=dcrb.lbs/100000,fill=VMS)) +
  geom_col() +
  facet_grid(cols=vars(crab_year),rows=vars(vessel_cat),scales="free_y") +
  xlab("Month") + ylab("Landed Lbs of D. Crab (x100k)") +
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched VMS")) +
  scale_fill_manual(values=c("coral2","cyan3")) +
  scale_x_discrete(name="Month",labels=c("","Dec","","Feb","","Apr","","Jun","","Aug","","Oct")) +
  theme_bw() + theme(panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank(),
                     axis.text.x=element_text(angle=90,hjust=1,vjust=0.2,size=12),
                     axis.text.y=element_text(size=12), strip.text=element_text(size=14),
                     axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_text(size=14))
png(paste0("R_Output/quality/vms_coverage/dcrb_lbs_wVMS_bycat_matchVMS_yearly_",states[1],"_wNA.png"),width=720,height=400)
myplot
dev.off()

#revenue
myplot <- ggplot(data=filter(sumdat,!is.na(vessel_cat)),aes(x=month,y=dcrb.rev/100000,fill=VMS)) +
  geom_col() +
  facet_grid(cols=vars(crab_year),rows=vars(vessel_cat),scales="free_y") +
  xlab("Month") + ylab("Landed Lbs of D. Crab (x100k)") +
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched VMS")) +
  scale_fill_manual(values=c("coral2","cyan3")) +
  scale_x_discrete(name="Month",labels=c("","Dec","","Feb","","Apr","","Jun","","Aug","","Oct")) +
  theme_bw() + theme(panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank(),
                     axis.text.x=element_text(angle=90,hjust=1,vjust=0.2,size=12),
                     axis.text.y=element_text(size=12), strip.text=element_text(size=14),
                     axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_text(size=14))
png(paste0("R_Output/quality/vms_coverage/dcrb_exrev_wVMS_bycat_matchVMS_yearly_",states[1],".png"),width=720,height=400)
myplot
dev.off()

myplot <- ggplot(data=sumdat,aes(x=month,y=dcrb.rev/100000,fill=VMS)) +
  geom_col() +
  facet_grid(cols=vars(crab_year),rows=vars(vessel_cat),scales="free_y") +
  xlab("Month") + ylab("Landed Lbs of D. Crab (x100k)") +
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched VMS")) +
  scale_fill_manual(values=c("coral2","cyan3")) +
  scale_x_discrete(name="Month",labels=c("","Dec","","Feb","","Apr","","Jun","","Aug","","Oct")) +
  theme_bw() + theme(panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank(),
                     axis.text.x=element_text(angle=90,hjust=1,vjust=0.2,size=12),
                     axis.text.y=element_text(size=12), strip.text=element_text(size=14),
                     axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_text(size=14))
png(paste0("R_Output/quality/vms_coverage/dcrb_exrev_wVMS_bycat_matchVMS_yearly_",states[1],"_wNA.png"),width=720,height=400)
myplot
dev.off()


# Proportion of Pounds / Revenue per month, by vessel size ----------------------------------------------------
sumdat.lbs <- sumdat %>%
  dplyr::select(-dcrb.rev) %>%
  spread(key=VMS,value=dcrb.lbs)
sumdat.lbs$No[which(is.na(sumdat.lbs$No))] <- 0
sumdat.lbs$Yes[which(is.na(sumdat.lbs$Yes))] <- 0
sumdat.lbs <- sumdat.lbs %>%
  mutate(`without VMS` = No / (No + Yes), `with VMS` = Yes / (No + Yes)) %>%
  dplyr::select(-No,-Yes) %>%
  melt(id.vars=c("crab_year","month","vessel_cat"))
sumdat.lbs$month <- factor(sumdat.lbs$month, levels=c("Nov","Dec","Jan","Feb",
                                              "Mar","Apr","May","Jun","Jul","Aug",
                                              "Sep","Oct"))

#lbs
myplot <- ggplot(data=filter(sumdat.lbs, !is.na(vessel_cat)),aes(x=factor(month),y=value,fill=variable)) +
  geom_col(width=1.01) +
  facet_grid(cols=vars(crab_year),rows=vars(vessel_cat),scales="free_y") +
  ylab("Proportion Landed Lbs of D. Crab") +
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched VMS")) +
  scale_fill_manual(values=c("coral2","cyan3")) +
  scale_x_discrete(name="Month",labels=c("","Dec","","Feb","","Apr","","Jun","","Aug","","Oct")) +
  theme_bw() + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                     axis.text.x=element_text(angle=90,hjust=1,vjust=0.2,size=12),
                     axis.text.y=element_text(size=12),
                     axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_blank(),
                     strip.text=element_text(size=12))
png(paste0("R_Output/quality/vms_coverage/dcrb_proplbs_wVMS_bycat_matchVMS_yearly_",states[1],".png"),width=820,height=400)
myplot
dev.off()

myplot <- ggplot(data=sumdat.lbs,aes(x=month,y=value,fill=variable)) +
  geom_col(width=1.01) +
  facet_grid(cols=vars(crab_year),rows=vars(vessel_cat),scales="free_y") +
  xlab("Month") + ylab("Proportion Landed Lbs of D. Crab") +
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched VMS")) +
  scale_fill_manual(values=c("coral2","cyan3")) +
  scale_x_discrete(name="Month",labels=c("","Dec","","Feb","","Apr","","Jun","","Aug","","Oct")) +
  theme_bw() + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                     axis.text.x=element_text(angle=90,hjust=1,vjust=0.2,size=12),
                     axis.text.y=element_text(size=12),
                     axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_blank(),
                     strip.text=element_text(size=12))
png(paste0("R_Output/quality/vms_coverage/dcrb_proplbs_wVMS_bycat_matchVMS_yearly_",states[1],"_wNA.png"),width=820,height=400)
myplot
dev.off()

#revenue
sumdat.rev <- sumdat %>%
  dplyr::select(-dcrb.lbs) %>%
  spread(key=VMS,value=dcrb.rev)
sumdat.rev$No[which(is.na(sumdat.rev$No))] <- 0
sumdat.rev$Yes[which(is.na(sumdat.rev$Yes))] <- 0
sumdat.rev <- sumdat.rev %>%
  mutate(`without VMS` = No / (No + Yes), `with VMS` = Yes / (No + Yes)) %>%
  dplyr::select(-No,-Yes) %>%
  melt(id.vars=c("crab_year","month","vessel_cat"))

sumdat.rev$month <- factor(sumdat.rev$month, levels=c("Nov","Dec","Jan","Feb",
                                                      "Mar","Apr","May","Jun","Jul","Aug",
                                                      "Sep","Oct"))


myplot <- ggplot(data=filter(sumdat.rev, !is.na(vessel_cat)),aes(x=month,y=value,fill=variable)) +
  geom_col(width=1.01) +
  facet_grid(cols=vars(crab_year),rows=vars(vessel_cat),scales="free_y") +
  xlab("Month") + ylab("Proportion of D. Crab Revenue") +
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched VMS")) +
  scale_fill_manual(values=c("coral2","cyan3")) +
  scale_x_discrete(name="Month",labels=c("","Dec","","Feb","","Apr","","Jun","","Aug","","Oct")) +
  theme_bw() + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                     axis.text.x=element_text(angle=90,hjust=1,vjust=0.2,size=12),
                     axis.text.y=element_text(size=12),
                     axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_blank(),
                     strip.text=element_text(size=12))
png(paste0("R_Output/quality/vms_coverage/dcrb_proprev_wVMS_bycat_matchVMS_yearly_",states[1],".png"),width=820,height=400)
myplot
dev.off()

myplot <- ggplot(data=sumdat.rev,aes(x=month,y=value,fill=variable)) +
  geom_col(width=1.01) +
  facet_grid(cols=vars(crab_year),rows=vars(vessel_cat),scales="free_y") +
  xlab("Month") + ylab("Proportion of D. Crab Revenue") +
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched VMS")) +
  scale_fill_manual(values=c("coral2","cyan3")) +
  scale_x_discrete(name="Month",labels=c("","Dec","","Feb","","Apr","","Jun","","Aug","","Oct")) +
  theme_bw() + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                     axis.text.x=element_text(angle=90,hjust=1,vjust=0.2,size=12),
                     axis.text.y=element_text(size=12),
                     axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_blank(),
                     strip.text=element_text(size=12))
png(paste0("R_Output/quality/vms_coverage/dcrb_proprev_wVMS_bycat_matchVMS_yearly_",states[1],"_wNA.png"),width=820,height=400)
myplot
dev.off()



# Proportion Lbs/ Revenue per month, all ----------------------------------
sumdat.all <- matchdat.trips %>% 
  group_by(crab_year,month,VMS) %>%
  summarise(dcrb.lbs=sum(total.lbs),dcrb.rev=sum(total.rev))
sumdat.all$month <- factor(sumdat.all$month, levels=c("Nov","Dec","Jan","Feb",
                                                      "Mar","Apr","May","Jun","Jul","Aug",
                                                      "Sep","Oct"))

#pounds
sumdat.all.lbs <- sumdat.all %>%
  dplyr::select(-dcrb.rev) %>%
  spread(key=VMS,value=dcrb.lbs)
sumdat.all.lbs$No[which(is.na(sumdat.all.lbs$No))] <- 0
sumdat.all.lbs$Yes[which(is.na(sumdat.all.lbs$Yes))] <- 0
sumdat.all.lbs <- sumdat.all.lbs %>%
  mutate(`without VMS` = No / (No + Yes), `with VMS` = Yes / (No + Yes)) %>%
  dplyr::select(-No,-Yes) %>%
  melt(id.vars=c("crab_year","month"))


myplot <- ggplot(data=sumdat.all.lbs,aes(x=month,y=value,fill=variable)) +
  geom_col(width=1.01) +
  facet_wrap(~crab_year,ncol=4) +
  xlab("Month") + ylab("Proportion Landed Lbs of D. Crab") +
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched VMS")) +
  scale_fill_manual(values=c("coral2","cyan3")) +
  theme_bw() + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                     axis.text.x=element_text(angle=90,hjust=1,vjust=0,size=12),
                     axis.text.y=element_text(size=12),
                     axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_blank(),
                     strip.text=element_text(size=12))
png(paste0("R_Output/quality/vms_coverage/dcrb_proplbs_wVMS_matchVMS_yearly_",states[1],".png"),width=820,height=400)
myplot
dev.off()


#revenue
sumdat.all.rev <- sumdat.all %>%
  dplyr::select(-dcrb.lbs) %>%
  spread(key=VMS,value=dcrb.rev)
sumdat.all.rev$No[which(is.na(sumdat.all.rev$No))] <- 0
sumdat.all.rev$Yes[which(is.na(sumdat.all.rev$Yes))] <- 0
sumdat.all.rev <- sumdat.all.rev %>%
  mutate(`without VMS` = No / (No + Yes), `with VMS` = Yes / (No + Yes)) %>%
  dplyr::select(-No,-Yes) %>%
  melt(id.vars=c("crab_year","month"))


myplot <- ggplot(data=sumdat.all.rev,aes(x=month,y=value,fill=variable)) +
  geom_col(width=1.01) +
  facet_wrap(~crab_year,ncol=4) +
  xlab("Month") + ylab("Proportion of D. Crab Revenue") +
  ggtitle(paste0(state, " Dungeness crab Vessels\n Matched VMS")) +
  scale_fill_manual(values=c("coral2","cyan3")) +
  theme_bw() + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                     axis.text.x=element_text(angle=90,hjust=1,vjust=0,size=12),
                     axis.text.y=element_text(size=12),
                     axis.title=element_text(size=14),title = element_text(size=15),
                     legend.text=element_text(size=12),legend.title=element_blank(),
                     strip.text=element_text(size=12))
png(paste0("R_Output/quality/vms_coverage/dcrb_proprev_wVMS_matchVMS_yearly_",states[1],".png"),width=820,height=400)
myplot
dev.off()

