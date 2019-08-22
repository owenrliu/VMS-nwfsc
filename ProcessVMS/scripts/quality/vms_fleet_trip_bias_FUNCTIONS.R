################# FUNCTIONS to explore VMS representation of Dungeness crab fleet ################
#
# 4/21/2019 - M. Fisher
#
##################################################################################################


######### Vessel Representation #########
#
# What proportion of vessels landing Dungeness crab are
#    represented by the VMS data? 
#
# Works with raw VMS and raw fish ticket data
#
#########################################

vessel_representation <- function(tickets, vms, length_data, year){
  # Subset ticket data
  tix <- filter(tickets, LANDING_YEAR == year)
  vlengths <- filter(length_data, year == year)
  
  # Get list of vessels landing D.crab
  dcrb_vessels <- tix %>%
    filter(SPECIES_CODE == "DCRB") %>%
    dplyr::select(VESSEL_NUM) %>%
    distinct()
  
  # Get lengths for those vessels
  dcrb_vessels <- left_join(dcrb_vessels, vlengths, by=c("VESSEL_NUM" = "drvid"))
  
  # Get total VMS records per vessel 
  ## check for duplicates
  vms_nodup <- vms[c(which(!(duplicated(subset(vms,select=c(DOCNUM, UTCDATETIM)),fromLast=TRUE)))),]
  ## counts
  nvms <- vms_nodup %>%
    group_by(DOCNUM) %>%
    summarise(n.records = n())
  
  # Combine
  dcrb_vessels_vms <- left_join(dcrb_vessels, nvms, by=c("VESSEL_NUM" = "DOCNUM"))
  return(dcrb_vessels_vms)
}


vessel_representation_processed <- function(tickets, vms, length_data, y,region=TRUE){
  # Subset ticket data
  dcrb_tix <- tickets %>%
    filter(crab_year == y) %>%
    filter(TARGET=="DCRB")
  vlengths <- filter(length_data, crab_year == y)
  
  # Get list of vessels landing D.crab
  if(region){
    dcrb_vessels <- dcrb_tix %>%
      group_by(drvid,agency_code,region) %>%
      summarise(n.dcrb.trips = length(unique(Rec_ID)))
  } else{
    dcrb_vessels <- dcrb_tix %>%
      group_by(drvid,agency_code) %>%
      summarise(n.dcrb.trips = length(unique(Rec_ID)))
  }
  
  # Get lengths for those vessels
  dcrb_vessels <- left_join(dcrb_vessels, vlengths, by=c("drvid","agency_code"))
  dcrb_vessels$vessel_cat <- as.character(dcrb_vessels$vessel_cat)
  dcrb_vessels$vessel_cat[which(is.na(dcrb_vessels$vessel_cat))] <- "unknown"
  message("missing ", (sum(dcrb_vessels$vessel_cat=="unknown")/dim(dcrb_vessels)[1])*100, "% vessel lengths.")
  
  # Get total VMS records per vessel, for crab trips only
  ## check for duplicates
  vms_nodup <- vms[c(which(!(duplicated(subset(vms,select=c(DOCNUM, UTCDATETIM)),fromLast=TRUE)))),]
  ## counts
  nvms <- vms_nodup %>%
    filter(Rec_ID %in% dcrb_tix$Rec_ID) %>%  #make sure record id is from given crab year
    group_by(DOCNUM) %>%
    summarise(n.dcrb.trips.vms = length(unique(Rec_ID)),n.dcrb.records = n())
  
  # Combine
  dcrb_vessels_vms <- left_join(dcrb_vessels, nvms, by=c("drvid" = "DOCNUM"))
  return(dcrb_vessels_vms)
}
#########################################



######### Trip Representation #########
#
# What proportion of Dungeness crab trips are
#    represented by the VMS data?
#
# Works with processed fish ticket and process VMS data
#   also writes out the pounds landed / revenue associated with trip
#
#########################################

trip_representation <- function(tickets, vms, y){
  # Get total VMS records per vessel 
  ## just DCRB trips, no duplicate time stamps
  vms <- vms %>%
    dplyr::select(Rec_ID, DOCNUM, UTCDATETIM, Port_Of_Landing) %>%
    distinct()
  ## counts
  nvms <- vms %>%
    group_by(Rec_ID, DOCNUM) %>%
    summarise(n.records = n())
  
  # Subset ticket data
  dcrb_tix <- tickets %>%
    filter(crab_year==y) %>%
    filter(TARGET == "DCRB") %>%
    dplyr::select(Rec_ID, drvid, port_group_code, DCRB..lbs., DCRB_prop, DCRB...., FINAL_LENGTH, year)
  
  # Combine
  dcrb_vessels_vms <- left_join(dcrb_tix, nvms, by=c("drvid" = "DOCNUM", "Rec_ID"))
  dcrb_vessels_vms$n.records[which(is.na(dcrb_vessels_vms$n.records))] <- 0
  return(dcrb_vessels_vms)
}



trip_representation_processed <- function(tickets, length_data, vms, y){
  # Get total VMS records per vessel 
  ## just DCRB trips, no duplicate time stamps
  vms_nodup <- vms[c(which(!(duplicated(subset(vms,select=c(DOCNUM, UTCDATETIM)),fromLast=TRUE)))),]
  nvms <- vms_nodup %>%
    dplyr::select(Rec_ID, DOCNUM, UTCDATETIM) %>%
    group_by(Rec_ID, DOCNUM) %>%
    summarise(n.records = n())
  
  # Subset ticket data
  dcrb_tix <- tickets %>%
    filter(TARGET == "DCRB") %>%
    filter(crab_year == y) %>%
    dplyr::select(Rec_ID, drvid, agency_code, DCRB..lbs., DCRB_prop, DCRB...., crab_year)
  vlengths <- filter(length_data, crab_year == y)
  dcrb_tix <- left_join(dcrb_tix, vlengths, by=c("drvid","agency_code","crab_year"))
  dcrb_vessels$vessel_cat <- as.character(dcrb_vessels$vessel_cat)
  dcrb_vessels$vessel_cat[which(is.na(dcrb_vessels$vessel_cat))] <- "unknown"
  message("missing ", (sum(dcrb_vessels$vessel_cat=="unknown")/dim(dcrb_vessels)[1])*100, "% vessel lengths.")
  
  # Combine
  dcrb_vessels_vms <- left_join(dcrb_tix, nvms, by=c("drvid" = "DOCNUM", "Rec_ID"))
  dcrb_vessels_vms$n.records[which(is.na(dcrb_vessels_vms$n.records))] <- 0
  return(dcrb_vessels_vms)
}


trip_representation_monthly <- function(tickets, length_data, vms, y){
  # Get total VMS records per vessel 
  ## just DCRB trips, no duplicate time stamps
  vms_nodup <- vms[c(which(!(duplicated(subset(vms,select=c(DOCNUM, UTCDATETIM)),fromLast=TRUE)))),]
  nvms <- vms_nodup %>%
    dplyr::select(Rec_ID, DOCNUM, UTCDATETIM, Port_Of_Landing) %>%
    group_by(Rec_ID, DOCNUM) %>%
    summarise(n.records = n())
  
  # Subset ticket data
  dcrb_tix <- tickets %>%
    filter(TARGET == "DCRB") %>%
    filter(crab_year == y) %>%
    dplyr::select(Rec_ID, drvid, agency_code, month, DCRB..lbs., DCRB_prop, DCRB...., crab_year)
  vlengths <- filter(length_data, crab_year == y)
  dcrb_tix <- left_join(dcrb_tix, vlengths, by=c("drvid","agency_code","crab_year"))
  
  # Combine
  dcrb_vessels_vms <- left_join(dcrb_tix, nvms, by=c("drvid" = "DOCNUM", "Rec_ID"))
  dcrb_vessels_vms$n.records[which(is.na(dcrb_vessels_vms$n.records))] <- 0
  return(dcrb_vessels_vms)
}
#########################################




######### Days per Trip #########
#
# How long are Dungeness crab trips 
#    based on the VMS data?
#
# Works with processed VMS data
#   also writes out the length of the vessel
#
#########################################

calc_tripdays <- function(tickets, vms, year){
  # Get total VMS records per vessel 
  ## just DCRB trips, no duplicate time stamps
  vms <- vms %>%
    filter(TARGET_max == "DCRB") %>%
    dplyr::select(Rec_ID, DOCNUM, UTCDATETIM, westcoastdate, Port_Of_Landing) %>%
    distinct()
  
  # Create date objects
  vms$UTCDATETIM <- as.character(vms$UTCDATETIM)
  vms$UTCDATETIM <- parse_date_time(vms$UTCDATETIM, orders = c("Ymd HMS", "Ymd HM"))
  vms$westcoastdate <- parse_date_time(vms$westcoastdate, orders = c("Ymd HMS", "Ymd HM"), tz="America/Los_Angeles")
  ## count days
  days_per_trip <- vms %>%
    group_by(Rec_ID, DOCNUM) %>%
    summarise(n.days = difftime(max(westcoastdate), min(westcoastdate), tz="America/Los_Angeles", units = "days"), n.hours = difftime(max(westcoastdate), min(westcoastdate), tz="America/Los_Angeles", units = "hours"))
  
  # Subset ticket data to get vessel length
  tix <- tickets %>%
    filter(year == y) %>%
    dplyr::select(Rec_ID, drvid, port_group_code, FINAL_LENGTH, year)
  
  # Combine
  dcrb_vessels_vms <- left_join(days_per_trip, tix, by=c("DOCNUM" = "drvid", "Rec_ID"))
  return(dcrb_vessels_vms)
}
#########################################


