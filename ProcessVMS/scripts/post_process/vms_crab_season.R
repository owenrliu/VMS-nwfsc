###### Calendar to Crab Year - VMS #####
#
# Combine calendar years of VMS data to produce one file with 
#    all VMS within a crab fishing season
#
# 8/28/2019 - M. Fisher
#
########################################

rm(list=ls()) #clean workspace
library(dplyr)

setwd("C:/Users/Mary.Fisher/Documents/VMS-repo/ProcessVMS")


# Set Arguments ------------------------------------------
crab_seasons <- c(2015,2016)  # which crab seasons do you need? (2015 = 2015-16)
vms_indir <- "R_Output/interpolation/"   # where are the calendar year VMS files?
tix_indir <- "R_Output/match/unfiltered/"
outdir <- "R_Output/interpolation/"   # where do you want the output files to go?
vms_type = "_interpolated_regular"   # do you want just the cleaned VMS data (""), 
                                     # the interpolated VMS data ("_interpolated_regular"), or
                                     # the interpolated + all original points VMS data ("_interpolated_all")?
### Change VMS input file name within the `cal2crab` function




# Define Function  ---------------------------------------------------------------
cal2crab <- function(seasons, vms_directory, vms_filetype, ticket_directory){
  cal_years = seq(min(seasons), max(seasons)+1) #which calendar years of data are needed for these crab years?
  message("reading in ticket and vms data for calendar years: ", min(cal_years), " to ", max(cal_years), "\n\n")
  # read in the fish ticket and vms data for each 
  for(i in seq(1,length(cal_years))){
    tmptix <- read.csv(paste0(tix_indir, "FishTix_wVMS_10d_lookback_",cal_years[i], ".csv"))
    tmpvms <- read.csv(paste0(vms_indir, "VMS_Outputs_wTARGET_10d_lookback_",cal_years[i],"_final_cleaned_",vms_filetype,".csv"))  # !!! Change VMS file name !!!
    # thin out ticket file columns, add in crab year column
    tmptix <- tmptix %>%
      dplyr::select(Rec_ID, drvid, agency_code, pacfin_port_code, port_group_code,removal_type_code, date, year,
                            Week, species_code_all, gear_code_all, TARGET, TARGET..lbs., TARGET...., FINAL_LENGTH) %>%
      mutate(crab_year = ifelse(Week > 45, year, year - 1))
    # merge tickets / vms data
    vms_wtix <- left_join(tmpvms, tmptix, by=c("DOCNUM" = "drvid", "Rec_ID"))
    # save to single data frame
    if(i==1){
      out = vms_wtix
    } else{
      out = rbind(out,vms_wtix)
    }
    message("finished with ", cal_years[i], "\n")
  }
  return(out)
}



# Read in & write out data with function ----------------------------------------------
#run function 
mydat <- cal2crab(seasons=crab_seasons, vms_directory=vms_indir, vms_filetype = vms_type, ticket_directory = tix_indir)
colnames(mydat) # what columns are in the new data frame?
colnames(mydat)[which(colnames(mydat) == "TARGET.x")] <- "TARGETlist"
colnames(mydat)[which(colnames(mydat) == "TARGET.y")] <- "TARGET"
colnames(mydat)[which(colnames(mydat) == "TARGET....")] <- "TARGET..rev."
colnames(mydat)[which(colnames(mydat) == "date")] <- "ticket.date"

#write separate file for each crab season
for(y in crab_seasons){
  tmpdat <- filter(mydat, crab_year==y)
  write.csv(tmpdat, paste0(outdir, "VMS_wTickets_10d_lookback_crab",y,"_final_cleaned",vms_filetype,".csv"))
}



