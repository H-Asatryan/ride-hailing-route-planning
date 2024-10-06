# The purpose of this file is the insertion of a column for preceding trip
# based on the original data from "WLAN_TripID_ride_hailing".
# If we already have the data set "WLAN_TripID_ride_hailing.RData",
# we could begin with this file!

# Reading the data sets
pacman::p_load(plyr,dplyr,lubridate,rio)
# Importing the enhanced data set "WLAN_TripID_ride_hailing.RData"
WLAN_TripID_ride_hailing_extended =
  import("./02_output_data/08_WLAN_TripID_ride_hailing.RData")

WLAN_TripID_ride_hailing_extended$PrecedingTrip = NA

# "foreach" version of the preceding trip insertion algorithm
# Takes about 4.5 hours to complete on my PC
#############################################################
# Libraries for parallelizing for loops and CPU core setup
library(foreach)
library(doParallel)

# 11262 rows in 11 min!!! Parallelize or wait 50x longer!
# WLAN_rows = 1:10
WLAN_rows = 1:nrow(WLAN_TripID_ride_hailing_extended)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

output_vec = foreach(row_idx=WLAN_rows) %dopar% {
  output = max(dplyr::filter(
    WLAN_TripID_ride_hailing_extended,
    StartStation == WLAN_TripID_ride_hailing_extended$StartStation[row_idx] &
      StartDate == WLAN_TripID_ride_hailing_extended$StartDate[row_idx] &
      StartTime < WLAN_TripID_ride_hailing_extended$StartTime[row_idx]
  )$StartTime
  )

  output

}

#stop cluster
stopCluster(cl)

WLAN_TripID_ride_hailing_extended$PrecedingTrip = output_vec

# Computing NAs percentage
sum(is.na(WLAN_TripID_ride_hailing_extended$PrecedingTrip)
    )*100/nrow(WLAN_TripID_ride_hailing_extended)
# 4,6 % NAs
#########################################################

# To do:
# for NAs between 00:00-00:59, consider the trips 23:00-23:59 of the previous day
# for remaining NAs, i.e., the first rides of buses, sample waiting time of max 10 min
# We perform this in the next file!

# Exporting the data set "WLAN_TripID_ride_hailing_extended" as "csv"
export(WLAN_TripID_ride_hailing_extended,
       file = "./02_output_data/08a_WLAN_TripID_ride_hailing_extended_temp.csv")
# We add "_temp" at the end of the exported file name, since we
# need to complete the missing 4,6 % of preceding trips

# Appendix ================================================
# Here is the non-parallelized version with a "for" loop
# 1000 iterations take exactly 2 min, therefore the
# overall time could be about 20 hours (4x longer!)
WLAN_TripID_ride_hailing_extended_2 =
  import("./02_output_data/08_WLAN_TripID_ride_hailing.RData")

WLAN_rows_2 = nrow(WLAN_TripID_ride_hailing_extended_2)
iterations_no = 1000
# iterations_no = WLAN_rows_2
WLAN_TripID_ride_hailing_extended_2$PrecedingTrip = NA

for (row_idx in 1:iterations_no) {
  preceding_trips_df = filter(
    WLAN_TripID_ride_hailing_extended_2,
    StartStation == WLAN_TripID_ride_hailing_extended_2$StartStation[row_idx] &
      StartDate == WLAN_TripID_ride_hailing_extended_2$StartDate[row_idx] &
      StartTime < WLAN_TripID_ride_hailing_extended_2$StartTime[row_idx])
  WLAN_TripID_ride_hailing_extended_2$PrecedingTrip[row_idx] =
    max(preceding_trips_df$StartTime)
}

# The following compare test shows that the first 1000 iterations
# yield the same results as we obtained above:
output1 = WLAN_TripID_ride_hailing_extended$PrecedingTrip[1:WLAN_rows_2]
output2 = WLAN_TripID_ride_hailing_extended_2$PrecedingTrip[1:WLAN_rows_2]

# This function returns TRUE wherever elements are the same, including NA's,
# and FALSE everywhere else.
compareNA <- function(v1, v2)
{
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

compareNA(output1, output2)
