# The purpose of this file is the insertion of a column for preceding trip
# based on the original data from "WLAN_TripID_ride_hailing".
# If we already have the data set "WLAN_TripID_ride_hailing_extended",
# we could begin with this file!

# Reading the data sets
pacman::p_load(dplyr,lubridate,rio)
# Importing the enhanced data set "WLAN_TripID_ride_hailing_extended"
WLAN_TripID_ride_hailing_extended =
  import("./02_output_data/08a_WLAN_TripID_ride_hailing_extended_temp.csv")

# NA check in the column "PrecedingTrip"
# sum(is.na(WLAN_TripID_ride_hailing_extended$PrecedingTrip)) # no NAs !!!
# Rows with empty "PrecedingTrip" info
WLAN_row_indexes_no_preceding_trip =
  which(WLAN_TripID_ride_hailing_extended$PrecedingTrip == "")
# length(WLAN_row_indexes_no_preceding_trip) # 29272 rows

# Add dates to the preceding trips times
WLAN_TripID_ride_hailing_extended$PrecedingTrip =
  strptime(
  paste0(WLAN_TripID_ride_hailing_extended$StartDate,
         WLAN_TripID_ride_hailing_extended$PrecedingTrip),
  format = "%Y-%m-%d %H:%M:%S")

# NA check in the column "PrecedingTrip"
# sum(is.na(WLAN_TripID_ride_hailing_extended$PrecedingTrip)) # 29272 rows are NA
# All empty fields became NAs!!!

# Fix rows corresponding to midnight trips during 00:00-00:59
# We search for the previous rides during 23:00-23:59 on the preceding day
WLAN_row_indices_00h = which(
  WLAN_TripID_ride_hailing_extended$StartTime >= "00:00" & 
    WLAN_TripID_ride_hailing_extended$StartTime < "00:59" &
    is.na(WLAN_TripID_ride_hailing_extended$PrecedingTrip)
  )
# Result: 298 rows

# Enter previous rides for midnight trips during 00:00-00:59
# 20 sec to complete
for (row_idx in WLAN_row_indices_00h){
  preceding_trips_df = filter(
    WLAN_TripID_ride_hailing_extended,
    StartStation == WLAN_TripID_ride_hailing_extended$StartStation[row_idx] & 
      StartDate == WLAN_TripID_ride_hailing_extended$StartDate[row_idx]-1 & 
      StartHour == 23)
  WLAN_TripID_ride_hailing_extended$PrecedingTrip[row_idx] =
    strptime(
      paste0(WLAN_TripID_ride_hailing_extended$StartDate[row_idx]-1,
             max(preceding_trips_df$StartTime)),
      format = "%Y-%m-%d %H:%M:%S")
}

# New NA check in the column "PrecedingTrip"
# sum(is.na(WLAN_TripID_ride_hailing_extended$PrecedingTrip)) # 29157 rows are NA
# Thus, we fixed 115 midnight trips.
# All empty fields in the column are now NAs!!!

# Creating an auxiliary StartDateTime column
WLAN_TripID_ride_hailing_extended$StartDateTime =
  strptime(
    paste0(WLAN_TripID_ride_hailing_extended$StartDate,
           WLAN_TripID_ride_hailing_extended$StartTime),
    format = "%Y-%m-%d %H:%M:%S")

# A temporary column for maximal waiting times
# Allocation using the duration between successive rides
WLAN_TripID_ride_hailing_extended$WaitingTimeMax =
  as.integer(difftime(WLAN_TripID_ride_hailing_extended$StartDateTime,
                      WLAN_TripID_ride_hailing_extended$PrecedingTrip,
                      units="secs")
             )

# Remove waiting times longer than 1h (3600 sec)
# We let people missing their buses / trains, which arrive once
# during an hour, to wait in the station
WLAN_TripID_ride_hailing_extended$WaitingTimeMax[
  which(WLAN_TripID_ride_hailing_extended$WaitingTimeMax > 3600)
  ] = NA

# Set maximum waiting time of 600 sec instead of NAs
WLAN_TripID_ride_hailing_extended$WaitingTimeMax[
  is.na(WLAN_TripID_ride_hailing_extended$WaitingTimeMax)
  ] = 600

# Sampling waiting minutes using the value of "WaitingTimeMax"
# Preallocation
WLAN_TripID_ride_hailing_extended$WaitingTime =
  WLAN_TripID_ride_hailing_extended$WaitingTimeMax

WLAN_TripID_ride_hailing_extended$WaitingTime =
  sapply(WLAN_TripID_ride_hailing_extended$WaitingTime,sample,size = 1)

# Convert to min
WLAN_TripID_ride_hailing_extended$WaitingTime =
  round(WLAN_TripID_ride_hailing_extended$WaitingTime / 60,
        digits = 2)

# Remove the auxiliary columns
WLAN_TripID_ride_hailing_extended$WaitingTimeMax = NULL
WLAN_TripID_ride_hailing_extended$StartDateTime = NULL

WLAN_TripID_ride_hailing_extended$PrecedingTrip =
  as.character(WLAN_TripID_ride_hailing_extended$PrecedingTrip)
# The last line enables csv export!!!

# Exporting the data set "WLAN_TripID_ride_hailing_extended" as "csv"
export(WLAN_TripID_ride_hailing_extended,
       file = "./02_output_data/08b_WLAN_TripID_ride_hailing_extended.csv")

# Creating a reduced version of the data set
WLAN_TripID_ride_hailing_extended_optim = 
  select(WLAN_TripID_ride_hailing_extended,
         -MacID,-station.start,-station.stop,
         -trip.id,-PrecedingTrip)

WLAN_TripID_ride_hailing_extended_optim =
  filter(WLAN_TripID_ride_hailing_extended_optim,
         SchoolHoliday == FALSE)

WLAN_TripID_ride_hailing_extended_optim$SchoolHoliday = NULL

# Convert distances into km
WLAN_TripID_ride_hailing_extended_optim$Distance =
  WLAN_TripID_ride_hailing_extended_optim$Distance/1000

# Importing the station (stop) information
ride_hailing_stations <- import(
  "./02_output_data/06_ride_hailing_stations.csv")

# Rename the column "StopStation" into "Destination"
WLAN_TripID_ride_hailing_extended_optim =
  rename(WLAN_TripID_ride_hailing_extended_optim,
         Destination=StopStation)

# Inserting the coordinates of start stations
WLAN_TripID_ride_hailing_extended_optim$StartStation_lat <- ride_hailing_stations$stop_lat[
  match(WLAN_TripID_ride_hailing_extended_optim$StartStation, ride_hailing_stations$stop_name)
]
WLAN_TripID_ride_hailing_extended_optim$StartStation_lon <- ride_hailing_stations$stop_lon[
  match(WLAN_TripID_ride_hailing_extended_optim$StartStation, ride_hailing_stations$stop_name)
]

WLAN_TripID_ride_hailing_extended_optim$StartStation_lat <-
  as.numeric(WLAN_TripID_ride_hailing_extended_optim$StartStation_lat)
WLAN_TripID_ride_hailing_extended_optim$StartStation_lon <-
  as.numeric(WLAN_TripID_ride_hailing_extended_optim$StartStation_lon)

# Inserting the coordinates of destinations
WLAN_TripID_ride_hailing_extended_optim$Destination_lat <- ride_hailing_stations$stop_lat[
  match(WLAN_TripID_ride_hailing_extended_optim$Destination, ride_hailing_stations$stop_name)
]
WLAN_TripID_ride_hailing_extended_optim$Destination_lon <- ride_hailing_stations$stop_lon[
  match(WLAN_TripID_ride_hailing_extended_optim$Destination, ride_hailing_stations$stop_name)
]

WLAN_TripID_ride_hailing_extended_optim$Destination_lat <-
  as.numeric(WLAN_TripID_ride_hailing_extended_optim$Destination_lat)
WLAN_TripID_ride_hailing_extended_optim$Destination_lon <-
  as.numeric(WLAN_TripID_ride_hailing_extended_optim$Destination_lon)

WLAN_TripID_ride_hailing_extended_optim = 
  select(WLAN_TripID_ride_hailing_extended_optim,
         StartDate,StartDay,StartHour,StartTime,
         StartStation_lat,StartStation_lon,
         Destination_lat,Destination_lon,
         WaitingTime,StopTime,Distance)

WLAN_TripID_ride_hailing_extended_optim =
  rename(WLAN_TripID_ride_hailing_extended_optim,
         ArrivalTime=StopTime)

# Exporting the data set "WLAN_TripID_ride_hailing_extended" as "csv"
export(WLAN_TripID_ride_hailing_extended_optim,
       file = "./02_output_data/08c_WLAN_TripID_ride_hailing_extended_optim.csv")
