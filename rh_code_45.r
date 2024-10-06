# The purpose of this file is the preparation of a "Boarding Summary"
# for all stops, based on the original data from "WLAN_TripID_ride_hailing".
# If we already have the data set "WLAN_TripID_ride_hailing.RData",
# we could begin with this file!

# Reading the data sets
pacman::p_load(plyr,dplyr,lubridate,rio)
# Importing the enhanced data set "WLAN_TripID_ride_hailing.RData"
WLAN_ride_hailing_with_coordinates =
  import("./02_output_data/08_WLAN_TripID_ride_hailing.RData")
# CAUTION: Importing .xlsx files via "rio::import" or "readxl::read_excel"
# both freeze RStudio, maybe because of the incompatibility of .xlsx files
# with the package "lubridate" Do not use these commands!
# ==========================================================
# Importing the station (stop) information
# to add it into "WLAN_ride_hailing_with_coordinates"
ride_hailing_stations <- import(
  "./02_output_data/06_ride_hailing_stations.csv")

# Remove superflous columns
WLAN_ride_hailing_with_coordinates = select(
  WLAN_ride_hailing_with_coordinates,
  -c(StopDay,StopDate,StopTime)
  )

# Rename the column "StopStation" into "Destination"
WLAN_ride_hailing_with_coordinates =
  rename(WLAN_ride_hailing_with_coordinates,
         Destination=StopStation)
  
# Inserting the coordinates of start stations
WLAN_ride_hailing_with_coordinates$StartStation_lat <- ride_hailing_stations$stop_lat[
  match(WLAN_ride_hailing_with_coordinates$StartStation, ride_hailing_stations$stop_name)
]
WLAN_ride_hailing_with_coordinates$StartStation_lon <- ride_hailing_stations$stop_lon[
  match(WLAN_ride_hailing_with_coordinates$StartStation, ride_hailing_stations$stop_name)
]

WLAN_ride_hailing_with_coordinates$StartStation_lat <-
  as.numeric(WLAN_ride_hailing_with_coordinates$StartStation_lat)
WLAN_ride_hailing_with_coordinates$StartStation_lon <-
  as.numeric(WLAN_ride_hailing_with_coordinates$StartStation_lon)

# Inserting the coordinates of destinations
WLAN_ride_hailing_with_coordinates$Destination_lat <- ride_hailing_stations$stop_lat[
  match(WLAN_ride_hailing_with_coordinates$Destination, ride_hailing_stations$stop_name)
]
WLAN_ride_hailing_with_coordinates$Destination_lon <- ride_hailing_stations$stop_lon[
  match(WLAN_ride_hailing_with_coordinates$Destination, ride_hailing_stations$stop_name)
]

WLAN_ride_hailing_with_coordinates$Destination_lat <-
  as.numeric(WLAN_ride_hailing_with_coordinates$Destination_lat)
WLAN_ride_hailing_with_coordinates$Destination_lon <-
  as.numeric(WLAN_ride_hailing_with_coordinates$Destination_lon)

# Inserting the districts of origins and destinations
WLAN_ride_hailing_with_coordinates$StartDistrict <- ride_hailing_stations$district[
  match(WLAN_ride_hailing_with_coordinates$StartStation, ride_hailing_stations$stop_name)
]
WLAN_ride_hailing_with_coordinates$DestinationDistrict <- ride_hailing_stations$district[
  match(WLAN_ride_hailing_with_coordinates$Destination, ride_hailing_stations$stop_name)
]

# Reordering columns
col_order <- c("StartDate","StartHour","StartDay","StartTime","SchoolHoliday","StartStation",
               "StartDistrict","StartStation_lat","StartStation_lon","Destination",
               "DestinationDistrict","Destination_lat","Destination_lon","MacID")
WLAN_ride_hailing_with_coordinates <- WLAN_ride_hailing_with_coordinates[, col_order]

# View(WLAN_ride_hailing_with_coordinates)

# Exporting the data set "WLAN_ride_hailing_with_coordinates" as "RData"
save(WLAN_ride_hailing_with_coordinates,
     file = "./02_output_data/10b_WLAN_ride_hailing_with_coordinates.RData")

# List of all dates and corresponding trips
WLAN_trips_by_date_tmp = dlply(
  WLAN_ride_hailing_with_coordinates, "StartDate",
  function(x) subset(x, select = -StartDate)
)

# The function "merge_trips" takes a data frame as an input
# (in our case, inputs are entries of the list "WLAN_trips_by_date")
# and merges trips with identical start and stop as a data frame

# Arguments
# input_data_frame: "data.frame" / with columns col_order

# Output: "data.frame"

merge_trips = function(input_data_frame) {
  output_data_frame = input_data_frame %>%
    group_by(StartHour,StartDay,SchoolHoliday,StartStation,
             StartDistrict,StartStation_lat,StartStation_lon,Destination,
             DestinationDistrict,Destination_lat,Destination_lon) %>%
    summarize(Passengers = n()) %>% # A new column for the groupped values
    ungroup()
  return(output_data_frame)
}

# Merging trips with identical start and stop
WLAN_trips_by_date = lapply(WLAN_trips_by_date_tmp,merge_trips)
# View(WLAN_trips_by_date)

# Exporting the data set "WLAN_trips_by_date" as "RData"
save(WLAN_trips_by_date,
     file = "./02_output_data/10c_WLAN_trips_by_date.RData")
