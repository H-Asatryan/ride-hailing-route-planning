# The purpose of this file is the preparation of the data sets
# "vrr_stations","WSW_stops" and "WLAN_TripID_merged.RData"

# Importing the station (stop) information
library(rio)
vrr_stations <- import("./01_input_data/2020_01_13_verbundweit_spnv/stops.txt")
# vrr_stations=import("./01_input_data/2020_01_13_verbundweit_spnv/stops.txt",header=FALSE)
# VRR-Fahrplandaten Januar 2020 (GTFS, SPNV)
# https://www.openvrr.de/dataset/gtfs
# The imported data set is available at
# https://archiv.opendata-oepnv.de/VRR/Soll-Fahrplandaten/GTFS/
# Direct link:
# https://archiv.opendata-oepnv.de/VRR/Soll-Fahrplandaten/GTFS/aktuell/google_transit_opendata.zip

# Keeping only the columns stop_id, stop_name, stop_lat, stop_lon
library(dplyr)
vrr_stations <- select(vrr_stations, stop_id, stop_name, stop_lat, stop_lon)

# Reading the WLAN_TripID monthly data sets
names_of_data_files = c("WLAN_TripID_2019_Jan","WLAN_TripID_2019_Feb",
                        "WLAN_TripID_2019_Mrz","WLAN_TripID_2019_Apr",
                        "WLAN_TripID_2019_Mai","WLAN_TripID_2019_Jun")
number_of_data_files = length(names_of_data_files)

WLAN_TripID_datasets_list = vector("list", length = number_of_data_files)

for (month_no in 1:number_of_data_files) {
  WLAN_TripID_datasets_list[[month_no]] = import(
    paste0("./01_input_data/WSW_LAN_TripID/",names_of_data_files[month_no],".xlsx")
  )
}

# Alternatively, one could use "readxl::read_excel" to import xlsx files;
# then we obtain a data frame of class "tbl_df".

# Merging the monthly data sets
WLAN_TripID_merged = as.data.frame(data.table::rbindlist(WLAN_TripID_datasets_list))
# Alternatively, one can use the command "dplyr::bind_rows";
# this command and "data.table::rbindlist" are 3x faster than "rbind"

# Clear unnecessary variables from the environment
rm(names_of_data_files,number_of_data_files,WLAN_TripID_datasets_list,month_no)

# Removing superfluous columns
WLAN_TripID_merged <- select(
  WLAN_TripID_merged, MacID, Start, Stop,
  station.start, station.stop, Distance, trip.id
)
# The column "SessionDuration" could be of interest at later
# stages of the project, hence select also it on demand

# Removing NA's ----------------------------------------------
# Info: about 24% of all rows contain NA's. All NA's
# are in the last 4 columns. One could check them, e.g.,
# without_trip<-subset(WLAN_TripID_merged,is.na(station.start))
# sum(is.na(WLAN_TripID_merged$station.start))
WLAN_TripID_merged <- WLAN_TripID_merged[complete.cases(WLAN_TripID_merged), ]

# Inserting columns "StartDate" and "StartDay" ----------------------
library(lubridate)
# Changing locale to get weekdays in English
Sys.setlocale("LC_TIME", "C") # Works under Linux
# Sys.setlocale("LC_TIME", "en_US.UTF-8") # Another way for Linux
# Tip: Use "Sys.getlocale("LC_TIME")" to obtain the format
# of the system locale, then use "Sys.setlocale" with the
# corresponding parameters to switch to English,
# e.g., for Windows one can use "Sys.setlocale("LC_TIME", "English")"
# or "Sys.setlocale("LC_TIME","English United States")",
# for some Unix systems we can use
# Sys.setlocale("LC_TIME", "en_US")

WLAN_TripID_merged$StartDate <- as_date(WLAN_TripID_merged$Start)
WLAN_TripID_merged$StartDay <- weekdays(WLAN_TripID_merged$StartDate)
# If needed, uncomment the following commnads to insert StopDate/StopDay
# WLAN_TripID_merged$StopDate <- as_date(WLAN_TripID_merged$Stop)
# WLAN_TripID_merged$StopDay <- weekdays(WLAN_TripID_merged$StopDate)

# Inserting columns StartTime/StopTime----------------------------
WLAN_TripID_merged$StartTime <-
  sapply(strsplit(as.character(WLAN_TripID_merged$Start), " "), "[", 2)
WLAN_TripID_merged$StopTime <-
  sapply(strsplit(as.character(WLAN_TripID_merged$Stop), " "), "[", 2)

# Extracting the full starting hours into the column StartHour
WLAN_TripID_merged$StartHour <- strptime(
  WLAN_TripID_merged$StartTime,
  format = "%H:%M:%S"
)
WLAN_TripID_merged$StartHour <- hour((WLAN_TripID_merged$StartHour))

# Removing further superfluous columns
WLAN_TripID_merged$Start <- NULL
WLAN_TripID_merged$Stop <- NULL

# Matching boarding stations ----------------------------------
WLAN_TripID_merged$StartStation <- vrr_stations$stop_name[
  match(WLAN_TripID_merged$station.start, vrr_stations$stop_id)
]

# Matching exit stations --------------------------------------
WLAN_TripID_merged$StopStation <- vrr_stations$stop_name[
  match(WLAN_TripID_merged$station.stop, vrr_stations$stop_id)
]

# Removing new NA's!!! ----------------------------------------
# The station matching generated about 130000 new NA's;
# probably because of typos in the data set.
WLAN_TripID_merged <- WLAN_TripID_merged[complete.cases(WLAN_TripID_merged), ]

# Exporting data (using "rio")
export(vrr_stations, "./02_output_data/01_vrr_stations.csv")

# Format conversion
WLAN_TripID_merged$StartStation <- as.character(WLAN_TripID_merged$StartStation)
WLAN_TripID_merged$StopStation <- as.character(WLAN_TripID_merged$StopStation)
WLAN_TripID_merged$StartDay <- as.character(WLAN_TripID_merged$StartDay)
WLAN_TripID_merged$StopDay <- as.character(WLAN_TripID_merged$StopDay)
WLAN_TripID_merged$StartTime <- as.character(WLAN_TripID_merged$StartTime)
WLAN_TripID_merged$StopTime <- as.character(WLAN_TripID_merged$StopTime)
WLAN_TripID_merged$StartHour <- as.character(WLAN_TripID_merged$StartHour)

# Extracting WSW stations from the data set "vrr_stations"
WSW_stations <- filter(vrr_stations,stop_name %in% WLAN_TripID_merged$StartStation
                    | stop_name %in% WLAN_TripID_merged$StopStation)
WSW_stations = WSW_stations[order(WSW_stations$stop_name), ]

# Exporting data (using "rio")
export(WSW_stations, "./02_output_data/02_WSW_stations.csv")

# Adding the school holidays to the data set
school_holidays <- import("./01_input_data/schulferien.csv")
# The imported data set is available at "fcal.ch"
# Direct link:
# https://www.feiertagskalender.ch/export.php?geo=3069&jahr=2019&klasse=3&hl=de#

colnames(school_holidays) <- "SchoolHolidays"
school_holidays$SchoolHolidays = as.Date(school_holidays$SchoolHolidays)

WLAN_TripID_merged$SchoolHoliday <- school_holidays$SchoolHolidays[
  match(WLAN_TripID_merged$StartDate, school_holidays$SchoolHolidays)
]
WLAN_TripID_merged$SchoolHoliday <- as.logical(WLAN_TripID_merged$SchoolHoliday)
WLAN_TripID_merged$SchoolHoliday[
  is.na(WLAN_TripID_merged$SchoolHoliday)
] <- FALSE

WLAN_TripID_merged = filter(WLAN_TripID_merged,
                            StartDate >= "2019-01-01")
  
# Exporting data
export(WLAN_TripID_merged, "./02_output_data/03_WLAN_TripID_merged.RData")
# CAUTION: Importing "WLAN_TripID_merged" in .xlsx format
# via "rio::import" or "readxl::read_excel" freezes RStudio,
# maybe because of the incompatibility of .xlsx files
# with the package "lubridate". Hence we use the RData format!

rm(list = ls()) # Clear environment
cat("\014") # Clear console (Ctrl+L)

##############################################################
# Use in the following files / at later stages
##############################################################
# Now we no longer need the "stop_id" column of the data set "vrr_stations".
# That column is useful for so-called "homonymous" stops, i.e., for
# those with identical names. For those stops we will save only
# the coordinates of the first occurance. In other words, the
# "homonymous" stops are regarded as the same stop. The fact that
# these stops lie next to each other justifies our point of view.

vrr_stations <- select(vrr_stations, -stop_id) # Remove the column stop_id
vrr_stations <- distinct(vrr_stations, stop_name, .keep_all = TRUE)

# Now we no longer need the "station.start" and "station.stop"
# columns of the data set "WLAN_TripID_merged"; we have already
# used them to find out the stop names and, as we said before,
# we regard the "homonymous" stops as the same stop. Hence,
# we remove these columns from "WLAN_TripID_merged".

WLAN_TripID_merged <- select(WLAN_TripID_merged, -station.start, -station.stop)
