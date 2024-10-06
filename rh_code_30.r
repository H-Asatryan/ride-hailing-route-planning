# The purpose of this file is the preparation of the data sets
# "ride_hailing_stations" and "WLAN_TripID_ride_hailing_temp"
# corresponding to the Wuppertal districts with Ride Hailing
# services, revealed in the previous file. Moreover, we will
# add a column showing the district of every station.
# We use the suffix "_temp" in the name of
# "WLAN_TripID_ride_hailing_temp", since the data set
# will be cleaned up during the next step.

# Importing the station (stop) information
library(rio)
WSW_stations <- import("./02_output_data/02_WSW_stations.csv")

# Reading Wuppertal districts
library(rgdal)
districts <- readOGR(dsn="./01_input_data/Quartiere_EPSG25832_SHAPE/.",
                     layer="Quartiere_EPSG25832_SHAPE",
                     encoding = "UTF-8")
# The data set is available at
# https://www.offenedaten-wuppertal.de/dataset/quartiere-wuppertal
# Direct link:
# http://daten.wuppertal.de/Infrastruktur_Bauen_Wohnen/Quartiere_EPSG25832_SHAPE.zip

districts <- spTransform(districts, CRS("+proj=longlat +ellps=GRS80"))

# Selecting districts with Ride Hailing services
# See the previous file for revealing district codes
subdistrict_codes_elberfeld <- c("00","01","02","03","04","05")
subdistrict_codes_elberfeld_west <- c("10","11","12","13","14","15")
subdistrict_codes_uellendahl_katernberg <- c("20","21","22","23","24","25","26")
ride_hailing_districts_codes = list(subdistrict_codes_elberfeld,
                                 subdistrict_codes_elberfeld_west,
                                 subdistrict_codes_uellendahl_katernberg)

ride_hailing_districts = list(districts,districts,districts)

for (district_idx in 1:3) {
  ride_hailing_districts[[district_idx]]@polygons = subset(
    ride_hailing_districts[[district_idx]]@polygons,
    ride_hailing_districts[[district_idx]]$QUARTIER %in%
      ride_hailing_districts_codes[[district_idx]])
  ride_hailing_districts[[district_idx]]@data = subset(
    ride_hailing_districts[[district_idx]]@data,
    ride_hailing_districts[[district_idx]]$QUARTIER %in%
      ride_hailing_districts_codes[[district_idx]])
}

library(dplyr)
query_coordinate_pairs = select(WSW_stations,stop_lon,stop_lat)

library(secr) # Access to the command "pointsInPolygon"
WSW_stations$inside_elberfeld = pointsInPolygon(
  query_coordinate_pairs, ride_hailing_districts[[1]]
)
WSW_stations$inside_elberfeld_west = pointsInPolygon(
  query_coordinate_pairs, ride_hailing_districts[[2]]
)
WSW_stations$inside_uellendahl_katernberg = pointsInPolygon(
  query_coordinate_pairs, ride_hailing_districts[[3]]
)

WSW_stations$inside_ride_hailing_area = as.logical(
  WSW_stations$inside_elberfeld+
    WSW_stations$inside_elberfeld_west+
    WSW_stations$inside_uellendahl_katernberg
  )

ride_hailing_stations=filter(WSW_stations,inside_ride_hailing_area == TRUE)
ride_hailing_stations=select(ride_hailing_stations,-inside_ride_hailing_area)
ride_hailing_stations <- ride_hailing_stations[order(ride_hailing_stations$stop_name),]

ride_hailing_stations$district = ""
ride_hailing_stations$district[
  which(ride_hailing_stations$inside_elberfeld == TRUE)
  ] <- "Elberfeld"
ride_hailing_stations$district[
  which(ride_hailing_stations$inside_elberfeld_west == TRUE)
  ] <- "Elberfeld-West"
ride_hailing_stations$district[
  which(ride_hailing_stations$inside_uellendahl_katernberg == TRUE)
  ] <- "Uellendahl-Katernberg"

ride_hailing_stations=select(ride_hailing_stations,
                             -c(inside_elberfeld,
                                inside_elberfeld_west,
                                inside_uellendahl_katernberg)
                             )

# Exporting data (using "rio")
export(ride_hailing_stations, "./02_output_data/06_ride_hailing_stations.csv")

# Importing the enhanced data set "WLAN_TripID_merged.RData"
WLAN_TripID_merged = import("./02_output_data/03_WLAN_TripID_merged.RData")

WLAN_TripID_ride_hailing_temp=filter(
  WLAN_TripID_merged,
  StartStation %in% ride_hailing_stations$stop_name &
    StopStation %in% ride_hailing_stations$stop_name)

# Exporting data
export(WLAN_TripID_ride_hailing_temp,
       "./02_output_data/07_WLAN_TripID_ride_hailing_temp.RData")

rm(list = ls()) # Clear environment
cat("\014") # Clear console (Ctrl+L)
