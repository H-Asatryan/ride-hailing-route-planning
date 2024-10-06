# The purpose of this file is the preparation of a
# "Boarding Summary" for all stops. If we already have
# the data set "WLAN_TripID_ride_hailing.RData",
# we could begin with this file!

# Reading the data sets
library(dplyr)
library(lubridate)
# Importing the enhanced data set "WLAN_TripID_ride_hailing.RData"
WLAN_TripID_ride_hailing =
  rio::import("./02_output_data/08_WLAN_TripID_ride_hailing.RData")
# CAUTION: Importing .xlsx files via "rio::import" or "readxl::read_excel"
# both freeze RStudio, maybe because of the incompatibility of .xlsx files
# with the package "lubridate" Do not use these commands!
# ==========================================================

# Preparing "The Boarding Summary" for all stops
# We create the data "boarding" containing the number
# of passengers getting on a bus at each stop
# We calculate boardings for different combinations of
# stops, weekdays, arrival times and school holidays
# The new variable "boarding_num" equals the number of different
# boardings for each combination; we assume that different
# passengers have different MAC Addresses!

boarding = WLAN_TripID_ride_hailing %>%
  dplyr::select(
    MacID, StartDate,StartStation, StartDay,
    StartHour, SchoolHoliday
  ) %>% 
  dplyr::group_by(StartDate,StartStation, StartDay, StartHour, SchoolHoliday) %>%
  dplyr::summarize(boarding_num = length(MacID)) # A new column for the groupped values
# CAUTION: Though "dplyr" is loaded, we use "dplyr::" because of dependencies
# (like "plyr") with other packages! Removing it ruins the data set "boarding".
# After several hours I realized that we should either use commands with prefix
# "dplyr::" or detach the corresponding packages by detach(package:plyr) or
# "pacman::p_unload()". But detaching is inconvenient since "WLAN_TripID_2019"
# needs "lubridate" etc.
# 
# IMPORTANT: The boarding times of simultaneously boarded passengers differ,
# because the router registered their gadgets at different times. Hence it is
# necessary to count boardings within a reasonable spell of time, e.g., within
# a full hour or within a 10-min interval. Here we do it within full hours.

# boarding$StartDay = as.factor(boarding$StartDay)
# boarding$StartHour = as.factor(boarding$StartHour)
# boarding$SchoolHoliday <- as.factor(boarding$SchoolHoliday)

# Creating a completed boarding data set by adding 0's

# Now we construct "the set of characteristics" of new scenarios.
# The following important characteristics are considered:
# the starting station, full hour, weekday, school holidays.
# We will call the data set "new_data".

library(tidyr)
boarding_ungroup = ungroup(boarding)
boarding_extended = boarding_ungroup %>%
  complete(StartDate, StartStation, StartHour)
boarding_extended$boarding_num[
  is.na(boarding_extended$boarding_num)] = as.integer(0)
# View(boarding_extended)

# Complete the column StartDate
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

boarding_extended$StartDay <- weekdays(boarding_extended$StartDate)

# Adding the school holidays info for the new NAs
school_holidays <- rio::import("./01_input_data/schulferien.csv")
colnames(school_holidays) <- "SchoolHolidays"
school_holidays$SchoolHolidays = as.Date(school_holidays$SchoolHolidays)

boarding_extended$SchoolHoliday <- school_holidays$SchoolHolidays[
  match(boarding_extended$StartDate, school_holidays$SchoolHolidays)
]
boarding_extended$SchoolHoliday <- as.logical(boarding_extended$SchoolHoliday)
boarding_extended$SchoolHoliday[
  is.na(boarding_extended$SchoolHoliday)
] <- FALSE

# Rearange the columns of "boarding_extended" like "boarding"
boarding_extended = boarding_extended[,colnames(boarding)]
boarding_extended = dplyr::group_by(
  boarding_extended,StartDate,StartStation,
  StartDay, StartHour, SchoolHoliday)
boarding_extended$StartDay = as.factor(boarding_extended$StartDay)
boarding_extended$StartHour = as.factor(boarding_extended$StartHour)
# View(boarding)
# View(boarding_extended)

# Exporting data
rio::export(boarding, "./02_output_data/11_boarding.RData")
rio::export(boarding_extended, "./02_output_data/12_boarding_extended.RData")
# nrow(boarding) -  nrow(boarding[complete.cases(boarding), ])==0
# This shows that there are no NA's here!
