# The purpose of this file is the preparation of the calendar data sets
# for vrr

# Importing the calendar
library(rio)
setwd("./01_input_data/Timetables")
calendar_dates_01 <- import("./google_transit_2019_01/calendar_dates.txt")
calendar_dates_02 <- import("./google_transit_2019_02/calendar_dates.txt")
calendar_dates_03 <- import("./google_transit_2019_03/calendar_dates.txt")
calendar_dates_04 <- import("./google_transit_2019_04/calendar_dates.txt")
calendar_dates_05 <- import("./google_transit_2019_05/calendar_dates.txt")
calendar_dates_06 <- import("./google_transit_2019_06/calendar_dates.txt")

library(dplyr)
calendar_dates = bind_rows(calendar_dates_01,calendar_dates_02,calendar_dates_03,
                           calendar_dates_04,calendar_dates_05,calendar_dates_06)
rm(calendar_dates_01,calendar_dates_02,calendar_dates_03,
   calendar_dates_04,calendar_dates_05,calendar_dates_06)
calendar_dates = select(calendar_dates,date,service_id)
calendar_dates = arrange(calendar_dates,date,service_id)
calendar_dates = distinct(calendar_dates) # remove 15481 duplicate rows

calendar_dates_regular_rides =
  dplyr::filter(calendar_dates,!grepl("Special",service_id))
rm(calendar_dates)

# Reading the "stop_times" monthly data sets
number_of_data_files = 6
stop_times_list = vector("list", length = number_of_data_files)

for (month_no in 1:number_of_data_files) {
  stop_times_list[[month_no]] = import(
    paste0("./google_transit_2019_0",month_no,"/stop_times.txt")
  )
}

# Merging the "stop_times" monthly data sets
stop_times_merged = as.data.frame(data.table::rbindlist(stop_times_list))

# Clear unnecessary variables from the environment
rm(stop_times_list,number_of_data_files,month_no)

# Remove superflous columns
stop_times_merged =
  select(stop_times_merged,trip_id,stop_id,arrival_time)

# Dealing with hours greater as 23 (there are even
# times like 25:12; the maximum hour is 31)
# Create the "day_change" column
stop_times_merged$day_change =
  substr(stop_times_merged$arrival_time, 1, 2) > 23
day_change_row_indices =
  which(stop_times_merged$day_change == TRUE)


stop_times_merged$arrival_time_corrected =
  paste0(
    sprintf("%02d", # 2-digit text format for one-digit numbers
            as.integer(substr(stop_times_merged$arrival_time, 1, 2))-
              as.integer(stop_times_merged$day_change)*24
            ),
    substr(stop_times_merged$arrival_time, 3,8)
    )

stop_times_merged$arrival_time = NULL

stop_times_merged =
  rename(stop_times_merged,
         arrival_time = arrival_time_corrected)

stop_times_merged =
  select(stop_times_merged,
         trip_id,stop_id,arrival_time,day_change)

# Remove special rides (29% of all rides,
# uncomment the next command to check)
# nrow(filter(stop_times_merged,
#             grepl("Special",trip_id)))/nrow(stop_times_merged)
stop_times_merged =
  filter(stop_times_merged,!grepl("Special",trip_id))

# Generating weekday columns
stop_times_merged$Monday = grepl("Mo",stop_times_merged$trip_id)
stop_times_merged$Tuesday = grepl("Tu",stop_times_merged$trip_id)
stop_times_merged$Wednesday = grepl("We",stop_times_merged$trip_id)
stop_times_merged$Thursday = grepl("Th",stop_times_merged$trip_id)
stop_times_merged$Friday = grepl("Fr",stop_times_merged$trip_id)
stop_times_merged$Saturday = grepl("Sa",stop_times_merged$trip_id)
stop_times_merged$Sunday = grepl("Su",stop_times_merged$trip_id)

# Temp columns to shift column contents
stop_times_merged$Monday_tmp = stop_times_merged$Monday
stop_times_merged$Tuesday_tmp = stop_times_merged$Tuesday
stop_times_merged$Wednesday_tmp = stop_times_merged$Wednesday
stop_times_merged$Thursday_tmp = stop_times_merged$Thursday
stop_times_merged$Friday_tmp = stop_times_merged$Friday
stop_times_merged$Saturday_tmp = stop_times_merged$Saturday
stop_times_merged$Sunday_tmp = stop_times_merged$Sunday

stop_times_merged$Monday[day_change_row_indices] =
  stop_times_merged$Sunday_tmp[day_change_row_indices]

stop_times_merged$Tuesday[day_change_row_indices] =
  stop_times_merged$Monday_tmp[day_change_row_indices]

stop_times_merged$Wednesday[day_change_row_indices] =
  stop_times_merged$Tuesday_tmp[day_change_row_indices]

stop_times_merged$Thursday[day_change_row_indices] =
  stop_times_merged$Wednesday_tmp[day_change_row_indices]

stop_times_merged$Friday[day_change_row_indices] =
  stop_times_merged$Thursday_tmp[day_change_row_indices]

stop_times_merged$Saturday[day_change_row_indices] =
  stop_times_merged$Friday_tmp[day_change_row_indices]

stop_times_merged$Sunday[day_change_row_indices] =
  stop_times_merged$Saturday_tmp[day_change_row_indices]

# Remove temp columns
stop_times_merged =
  select(stop_times_merged,
         -day_change,-Monday_tmp,-Tuesday_tmp,-Wednesday_tmp,
         -Thursday_tmp,-Friday_tmp,-Saturday_tmp,-Sunday_tmp)
 
# Check that indeed all the regular rides are covered:
nrow(filter(
  stop_times_merged,
  Monday+Tuesday+Wednesday+Thursday+
    Friday+Saturday+Sunday>0)
) == nrow(stop_times_merged)

rm(list = ls()) # Clear environment
cat("\014") # Clear console (Ctrl+L)
