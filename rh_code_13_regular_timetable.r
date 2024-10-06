# The purpose of this file is the preparation of the
# vrr timetable for regular rides in 2019/01-2019/06

# Importing the "stop_times" monthly data sets
number_of_data_files = 6
stop_times_list = vector("list", length = number_of_data_files)

library(rio)
setwd("./01_input_data/Timetables")

for (month_no in 1:number_of_data_files) {
  stop_times_list[[month_no]] = import(
    paste0("./google_transit_2019_0",month_no,"/stop_times.txt")
  )
}

# Merging the "stop_times" monthly data sets
stop_times_merged = as.data.frame(data.table::rbindlist(stop_times_list))

# Clear unnecessary variables from the environment
rm(stop_times_list,number_of_data_files,month_no)
gc() # to free up about 2 GB of RAM

# Remove superflous columns
library(dplyr)
stop_times_merged =
  select(stop_times_merged,trip_id,stop_id,arrival_time)

# Remove duplicate rows (about 15 %)
stop_times_merged = distinct(stop_times_merged)

# Remove special rides (29% of all rides,
# uncomment the next command to check)
# nrow(filter(stop_times_merged,
#             grepl("Special",trip_id)))/nrow(stop_times_merged)
stop_times_merged =
  filter(stop_times_merged,!grepl("Special",trip_id))

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
rm(day_change_row_indices)
gc() # Purge RAM

# Check that indeed all the regular rides are covered:
nrow(filter(
  stop_times_merged,
  Monday+Tuesday+Wednesday+Thursday+
    Friday+Saturday+Sunday>0)
) == nrow(stop_times_merged)

# To do: 
# add dummy dates corresponding to weekdays, make a date/time column to ease
# comparison of times!!!
stop_times_per_weekday_list = vector("list", length = 7)
for (weekday_no in 1:7) {
  # Select only one weekday column:
  stop_times_per_weekday_list[[weekday_no]] =
    dplyr::select(stop_times_merged,c(1:3,3+weekday_no))
  # Select only the "TRUE" observations on the day:
  stop_times_per_weekday_list[[weekday_no]] =
    stop_times_per_weekday_list[[weekday_no]][
      stop_times_per_weekday_list[[weekday_no]][,4]=="TRUE",
    ]
  # Replace "TRUE"s by the weekday name:
  stop_times_per_weekday_list[[weekday_no]][,4] =
    colnames(stop_times_per_weekday_list[[weekday_no]])[4]
  # One column title for all list entries:
  colnames(stop_times_per_weekday_list[[weekday_no]])[4] = "weekday"
}

# Merging the "stop_times_per_weekday_list" entries:
timetable_df = as.data.frame(
  data.table::rbindlist(stop_times_per_weekday_list))

# Clear unnecessary variables from the environment
rm(stop_times_merged,stop_times_per_weekday_list,weekday_no)
gc() # Purge unused RAM

# Enter dummy dates starting from 2019-02-04 (Monday):
timetable_df$date_time = timetable_df$weekday

timetable_df$date_time[
  timetable_df$weekday=="Monday"] = "2019-02-04"
timetable_df$date_time[
  timetable_df$weekday=="Tuesday"] = "2019-02-05"
timetable_df$date_time[
  timetable_df$weekday=="Wednesday"] = "2019-02-06"
timetable_df$date_time[
  timetable_df$weekday=="Thursday"] = "2019-02-07"
timetable_df$date_time[
  timetable_df$weekday=="Friday"] = "2019-02-08"
timetable_df$date_time[
  timetable_df$weekday=="Saturday"] = "2019-02-09"
timetable_df$date_time[
  timetable_df$weekday=="Sunday"] = "2019-02-10"

timetable_df$date_time =
  strptime(
    paste0(timetable_df$date_time,
           timetable_df$arrival_time),
    format = "%Y-%m-%d %H:%M:%S")

# Reorder the columns, delete the column "arrival_time"
timetable_df = select(timetable_df,
                      stop_id,date_time,weekday,trip_id)
gc() # Purge unused RAM

# Exporting data
replicate(2,setwd(".."))
export(timetable_df,
       "./02_output_data/03b_regular_timetable.RData")

rm(list = ls()) # Clear environment
cat("\014") # Clear console (Ctrl+L)
