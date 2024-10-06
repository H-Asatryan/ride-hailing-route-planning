# The purpose of this file is the insertion of a column for
# the preceding scheduled regular trip (based on vrr data).
# We add this information into the data set "regular_timetable"
# from the previous file. We save the output as a list, since it
# will be convenient for later purposes.

# Importing the "regular_timetable" data set
library(rio)
library(dplyr)

regular_timetable =
  import("./02_output_data/03b_regular_timetable.RData")

# The following command takes about 30 min on my PC (8GB RAM)
regular_timetable = arrange(regular_timetable,
                            stop_id,date_time,trip_id)
gc() # Purge unused RAM

# Restructure simultaneous rides of different vehicles
regular_timetable_grouped =
  group_by(regular_timetable,stop_id,date_time) # takes >20 min
# and freezes my PC!

# Remove unnecessary data and free up more memory
rm(regular_timetable)
gc()

# Merge trip IDs (3 min to complete):
regular_timetable_merged_trips =
  summarise(regular_timetable_grouped,
            trip_ids=paste(trip_id,collapse = ";"))
# Sys.time()
rm(regular_timetable_grouped)
gc()

# Export the data set to restart R due to the low memory:
export(regular_timetable_merged_trips,
       "./02_output_data/03c_regular_timetable_merged_trips.RData")

# Restart RStudio to continue:
library(rio)
library(dplyr)
regular_timetable_merged_trips =
  import("./02_output_data/03c_regular_timetable_merged_trips.RData")
# memory.limit(size=4000) # limit memory to let PC breath
# limit memory does not work under Windows!

# Timetable of each station
# Convert format to apply "plyr::dlply"
regular_timetable_merged_trips$date_time =
  as.character(regular_timetable_merged_trips$date_time)

timetable_list_per_stop = plyr::dlply(
  regular_timetable_merged_trips, "stop_id")

# Export the list to restart R due to the low memory:
saveRDS(timetable_list_per_stop,
            "./02_output_data/03d_timetable_list_per_stop.rds")
# Tip: The Format ".rds" is better for R lists

# Restart RStudio to continue:
timetable_list_per_stop = readRDS(
  "./02_output_data/03d_timetable_list_per_stop.rds")

# timetable_list_per_stop =
#   rlist::list.load("./02_output_data/03d_timetable_list_per_stop.RData")

# Adding preceding trips info (takes 2 min to complete):
for (station in names(timetable_list_per_stop)) {
  timetable_list_per_stop[[station]]$date_time_preceding =
    c(timetable_list_per_stop[[station]]$date_time[
      length(timetable_list_per_stop[[station]]$date_time)],
      timetable_list_per_stop[[station]]$date_time[
      -length(timetable_list_per_stop[[station]]$date_time)])
  if (nrow(timetable_list_per_stop[[station]])>1 & strptime(
    timetable_list_per_stop[[
      station]]$date_time_preceding[1],
    format = "%Y-%m-%d %H:%M:%S") >
    strptime(timetable_list_per_stop[[
      station]]$date_time_preceding[2],
      format = "%Y-%m-%d %H:%M:%S")) {
    timetable_list_per_stop[[
      station]]$date_time_preceding[1] =
      as.character(strptime(timetable_list_per_stop[[
        station]]$date_time_preceding[1],
        format = "%Y-%m-%d %H:%M:%S")-
          lubridate::days(7))
  }
}

# Export data:
saveRDS(timetable_list_per_stop,
        "./02_output_data/03e_timetables_list_preceding_trips.rds")
