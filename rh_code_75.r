# The purpose of this file is the creation of
# 30x7=210 data sets for evening rides within 30 weeks.
# We use the data set "simulated_ride_requests_all_weeks"
# from the previous part of the code (rh_code_70.r)

# As we agreed, the evening hours 22-23 (precisely, 22:00-23:59)
# of each day are combined with the hours 0-3 of the next day as
# one evening. The hours 0-3 of the first Monday are considered as
# the second part of the evening for the last Sunday. The exported
# .csv data frames include a 3-digit number at the end of their names.
# The first 2 digits indicate the week number, and the last digit
# shows the weekday (i.e., 1 = Monday, 2 = Tuesday,..., 7 = Sunday)

# READING THE DATA SETS
pacman::p_load(rio,dplyr) # Attaching several packages
# Caution: do not load dplyr first because of dependencies!
# Importing the data set "simulated_ride_requests_all_weeks"
simulated_ride_requests_all_weeks =
  import("./02_output_data/18_simulated_ride_requests_all_weeks.csv")

# Select evening hours
simulated_ride_requests_all_weeks_evening = 
  filter(simulated_ride_requests_all_weeks,
         StartHour >= 22 | StartHour <= 3)
simulated_ride_requests_all_weeks_evening = 
  filter(simulated_ride_requests_all_weeks_evening,
         SchoolHoliday == FALSE)

# Removing superfluous columns
simulated_ride_requests_all_weeks_evening =
  select(simulated_ride_requests_all_weeks_evening,
         -SchoolHoliday,-StartStation,-StartDistrict,
         -Destination,-DestinationDistrict)

# An auxiliary vector for the order of weekdays
weekdays_order <- c("Monday", "Tuesday", "Wednesday",
                    "Thursday", "Friday", "Saturday", "Sunday")

# Replace the weekday names by numbers
simulated_ride_requests_all_weeks_evening$StartDay = 
  match(simulated_ride_requests_all_weeks_evening$StartDay, weekdays_order)

simulated_ride_requests_all_weeks_evening$Day_No = 
  10*simulated_ride_requests_all_weeks_evening$Week_No+
  simulated_ride_requests_all_weeks_evening$StartDay

# Removing superfluous columns
simulated_ride_requests_all_weeks_evening =
  select(simulated_ride_requests_all_weeks_evening,-Week_No)

week_number = 30 # integer between 1 and 99 from previous file


# ----------------------------------------------------------------
# The function "find_previous_day" calculates the previous day of
# a day "w.d" (Week w, Weekday d). Here d takes values 1:7

# Arguments
# current_day: "numeric" (should be of form "w.d" where "w" is
                          # the week ID and "d" is the weekday ID)
# Output: "numeric" (of form "w.d")

find_previous_day = function(current_day) {
  previous_day = current_day - 1
  # If the current day is Monday, we choose the Sunday of the previous week:
  if (previous_day%%10 == 0 & previous_day>10) previous_day = previous_day - 3
  # If we went out of the week range, we start again with the first week:
  if (previous_day == 10) previous_day =10*week_number + 7 # the last Sunday
  return(previous_day)
}
# =======================================================================

# Insert the evening number
simulated_ride_requests_all_weeks_evening$Evening_No = 
  simulated_ride_requests_all_weeks_evening$Day_No

evening_rides_number = nrow(simulated_ride_requests_all_weeks_evening)

for (row_idx in 1:evening_rides_number) {
  if (simulated_ride_requests_all_weeks_evening$StartHour[row_idx]<4) {
    simulated_ride_requests_all_weeks_evening$Evening_No[row_idx] =
      find_previous_day(
        simulated_ride_requests_all_weeks_evening$Day_No[row_idx]
      )}
}

# Removing superfluous columns
simulated_ride_requests_all_weeks_evening =
  select(simulated_ride_requests_all_weeks_evening,-Day_No)

# Reordering the columns with dplyr
simulated_ride_requests_all_weeks_evening =
  simulated_ride_requests_all_weeks_evening %>%
  select(Evening_No,everything())

# Sorting by multiple columns
# Caution: Sorting works improperly with decimal content!

# Version 1 (dplyr::arrange)
evening_hours_order <- c(22,23,0:3)

simulated_ride_requests_all_weeks_evening =
  simulated_ride_requests_all_weeks_evening %>%
  arrange(Evening_No,match(StartHour, evening_hours_order),
          StartMin,StartStation_lat,StartStation_lon,Passengers)

# Version 2
# simulated_ride_requests_all_weeks_evening =
#   simulated_ride_requests_all_weeks_evening[order(
#     simulated_ride_requests_all_weeks_evening$Evening_No,
#     simulated_ride_requests_all_weeks_evening$StartDay,
#     simulated_ride_requests_all_weeks_evening$StartHour,
#     simulated_ride_requests_all_weeks_evening$StartMin,
#     simulated_ride_requests_all_weeks_evening$StartStation_lat,
#     simulated_ride_requests_all_weeks_evening$StartStation_lon,
#     simulated_ride_requests_all_weeks_evening$Passengers), ]

# Version 3
# simulated_ride_requests_all_weeks_evening =
#   simulated_ride_requests_all_weeks_evening[with(
#     simulated_ride_requests_all_weeks_evening,
#     order(Evening_No,StartDay)
#     ), ]

#  Compare
# ID = 1:5
# Value1 = as.numeric(c("3.4","6.4","3.4","3.4","0.1"))
# Value2 = as.numeric(c("4","3","4","2","1"))
# df<-data.frame(ID,Value1,Value2,stringsAsFactors = FALSE)
# df= arrange(df,Value1,Value2)
# =============================================

# Replace the weekday numbers by names
simulated_ride_requests_all_weeks_evening$StartDay = 
  weekdays_order[simulated_ride_requests_all_weeks_evening$StartDay]

# List of all evenings and corresponding trips
simulated_rides_list_by_evenings = plyr::dlply(
  simulated_ride_requests_all_weeks_evening, "Evening_No",
  function(x) subset(x, select = -Evening_No)
)

# Adding extra 0s to all two-digit day numbers to obtain a nice 3-digit format
names(simulated_rides_list_by_evenings) = 
  sprintf("%03d", as.integer(names(simulated_rides_list_by_evenings)))

# Exporting the evening rides as separate files
for (evening_id in names(simulated_rides_list_by_evenings)) {
  export(simulated_rides_list_by_evenings[[evening_id]],
         file = paste0(
           "./02_output_data/20_simulated_evening_rides/simulated_evening_rides_22_03_no_",
           evening_id,".csv")
  )
}

# Exemplary data (22:00 - 03:59)
# for (number_of_simulations in 1:30) {
#   simulated_ride_requests = import(
#     paste0("./02_output_data/17_simulated_ride_requests_",
#            sprintf("%02d", number_of_simulations),".csv")
#     )
#   simulated_ride_requests_22_03 = filter(
#     simulated_ride_requests, StartHour>=22 | StartHour<=3)
#   export(simulated_ride_requests_22_03,
#          file = paste0("./02_output_data/20_simulated_ride_requests_22_03_no_",
#                        sprintf("%02d", number_of_simulations),".csv")
#          )
# }

# Exemplary data (Monday, 06:00-22:00)
# simulated_ride_requests_Mo_6_22 = filter(
#   simulated_ride_requests, StartDay=="Monday" & StartHour>=6 & StartHour<=22)
# simulated_ride_requests_Mo_22 = filter(
#   simulated_ride_requests_Mo_6_22, StartHour==22)
# simulated_ride_requests_Mo_22_sch = filter(
#   simulated_ride_requests_Mo_22, SchoolHoliday==FALSE)
# simulated_ride_requests_Mo_22_hol = filter(
#   simulated_ride_requests_Mo_22, SchoolHoliday==TRUE)
# View(simulated_ride_requests_Mo_22_sch)
# View(simulated_ride_requests_Mo_22_hol)
# 
# export(simulated_ride_requests_Mo_6_22,
#        file = "./02_output_data/18_simulated_ride_requests_Mo_6_22.csv"
# )

rm(list = ls()) # Clear environment
cat("\014") # Clear console (Ctrl+L)
'Tip: perform the two above commands and then
try new simulation parameters, i.e.,
start_hour,s_holiday and wk_day'
pacman::p_unload(all) # Clears all add-on packages
