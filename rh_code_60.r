# The purpose of this file is the preparation (and export) of
# 3 data sets which will be used for simulation purposes later.

### 1) The model list for origins ("origin_model_list")
# This is generated using the boarding summary data set
# "boarding_extended.RData" by fitting generalized linear models.
# Here we obtain boarding models for origins, where the
# dependence of boarding number on the starting station,
# full hour, weekday and school holidays is modelled.
# We export the results as "origin_model_list.RData".
 
### 2) The list of all origins and possible destinations
# ("origin_destination_list")
# Based on the data set "WLAN_TripID_ride_hailing", we first extract
# all the real destinations of each origin station for different values of
# the full hour, weekday and school holidays. Then we fit multinomial
# log-linear models for all the entries of "origin_destination_list",
# which is the most time-consuming task (about 40min). We save
# the results in the file "multinom_origin_destination.RData".
 
### 3) We construct "the set of characteristics" of new scenarios
# (full hour, weekday and school holidays) for further predictive
# simulations. We export it as "new_data.RData".

# Reading the data sets
pacman::p_load(plyr,dplyr,lubridate,rio)
# Importing the "Boarding Summary" data set "boarding.RData"
boarding = import("./02_output_data/12_boarding_extended.RData")
# Importing the enhanced data set "WLAN_TripID.RData"
WLAN_TripID_ride_hailing =
  import("./02_output_data/08_WLAN_TripID_ride_hailing.RData")

# CAUTION: Importing .xlsx files via "rio::import" or
# "readxl::read_excel" both freeze RStudio, maybe because of
# the incompatibility of .xlsx files with the package
# "lubridate". Do not use these commands!
# =========================================================

# General Poisson loglinear regression model per stop

# The model list for origins. Completes in 30 sec ---------
origin_model_list =
  dlply(
    boarding,
    "StartStation",
    function(x) {
      if (min(
        length(unique(x$SchoolHoliday)),
        length(unique(x$StartDay))
      ) > 1) {
        return(glm(
          formula = boarding_num ~ StartHour + StartDay + SchoolHoliday,
          family = poisson(link = "log"), data = x
        ))
      }
      else return(NA)
    }
  )

# The model list for origins in the evening ---------------
# boarding_evening=dplyr::filter(boarding,
#                                as.integer(StartHour)>19 & as.integer(StartHour)<24)
# 
# origin_model_list_evening =
#   dlply(
#     boarding_evening,
#     "StartStation",
#     function(x) {
#       if (min(
#         length(unique(x$SchoolHoliday)),
#         length(unique(x$StartDay))
#       ) > 1) {
#         return(glm(
#           formula = boarding_num ~ StartHour + StartDay + SchoolHoliday,
#           family = poisson(link = "log"), data = x
#         ))
#       }
#       else return(NA)
#     }
#   )

# Note that essentially
# origin_model_list[[1]]==origin_model_list$`Höchsten/St. Josef Krankenhaus`
# Exporting the data set "simul_boarding" as "RData"
save(origin_model_list,
  file = "./02_output_data/13_origin_model_list.RData"
)

# save(origin_model_list_evening,
#      file = "./02_output_data/13_origin_model_list_evening.RData"
# )
# Caution: exporting with "rio::export" because "rio" has
# limited export capabilities; it does not have full support
# for list exporting.

# Number of stations
# stop_number=length(unique(boarding$StartStation))
# Check that stop_number==length(origin_model_list)==225
# Note that we would have about 2.5x more stations
# in the case of the whole WSW area.

# Multinomial regression for all destinations
# List of all origins and possible destinations
# with hour, weekday and school holiday information
origin_destination_list = dlply(
  WLAN_TripID_ride_hailing, "StartStation",
  function(x) subset(x, select = c(
    StopStation, StartDay,
    StartHour, SchoolHoliday
  ))
)

# Fit Multinomial Log-linear Models
# Takes 25 min to complete (by the way, the running time for
# the larger data set "WLAN_TripID_merged" is about 4x longer).
# Observe that the command "lapply" in the code below is no time-saver;
# it is not faster than the similar code with a "for" loop
library(nnet)
multinom_origin_destination <- lapply(
  origin_destination_list, function(x) {
    if (min(
      length(unique(x$SchoolHoliday)),
      length(unique(x$StartDay))
    ) > 1) {
      return(multinom(StopStation ~ StartHour + StartDay + SchoolHoliday,
        data = x, MaxNWts = 50000
      ))
    }
    else return(NA)
  }
)
# To track the duration, paste the code above into "system.time()"
# Exporting the data set "simul_boarding" as "RData" (takes 8 sec)
save(multinom_origin_destination,
  file = "./02_output_data/14_multinom_origin_destination.RData"
)
# Observe that the size of this file is 5x smaller than the size of
# the file corresponding to the larger data set "WLAN_TripID_merged".

'Now we construct "the set of characteristics" of new scenarios.
The following important characteristics are considered:
the starting station, full hour, weekday, school holidays.
We will call the data set "new_data".'


# Ordered vector of weekdays (from "rh_code_4.r")
weekdays_order <- c("Monday", "Tuesday", "Wednesday",
                    "Thursday", "Friday", "Saturday", "Sunday")

Parameter_list_all = list(StartHour=0:23,
                          StartDay=weekdays_order,
                          SchoolHoliday= c(FALSE, TRUE))
Parameter_list_all$StartHour = as.character(Parameter_list_all$StartHour)


# Old version (with only nonzero boarding numbers)
# Parameter_list = as.list(
#   select(WLAN_TripID_2019_ride_hailing, StartHour, StartDay, SchoolHoliday)
# )
# Parameter_list$StartHour = unlist(Parameter_list$StartHour)
# Parameter_list$StartHour =
#   as.character(sort(unique(as.numeric(Parameter_list$StartHour))))
# Parameter_list$StartDay = unlist(Parameter_list$StartDay)
# Parameter_list$StartDay = unique(as.character(Parameter_list$StartDay))
# Parameter_list$SchoolHoliday = c(TRUE, FALSE)

new_data = expand.grid(Parameter_list_all, stringsAsFactors = FALSE)
# new_data = expand.grid(Parameter_list) # The old command
# new_data$StartHour = as.factor(new_data$StartHour)
# new_data$StartDay = as.factor(new_data$StartDay)
# new_data$SchoolHoliday = as.factor(new_data$SchoolHoliday)

# Exporting the data set "new_data" as "RData"
save(new_data, file = "./02_output_data/15_new_data.RData")

rm(list = ls()) # Clear environment
cat("\014") # Clear console (Ctrl+L)
pacman::p_unload(all) # Clears all add-on packages
