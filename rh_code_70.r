# The purpose of this file is the preparation and execution of
# the predictive (Poisson) simulation. If we already have the
# data sets "ride_hailing_stations.txt", "origin_model_list.RData"
# and "new_data.RData", we could begin with this file!
# After execution of this code we export the data set
# "simulated_trips" as "csv"

# READING THE DATA SETS
pacman::p_load(plyr,dplyr,lubridate,rio) # Attaching several packages
# Importing the model list for origins
origin_model_list =
  import("./02_output_data/13_origin_model_list.RData")
# Importing the "set of characteristics" of new scenarios
# (full hour, weekday and school holidays)
new_data = import("./02_output_data/15_new_data.RData")
# Importing the station (stop) information
ride_hailing_stations <- import(
  "./02_output_data/06_ride_hailing_stations.csv")

# CAUTION: Importing .xlsx files via "rio::import" or
# "readxl::read_excel" both freeze RStudio, maybe because of
# the incompatibility of .xlsx files with the package
# "lubridate". Do not use these commands!
# =========================================================

# PREDICTIVE SIMULATION OF NEW SCENARIOS
# The following important characteristics are considered:
# the starting station, full hour, weekday, school holidays.
# The simulation preserves the dependences between these parameters.

# Simulating new data with predict
# Observe that nrow(new_data)=336

# ----------------------------------------------------------------
# The function "reduce_new_data_for_model" excludes the superflous
# values of "StartHour","StartDay" and "SchoolHoliday" from the
# data set "new_data". These values correspond to times when
# no bus operates. For a given data set, we construct its
# "intersection" with the set of "origin_model_list[[]]" stations.
# The argument "model_list_entry" is a list. In our case it
# will be an output of the command "dlply".
# 
# Arguments
# new_data: "data.frame" / with columns "StartHour","StartDay","SchoolHoliday"
# model_list_entry: "glm" "lm" / an entry of the output list of "dlply")
# 
# Output: "data.frame"

reduce_new_data_for_model = function(new_data, model_list_entry) {
  in_new_data = subset(
    new_data, new_data$StartHour %in% model_list_entry$StartHour
  )
  in_new_data = subset(
    in_new_data, in_new_data$StartDay %in% model_list_entry$StartDay
  )
  in_new_data = subset(
    in_new_data, in_new_data$SchoolHoliday %in% model_list_entry$SchoolHoliday
  )
  return(in_new_data)
}

# Observations:
# 1) No NA's in "origin_model_list"; to get all NAs, use
# for (i in 1:length(origin_model_list)) if (
#   is.na(origin_model_list[[i]])) cat(i,",",sep="")

# ??? Check this
# 2) For some i, new_data_tmp in function has less rows than
# new_data; these are the last two rows!!!
# ==========================================================


# --------------------------------------------------------------------
# The function "predict_pois_intensities" performs a prediction of
# the Poisson parameter corresponding to the boarding using the values
# of "StartHour","StartDay" and "SchoolHoliday" from the data set
# "new_data". For each entry of the second argument, we first use
# the function "reduce_new_data_for_model" to construct the subset of
# parameters which are admisible for that station. Then we predict the
# Poisson parameters corresponding to each station and the corresponding
# subset of parameters "StartHour","StartDay","SchoolHoliday".
 
# Arguments
# new_data: "data.frame" / with columns "StartHour","StartDay","SchoolHoliday"
# origin_model_list: "list" / the output list of "dlply")
 
# Output: "list" / the non-NA output consists of columns "StartHour",
# "StartDay","SchoolHoliday","LambdaIntesity","StartStation"

predict_pois_intensities = function(new_data, origin_model_list) {
  l = length(origin_model_list) # Number of stations with nonzero boarding number
  predict_intens_list = vector("list", length = l)
  for (i in 1:l)
  {
    if (is.na(origin_model_list[[i]])) predict_intens_list[[i]] = NA
    else {
      new_data_tmp = reduce_new_data_for_model(new_data, origin_model_list[[i]]$data)
      lambda = predict(origin_model_list[[i]],
        type = "response", newdata = new_data_tmp
      )
      new_data_tmp$LambdaIntesity = lambda
      predict_intens_list[[i]] = merge(new_data, new_data_tmp,
        all.x = TRUE, sort = FALSE
      )
      predict_intens_list[[i]]$StartStation =
        origin_model_list[[i]]$data$StartStation[1]
      predict_intens_list[[i]]$StartHour =
        as.numeric(predict_intens_list[[i]]$StartHour)
      predict_intens_list[[i]]$StartDay =
        as.character(predict_intens_list[[i]]$StartDay)
      predict_intens_list[[i]]$SchoolHoliday =
        as.logical(predict_intens_list[[i]]$SchoolHoliday)
      predict_intens_list[[i]]$LambdaIntesity =
        as.numeric(predict_intens_list[[i]]$LambdaIntesity)
      predict_intens_list[[i]]$StartStation =
        as.character(predict_intens_list[[i]]$StartStation)
      predict_intens_list[[i]] <- predict_intens_list[[i]][
        complete.cases(predict_intens_list[[i]]), ] # Removing NA's
      predict_intens_list[[i]] = predict_intens_list[[i]][ , c(1:3,5,4)]
    }
  }
  
  # Remove NA entries from the list
  predict_intens_list[sapply(predict_intens_list,function(x) all(is.na(x)))] <- NULL
  
  return(predict_intens_list)
}

# Importing the data set of Hol mich app:
WSW_ride_requests =
  import("../Hol_mich_app/02_output_data/09_WSW_ride_requests_2021_11_12.RData")

pacman::p_load(discreteRV) # Creation of discrete random variables

group_sizes=prop.table(table(WSW_ride_requests$Number_of_passengers))
group_sizes=as.data.frame(group_sizes,stringsAsFactors = FALSE)
colnames(group_sizes) = c("size","frequency")
group_sizes$size = as.integer(group_sizes$size)
passenger_group_rv = RV(outcomes = group_sizes$size,
                        probs = group_sizes$frequency)
average_group_size = E(passenger_group_rv)
group_sizes$average_size = average_group_size
group_sizes$size = as.character(group_sizes$size) # to use with sample()
# View(group_sizes)
# !!! The group size calculations can be performed outside the
# function "simulate_groups" since we do not use hour, weekday etc
# There are some strange differences between Mo,Tu and Wed
# due to the first portion of the Hol Mich App data set
# hence we take the overall passenger distribution from
# Hol Mich App data (not the distribution for e.g., Monday)


# -------------------------------------------------------------
# The function "simulate_groups" simulates passenger group sizes
# corresponding to the data set "pred_boarding_intensities" and
# parameters start_hour, week_day and school_holiday.
# The simulation is based on the "Hol mich App" data set.

# Arguments
# pred_boarding_intensities_entry: "data.frame"
# group_sizes: "data.frame"
# start_hour: "numeric"
# week_day: "character"
# school_holiday: "logical"

# Output: "data.frame"

simulate_groups = function(pred_boarding_intensities_entry, group_sizes,
                           start_hour, week_day, school_holiday) {
  
  WSWLAN_boarding_scenario = filter(pred_boarding_intensities_entry, StartHour == start_hour
                                    & SchoolHoliday == school_holiday
                                    & StartDay == week_day)
  
  # WSW_Hol_mich_App_rides = filter(WSW_ride_requests, hour == start_hour
  #                                  & school_holidays == school_holiday
  #                                  & weekday == week_day)
  
  if (nrow(WSWLAN_boarding_scenario)==0) {
    request_number=0
  } else {
    lambda_group_per_trip = WSWLAN_boarding_scenario$LambdaIntesity/group_sizes$average_size[1]
    WSWLAN_boarding_scenario$group_number = 1
    WSWLAN_boarding_scenario$Passenger_groups=""
    for (i in 1:nrow(WSWLAN_boarding_scenario)) {
      WSWLAN_boarding_scenario$group_number[i] = rpois(1, lambda_group_per_trip[i])
      if (WSWLAN_boarding_scenario$group_number[i] > 0) {
        WSWLAN_boarding_scenario$Passenger_groups[i] =
          paste(sample(group_sizes$size,
                       size = WSWLAN_boarding_scenario$group_number[i],
                       replace = TRUE, prob = group_sizes$frequency),
                collapse = " ")
      } # i.e., run if there are groups!!!
    }
    
    request_number = sum(WSWLAN_boarding_scenario$group_number)
    
  }
  
  ride_request <- data.frame("StartHour" = integer(request_number),
                             "StartDay"= character(request_number),
                             "SchoolHoliday"= logical(request_number),
                             "StartStation"= character(request_number),
                             "Passengers"= integer(request_number),
                             "StartMin" = integer(request_number),
                             stringsAsFactors=FALSE)
  
  if (request_number==0) {
    ride_request = ride_request[ , c(1,6,2:5)]
    return(ride_request)
  }
  
  WSWLAN_boarding_scenario = filter(
    WSWLAN_boarding_scenario,
    WSWLAN_boarding_scenario$Passenger_groups>0)
  preceeding_row_idx=0
  for (i in 1:nrow(WSWLAN_boarding_scenario)) {
    passenger_groups_vector = unlist(
      strsplit(as.character(WSWLAN_boarding_scenario$Passenger_groups[i])," ")) # Checked if ...[i]>0
    for (j in 1:WSWLAN_boarding_scenario$group_number[i]) {
      ride_request[preceeding_row_idx+j,c(1:4)] = 
        WSWLAN_boarding_scenario[i,c(1:4)]
      ride_request$Passengers[preceeding_row_idx+j] =
        passenger_groups_vector[j]
    }
    preceeding_row_idx = preceeding_row_idx +
      WSWLAN_boarding_scenario$group_number[i]
  }
  
  ride_request$Passengers = as.integer(ride_request$Passengers)
  
  ride_request$StartMin = sample(0:59, size = nrow(ride_request),replace = TRUE)
  
  ride_request = ride_request[ , c(1,6,2:5)]
  
  # average_passenger_number = mean(boarding_scenario$Passengers)
  # lambda_group = average_passenger_number/average_group_size
  # simulated_group_number = rpois(1,lambda_group)
  
  return(ride_request)
}


# -----------------------------------------------------------
# The function "simulate_groups_all" simulates all passenger
# group sizes corresponding to the data sets
# "pred_boarding_intensities" and "WSW_ride_requests"
# The dependences between group sizes, starting station,
# full hour, weekday and school holidays are preserved

# Arguments
# pred_boarding_intensities: list of data frames
# WSW_ride_requests: "data.frame"

# Output: "data.frame"

simulate_groups_all = function(pred_boarding_intensities, group_sizes) {
  
  l = length(pred_boarding_intensities) # Number of stations with nonzero boardings number
  simul_group_list = vector("list", length = l)
  for (i in 1:l) {
    if (is.na(pred_boarding_intensities[[i]])) simul_group_list[[i]] = NA
    else {
      l_i = nrow(pred_boarding_intensities[[i]])
      current_station_group_list = vector("list", length = l_i)
      for (j in 1:l_i) {
        current_station_group_list[[j]] = simulate_groups(
          pred_boarding_intensities[[i]],
          group_sizes,
          pred_boarding_intensities[[i]]$StartHour[j],
          pred_boarding_intensities[[i]]$StartDay[j],
          pred_boarding_intensities[[i]]$SchoolHoliday[j]
        )
      }
      
      # Remove NA entries from the list
      current_station_group_list[sapply(
        current_station_group_list,function(x) all(is.na(x)))] <- NULL
      
      simul_group_list[[i]] = data.table::rbindlist(
        current_station_group_list) # merge all entries
      simul_group_list[[i]] = as.data.frame(simul_group_list[[i]])
    }
  }
  
  # Remove NA entries from the list
  simul_group_list[sapply(
    simul_group_list,function(x) all(is.na(x)))] <- NULL
  
  all_groups_df = as.data.frame(data.table::rbindlist(simul_group_list))
  
  return(all_groups_df)
  
}

# Importing the list of all origins and possible
# destinations, fitted using multinomial regression
library(nnet)
multinom_origin_destination =
  import("./02_output_data/14_multinom_origin_destination.RData")

# Remove NA entries
multinom_origin_destination[
  sapply(multinom_origin_destination,function(x) all(is.na(x)))] <- NULL

# -------------------------------------------------------------
# The function "reduce_multonom_for_simul_boarding" reduces
# the multinom fitting list by removing those stations for
# which there were no simulated data generated.

# Arguments
# simul_boarding_groups: "data.frame" / the output of "simulate_groups_all"
# multinom_origin_destination: "list" / the list of all origins and possible
# destinations, fitted using multinomial regression

# Output: "list" / a subset of "multinom_origin_destination"

reduce_multonom_for_simul_boarding = function(simul_boarding_groups,
                                              multinom_origin_destination) {
  l = length(multinom_origin_destination)
  multinom_origin_destination_new = multinom_origin_destination
  for (i in 1:l)
  {
    station_name = names(multinom_origin_destination)[i]
    station_boarding_groups = filter(simul_boarding_groups, StartStation==station_name)
    if (nrow(station_boarding_groups)==0)
    {multinom_origin_destination_new[[station_name]] <- NULL}}
  
  return(multinom_origin_destination_new)
}


# -------------------------------------------------------------
# The function "reduce_new_data_for_predict" excludes the superflous
# values of "StartHour","StartDay" from the data set "new_data".
# These values correspond to times when no bus operates. For a
# given data set, we construct its "intersection" with the level
# set of "list_entry_data" stations.
# The argument "list_entry_data" is a list. In our case it
# will be an output of the command "multinom".
# The function also convertes the factor column "SchoolHoliday"
# back to "logical".

# Arguments
# new_data: "data.frame" / with columns "StartHour","StartDay","SchoolHoliday"
# list_entry_data: "list" / levels of the output list of "multinom")

# Output: "data.frame"  model_destination$xlevels

reduce_new_data_for_predict = function(new_data, list_entry_data) {
  in_new_data = subset(
    new_data, new_data$StartHour %in% list_entry_data$StartHour
  )
  in_new_data = subset(
    in_new_data, in_new_data$StartDay %in% list_entry_data$StartDay
  )
  in_new_data$SchoolHoliday =
    as.logical(in_new_data$SchoolHoliday)
  'in_new_data=subset(
    in_new_data,in_new_data$SchoolHoliday%in%list_entry_data$SchoolHoliday)
  Why we should not check this?'
  return(in_new_data)
}

# ===========================================================


# -------------------------------------------------------------
# The function "predict_destinations" predicts the destinations for
# the boarding data "simul_boarding_groups" by means of multinomial
# regression. It uses the values of "StartHour","StartDay" and
# "SchoolHoliday" from the data set "new_data" and the list of all
# corresponding possible destinations from "multinom_origin_destination".
# For each entry of the second argument, we first run the multinomial
# regression command and filter its output using the function
# "reduce_new_data_for_predict" to construct the subset of
# parameters which are admisible for that station. Based on this
# parameter subset, we predict the probabilities of possible
# destinations. The procedure is combined with check of missing values.

# Arguments
# new_data: "data.frame" / with columns "StartHour","StartDay","SchoolHoliday"
# simul_boarding_groups: "data.frame" / the output of "simulate_groups_all"
# multinom_origin_destination: "list" / the list of all origins and possible
# destinations, fitted using multinomial regression

# Output: "data.frame" / the non-NA output consists of columns
# "StartHour", "StartDay", "SchoolHoliday", "SimulatedPassengers",
# "StartStation" and "Destinations"  

predict_destinations = function(new_data, simul_boarding_groups,
                                multinom_origin_destination) {
  l = length(multinom_origin_destination)
  station_boarding_groups = vector("list", length = l)
  station_boarding_groups_destinations = vector("list", length = l)
  for (i in 1:l)
  {
    station_name = names(multinom_origin_destination)[i]
    station_boarding_groups[[i]] = filter(simul_boarding_groups, StartStation==station_name)
    station_boarding_groups_destinations[[i]] = station_boarding_groups[[i]]
    
    station_boarding_groups_destinations[[i]]$Destination = ""
    
    for (row_idx in 1:nrow(station_boarding_groups[[i]])) {
      new_data_current_row = station_boarding_groups[[i]] %>%
        select(StartHour,StartDay,SchoolHoliday) %>%
        filter(StartHour==station_boarding_groups[[i]]$StartHour[row_idx],
               StartDay==station_boarding_groups[[i]]$StartDay[row_idx],
               SchoolHoliday==station_boarding_groups[[i]]$SchoolHoliday[row_idx])
      
      new_data_for_destination = reduce_new_data_for_predict(
        new_data, multinom_origin_destination[[i]]$xlevels)
      new_data_for_destination = reduce_new_data_for_model(
        new_data_for_destination, new_data_current_row)
      
      probability_destination = predict(
        multinom_origin_destination[[i]],
        type = "probs",
        newdata = new_data_for_destination
      )
      if (is.null(colnames(probability_destination))) {
        probability_destination=t(as.matrix(probability_destination))
      } # Added on 2021.12.22 for the case of a "numeric" vector
      
      if (!is.null(colnames(probability_destination))) {
        destination_vector = vector("character", length = nrow(probability_destination))
        for (j in 1:nrow(probability_destination))
        {
          destination_vector[j]=sample(
            colnames(probability_destination),
            prob = probability_destination[j, ],
            replace = TRUE,
            size = 1
          )
        }
      }        
      
      station_boarding_groups_destinations[[i]]$Destination[row_idx] =
        sample(destination_vector,size = 1)
      
    }
    
  }
  all_boarding_destinations = as.data.frame(
    data.table::rbindlist(station_boarding_groups_destinations))
  all_boarding_destinations = all_boarding_destinations[ ,c(1:5,7,6)]
  return(all_boarding_destinations)
}

# REMARK
# The loop over j in the function "predict_destinations" can be replaced by
# the following "sapply" or "future.apply::future_sapply" (better) command:
# 
# destination_list[[i]]$Destinations[1:nrow(probability_destination)]=
#   future_sapply(destination_list[[i]], function(index) sample(
#     colnames(probability_destination),
#     prob =probability_destination[index,],
#     replace = TRUE,size = 1))
# 
# I observed that this is no time-saver for a small set of i.


# ================================================================
# ================================================================
# The time consuming part of the simulations (3.5 min per simulation)
# It takes about 2 hours to simulate 30 data frames (30 weeks)
week_number = 30 # enter an integer between 1 and 99
# Initialize a list of empty data frames
simulated_ride_requests_list = vector("list", length = week_number)
# Column order for the prosperious data frames
col_order <- c("StartDay","StartHour","StartMin","SchoolHoliday","StartStation",
               "StartDistrict","StartStation_lat","StartStation_lon","Destination",
               "DestinationDistrict","Destination_lat","Destination_lon","Passengers")
# Another auxiliary vector to sort by weekdays
weekdays_order <- c("Monday", "Tuesday", "Wednesday",
                    "Thursday", "Friday", "Saturday", "Sunday")


for (number_of_simulations in 1:week_number) {
  # Predicting intensities
  # Temporarily suppress warnings
  defaultW <- getOption("warn")
  options(warn = -1)
  # Intensity prediction
  pred_boarding_intensities =
    predict_pois_intensities(new_data, origin_model_list)
  # Turn warnings back on
  options(warn = defaultW)
  
  # Exporting the data set "pred_boarding_intensities" (run on demand)
  # save(pred_boarding_intensities,
  #      file = "./02_output_data/16_pred_boarding_intensities.RData")
  
  # Upscaling the intensities by a factor 3.5 to compensate the difference
  # between the numbers of WSW-LAN users and all passengers
  pred_boarding_intensities = lapply(
    pred_boarding_intensities,
    function(x) {
      x$LambdaIntesity=3.5*x$LambdaIntesity
      return(x)
    }
  )
  # Simulating all group boardings
  # Temporarily suppress warnings
  defaultW <- getOption("warn")
  options(warn = -1)
  # Simulating boarding groups (normally completes in 1-2 min, after upscale > 16 min)
  simul_boarding_groups = simulate_groups_all(pred_boarding_intensities, group_sizes)
  # Turn warnings back on
  options(warn = defaultW)
  
  # Reduce the multinom fitting list by applying the previous function
  # Added on 2021.12.22 to remove new NA's
  multinom_origin_destination_reduced =
    reduce_multonom_for_simul_boarding(
      simul_boarding_groups, multinom_origin_destination)
  
  # Simulating destinations (takes about 1.5 min to complete)
  simulated_ride_requests <-
    predict_destinations(
      new_data, simul_boarding_groups,
      multinom_origin_destination_reduced
    )
  
  # Inserting the coordinates of start stations
  simulated_ride_requests$StartStation_lat <- ride_hailing_stations$stop_lat[
    match(simulated_ride_requests$StartStation, ride_hailing_stations$stop_name)
  ]
  simulated_ride_requests$StartStation_lon <- ride_hailing_stations$stop_lon[
    match(simulated_ride_requests$StartStation, ride_hailing_stations$stop_name)
  ]
  
  simulated_ride_requests$StartStation_lat <-
    as.numeric(simulated_ride_requests$StartStation_lat)
  simulated_ride_requests$StartStation_lon <-
    as.numeric(simulated_ride_requests$StartStation_lon)
  
  # Inserting the coordinates of destinations
  simulated_ride_requests$Destination_lat <- ride_hailing_stations$stop_lat[
    match(simulated_ride_requests$Destination, ride_hailing_stations$stop_name)
  ]
  simulated_ride_requests$Destination_lon <- ride_hailing_stations$stop_lon[
    match(simulated_ride_requests$Destination, ride_hailing_stations$stop_name)
  ]
  
  simulated_ride_requests$Destination_lat <-
    as.numeric(simulated_ride_requests$Destination_lat)
  simulated_ride_requests$Destination_lon <-
    as.numeric(simulated_ride_requests$Destination_lon)
  
  # Inserting the districts of origins and destinations
  simulated_ride_requests$StartDistrict <- ride_hailing_stations$district[
    match(simulated_ride_requests$StartStation, ride_hailing_stations$stop_name)
  ]
  simulated_ride_requests$DestinationDistrict <- ride_hailing_stations$district[
    match(simulated_ride_requests$Destination, ride_hailing_stations$stop_name)
  ]
  
  # Reordering columns
  simulated_ride_requests <- simulated_ride_requests[, col_order]
  
  # Sorting by multiple columns
  
  simulated_ride_requests = simulated_ride_requests[order(
    match(simulated_ride_requests$StartDay, weekdays_order),
    simulated_ride_requests$StartHour, simulated_ride_requests$StartMin,
    simulated_ride_requests$SchoolHoliday,simulated_ride_requests$StartStation,
    simulated_ride_requests$Destination,simulated_ride_requests$Passengers), ]
  # View(simulated_ride_requests)
  
  # Exporting the data set "simulated_ride_requests" as "csv"
  export(simulated_ride_requests,
         file = paste0("./02_output_data/17_simulated_ride_requests_",
                       sprintf("%02d", number_of_simulations),".csv")
  )
  # Use the following export command instead of the
  # previous one to add the time to the file name
  # export(simulated_ride_requests,
  #        file = paste0(
  #          "./02_output_data/17_simulated_ride_requests_",
  #          gsub(":", "", unlist(strsplit(as.character(Sys.time())," "))[2]),
  #          ".csv"
  #        )
  # )
  simulated_ride_requests$Week_No = number_of_simulations
  simulated_ride_requests_list[[number_of_simulations]] = simulated_ride_requests
}


# The end of the time consuming part of the simulations
# =====================================================================
simulated_ride_requests_all_weeks = as.data.frame(
  data.table::rbindlist(simulated_ride_requests_list))
# Note that the last data frame is sorted by multiple columns
# according to the following preference sceme:
# Week_No, StartDay, StartHour, StartMin, SchoolHoliday,
# StartStation, Destination, Passengers

# Exporting the data set "simulated_ride_requests_all_weeks" as "csv"
export(simulated_ride_requests_all_weeks,
       file = paste0("./02_output_data/18_simulated_ride_requests_all_weeks.csv")
)

# Creating a simplified data set for maps by merging
# the rides differing only by min or passenger number
simulated_hourly_rides_singly = simulated_ride_requests_list[[1]] %>%
  dplyr::group_by(
    StartHour, StartDay, SchoolHoliday, StartStation,StartDistrict,
    StartStation_lat, StartStation_lon, Destination, DestinationDistrict,
    Destination_lat, Destination_lon
  ) %>%
  dplyr::summarize(Passengers = sum(Passengers)) # the new "Passengers" column
# The new column is the total number of passengers for the specified parameters
simulated_hourly_rides_singly = ungroup(simulated_hourly_rides_singly)

simulated_hourly_rides_singly = simulated_hourly_rides_singly[order(
  simulated_hourly_rides_singly$StartHour,
  match(simulated_hourly_rides_singly$StartDay, weekdays_order),
  simulated_hourly_rides_singly$SchoolHoliday,simulated_hourly_rides_singly$StartStation,
  simulated_hourly_rides_singly$Destination,simulated_hourly_rides_singly$Passengers), ]
# View(simulated_hourly_rides_singly)

# Exporting the data set "simulated_hourly_rides_singly" as "csv"
export(simulated_hourly_rides_singly,
       file = "./02_output_data/19_simulated_hourly_rides_singly.csv"
)

rm(list = ls()) # Clear environment
cat("\014") # Clear console (Ctrl+L)
# Tip: perform the two above commands and then
# try new simulation parameters, i.e.,
# start_hour,s_holiday and wk_day
pacman::p_unload(all) # Clears all add-on packages
