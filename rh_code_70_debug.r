week_number = 1

# 1h 40 min
system.time(for (number_of_simulations in 1:week_number) {
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
)
