# Visualization of the simulation on a map
# Making a map based on the data set "simulated_hourly_rides_singly".
# If we have already exported it, we could begin with this file!

# Importing the data set of a predicted simulation with destinations:
simulated_hourly_rides_singly=
  rio::import("./02_output_data/19_simulated_hourly_rides_singly.csv"
  )

# Loading multiple packages
pacman::p_load(dplyr,leaflet,
               htmltools) # Command "HTML" for multi-line labels

# Processing school holiday information
holiday_info <- c("school time", "school holidays")
holiday_info_v2 <- c("school_time", "school_holidays")
weekdays_order <- c(
  "Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday", "Sunday"
)

# -----------------------------------------------------------
# The function "trip_scenario_map" generates the scenario map
# corresponding to the data set "simulated_hourly_rides_singly"
# and parameters start_hour, week_day and school_holiday
# The argument "simulated_trips" is a data frame; in our case
# it will be an output of the command "dlply".
# 
# Arguments
# simulated_trips: "data.frame"
# start_hour: "numeric"
# week_day: "character"
# school_holiday: "logical"
# 
# Output: "leaflet"    "htmlwidget"

trip_scenario_map = function(simulated_trips, start_hour, week_day, school_holiday) {
  start_hour_txt = as.character(start_hour)

  trip_scenario_map <- leaflet()
  trip_scenario_map <- addTiles(trip_scenario_map)
  # centering Wuppertal Hbf
  trip_scenario_map = setView(
    trip_scenario_map,
    lat = 51.25486, lng = 7.151039, zoom = 12
  )
  # 20 gives the maximum zoom here; larger zoom factors give the same result

  # One can also use Nominatim (https://nominatim.openstreetmap.org/) to get
  # the latitude/longitude of the desired place

  boarding_scenario = filter(simulated_trips, StartHour == start_hour
  & SchoolHoliday == school_holiday
  & StartDay == week_day)

  boarding_scenario_grouped = boarding_scenario %>%
    dplyr::group_by(
      StartHour, StartDay, SchoolHoliday, StartStation,
      StartStation_lat, StartStation_lon
    ) %>%
    dplyr::summarize(destination_status = paste(Passengers, Destination,
      sep = ' to ', collapse = ", "
    )) # A new column for the groupped values

  # Marking destinations where nobody gets on the bus
  empty_markers = filter(
    boarding_scenario,
    !Destination %in%
      boarding_scenario$StartStation
  )

  empty_markers = select(
    empty_markers, Destination, Destination_lat, Destination_lon
  )
  empty_markers = distinct(empty_markers)

  if (nrow(empty_markers)>0) {
    trip_scenario_map = addCircleMarkers(
      trip_scenario_map,
      lat = empty_markers$Destination_lat,
      lng = empty_markers$Destination_lon,
      radius = 0.2,
      label = empty_markers$Destination,
      color = "grey"
    )  
  }
  
  for (station in boarding_scenario_grouped$StartStation) {
    passenger_num = sum(boarding_scenario$Passengers[
      boarding_scenario$StartStation == station
    ])
    start_lat = boarding_scenario_grouped$StartStation_lat[
      boarding_scenario_grouped$StartStation == station
    ]
    start_lon = boarding_scenario_grouped$StartStation_lon[
      boarding_scenario_grouped$StartStation == station
    ]
    destinations_for_station = filter(
      boarding_scenario,
      StartStation == station
    )
    for (i in 1:nrow(destinations_for_station)) {
      destin_lat = destinations_for_station$Destination_lat[i]
      destin_lon = destinations_for_station$Destination_lon[i]
      passengers_to_dest = destinations_for_station$Passengers[i]
      trip_scenario_map <- addPolylines(
        trip_scenario_map,
        lat = c(start_lat, destin_lat),
        lng = c(start_lon, destin_lon),
        weight = passengers_to_dest,
        color = "darkblue",
        group = paste(station)
      )
    }
    trip_scenario_map = addCircleMarkers(
      trip_scenario_map,
      lat = start_lat,
      lng = start_lon,
      radius = sqrt(passenger_num / pi),
      label = station,
      popup = HTML(paste0(
        "<style> div.leaflet-popup-content {
    max-width: 700px;
    min-width: 300px;
    width:auto;
    max-height: 150px;
    height:auto;
    overflow-y: auto;
    } </style>",
        "<b>", station, '</b><br/>',
        ' on ', week_day, ' at ',
        start_hour_txt, ':00 <br/>',
        holiday_info[school_holiday + 1], '<br/><br/>',
        '<b>', passenger_num, ' Passengers:</b><br/>',
        boarding_scenario_grouped$destination_status[
          boarding_scenario_grouped$StartStation == station
        ]
      )),
      color = "blue"
    )
  }
  trip_scenario_map <- addMeasure(
    trip_scenario_map,
    primaryLengthUnit = "kilometers",
    primaryAreaUnit = "sqmeters", activeColor = "#3D535D",
    completedColor = "#006400"
  )
  trip_scenario_map = addLayersControl(
    trip_scenario_map,
    baseGroups = c(
      "Start unselected",
      boarding_scenario_grouped$StartStation
    ),
    options = layersControlOptions(collapsed = FALSE)
  )
  trip_scenario_map = addEasyButton(
    trip_scenario_map,
    easyButton(
      icon = shiny::icon("resize-small", lib = "glyphicon"),
      title= "Reset Zoom",
      onClick = JS(
        c("function(btn, map) {map.setView(new L.LatLng(51.25486, 7.151039), 12);}")
      )
    )
  )
  return(trip_scenario_map)
}

library(htmlwidgets) # Export as HTML, use JavaScript (command "JS")
setwd("./02_output_data/scenario_maps")

# Generating all scenario maps (completes in 3-4 min; direction lines take 1-2 min)
for (start_hour in 0:23) {
  for (week_day in weekdays_order) {
    for (school_holiday in c(FALSE, TRUE)) {
      saveWidget(
        widget = trip_scenario_map(
          simulated_hourly_rides_singly, start_hour, week_day, school_holiday
        ),
        file = paste0(
          "trip_scenario_", sprintf("%02d", start_hour), "_00_",
          week_day, "_", holiday_info_v2[school_holiday + 1], ".html"
        ),
        title = paste0(
          "Visualization: ", week_day, " at ",
          sprintf("%02d", start_hour), ":00, ",
          holiday_info[school_holiday + 1]
        )
      )
    }
  }
}

# Motion map example
setwd("../")
ride_hailing_stations=rio::import(
  "./06_ride_hailing_stations.csv")

stations_minmax=filter(
  ride_hailing_stations,stop_lat+stop_lon==min(stop_lat+stop_lon)
  | stop_lat+stop_lon==max(stop_lat+stop_lon))

common_difference_lat=(stations_minmax[1,2]-stations_minmax[2,2])/20
common_difference_lon=(stations_minmax[1,3]-stations_minmax[2,3])/20

motion_data=cbind(
  rep(paste0(stations_minmax[2,1]," to ",stations_minmax[1,1]),21),
  seq(stations_minmax[2,2],stations_minmax[1,2],by=common_difference_lat),
  seq(stations_minmax[2,3],stations_minmax[1,3],by=common_difference_lon)
)

motion_data=as.data.frame(motion_data,stringsAsFactors = FALSE)
colnames(motion_data)=c("Path","stop_lat", "stop_lon")

motion_data$stop_lat=as.numeric(motion_data$stop_lat)
motion_data$stop_lon=as.numeric(motion_data$stop_lon)

library(sp)
coordinates(motion_data) <- c("stop_lon", "stop_lat")
# View(motion_data)

library(sf)
motion_sf_data = st_as_sf(motion_data, coords = c(1,2), agr = "constant")
motion_sf_data <- st_cast(motion_sf_data, "POINT")
motion_sf_data$time = as.POSIXct(seq.POSIXt(
  Sys.time() - 1000, Sys.time(), length.out = nrow(motion_sf_data)))
# View(motion_sf_data)

library(leaflet.extras2)

example_motion_map_simple = leaflet() %>%
  addTiles() %>%
  setView(lat = 51.25486, lng = 7.151039, zoom = 12) %>%
  addPlayback(data = motion_sf_data,
              options = playbackOptions(radius = 3),
              pathOpts = pathOptions(weight = 5))

example_motion_map = trip_scenario_map(
  simulated_hourly_rides_singly, 10, "Monday", TRUE
)

example_motion_map=addPlayback(
  example_motion_map,data = motion_sf_data,
  options = playbackOptions(radius = 3),
  pathOpts = pathOptions(weight = 5)
)

setwd("./scenario_maps")
saveWidget(widget = example_motion_map_simple,
           file = "example_motion_map_simple.html")
saveWidget(widget = example_motion_map,
           file = "example_motion_map_10_00_Monday_school_holidays.html",
           title = "Visualization: Monday at 10:00, school holidays")

rm(list = ls()) # Clear environment
cat("\014") # Clear console (Ctrl+L)
# Tip: perform the two above commands and then
# try new simulation parameters, i.e.,
# start_hour,school_holiday and week_day
pacman::p_unload(all) # Clears all add-on packages
