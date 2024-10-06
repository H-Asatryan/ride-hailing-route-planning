# Visualization of the WSWLAN rides on a map
# Making a map based on the data set "WLAN_trips_by_date".
# If we have already created it, we could begin with this file!

# Importing the data set of the WSWLAN rides:
WLAN_trips_by_date=
  rio::import("./02_output_data/10c_WLAN_trips_by_date.RData"
  )

# Loading multiple packages
pacman::p_load(dplyr,leaflet,
               htmltools) # Command "HTML" for multi-line labels

# -----------------------------------------------------------
# The function "ride_scenario_map" generates the scenario map
# corresponding to the list "WLAN_trips_by_date"
# and to the parameters ride_date, ride_hour
# The argument "ride_list" is a list of data frames; in our case
# it will be an output of the command "dlply".
# 
# Arguments
# ride_list_entry: "data.frame"
# ride_date: "character"
# ride_hour: "integer"
# 
# Output: "leaflet"    "htmlwidget"

ride_scenario_map = function(ride_list, ride_date, ride_hour) {
  start_hour_txt = as.character(ride_hour)

  ride_scenario_map <- leaflet()
  ride_scenario_map <- addTiles(ride_scenario_map)
  # centering Wuppertal Hbf
  ride_scenario_map = setView(
    ride_scenario_map,
    lat = 51.25486, lng = 7.151039, zoom = 12
  )
  # 20 gives the maximum zoom here; larger zoom factors give the same result

  # One can also use Nominatim (https://nominatim.openstreetmap.org/) to get
  # the latitude/longitude of the desired place

  boarding_scenario = filter(ride_list[[ride_date]], StartHour == ride_hour)

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
    ride_scenario_map = addCircleMarkers(
      ride_scenario_map,
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
      ride_scenario_map <- addPolylines(
        ride_scenario_map,
        lat = c(start_lat, destin_lat),
        lng = c(start_lon, destin_lon),
        weight = passengers_to_dest,
        color = "darkblue",
        group = paste(station)
      )
    }
    ride_scenario_map = addCircleMarkers(
      ride_scenario_map,
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
        ' on ', ride_date, ' at ',
        start_hour_txt,":00", '<br/><br/>',
        '<b>', passenger_num, ' Passengers:</b><br/>',
        boarding_scenario_grouped$destination_status[
          boarding_scenario_grouped$StartStation == station
        ]
      )),
      color = "blue"
    )
  }
  ride_scenario_map <- addMeasure(
    ride_scenario_map,
    primaryLengthUnit = "kilometers",
    primaryAreaUnit = "sqmeters", activeColor = "#3D535D",
    completedColor = "#006400"
  )
  ride_scenario_map = addLayersControl(
    ride_scenario_map,
    baseGroups = c(
      "Start unselected",
      boarding_scenario_grouped$StartStation
    ),
    options = layersControlOptions(collapsed = FALSE)
  )
  ride_scenario_map = addEasyButton(
    ride_scenario_map,
    easyButton(
      icon = shiny::icon("resize-small", lib = "glyphicon"),
      title= "Reset Zoom",
      onClick = JS(
        c("function(btn, map) {map.setView(new L.LatLng(51.25486, 7.151039), 12);}")
      )
    )
  )
  return(ride_scenario_map)
}

library(htmlwidgets) # Export as HTML, use JavaScript (command "JS")
setwd("./02_output_data/scenario_maps_original_data")

# Select 10 random dates from the WSWLAN ride list
sample_dates = sort(sample(names(WLAN_trips_by_date),size = 10))
# 

# Generating all scenario maps (completes in 1-2 min)
for (ride_date in sample_dates) {
  for (ride_hour in 0:23) {
    saveWidget(
      widget = ride_scenario_map(
        WLAN_trips_by_date, ride_date, ride_hour
      ),
      file = paste0(
        "ride_scenario_", ride_date, "_", sprintf("%02d", ride_hour),
        "00",".html"
      ),
      title = paste0(
        "Visualization: ", ride_date, " at ",
        sprintf("%02d", ride_hour), ":00"
      )
    )
  }
}  

rm(list = ls()) # Clear environment
cat("\014") # Clear console (Ctrl+L)

pacman::p_unload(all) # Clears all add-on packages
