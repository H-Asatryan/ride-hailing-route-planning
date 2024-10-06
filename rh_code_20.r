# The purpose of this file is the extraction of the
# Wuppertal districts with Ride Hailing services.
# First we display all the districts with their codes on a
# map, which allows to read the codes of necessary districts
# Then we create the map of the Ride Hailing area

# Reading Wuppertal districts
library(rgdal)
districts_all <- readOGR(dsn="./01_input_data/Quartiere_EPSG25832_SHAPE/.",
                     layer="Quartiere_EPSG25832_SHAPE",
                     encoding = "UTF-8")
# The data set is available at
# https://www.offenedaten-wuppertal.de/dataset/quartiere-wuppertal
# Direct link:
# http://daten.wuppertal.de/Infrastruktur_Bauen_Wohnen/Quartiere_EPSG25832_SHAPE.zip

districts_all <- spTransform(districts_all, CRS("+proj=longlat +ellps=GRS80"))

library(leaflet)
districts_map <- leaflet()
districts_map <- addTiles(districts_map)
# Centering Wuppertal Hbf
districts_map = setView(
  districts_map, lat = 51.25486, lng = 7.151039, zoom = 12)

districts_map_with_district_numbers <- addPolygons(districts_map,
                                                   data = districts_all, color = "#555555",
                                                   weight = 1, smoothFactor = 0.5,
                                                   opacity = 1.0, fillOpacity = 0.5,
                                                   highlightOptions = highlightOptions(
                                                     color = "white", weight = 2, bringToFront = FALSE),
                                                   label = as.character(districts_all$QUARTIER),
                                                   group = "Quartiersgrenzen"
)

districts_map_with_district_names <- addPolygons(districts_map,
                                                   data = districts_all, color = "#F3F781",
                                                   weight = 1, smoothFactor = 0.5,
                                                   opacity = 1, fillOpacity = 0.45,
                                                   highlightOptions = highlightOptions(
                                                     color = "white", weight = 2, bringToFront = FALSE),
                                                   label = as.character(districts_all$NAME),
                                                   group = "Quartiersgrenzen"
)

districts_map_with_district_numbers
districts_map_with_district_names

library(htmlwidgets)
setwd("./02_output_data")
saveWidget(widget = districts_map_with_district_numbers,
           file = "04a_Wuppertal_districts_map_with_district_no.html")
saveWidget(widget = districts_map_with_district_names,
           file = "04b_Wuppertal_districts_map_with_district_names.html")

# Selecting districts with Ride Hailing services
subdistrict_codes_elberfeld <- c("00","01","02","03","04","05")
subdistrict_codes_elberfeld_west <- c("10","11","12","13","14","15")
subdistrict_codes_uellendahl_katernberg <- c("20","21","22","23","24","25","26")

ride_hailing_districts_map_codes <- c(
  subdistrict_codes_elberfeld,
  subdistrict_codes_elberfeld_west,
  subdistrict_codes_uellendahl_katernberg
  )

ride_hailing_districts = districts_all
ride_hailing_districts@polygons = subset(
  ride_hailing_districts@polygons,
  ride_hailing_districts$QUARTIER %in%
    ride_hailing_districts_map_codes)
ride_hailing_districts@data = subset(
  ride_hailing_districts@data,
  ride_hailing_districts$QUARTIER %in%
    ride_hailing_districts_map_codes)

ride_hailing_districts_map <- leaflet()
ride_hailing_districts_map <- addTiles(ride_hailing_districts_map)
# Centering Wuppertal Hbf
ride_hailing_districts_map = setView(
  ride_hailing_districts_map, lat = 51.25486, lng = 7.151039, zoom = 12)

ride_hailing_districts_map <- addPolygons(ride_hailing_districts_map,
                                          data = ride_hailing_districts, color = "#555555",
                                          weight = 1, smoothFactor = 0.5,
                                          opacity = 1.0, fillOpacity = 0.5,
                                          highlightOptions = highlightOptions(
                                            color = "white", weight = 2, bringToFront = FALSE),
                                          # Uncomment one of the following label commands.
                                          # Use this to show the district names on the map
                                          label = as.character(ride_hailing_districts$NAME),
                                          # Use this to show the district numbers on the map
                                          # label = as.character(ride_hailing_districts$QUARTIER),
                                          group = "Quartiersgrenzen"
)

ride_hailing_districts_map_and_Wuppertal <- addPolygons(districts_map_with_district_names,
                                                        data = ride_hailing_districts, color = "#555555",
                                                        weight = 1, smoothFactor = 0.5,
                                                        opacity = 1.0, fillOpacity = 0.2,
                                                        highlightOptions = highlightOptions(
                                                          color = "white", weight = 2, bringToFront = FALSE),
                                                        # Uncomment one of the following label commands.
                                                        # Use this to show the district names on the map
                                                        label = as.character(ride_hailing_districts$NAME),
                                                        # Use this to show the district numbers on the map
                                                        # label = as.character(ride_hailing_districts$QUARTIER),
                                                        group = "Quartiersgrenzen"
)


ride_hailing_districts_map
ride_hailing_districts_map_and_Wuppertal

saveWidget(widget = ride_hailing_districts_map, file = "05a_ride_hailing_districts_map.html")

saveWidget(widget = ride_hailing_districts_map_and_Wuppertal,
           file = "05b_Wuppertal_and_ride_hailing_districts_map.html")
