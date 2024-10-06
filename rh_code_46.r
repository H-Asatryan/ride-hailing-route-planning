# Visualizing bar charts for the data set WSWLAN rides
# The hour and week profiles from the data set "WLAN_trips_by_date"
# are visualized by means of bar charts
# If we have already created it, we could begin with this file!

# Importing the data set of the WSWLAN rides:
WLAN_trips_by_date=
  rio::import("./02_output_data/10c_WLAN_trips_by_date.RData"
  )

# Merging all lists corresponding to different
# dates into a single data frame
WLAN_trips_all_dates_df = data.table::rbindlist(WLAN_trips_by_date)

# Loading multiple packages
pacman::p_load(dplyr, ggplot2)

# Hourly trips in the first half of 2019
WLAN_trip_numbers_by_hour = WLAN_trips_all_dates_df %>%
  group_by(StartHour) %>% 
  summarize(Rides=sum(Passengers))
colnames(WLAN_trip_numbers_by_hour)[1]="Hour"

hour_order <- as.character(0:23)
# To choose HTML colors, visit https://html-color-codes.info/

ggplot(data = WLAN_trip_numbers_by_hour) +
  geom_col(aes(x = Hour, y = Rides, fill = Hour)) +
  scale_fill_manual(breaks=hour_order,
                    values = c(rep("#8A084B",4),
                               rep("#08298A",18),
                               rep("#8A084B",2)))+
  scale_x_discrete(limits = hour_order) + # use our hour order
  labs(title = "Hourly Rides in the First Half of 2019",
       x = "Hour", y = "Number of Rides") +
  theme(plot.title=element_text(hjust=0.5), # centered title
        legend.text = element_text(size = 15), # title font size
        legend.position = "none", # no legend
        axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
        ) 

ggsave("./02_output_data/charts/rides_hourly.pdf",
       height = 9, width = 16)
# Creating reducing the list "WLAN_trips_by_date" 
# to show only the numbers of daily rides

# -----------------------------------------------------------
# The function "combine_weekday_and_rides" processes a data frame
# having columns "StartDay" and "Passengers", in our case it is an
#  entry of "WLAN_trips_by_date". I.e, "StartDay" is a constant
# vector and "Passengers" is an integer vector
# As an output we get the weekday and the number of passengers
# The output is a data frame consisting of only 1 row
# 
# Arguments
# origin_data_frame: "data.frame"
# 
# Output: "data.frame"

combine_weekday_and_rides = function(origin_data_frame) {
  daily_rides = dplyr::group_by(origin_data_frame, StartDay)
  daily_rides = dplyr::summarize(daily_rides, Trips=sum(Passengers))
  colnames(daily_rides)[1] = "Weekday"
  return(daily_rides)
}

# Listing dates as names, we construct entries as one row data frames
# containing the weekday and number of trips
WLAN_trip_numbers_by_date = lapply(WLAN_trips_by_date,combine_weekday_and_rides)
WLAN_trip_numbers_by_date_df = data.table::rbindlist(WLAN_trip_numbers_by_date)
WLAN_trip_numbers_by_date_df = cbind.data.frame(
  names(WLAN_trip_numbers_by_date),
  WLAN_trip_numbers_by_date_df
)
colnames(WLAN_trip_numbers_by_date_df)[1] = "Date"

# Now we sum up the trips per weekday
WLAN_trip_numbers_by_weekday = WLAN_trip_numbers_by_date_df %>%
  group_by(Weekday) %>% 
  summarize(Rides = sum(Trips))
# View(WLAN_trip_numbers_by_weekday)

# Sorting by weekday
weekdays_order <- c("Monday", "Tuesday", "Wednesday",
                    "Thursday", "Friday", "Saturday", "Sunday")
WLAN_trip_numbers_by_weekday = WLAN_trip_numbers_by_weekday[
  order(match(WLAN_trip_numbers_by_weekday$Weekday, weekdays_order)), ]

# weekday_alph_to_norm_idx =
#   match(weekdays_order,weekdays_order[order(weekdays_order)])
# i.e., weekdays_order[order(weekdays_order)][weekday_alph_to_norm_idx] == weekdays_order

# View(WLAN_trip_numbers_by_date_df)
# library(paletteer) # Comprehensive Collection of Color Palettes
# fill_colors = paletteer::paletteer_d("ggthemes::stata_economist")[2:8] # choice
# fill_colors = paletteer_d("ggthemes::Nuriel_Stone")[1:7] # OK
# fill_colors = paletteer_d("ggsci::default_aaas")[1:7] # much variation
# fill_colors = paletteer_d("ggsci::dark_uchicago")[1:7] # too dark
# fill_colors = paletteer_d("ggsci::hallmarks_dark_cosmic")[1:7] # too dark
# fill_colors = paletteer_c("ggthemes::Green-Blue Diverging", 7)
# fill_colors = paletteer_c("ggthemes::Classic Gray", 7)
# fill_colors = as.character(fill_colors)
# Gray colors arranged by darkness

# Light gray
# fill_colors = rep("gray",7)
# fill_colors = rep("darkgray",7)
# fill_colors = rep("azure4",7) # choice 7

# Dark gray
# fill_colors = rep("#8A8B8C",7) # choice 6
# fill_colors = rep("#808182",7)
# fill_colors = rep("#808080",7) # choice 5
# fill_colors = rep("#767778",7) # choice 4
# fill_colors = rep("#6C6D6E",7) # choice 3
# fill_colors = rep("dimgray",7) # choice 2 (code: #696969)
# fill_colors = rep("#626364",7) # choice 1
# fill_colors = rep("#58595A",7)
# fill_colors = rep("#444546",7)
# fill_colors = rep("#3A3B3C",7)

# Dark blue
fill_colors = rep("#08298A",7)

ggplot(data = WLAN_trip_numbers_by_weekday) +
  geom_col(aes(x = Weekday, y = Rides, fill = Weekday), width=0.77) +
  scale_fill_manual(values = fill_colors)+
  # scale_fill_grey()+
  scale_x_discrete(limits = weekdays_order) + # use our weekday order
  labs(title = "Rides in the First Half of 2019",
       x = "Weekday", y = "Number of Rides") +
  # scale_fill_discrete(name = "Weekday", labels = weekdays_order)+
  theme(plot.title=element_text(hjust=0.5), # centered title
        legend.position = "none", # no legend
        legend.text = element_text(size = 15), # title font size
        axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.text = element_text(size = 10.5)) 

ggsave("./02_output_data/charts/rides_per_weekday.pdf",
       height = 9, width = 16)

rm(list = ls()) # Clear environment
cat("\014") # Clear console (Ctrl+L)

pacman::p_unload(all) # Clears all add-on packages
