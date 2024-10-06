# The purpose of this file is the visualization of the data set
# "WLAN_TripID_ride_hailing_temp" corresponding to the Wuppertal
# districts with Ride Hailing services, generated in the previous
# script. Moreover, we will reveal some outliers and remove them.
# We will export the new data set as "WLAN_TripID_ride_hailing"

# Reading the data sets
library(dplyr)
library(lubridate)
# Importing the enhanced data set "WLAN_TripID_ride_hailing.RData"
WLAN_TripID_ride_hailing_temp =
  rio::import("./02_output_data/07_WLAN_TripID_ride_hailing_temp.RData")
# CAUTION: Importing .xlsx files via "rio::import" or "readxl::read_excel"
# both freeze RStudio, maybe because of the incompatibility of .xlsx files
# with the package "lubridate" Do not use these commands!
# ==========================================================

# Outlier detection
rides_by_date = WLAN_TripID_ride_hailing_temp %>%
  count(StartDate,name="total_rides")
summary(rides_by_date$total_rides)

# tip to control hidden parameters
.controlList=list() # Hidden list of control values
.controlList$min_rides=250
# Outliers etc

less_than_min_rides=filter(rides_by_date,total_rides<.controlList$min_rides)
rm(.controlList) # remove the (invisible) object ".controlList"
nrow(less_than_min_rides)
View(less_than_min_rides)
# The lines above show that there were only 5 days
# when the ride number was less than min_rides==250
# The ride number for these days was less than 41 !!!
# Hence, these dates are outliers, we will remove them.

outlier_dates=select(less_than_min_rides,StartDate)

WLAN_TripID_ride_hailing <-
  WLAN_TripID_ride_hailing_temp[
    !WLAN_TripID_ride_hailing_temp$StartDate %in% outlier_dates$StartDate,]
View(WLAN_TripID_ride_hailing)

# Exporting the polished data
rio::export(WLAN_TripID_ride_hailing,
       "./02_output_data/08_WLAN_TripID_ride_hailing.RData")

# Count rides by group
weekdays_order <- c("Monday", "Tuesday", "Wednesday",
                    "Thursday", "Friday", "Saturday", "Sunday")

ride_summary = WLAN_TripID_ride_hailing %>%
  count(StartDate,StartDay,StartHour,SchoolHoliday,name="rides")

ride_summary$StartHour = as.integer(ride_summary$StartHour)

# Strange errors with "summarize":
ride_summary_brief = ride_summary %>%
  group_by(StartDay,StartHour,SchoolHoliday) %>%
  summarize(AverageRides = mean(rides),
            MinRides = min(rides),
            MaxRides = max(rides))
# dplyr seems to have conflicts!

ride_summary_brief = ride_summary_brief[order(
  match(ride_summary_brief$StartDay, weekdays_order),
  ride_summary_brief$StartHour,ride_summary_brief$SchoolHoliday), ]

# View(ride_summary_brief)

# Exporting data
rio::export(ride_summary_brief,
            "./02_output_data/09_ride_summary_brief.csv")

# Visualizing total rides
par(mfrow=c(1,3)) # Set up a multiple plot grid

plot(rides_by_date$StartDate,rides_by_date$total_rides,
     col = "purple", pch=20, type="b",
     main = paste0("All rides"),
     xlab = "Ride date",
     ylab = "Number of rides")

school_holiday_dates = WLAN_TripID_ride_hailing %>%
  filter(SchoolHoliday==TRUE) %>%
  select(StartDate) %>%
  distinct()

rides_by_date_filtered=list()
rides_by_date_filtered[[1]]=filter(
  rides_by_date,
  !rides_by_date$StartDate %in% school_holiday_dates$StartDate
)
rides_by_date_filtered[[2]]=filter(
  rides_by_date,
  rides_by_date$StartDate %in% school_holiday_dates$StartDate
)

for (test_sch_hol in c(FALSE,TRUE)) {
  rides_tmp=rides_by_date_filtered[[test_sch_hol+1]]
  plot(rides_tmp$StartDate,rides_tmp$total_rides,
       col = "purple", pch=20, type="b",
       main = paste0("Rides on ",
                     c("School time","School holidays")[test_sch_hol+1]),
       xlab = "Ride date",
       ylab = "Number of rides") 
}

par(mfrow=c(1,1)) # single plot

# Visualizing boardings
# Prepare the plot area
if(!is.null(dev.list())) dev.off() # Clear existing plots
par(mar=c(3,2,2,1.5)) # set the margins
par(mfrow = c(7,2)) # set up a grid for 7x2=2 plots filled in row-by-row

# Maximize the plot sub-window to occupy the page height before running
for (wday_idx in weekdays_order) {
  rides_day = filter(ride_summary, StartDay==wday_idx)
  rides_day_sch=filter(rides_day,SchoolHoliday==FALSE)
  rides_day_hol=filter(rides_day,SchoolHoliday==TRUE)
  plot(rides_day_sch$StartHour,rides_day_sch$rides,
       xlim=c(0, 23),
       main = paste0("Real rides on ",wday_idx,", school time")
  )
  plot(rides_day_hol$StartHour,rides_day_hol$rides,
       xlim=c(0, 23),
       main = paste0("Real rides on ",wday_idx,", school holidays")
  )
}

# Set the file for saving the contents of the graph window
dev.copy(pdf,"./02_output_data/10_real_rides.pdf")
# Save the scatter plots
dev.off()

par(mfcol = c(1,1)) # set up a grid for 1x1=1 plot

# Plots with "ggplot2"
library(ggplot2)

# rides_tmp = rides_by_date # All rides
rides_tmp = rides_by_date_filtered[[1]] # Schooltime-rides
ggplot(rides_tmp, 
       aes(x=StartDate,
           y=total_rides)) +
  geom_line() +
  geom_point() +
  labs(title = "Hourly Rides in the First Half of 2019",
       x = "Hour", y = "Number of Rides") +
  theme(plot.title=element_text(hjust=0.5), # centered title
        legend.text = element_text(size = 15), # title font size
        legend.position = "none", # no legend
        axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
  )

ggsave("./02_output_data/10_Schooltime-rides.pdf", height = 9, width = 16)

rm(list = ls()) # Clear environment
cat("\014") # Clear console (Ctrl+L)
