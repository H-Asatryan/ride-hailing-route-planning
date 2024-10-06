pacman::p_load(plyr,dplyr,lubridate,rio)

WLAN_TripID_ride_hailing =
  import("./02_output_data/08_WLAN_TripID_ride_hailing.RData")
date_wkday_pairs=select(WLAN_TripID_ride_hailing,
                        StartDate, StartDay)
date_wkday_pairs=distinct(date_wkday_pairs)

pred_boarding_intensities=
  import("./02_output_data/16_pred_boarding_intensities.RData")
simulated_ride_requests=
  import("./02_output_data/17_simulated_ride_requests.csv"
)

WSW_ride_requests =
  import("../Hol_mich_app/02_output_data/08_WSW_ride_requests_edited.RData")

group_sizes=prop.table(table(WSW_ride_requests$Number_of_passengers))
group_sizes=as.data.frame(group_sizes,stringsAsFactors = FALSE)

# View(WLAN_TripID_ride_hailing)
test_weekday="Friday"
test_sch_hol=FALSE
# test_date=date_wkday_pairs$StartDate[sample(
#   which(date_wkday_pairs$StartDay==test_weekday),
#   size=1
# )]

# station_example="W.-tal Gym. Bayreuther Straße"
# orig_daten_date_example = filter(WLAN_TripID_2019_ride_hailing,
#                                StartDate==test_date)
# orig_daten_date_example_station=filter(orig_daten_date_example,
#                                   StartStation==station_example)

observation_per_station_0 = data.frame("StartStation"= character(0),
                                       "StartHour"= integer(0),
                                       "Intesity" = numeric(0),
                                       "Passengers"= integer(0),
                                       "ActualPassengers"= integer(0),
                                       stringsAsFactors=FALSE)

observation_per_station_0 = as_tibble(observation_per_station_0)

observation_all_stations = data.frame("StartStation"= character(0),
                                      "StartHour"= integer(0),
                                      "Intesity" = numeric(0),
                                      "Passengers"= integer(0),
                                      "ActualPassengers"= integer(0),
                                      stringsAsFactors=FALSE)

for (i in 1:length(pred_boarding_intensities)) {
  
  start_station=pred_boarding_intensities[[i]]$StartStation[1]
  
  intensities_wday_sch_hol=filter(pred_boarding_intensities[[i]],
                          StartDay==test_weekday & SchoolHoliday==test_sch_hol)

  orig_daten_station_sch_hol=filter(WLAN_TripID_ride_hailing,
                            StartStation==start_station & SchoolHoliday==test_sch_hol)

  test_date=date_wkday_pairs$StartDate[sample(
    which(date_wkday_pairs$StartDay==test_weekday),
    size=1
  )]
  
  orig_daten_station_date_sch_hol = filter(orig_daten_station_sch_hol,
                                   StartDate==test_date)
  
  simul_daten_wday_sch_hol=filter(
    simulated_ride_requests,
    StartStation==start_station &
      StartDay==test_weekday & SchoolHoliday==test_sch_hol)  
  
  observation_per_station = observation_per_station_0
  for (hour_idx in 0:23) {
    intensities_wday_sch_hol_hour=filter(intensities_wday_sch_hol,
                                 StartHour==hour_idx)
    simul_daten_wday_sch_hol_hour=filter(simul_daten_wday_sch_hol,
                                 StartHour==hour_idx)
    orig_daten_station_date_sch_hol_hour=filter(orig_daten_station_date_sch_hol,
                                                StartHour==hour_idx)
    
    a=sum(intensities_wday_sch_hol_hour$LambdaIntesity)
    b=sum(simul_daten_wday_sch_hol_hour$Passengers)
    c=nrow(orig_daten_station_date_sch_hol_hour)
    observation_per_station = dplyr::add_row(observation_per_station,
                                                 StartStation = start_station,
                                                 StartHour = hour_idx,
                                                 Intesity = a,
                                                 Passengers = b,
                                                 ActualPassengers = c)
  }
  
  observation_per_station_df = as.data.frame(observation_per_station)
  observation_all_stations = bind_rows(observation_all_stations,
                                       observation_per_station_df)
  rm(observation_per_station)
}

observation_all_stations_20_23=filter(observation_all_stations,
                                      StartHour>=20 & StartHour<=23)
View(observation_all_stations)
View(observation_all_stations_20_23)

# summary(observation_all_stations)
# colMeans(observation_all_stations)
# colMeans(observation_all_stations_20_23)

mean(observation_all_stations$Passengers >
       observation_all_stations$ActualPassengers)
mean(observation_all_stations$Intesity*sum((1:4)*group_sizes$Freq) >
       observation_all_stations$ActualPassengers)
mean(observation_all_stations_20_23$Passengers*group_sizes$Freq[1]
     >observation_all_stations_20_23$ActualPassengers)

# These comparision plots use "sample" for hours 0-23
# There are nrow(observation)/24==218 blocks for hours 0-23
# Each block corresponds to a certain fixed station
# We sample the block number randomly
# We randomly choose 10 stations to compare the intensities,
# the numbers of simulated and actual passengers
if(!is.null(dev.list())) dev.off()  #  clears plots
par(mfrow=c(1,3))
# Set the file for plots
dev.copy(pdf,
         "./02_output_data/temp/comparison_10_stations.pdf",
         width=16,height=9)
for (sample_station_nr in 1:10) {
  block_number=sample(1:(nrow(observation_all_stations)/24), size = 1)
  start_row=1+(block_number-1)*24
  end_row=start_row+23
  observation_cut=observation_all_stations %>% slice(start_row:end_row)
  
  plot(observation_cut$StartHour,observation_cut$Intesity*sum((1:4)*group_sizes$Freq),
       col = "purple", pch=20, type="b",
       main = paste0(observation_cut$StartStation[1],"\n",
                     test_weekday,", ",
                     c("School time","School holidays")[test_sch_hol+1]),
       xlab = colnames(observation_cut)[2],
       ylab = colnames(observation_cut)[3])
  plot(observation_cut$StartHour,observation_cut$Passengers,
       col = "red", pch=20, type="b",
       main = paste0(observation_cut$StartStation[1],"\n",
                     test_weekday,", ",
                     c("School time","School holidays")[test_sch_hol+1]),
       xlab = colnames(observation_cut)[2],
       ylab = colnames(observation_cut)[4])
  plot(observation_cut$StartHour,observation_cut$ActualPassengers,
       col = "blue", pch=20, type="b",
       main = paste0(observation_cut$StartStation[1],"\n",
                     test_weekday,", ",
                     c("School time","School holidays")[test_sch_hol+1]),
       xlab = colnames(observation_cut)[2],
       ylab = colnames(observation_cut)[5])
  
}
# Saving plots as pdf, jpeg or png (works for above-mentioned graphic methods)
dev.off() # Important!!! Saves the graphics file "comparison_10_stations.pdf"

# Interesting: block_number==112; in this case observation_cut$ActualPassengers==0 !!!
station112=pred_boarding_intensities[[112]]
View(station112)
station112_orig_daten=filter(WLAN_TripID_2019_ride_hailing,
                             StartStation=="Wuppertal Ibach")

#################

# Visualizing the overall distribution
for(i in 3:5) {
  hist_dat=select(observation_all_stations,i)
  hist_dat_tmp=hist_dat
  colnames(hist_dat)[1]="query_column"
  # hist_dat=filter(hist_dat,ceiling(query_column)>0)
  colnames(hist_dat)[1]=colnames(hist_dat_tmp)[1]
  hist(unlist(hist_dat),
       breaks = seq(floor(min(hist_dat)),
                    ceiling(max(hist_dat))),
       main = colnames(hist_dat)[1],
       xlab = paste0("Rides on ",test_weekday,", ",
                     c("School time","School holidays")[test_sch_hol+1]
       )
  )
}

par(mfrow=c(1,1))

plot(observation[,1],observation[,3],asp=1)
plot(observation[,1],observation[,3],xlim=c(0,300),ylim=c(0,300))
abline(a=0,b=1)
