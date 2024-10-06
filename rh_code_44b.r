pacman::p_load(rio,dplyr)

WLAN_TripID_ride_hailing_extended =
  import(file = "./02_output_data/08b_WLAN_TripID_ride_hailing_extended.csv")

test_trip_id = filter(stop_times_merged,
                      trip_id=="wsw-66-876-1-1-1-H-0-MoTuWeThFrSaSu#2-11-1")


attempt_row_4 <- filter(stop_times_merged,
                        trip_id=="wsw-66-877-1-1-1-H-2-MoTuWeThFrSaSu#2-23-3",
                        stop_id=="de:05124:11376:2:3"
)

# Takes 52 sec; to check put the code inside system.time() !!!
# There are times like 25:14 !!!
attempt_row_4 <- filter(stop_times_merged,
                        trip_id=="wsw-66-877-1-1-1-H-2-MoTuWeThFrSaSu#2-23-3",
                        stop_id=="de:05124:11376:2:3",
                        strptime(arrival_time, format = "%H:%M:%S") > 
                          strptime("00:39:21", format = "%H:%M:%S")-3*60,
                        strptime(arrival_time, format = "%H:%M:%S") < 
                          strptime("00:39:21", format = "%H:%M:%S")+3*60
)

