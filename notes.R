start_date_text <- c("15/07/2010", "15/07/2010", "11/07/2015", "07/07/2015", "08/07/2015")
start_time_text <- c("13:20:00", "20:50:00", "12:18:00", "12:08:00", "11:17:00")
start_date_time_text<-paste(start_date_text, start_time_text, sep=" ")
start_date_time<-as.POSIXct(start_date_time_text, tz="UTC", format="%d/%m/%Y %H:%M:%S")



source("deg.dist.R")



# 0.02°LONG * 0.01°LAT
# 17.4 E,57.4 N

deg.dist(17.4, 57.4, 17.42, 57.4)

deg.dist(17.4, 57.4, 17.4, 57.41)



summary(flight.details$species == "murre"  &
          flight.details$device_type == "uva")

hist(flight.details$altitude_callib_extm_05[flight.details$species == "murre"  &
                                              flight.details$device_type == "uva"])
hist(flight.details$altitude_callib_extm_no_filter[flight.details$species == "murre"  &
                                              flight.details$device_type == "uva"])
