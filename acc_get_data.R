# Script to extract Acc data for flights where this exists


# Load in data -----

# Load flight summary data
load("flight_details.RData")


# Make DB connection -----
# Datbase functions
# Required library
library(RODBC)

# Establish a connection to the databases
gull.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

murre.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# For each flight get info on where Acc records exist ----


# i <- 200
z <- 1
# str
acc.ls <- list()

n <- nrow(flight.details)
for(i in 1:n){
  
  if(flight.details$device_type[i] == "uva"){
    sql_query <- paste("SELECT DISTINCT gps_ee_acc_start_limited.device_info_serial, gps_ee_acc_start_limited.date_time, gps_ee_acc_start_limited.line_counter, gps_ee_acc_start_limited.timesynced, gps_ee_acc_start_limited.accii, gps_ee_acc_start_limited.accsn, gps_ee_acc_start_limited.f
FROM gps_ee_acc_start_limited
WHERE (((gps_ee_acc_start_limited.device_info_serial)= ",
                       as.numeric(flight.details$device_info_serial[i]),
                       ") AND ((gps_ee_acc_start_limited.date_time)>=#",
                       flight.details$start_time[i],
                       "# And (gps_ee_acc_start_limited.date_time)<=#",
                       flight.details$end_time[i],
                       "#))
ORDER BY gps_ee_acc_start_limited.device_info_serial, gps_ee_acc_start_limited.date_time;",
                       sep = "")
    
    acc_rec <- sqlQuery(gull.db, query= gsub("\n", " ", sql_query))
    
    if(nrow(acc_rec)>0){
      
      acc_rec$flight_id_combined  <- flight.details$flight_id_combined[i]
      
      acc.ls[z] <- list(acc_rec)
      
      z <- z + 1
      
    }
    
  }
  
}



# Combine into single table -----
acc.rec.df <- do.call(rbind , acc.ls)


# See sample sizes -----
flight.details.sub <- subset(flight.details, select = c(
  flight_id_combined, species, ring_number))

acc.rec.df.detailed <- merge(acc.rec.df, flight.details.sub, by = "flight_id_combined")

acc.rec.df.detailed.unique <- unique(subset(acc.rec.df.detailed, select = c(
  flight_id_combined, species, ring_number)))

summary(as.factor(acc.rec.df.detailed.unique$species))
summary(as.factor(acc.rec.df.detailed.unique$ring_number))



summary(as.factor(flight.details$species[flight.details$device_type == "uva"]))
summary(as.factor(flight.details$ring_number[flight.details$device_type == "uva"]))


# summary(acc.rec.df$f)

# Save the acc rec dataframe -----
save(acc.rec.df, file = "acc.rec.df.Rdata")



# Get Acc data ------


acc.ls <- list()
i <- 1

n <- nrow(acc.rec.df)
for(i in 1:n){
  
    sql_query <- paste("SELECT DISTINCT gps_ee_acceleration_limited.device_info_serial, gps_ee_acceleration_limited.date_time, gps_ee_acceleration_limited.index, gps_ee_acceleration_limited.x_acceleration, gps_ee_acceleration_limited.y_acceleration, gps_ee_acceleration_limited.z_acceleration
FROM gps_ee_acceleration_limited
                       WHERE (((gps_ee_acceleration_limited.device_info_serial)= ",
                       as.numeric(acc.rec.df$device_info_serial[i]),
                       ") AND ((gps_ee_acceleration_limited.date_time)=#",
                       acc.rec.df$date_time[i],
                       "#))
ORDER BY gps_ee_acceleration_limited.device_info_serial, gps_ee_acceleration_limited.date_time, gps_ee_acceleration_limited.index;",
                       sep = "")
    
    acc_rec <- sqlQuery(gull.db, query= gsub("\n", " ", sql_query))
    
    acc_rec$flight_id_combined  <- acc.rec.df$flight_id_combined[i]
    
    acc_rec$date_time_acc <- acc_rec$date_time + ((acc_rec$index-1)*0.05)
#     format(acc_rec$date_time_acc, "%Y-%m-%d %H:%M:%OS3", tz = "utc")
#     ?format
    acc.ls[i] <- list(acc_rec)
      
}



# Combine into single table -----
acc.dat.df <- do.call(rbind , acc.ls)

# Output file
save(acc.dat.df, file = "acc.dat.df.RData")
