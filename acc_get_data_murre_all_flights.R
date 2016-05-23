# Get all murre acc data from flights (any flights)



# Get data from DB ------
library(RODBC)

murre.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')
gull.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


# Get flights table from DB - only UvA devices ---



sql_query <- "SELECT guillemots_gps_flights.flight_id, guillemots_gps_flights.ring_number, guillemots_gps_flights.deploy_id, guillemots_gps_flights.device_info_serial, guillemots_gps_flights.start_time, guillemots_gps_flights.end_time, guillemots_gps_flights.n_points, guillemots_gps_flights.col_dist_start, guillemots_gps_flights.col_dist_end, guillemots_gps_flights.col_dist_dif, guillemots_gps_flights.col_dist_max, guillemots_gps_flights.trip_id, guillemots_gps_flights.duration, guillemots_gps_flights.dist_straight, guillemots_gps_flights.p2p2_dist, guillemots_gps_flights.speed_median, guillemots_gps_flights.type, guillemots_gps_flights.trip_flight_n, guillemots_gps_flights.flight_type, guillemots_gps_flights.flight_trip_n
FROM guillemots_gps_flights
WHERE (((guillemots_gps_flights.device_type)='uva'))
ORDER BY guillemots_gps_flights.device_info_serial, guillemots_gps_flights.start_time;"

flights <- sqlQuery(murre.db, query= gsub("\n", " ", sql_query))

unique(flights$deploy_id)
# 12 sepperate UvA deployments with at least some flight data


# For each flight querry DB for available Acc data records ----

i <- 5

z <- 1
# str
acc.ls <- list()

n <- nrow(flight.details)
for(i in 1:n){
  
 
    sql_query <- paste("SELECT DISTINCT gps_ee_acc_start_limited.device_info_serial, gps_ee_acc_start_limited.date_time, gps_ee_acc_start_limited.line_counter, gps_ee_acc_start_limited.timesynced, gps_ee_acc_start_limited.accii, gps_ee_acc_start_limited.accsn, gps_ee_acc_start_limited.f
                       FROM gps_ee_acc_start_limited
                       WHERE (((gps_ee_acc_start_limited.device_info_serial)= ",
                       as.numeric(flights$device_info_serial[i]),
                       ") AND ((gps_ee_acc_start_limited.date_time)>=#",
                       flights$start_time[i],
                       "# And (gps_ee_acc_start_limited.date_time)<=#",
                       flights$end_time[i],
                       "#))
                       ORDER BY gps_ee_acc_start_limited.device_info_serial, gps_ee_acc_start_limited.date_time;",
                       sep = "")
    
    acc_rec <- sqlQuery(gull.db, query= gsub("\n", " ", sql_query))
    
    if(nrow(acc_rec)>0){
      
      acc_rec$flight_id_combined  <- flights$flight_id[i]
      acc_rec$ring_number <- flights$ring_number[i]
      acc_rec$deploy_id <- flights$deploy_id[i]
      
      acc.ls[z] <- list(acc_rec)
      
      z <- z + 1
      
    }
    
  
}



# Combine into single table -----
acc.rec.df <- do.call(rbind , acc.ls)




# All acc data from guillemot UvA deployments ------

# All deployments
sql_query <- "SELECT guillemots_track_session.device_info_serial, guillemots_track_session.ring_number, guillemots_track_session.start_date, guillemots_track_session.end_date
FROM guillemots_track_session
WHERE (((guillemots_track_session.device_type)='uva'));"

deployments <- sqlQuery(murre.db, query= gsub("\n", " ", sql_query))


# For each deployment get Acc records ----
z <- 1
# str
# i <- 1
acc.ls <- list()

n <- nrow(deployments)
for(i in 1:n){
  
  
  sql_query <- paste("SELECT DISTINCT gps_ee_acc_start_limited.device_info_serial, gps_ee_acc_start_limited.date_time, gps_ee_acc_start_limited.line_counter, gps_ee_acc_start_limited.timesynced, gps_ee_acc_start_limited.accii, gps_ee_acc_start_limited.accsn, gps_ee_acc_start_limited.f
                     FROM gps_ee_acc_start_limited
                     WHERE (((gps_ee_acc_start_limited.device_info_serial)= ",
                     as.numeric(deployments$device_info_serial[i]),
                     ") AND ((gps_ee_acc_start_limited.date_time)>=#",
                     deployments$start_date[i],
                     "# And (gps_ee_acc_start_limited.date_time)<=#",
                     deployments$end_date[i],
                     "#))
                     ORDER BY gps_ee_acc_start_limited.device_info_serial, gps_ee_acc_start_limited.date_time;",
                       sep = "")
    
    acc_rec <- sqlQuery(gull.db, query= gsub("\n", " ", sql_query))
    
    if(nrow(acc_rec)>0){
      
      acc_rec$flight_id_combined  <- flights$flight_id[i]
      acc_rec$ring_number <- flights$ring_number[i]
      acc_rec$deploy_id <- flights$deploy_id[i]
      
      acc.ls[z] <- list(acc_rec)
      
      z <- z + 1
      
    }
    
  
}





# Get Raw Acc data ------

acc.ls <- list()
# i <- 1
z <- 1
n <- nrow(flight.details)
for(i in 1:n){
  
  sql_query <- paste("SELECT DISTINCT gps_ee_acceleration_limited.device_info_serial, gps_ee_acceleration_limited.date_time, gps_ee_acceleration_limited.index, gps_ee_acceleration_limited.x_acceleration, gps_ee_acceleration_limited.y_acceleration, gps_ee_acceleration_limited.z_acceleration
FROM gps_ee_acceleration_limited
                     WHERE (((gps_ee_acceleration_limited.device_info_serial)= ",
                       as.numeric(flights$device_info_serial[i]),
                       ") AND ((gps_ee_acceleration_limited.date_time)>=#",
                     flights$start_time[i],
                     "# And (gps_ee_acceleration_limited.date_time)<=#",
                     flights$end_time[i],
"#))
ORDER BY gps_ee_acceleration_limited.device_info_serial, gps_ee_acceleration_limited.date_time, gps_ee_acceleration_limited.index;",
                     sep = "")
  
  
  
    acc_rec <- sqlQuery(gull.db, query= gsub("\n", " ", sql_query))
  
    
    if(nrow(acc_rec)>0){
      
      
      acc_rec$ring_number  <- flights$ring_number[i]
      acc_rec$flight_id  <- flights$flight_id[i]
      acc_rec$deploy_id  <- flights$deploy_id[i]
      
      acc_rec$date_time_acc <- acc_rec$date_time + ((acc_rec$index-1)*0.05)
      #     format(acc_rec$date_time_acc, "%Y-%m-%d %H:%M:%OS3", tz = "utc")
      #     ?format
      acc.ls[z] <- list(acc_rec)
      
#       acc_rec$flight_id_combined  <- flights$flight_id[i]
#       acc_rec$ring_number <- flights$ring_number[i]
#       acc_rec$deploy_id <- flights$deploy_id[i]
#       
#       acc.ls[z] <- list(acc_rec)
      
      z <- z + 1
      
    }
    
}



# Combine into single table -----
acc.dat.df <- do.call(rbind , acc.ls)

a <- c(1:30)
a <- a + 30
par(mfrow = c(3,1))
plot(acc.dat.df$z_acceleration[a], type = "l")
points(acc.dat.df$z_acceleration[a])
plot(acc.dat.df$x_acceleration[a], type = "l")
points(acc.dat.df$x_acceleration[a])
plot(acc.dat.df$y_acceleration[a], type = "l")
points(acc.dat.df$y_acceleration[a])


z_acc_cen <- acc.dat.df$z_acceleration[a] - mean(abs(acc.dat.df$z_acceleration[a]))
# plot(z_acc_cen, type = "l")

z_acc_cen_norm <- z_acc_cen/max(abs(z_acc_cen))
# plot(z_acc_cen_norm, type = "l")
# ?spec.ar
x <- spec.ar(z_acc_cen, log="no", plot = TRUE)

max.spec <- max(unlist(x[['spec']]))
freq.max <- unlist(x[['spec']]) == max.spec
# str(x['freq'])
freq <- unlist(x['freq'])[freq.max][1]  # First maxima (if there are multiple maxima of same value[rare])
spec.freq <- freq*20  # multiply by sampling interval (Hz)







a
st <- acc.dat.df$date_time[a[1]] - 10000
et <- acc.dat.df$date_time[a[1]] + 10000

sql_query <- paste("SELECT DISTINCT gps_ee_acc_start_limited.device_info_serial, gps_ee_acc_start_limited.date_time, gps_ee_acc_start_limited.line_counter, gps_ee_acc_start_limited.timesynced, gps_ee_acc_start_limited.accii, gps_ee_acc_start_limited.accsn, gps_ee_acc_start_limited.f
                       FROM gps_ee_acc_start_limited
                       WHERE (((gps_ee_acc_start_limited.device_info_serial)= ",
                   as.numeric(acc.dat.df$device_info_serial[151]),
                   ") AND ((gps_ee_acc_start_limited.date_time)>=#",
                   st,
                   "# And (gps_ee_acc_start_limited.date_time)<=#",
                   et,
                   "#))
                       ORDER BY gps_ee_acc_start_limited.device_info_serial, gps_ee_acc_start_limited.date_time;",
                   sep = "")

acc_rec <- sqlQuery(gull.db, query= gsub("\n", " ", sql_query))




# Get above Acc data ---