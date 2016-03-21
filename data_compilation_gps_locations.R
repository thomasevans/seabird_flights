# Script to sort and compile GPS data for flights from both murres and gulls


# Load flight data -----
load("flights.RData")


# Make SQL querries for data for both murres and gulls ------


# Datbase functions
# Required library
library(RODBC)

# Establish a connection to the databases
gull.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

murre.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')



# Murres - IGU -----
murres_igu <- flights[flights$device_type == "igu" & flights$species == "murre", ]
# i <- 5

points.murre.igu <- list()

for(i in 1:nrow(murres_igu)){
  
  sql_query <- paste("SELECT DISTINCT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_gps_points_igu.elev, guillemots_gps_points_igu.speed_ms, guillemots_gps_points_igu.course, guillemots_gps_points_igu.ehpe, guillemots_gps_points_igu.sat_n, guillemots_gps_points_igu.timeout, guillemots_gps_points_igu.MSVs_QCN, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_v, guillemots_gps_points_movebank_ecmwf.ecmwf_surf_roughness, guillemots_gps_points_movebank_ecmwf.ecmwf_charnock, guillemots_gps_points_movebank_ecmwf.ecmwf_boundary_lay_ht, guillemots_gps_points_movebank_ecmwf.ecmwf_temp_2m, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_u, guillemots_gps_points_movebank_ecmwf.ecmwf_pressure_sea_lev
FROM guillemots_track_session, (guillemots_gps_points_flight_id INNER JOIN (guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_igu_class INNER JOIN guillemots_gps_points_igu ON (guillemots_gps_points_igu_class.date_time = guillemots_gps_points_igu.date_time) AND (guillemots_gps_points_igu_class.device_info_serial = guillemots_gps_points_igu.device_info_serial)) ON (guillemots_gps_points_trip_id.date_time = guillemots_gps_points_igu_class.date_time) AND (guillemots_gps_points_trip_id.device_info_serial = guillemots_gps_points_igu_class.device_info_serial)) ON (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_igu.date_time) AND (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_igu.device_info_serial)) INNER JOIN guillemots_gps_points_movebank_ecmwf ON (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_movebank_ecmwf.date_time) AND (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_movebank_ecmwf.device_info_serial)
WHERE (((guillemots_gps_points_igu.date_time)>= #",
                     murres_igu$start_time[i],
                     "# And (guillemots_gps_points_igu.date_time)<= #",
                     murres_igu$end_time[i],
                     "# ) AND ((guillemots_track_session.device_info_serial)= '",
                     murres_igu$device_info_serial[i],
                     "' ) AND ((guillemots_gps_points_igu.device_info_serial)= '",
                     murres_igu$device_info_serial[i],
                     "' ))
ORDER BY guillemots_gps_points_igu.date_time;",
                     sep = "")
  
  points <- sqlQuery(murre.db, query= gsub("\n", " ", sql_query))
  points$flight_id_combined <- murres_igu$flight_id_combined[i]
  points.murre.igu[i] <- list(points)
  
  
}


points.murre.igu.df <- do.call(rbind , points.murre.igu)

# nrow(unique(points.murre.igu.df)) == nrow(points.murre.igu.df)

# Check all flights represented with locations
murres_igu$flight_id_combined %in% points.murre.igu.df$flight_id_combined



# Murres - uva -----
murres_uva <- flights[flights$device_type == "uva" & flights$species == "murre", ]

# unique(murre.flights$device_info_serial)
# 
# murres_uva$device_info_serial[i]
# murres_uva$start_time[i]
# murres_uva$end_time[i]

# i <- 3



points.murres_uva <- list()

for(i in 1:nrow(murres_uva)){
  
  sql_query <- paste("SELECT DISTINCT gps_ee_tracking_speed_limited.device_info_serial, gps_ee_tracking_speed_limited.date_time, gps_ee_tracking_speed_limited.latitude, gps_ee_tracking_speed_limited.longitude, gps_ee_tracking_speed_limited.altitude, gps_ee_tracking_speed_limited.speed_2d, gps_ee_tracking_speed_limited.direction, gps_ee_tracking_speed_limited.h_accuracy, gps_ee_tracking_speed_limited.v_accuracy, gps_ee_tracking_speed_limited.satellites_used, gps_ee_tracking_speed_limited.positiondop, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_v, guillemots_gps_points_movebank_ecmwf.ecmwf_surf_roughness, guillemots_gps_points_movebank_ecmwf.ecmwf_charnock, guillemots_gps_points_movebank_ecmwf.ecmwf_boundary_lay_ht, guillemots_gps_points_movebank_ecmwf.ecmwf_temp_2m, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_u
FROM guillemots_track_session, ((guillemots_gps_points_trip_id INNER JOIN guillemots_gps_points_uva_class ON (guillemots_gps_points_trip_id.device_info_serial = guillemots_gps_points_uva_class.device_info_serial) AND (guillemots_gps_points_trip_id.date_time = guillemots_gps_points_uva_class.date_time)) INNER JOIN gps_ee_tracking_speed_limited ON (guillemots_gps_points_uva_class.device_info_serial = gps_ee_tracking_speed_limited.device_info_serial) AND (guillemots_gps_points_uva_class.date_time = gps_ee_tracking_speed_limited.date_time)) INNER JOIN (guillemots_gps_points_movebank_ecmwf INNER JOIN guillemots_gps_points_flight_id ON (guillemots_gps_points_movebank_ecmwf.device_info_serial = guillemots_gps_points_flight_id.device_info_serial) AND (guillemots_gps_points_movebank_ecmwf.date_time = guillemots_gps_points_flight_id.date_time)) ON (guillemots_gps_points_trip_id.device_info_serial = guillemots_gps_points_movebank_ecmwf.device_info_serial) AND (guillemots_gps_points_trip_id.date_time = guillemots_gps_points_movebank_ecmwf.date_time)
                     WHERE (((gps_ee_tracking_speed_limited.device_info_serial)= '",
                     murres_uva$device_info_serial[i],
                     "' ) AND ((gps_ee_tracking_speed_limited.date_time)>= #",
                     murres_uva$start_time[i],
                     "# And (gps_ee_tracking_speed_limited.date_time)<=#",
                     murres_uva$end_time[i],
                     "#) AND ((guillemots_track_session.device_info_serial)='",
                     murres_uva$device_info_serial[i],
                     "'));",
                     sep = "")
  
  points <- sqlQuery(murre.db, query= gsub("\n", " ", sql_query))
  points$flight_id_combined <- murres_uva$flight_id_combined[i]
  points.murres_uva[i] <- list(points)
  
  
}


points.murres_uva.df <- do.call(rbind , points.murres_uva)

# Check all flights represented with locations
murres_uva$flight_id_combined %in% points.murres_uva.df$flight_id_combined


# LBBG - uva -----
lbbg_uva <- flights[flights$device_type == "uva" & flights$species == "gull", ]




points.lbbg_uva <- list()

for(i in 1:nrow(lbbg_uva)){
  
  sql_query <- paste("SELECT DISTINCT gps_ee_tracking_speed_limited_local.device_info_serial, gps_ee_tracking_speed_limited_local.date_time, gps_ee_tracking_speed_limited_local.latitude, gps_ee_tracking_speed_limited_local.longitude, gps_ee_tracking_speed_limited_local.altitude, gps_ee_tracking_speed_limited_local.speed_2d, gps_ee_tracking_speed_limited_local.direction, gps_ee_tracking_speed_limited_local.h_accuracy, gps_ee_tracking_speed_limited_local.v_accuracy, gps_ee_tracking_speed_limited_local.satellites_used, gps_ee_tracking_speed_limited_local.positiondop, move_bank_variables_all.wind_v_10m, move_bank_variables_all.surface_roughness, move_bank_variables_all.temperature_2m, move_bank_variables_all.wind_u_10m
FROM gps_ee_track_session_limited_local, gps_ee_tracking_speed_limited_local INNER JOIN move_bank_variables_all ON (gps_ee_tracking_speed_limited_local.date_time = move_bank_variables_all.date_time) AND (Val(gps_ee_tracking_speed_limited_local.device_info_serial) = move_bank_variables_all.device_info_serial)
                     WHERE (((gps_ee_tracking_speed_limited_local.device_info_serial)= '",
                     lbbg_uva$device_info_serial[i],
                     "') AND ((gps_ee_tracking_speed_limited_local.date_time)>=#",
                     lbbg_uva$start_time[i],
                     "# And (gps_ee_tracking_speed_limited_local.date_time)<=#",
                     lbbg_uva$end_time[i],
                     "#) AND ((gps_ee_track_session_limited_local.device_info_serial)=",
                     lbbg_uva$device_info_serial[i],
                     "));",
                     sep = "")
  
  points <- sqlQuery(gull.db, query= gsub("\n", " ", sql_query))
  points$flight_id_combined <- lbbg_uva$flight_id_combined[i]
  points.lbbg_uva[i] <- list(points)
  
  
}


points.lbbg_uva.df <- do.call(rbind , points.lbbg_uva)

# Check all flights represented with locations
all(lbbg_uva$flight_id_combined %in% points.lbbg_uva.df$flight_id_combined)




# Combine these data ------

# First give common names to DF fields with common names

# Two UVA tables
names_same <- names(points.lbbg_uva.df) %in% names(points.murres_uva.df)
names(points.lbbg_uva.df)[!names_same] <- c("ecmwf_wind_10m_v",
                                            "ecmwf_surf_roughness",
                                            "ecmwf_temp_2m",
                                            "ecmwf_wind_10m_u")
# points.lbbg_uva.df.2 <- points.lbbg_uva.df[,names(points.murres_uva.df)]



# Merge UvA data
library(dplyr)
points.uva <- bind_rows(points.lbbg_uva.df, points.murres_uva.df)
# ?bind_rows
# IGU + UVA data
names_same <- names(points.murre.igu.df) %in% names(points.uva)
names(points.murre.igu.df)[!names_same] <- c("altitude",
                                            "speed_2d",
                                            "direction",
                                            "ehpe",
                                            "satellites_used",
                                            "timeout",
                                            "MSVs_QCN",
                                            "ecmwf_pressure_sea_lev")
str(points.murre.igu.df)

points.murre.igu.df$device_info_serial <- as.character(points.murre.igu.df$device_info_serial)
points.uva$device_info_serial <- as.character(points.uva$device_info_serial)

# Merge all data
points.all <- bind_rows(points.uva, points.murre.igu.df)

# str(points.uva)

# Output to csv
write.table(points.all, file = "points_all.csv", col.names = TRUE,
            row.names = FALSE, sep = ",")

save(points.all, file = "points_all.RData")

# hist(points.all$altitude[points.all$altitude >-100], breaks = 200, xlim = c(-50,200))


# Calculate mode using function from here: http://stackoverflow.com/a/8189441/1172358
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

flight_ids <- unique(points.all$flight_id_combined)


flight.modes <- NULL
for(i in 1:length(flight_ids)){
  flight.modes[i] <- Mode(points.all$altitude[points.all$flight_id_combined == flight_ids[i] ])
}




hist(flight.modes, breaks = 100, xlim = c(-200,200))
hist(points.all$altitude, breaks = 200, xlim = c(-200,200))

hist(points.all$surface_roughness, breaks = 1000, xlim = c(0,0.1))
range(points.all$surface_roughness, na.rm = TRUE)
hist(log(points.all$surface_roughness))

hist(points.all$ecmwf_boundary_lay_ht, breaks = 100)
hist(points.all$ecmwf_boundary_lay_ht, breaks = 100, xlim = c(0,200))

bound_lay_dist <- points.all$ecmwf_boundary_lay_ht - points.all$altitude
hist(bound_lay_dist, breaks = 100)
plot(points.all$ecmwf_boundary_lay_ht~points.all$altitude, xlim = c(-50,150), ylim = c(-50,1500))


summary(points.all$altitude < -10)
