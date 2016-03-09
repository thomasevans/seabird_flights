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
  
  sql_query <- paste("SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_gps_points_igu.elev, guillemots_gps_points_igu.speed_ms, guillemots_gps_points_igu.course, guillemots_gps_points_igu.ehpe, guillemots_gps_points_igu.sat_n, guillemots_gps_points_igu.timeout, guillemots_gps_points_igu.MSVs_QCN, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_v, guillemots_gps_points_movebank_ecmwf.ecmwf_surf_roughness, guillemots_gps_points_movebank_ecmwf.ecmwf_charnock, guillemots_gps_points_movebank_ecmwf.ecmwf_boundary_lay_ht, guillemots_gps_points_movebank_ecmwf.ecmwf_temp_2m, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_u, guillemots_gps_points_movebank_ecmwf.ecmwf_pressure_sea_lev
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

# Check all flights represented with locations
murres_igu$flight_id_combined %in% points.murre.igu.df$flight_id_combined



# Murres - uva -----
murres_uva <- flights[flights$device_type == "uva" & flights$species == "murre", ]


unique(murre.flights$device_info_serial)




# Combine these data ------
# Put NAs for missing data (e.g. columns not common between the igu and uva GPS devices)
