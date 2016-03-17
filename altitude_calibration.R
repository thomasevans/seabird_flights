# Altitude correction script
# calculate mean/ median altitude for 'sea surface' GPS locations, then use
# as correction factor of recorded GPS altitude - to get 'true' altitude above
# mean sea level


# Set bounding box (sea only between Stora Karlsö and Öland) -----
# Latitude range
lat.lim <- c(56.9, 57.6)

# Longitude range
long.lim <- c(17.2, 17.9)

# speed threshold
sp.lim <- 1

# Read in GPS point data (both IGU and UvA) -----
# Only data within bounding box where speed is < 1 ms-1

# Datbase functions
# Required library
library(RODBC)

# Establish a connection to the database
murre.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')

# Get UvA data
sql_query <- paste("SELECT gps_ee_tracking_speed_limited.device_info_serial, gps_ee_tracking_speed_limited.date_time, gps_ee_tracking_speed_limited.latitude, gps_ee_tracking_speed_limited.longitude, gps_ee_tracking_speed_limited.altitude
FROM gps_ee_tracking_speed_limited
                   WHERE (((gps_ee_tracking_speed_limited.latitude)>",
                   lat.lim[1],
                   "And (gps_ee_tracking_speed_limited.latitude)<",
                   lat.lim[2],
                   ") AND ((gps_ee_tracking_speed_limited.longitude)>",
                   long.lim[1],
                   " And (gps_ee_tracking_speed_limited.longitude)<",
                   long.lim[2],
                   ") AND ((gps_ee_tracking_speed_limited.speed_2d)<",
                   sp.lim,
                   "))
ORDER BY gps_ee_tracking_speed_limited.device_info_serial, gps_ee_tracking_speed_limited.date_time;",
                   sep = "")

points.uva <- sqlQuery(murre.db, query= gsub("\n", " ", sql_query))


# Get IGU data
sql_query <- paste("SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_gps_points_igu.elev
FROM guillemots_gps_points_igu
                   WHERE (((guillemots_gps_points_igu.latitude)>",
                   lat.lim[1],
                   " And (guillemots_gps_points_igu.latitude)<",
                   lat.lim[2],
                   ") AND ((guillemots_gps_points_igu.longitude)>",
                   long.lim[1],
                   " And (guillemots_gps_points_igu.longitude)<",
                   long.lim[2],
                   ") AND ((guillemots_gps_points_igu.speed_ms)<",
                   sp.lim,
                   "))
ORDER BY guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time;",
                   sep = "")

points.igu <- sqlQuery(murre.db, query= gsub("\n", " ", sql_query))

# Close DB connection
odbcClose(murre.db)

hist(points.igu$elev, xlim = c(-20,20), breaks = 100000)
mean(points.igu$elev[points.igu$elev> - 50 & points.igu$elev < 50])



# Have a look at distributions and decide on cut-off values ------

# Seasonal fluctuations:
# it appears that this can be ignored for our purposes, in range of 0 - 25 cm
# see: Medvedev, I.P., 2015. Seasonal fluctuations of the Baltic Sea level. Russ. Meteorol. Hydrol. 39, 814–822. doi:10.3103/S106837391412005X
# Perhaps cite: chapter 8 in: Leppäranta, M., Myrberg, K., 2008. Physical oceanogprahy of the Baltic Sea, 1st ed. ed, Praxis geophysical sciences 4110. Springer, New York.

# 3 sets of data:

# Murre IGU

# gull.uva
points.uva.gulls <- points.uva[points.uva$device_info_serial > 500 &
                                 points.uva$device_info_serial < 1000,]

# Murre uva
points.uva.murres <- points.uva[points.uva$device_info_serial > 1000,]
  
# device_ids <- unique(points.uva$device_info_serial)

# Histograms of altitude ----

# Calculate mode using function from here: http://stackoverflow.com/a/8189441/1172358
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

hist(points.uva.gulls$altitude, main = "UvA - gulls")
hist(points.uva.gulls$altitude, xlim = c(-25,25), breaks = 10000, main = "UvA - gulls",
     xlab = "GPS altitude (m)")
median(points.uva.gulls$altitude, na.rm = TRUE)
Mode(points.uva.gulls$altitude)
mean(points.uva.gulls$altitude[points.uva.gulls$altitude > -25 & 
                                 points.uva.gulls$altitude < 25 ], na.rm = TRUE)
#-2.820093
summary(points.uva.gulls$altitude, na.rm = TRUE)
summary(points.uva.gulls$altitude[points.uva.gulls$altitude > -25 & 
                                    points.uva.gulls$altitude < 25 ], na.rm = TRUE)

hist(points.uva.murres$altitude, xlim = c(-25,25), breaks = 10000, main = "UvA - murres",
     xlab = "GPS altitude (m)")
median(points.uva.murres$altitude, na.rm = TRUE)
Mode(points.uva.murres$altitude)
mean(points.uva.murres$altitude[points.uva.murres$altitude > -25 & points.uva.murres$altitude < 25 ], na.rm = TRUE)
#-1.279747
summary(points.uva.murres$altitude, na.rm = TRUE)
summary(points.uva.murres$altitude[points.uva.murres$altitude > -25 & points.uva.murres$altitude < 25 ], na.rm = TRUE)

hist(points.igu$elev, xlim = c(-25,25), breaks = 50000, main = "IGU - murres",
     xlab = "GPS altitude (m)")
median(points.igu$elev, na.rm = TRUE)
Mode(points.igu$elev)
abline(v = 1.4, col = "red", lwd = 2)
mean(points.igu$elev[points.igu$elev > -25 & points.igu$elev < 25 ], na.rm = TRUE)
#0.07854506
summary(points.igu$elev, na.rm = TRUE)
summary(points.igu$elev[points.igu$elev > -25 & points.igu$elev < 25 ], na.rm = TRUE)
