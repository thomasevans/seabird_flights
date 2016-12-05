# Calculations based on wind etc to get Va, alpha, cross/ tail wind components,
# and wind at flight height (using wind-shear calculations)

library(dplyr)

# Load in flight and point data ----
load("flights.RData")
load("points_all.RData")
# mean(points.all$ecmwf_surf_roughness)

# Connect point data and flight data ----
# ?merge
points.info <- merge(points.all, flights, by = "flight_id_combined")


# Create new direction column, with common format -----
# hist(points.info$direction[points.info$device_type == "uva"])
# hist(points.info$direction[points.info$device_type == "igu"])
# summary(is.na(points.info$direction[points.info$device_type == "igu"]))


points.info$direction_common <- points.info$direction

# Correct UvA to 0 - 360 range
points.info$direction_common[points.info$device_type == "uva"] <- 
  (points.info$direction[points.info$device_type == "uva"]+360) %% 360
# hist(points.info$direction_common)


# Calculate bearings for 2009 IGU data

igu_2009 <- points.info$date_time < as.POSIXct("2010-01-01 00:00:00", tz = "UTC")
# summary(igu_2009)

library(fossil)

n <- sum(igu_2009)
dir.igu <- earth.bear(points.info$longitude[igu_2009][-n],
                      points.info$latitude[igu_2009][-n],
                      points.info$longitude[igu_2009][-1],
                      points.info$latitude[igu_2009][-1]
                      )
dir.igu.2009 <- c(dir.igu, NA)
# hist(dir.igu.2009)

points.info$direction_common[igu_2009] <- dir.igu.2009

flights.murres <- unique(points.info$flight_id_combined[igu_2009])

for(i in 1:length(flights.murres)){
  np <- length(points.info$direction_common[points.info$flight_id_combined == flights.murres[i]])
  points.info$direction_common[points.info$flight_id_combined == flights.murres[i]][np] <- NA
}

# hist(points.info$direction_common)
# summary(is.na(points.info$direction_common))


# Direction to island centre ('goal') -----


# Get nest locations
# Establish a connection to the database
library(RODBC)
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


# Get DB table for deployment_info (including nest location and island location)
dep.info <- sqlQuery(gps.db,
                     query = "SELECT gps_ee_nest_limited.latitude, gps_ee_nest_limited.longitude, gps_ee_nest_limited.nest_id, gps_ee_nest_inhabitant_limited.ring_number, gps_ee_nest_inhabitant_limited.device_info_serial
FROM gps_ee_nest_inhabitant_limited INNER JOIN gps_ee_nest_limited ON gps_ee_nest_inhabitant_limited.nest_id = gps_ee_nest_limited.nest_id;
                     "
                     ,as.is = TRUE)
# str(dep.info)
# dep.info$device_info_serial
# points.info$device_info_serial.x

names(points.info)[names(points.info) %in% c("device_info_serial.x")] <-"device_info_serial"

# dep.info$device_info_serial <- as.character(dep.info$device_info_serial)
points.info <- merge(points.info, dep.info, by = "device_info_serial",
           all.x = TRUE)

# Add nest location for IGU records
# Find only device IDs starting with 'g' (IGU data)
points.info$longitude.y[substr((points.info$device_info_serial),1,1) == "g"] <- 17.95829200
points.info$latitude.y[substr((points.info$device_info_serial),1,1) == "g"] <- 57.28998600

# Fix duplicate column names (long and lat etc?)
names(points.info)[names(points.info) %in% c("latitude.x","longitude.x",
                                           "latitude.y", "longitude.y")] <-
  c("latitude","longitude",
    "latitude.nest", "longitude.nest")

str(points.info)

points.info$longitude.nest <- as.numeric(points.info$longitude.nest)
points.info$latitude.nest <- as.numeric(points.info$latitude.nest)


# ** Wind shear calculations -----
# Flight height for calculations (NA for extremes, and 1 m for <0.5 m)



# Altitude values ------

points.info$altitude_callib <- NULL

# For selection of correction values see 'altitude_calibration.R' and
# supplementary document describing altitude calibration, precision,
# and filtering

# Correction for gull UvA: +2.84
gulls.uva <- points.info$device_type == "uva" & points.info$species == "gull"
points.info$altitude_callib[gulls.uva] <- points.info$altitude[gulls.uva] +2.84
# hist(points.info$altitude_callib[gulls.uva], breaks = 1000, xlim = c(-20,100))

# Correction for murre UvA: +1.28
murre.uva <- points.info$device_type == "uva" & points.info$species == "murre"
points.info$altitude_callib[murre.uva] <- points.info$altitude[murre.uva] +1.28
# hist(points.info$altitude_callib[murre.uva], breaks = 1000, xlim = c(-20,100))


# Correction for murre IGU: -0.18
murre.igu <- points.info$device_type == "igu" & points.info$species == "murre"
points.info$altitude_callib[murre.igu] <- points.info$altitude[murre.igu] -0.18
# hist(points.info$altitude_callib[murre.igu], breaks = 1000, xlim = c(-20,100))

# Difference reduced between the two tags, though still significant at p<0.05 with simple t-test
# t.test(points.info$altitude_callib[murre.igu], points.info$altitude_callib[murre.uva])
# t.test(points.info$altitude[murre.igu], points.info$altitude[murre.uva])


# see all altitudes
# hist(points.info$altitude_callib, breaks = 1000, xlim = c(-50,200))
# summary(points.info$altitude_callib)
# extremes_1 <- (quantile(points.info$altitude_callib, c(0.005, 0.995), na.rm = TRUE))
# abline(v=extremes_1, col = "red", lwd = 2)

points.info$altitude_callib_extm <- points.info$altitude_callib
points.info$altitude_callib_extm[points.info$altitude_callib < extremes_1[1]] <- NA
points.info$altitude_callib_extm[points.info$altitude_callib > extremes_1[2]] <- NA


# hist(points.info$altitude_callib_extm, breaks = 100)

# For wind at flight altitude calculations set altitudes below 0.5 to 0.5
points.info$altitude_callib_extm_05 <- points.info$altitude_callib_extm
points.info$altitude_callib_extm_05[points.info$altitude_callib_extm < 0.5] <- 0.5
# hist(points.info$altitude_callib_extm_05)


# Wind speed at flight height -----

# Function to calculate wind speed based on roughness, height, and

# wind speed at 10 m reference altitude
wind.shear <- function(wind10, alt, roughness){
  a <- log(alt/roughness)
  b <- log(10/roughness)
  c <- a/b
  wind10*c  
}
# This equation from: Ragheb, M. (2012). Wind Shear, Roughness Classes and Turbine Energy Production.
# http://mragheb.com/NPRE%20475%20Wind%20Power%20Systems/Wind%20Shear%20Roughness%20Classes%20and%20Turbine%20Energy%20Production.pdf

# For v first

# Wind speed at flight height, 1, 50 m and differnce between (some index of wind gradient) -----
# Wind at flight height:
points.info$ecmwf_wind_10m_v_flt_ht <- wind.shear(points.info$ecmwf_wind_10m_v,
                                                  points.info$altitude_callib_extm_05,
                                                  points.info$ecmwf_surf_roughness)
# 1m
points.info$ecmwf_wind_10m_v_1m <- wind.shear(points.info$ecmwf_wind_10m_v,
                                                  1,
                                                  points.info$ecmwf_surf_roughness)

# 2m
points.info$ecmwf_wind_10m_v_2m <- wind.shear(points.info$ecmwf_wind_10m_v,
                                              2,
                                              points.info$ecmwf_surf_roughness)

# 5m
points.info$ecmwf_wind_10m_v_5m <- wind.shear(points.info$ecmwf_wind_10m_v,
                                              5,
                                              points.info$ecmwf_surf_roughness)

# 50m
points.info$ecmwf_wind_10m_v_50m <- wind.shear(points.info$ecmwf_wind_10m_v,
                                              50,
                                              points.info$ecmwf_surf_roughness)

# wind gradient ratio
points.info$ecmwf_wind_10m_v_gradient_01_50_ratio <- 
  points.info$ecmwf_wind_10m_v_50m/ points.info$ecmwf_wind_10m_v_1m

points.info$ecmwf_wind_10m_v_gradient_01_50_dif <- 
  abs(points.info$ecmwf_wind_10m_v_50m) - abs(points.info$ecmwf_wind_10m_v_1m)


# hist(points.info$ecmwf_wind_10m_v_gradient_01_50_dif)
# hist(points.info$ecmwf_wind_10m_v_gradient_01_50_ratio, breaks = 1000,
#      xlim = c(0,2))

# mean(points.info$ecmwf_surf_roughness)

# For u

points.info$ecmwf_wind_10m_u_flt_ht <- wind.shear(points.info$ecmwf_wind_10m_u,
                                                  points.info$altitude_callib_extm_05,
                                                  points.info$ecmwf_surf_roughness)
# 1m
points.info$ecmwf_wind_10m_u_1m <- wind.shear(points.info$ecmwf_wind_10m_u,
                                              1,
                                              points.info$ecmwf_surf_roughness)

# 2m
points.info$ecmwf_wind_10m_u_2m <- wind.shear(points.info$ecmwf_wind_10m_u,
                                              2,
                                              points.info$ecmwf_surf_roughness)

# 1m
points.info$ecmwf_wind_10m_u_5m <- wind.shear(points.info$ecmwf_wind_10m_u,
                                              5,
                                              points.info$ecmwf_surf_roughness)



# 50m
points.info$ecmwf_wind_10m_u_50m <- wind.shear(points.info$ecmwf_wind_10m_u,
                                               50,
                                               points.info$ecmwf_surf_roughness)

# wind gradient ratio
points.info$ecmwf_wind_10m_u_gradient_01_50_ratio <- 
  points.info$ecmwf_wind_10m_u_50m/ points.info$ecmwf_wind_10m_u_1m

points.info$ecmwf_wind_10m_u_gradient_01_50_dif <- 
  abs(points.info$ecmwf_wind_10m_u_50m) - abs(points.info$ecmwf_wind_10m_u_1m)

# x <- points.info$ecmwf_wind_10m_u_gradient_01_50_dif < -2
# cbind(points.info$ecmwf_wind_10m_u_50m[x], points.info$ecmwf_wind_10m_u_1m[x])

# hist(abs(points.info$ecmwf_wind_10m_u_gradient_01_50_dif))
# hist(points.info$ecmwf_wind_10m_u_gradient_01_50_ratio, breaks = 1000,
#      xlim = c(0,2))

# Calculate scalar wind values and wind direction -----

# Originaly written in file 'wind_dir_speed.R' in R project 'lbbg_gps'
wind.dir.speed <- function(uwind10, vwind10){
  # This function calculates the wind speed and direction based on the u
  # v wind vectors
  
  if(is.na(uwind10) | is.na(vwind10)) return(t(c(NA,NA))) else {
    
    #Wind speed Pythagoras theorem
    wind.speed <- sqrt((uwind10 * uwind10) + (vwind10 * vwind10))
    
    # Calculate direction in radians (0 - 90 deg)
    # dir <- atan(abs(uwind10/ vwind10))
#     
#     u <- c(1,1,-1,-1)
#     v <- c(1,-1,1,-1)
    
    # wind_abs <- sqrt(uwind10^2 + v^2)
    wind_dir_trig_to <- atan2(uwind10/wind.speed, vwind10/wind.speed) 
    wind_dir_trig_to_degrees <- ((wind_dir_trig_to * 180/pi) + 360) %%360 ## -111.6 degrees
    
#     
#     #   atan(1)
#     #   atan(0.5)
#     #   dir <- atan(0.5)
#     #   ?atan
#     # Direction in degrees (0 - 90)
#     dir <- dir * 180 / pi
#     
#     # Make into bearing from North
#     if(uwind10 > 0 && vwind10 < 0){
#       wind.dir <- (180 - dir)
#     }else if(uwind10 < 0 && vwind10 < 0){
#       wind.dir <- (dir + 180)
#     }else if(uwind10 < 0 && vwind10 > 0){
#       wind.dir <- (360 - dir)
#     }else   wind.dir <- (dir)
#     
    wind.dir <- wind_dir_trig_to_degrees
    x <- cbind(wind.speed, wind.dir)
    return(x)
  }
}

# wind 10
wind10 <- t(mapply(wind.dir.speed,
                    points.info$ecmwf_wind_10m_u,
                   points.info$ecmwf_wind_10m_v))
points.info$ecmwf_wind_10m_dir <- wind10[,2]
points.info$ecmwf_wind_10m_speed <- wind10[,1]

# hist(points.info$ecmwf_wind_10m_dir)
# hist(points.info$ecmwf_wind_10m_speed)

# wind flight height
points.info$ecmwf_wind_10m_speed_flt_ht <- t(mapply(wind.dir.speed,
                   points.info$ecmwf_wind_10m_u_flt_ht,
                   points.info$ecmwf_wind_10m_v_flt_ht))[,1]

# Wind 1
points.info$ecmwf_wind_10m_speed_1m <- t(mapply(wind.dir.speed,
                                                    points.info$ecmwf_wind_10m_u_1m,
                                                    points.info$ecmwf_wind_10m_v_1m))[,1]


# Wind 2
points.info$ecmwf_wind_10m_speed_2m <- t(mapply(wind.dir.speed,
                                                points.info$ecmwf_wind_10m_u_2m,
                                                points.info$ecmwf_wind_10m_v_2m))[,1]

# Wind 5
points.info$ecmwf_wind_10m_speed_5m <- t(mapply(wind.dir.speed,
                                                points.info$ecmwf_wind_10m_u_5m,
                                                points.info$ecmwf_wind_10m_v_5m))[,1]


# wind 50
points.info$ecmwf_wind_10m_speed_50m <- t(mapply(wind.dir.speed,
                                                points.info$ecmwf_wind_10m_u_50m,
                                                points.info$ecmwf_wind_10m_v_50m))[,1]


# wind gradient ratio
points.info$ecmwf_wind_10m_speed_gradient_01_50_ratio <- 
  points.info$ecmwf_wind_10m_speed_50m/ points.info$ecmwf_wind_10m_speed_1m

points.info$ecmwf_wind_10m_speed_gradient_01_50_dif <- 
  (points.info$ecmwf_wind_10m_speed_50m) - points.info$ecmwf_wind_10m_speed_1m

# hist(points.info$ecmwf_wind_10m_speed_gradient_01_50_dif)
# hist(points.info$ecmwf_wind_10m_speed_gradient_01_50_ratio, xlim = c(0,10), breaks = 1000)

# **Component calculations -----
calc_hypotenuse <- function(a,b){
  h <- sqrt((a*a) + (b*b))
  return(h)
}

library(CircStats)

# Va and Vg vectors ------
# ?cos
# hist(points.info$speed_2d, xlim = c(0,25), breaks = 1000)
# Vg in u and v directions
points.info$vg_v <- points.info$speed_2d*(cos(rad(points.info$direction_common)))
# hist(points.info$vg_v, xlim = c(-50,50), breaks = 400)

# plot(points.info$direction~points.info$direction_common)

points.info$vg_u <- points.info$speed_2d*(sin(rad(points.info$direction_common)))
# hist(points.info$vg_u, xlim = c(-50,50), breaks = 100)

# Va in u and v directions
points.info$va_v_10m <- points.info$vg_v - points.info$ecmwf_wind_10m_v
# hist(points.info$va_v_10m, xlim = c(-50,50), breaks = 400)

points.info$va_u_10m <- points.info$vg_u - points.info$ecmwf_wind_10m_u


points.info$va_v_1m <- points.info$vg_v - points.info$ecmwf_wind_10m_v_1m
# hist(points.info$va_v_10m, xlim = c(-50,50), breaks = 400)

points.info$va_u_1m <- points.info$vg_u - points.info$ecmwf_wind_10m_u_1m


points.info$va_v_5m <- points.info$vg_v - points.info$ecmwf_wind_10m_v_5m
# hist(points.info$va_v_10m, xlim = c(-50,50), breaks = 400)

points.info$va_u_5m <- points.info$vg_u - points.info$ecmwf_wind_10m_u_5m


points.info$va_v_2m <- points.info$vg_v - points.info$ecmwf_wind_10m_v_2m
# hist(points.info$va_v_10m, xlim = c(-50,50), breaks = 400)

points.info$va_u_2m <- points.info$vg_u - points.info$ecmwf_wind_10m_u_2m


points.info$va_v_flt_ht <- points.info$vg_v - points.info$ecmwf_wind_10m_v_flt_ht
# hist(abs(points.info$va_v_flt_ht), xlim = c(0,50), breaks = 50)

points.info$va_u_flt_ht <- points.info$vg_u - points.info$ecmwf_wind_10m_u_flt_ht
# hist(points.info$va_u_flt_ht, xlim = c(-50,50), breaks = 50)

# Va scalar components
points.info$va_flt_ht <- calc_hypotenuse(points.info$va_u_flt_ht,
                                         points.info$va_v_flt_ht)
points.info$va_10m <- calc_hypotenuse(points.info$va_u_10m,
                                         points.info$va_v_10m)
# hist(points.info$va_10m, xlim = c(0,50), breaks = 1000)
# hist(points.info$va_flt_ht, xlim = c(0,50), breaks = 100)
# sd(points.info$va_10m, na.rm = TRUE)
# sd(points.info$va_flt_ht, na.rm = TRUE)

points.info$va_1m <- calc_hypotenuse(points.info$va_u_1m,
                                      points.info$va_v_1m)
# hist(points.info$va_1m, xlim = c(0,50), breaks = 1000)


points.info$va_2m <- calc_hypotenuse(points.info$va_u_2m,
                                     points.info$va_v_2m)

points.info$va_5m <- calc_hypotenuse(points.info$va_u_5m,
                                     points.info$va_v_5m)

# Va bear
points.info$va_flt_ht_bearing <- t(mapply(wind.dir.speed,
         points.info$va_u_flt_ht,
         points.info$va_v_flt_ht))[,2]

points.info$va_flt_10m_bearing <- t(mapply(wind.dir.speed,
                                          points.info$va_u_10m,
                                          points.info$va_v_10m))[,2]

points.info$va_flt_1m_bearing <- t(mapply(wind.dir.speed,
                                           points.info$va_u_1m,
                                           points.info$va_v_1m))[,2]

points.info$va_flt_2m_bearing <- t(mapply(wind.dir.speed,
                                          points.info$va_u_2m,
                                          points.info$va_v_2m))[,2]

points.info$va_flt_5m_bearing <- t(mapply(wind.dir.speed,
                                          points.info$va_u_5m,
                                          points.info$va_v_5m))[,2]
# hist(points.info$va_flt_1m_bearing)


# Alpha calculation ------

# alpha angle component
solve_alpha <- function(t, h, w){
  # Use law of cosines
  alpha <- acos(((t*t)+(h*h)-(w*w))/(abs(2*h*t)))
  # ?acos
  # Convert to degrees
  alpha <- 180*(alpha)/pi
  
  # Set a sign for alpha
  z <- t-h
  alpha <- sign(z) * alpha
  
    return(alpha)
}



points.info$alpha_flt_ht <- solve_alpha(points.info$speed_2d,
                          points.info$va_flt_ht,
                          points.info$ecmwf_wind_10m_speed_flt_ht)

points.info$alpha_10m <- solve_alpha(points.info$speed_2d,
                                        points.info$va_10m,
                                        points.info$ecmwf_wind_10m_speed)

points.info$alpha_1m <- solve_alpha(points.info$speed_2d,
                                     points.info$va_1m,
                                     points.info$ecmwf_wind_10m_speed_1m)

points.info$alpha_2m <- solve_alpha(points.info$speed_2d,
                                    points.info$va_2m,
                                    points.info$ecmwf_wind_10m_speed_2m)

points.info$alpha_5m <- solve_alpha(points.info$speed_2d,
                                    points.info$va_5m,
                                    points.info$ecmwf_wind_10m_speed_5m)

# hist(points.info$alpha_1m)
# hist((points.info$alpha_flt_ht))
# hist((points.info$alpha_10m))

# Cross wind calculations -----
# wind_angle_dif_10m <- points.info$ecmwf_wind_10m_dir -points.info$va_flt_10m_bearing 
wind_angle_dif_10m <- (points.info$ecmwf_wind_10m_dir -points.info$va_flt_10m_bearing + 360) %% 360 
points.info$wind_angle_dif_10m <- wind_angle_dif_10m


# 
# # RESUME FROM HERE !!!!!!! #####################################
# ang.dif <- function(a,b){
#   # Calculates angle of b with respect to a in clockwise direction
#   x <- b-a
#   if(x < -180){ x <- x+360}else if(x > 180) {x <- x-360}
#   return((x+360)%%360)
# }
# 
# ang.dif(20,300)
# 
# a <- rep(seq(0,355,5),5)
# a <- sample(a, length(a))
# b <- sample(a, length(a))
# x <- mapply(ang.dif, a = a, b = b)
# x2 <- (b -a + 360) %% 360 
# 
# test.x <- x == x2
# all(test.x)
# 
# double difference = secondAngle - firstAngle;
# while (difference < -180) difference += 360;
# while (difference > 180) difference -= 360;
# return difference;
# 
# 
# # hist(wind_angle_dif_10m, breaks = 72)
# 
# # sin(rad(160))
# 
# x <- 10
# #cross
# x*sin(rad(70))
# #tail
# x*cos(rad(70))


points.info$cross_wind_10m <- points.info$ecmwf_wind_10m_speed*sin(rad(wind_angle_dif_10m))

points.info$head_wind_10m <- points.info$ecmwf_wind_10m_speed*cos(rad(wind_angle_dif_10m))

# hist(points.info$cross_wind_10m)
# hist(points.info$head_wind_10m)


# Add for flight height
wind_angle_dif_flt_ht <- (points.info$ecmwf_wind_10m_dir - points.info$va_flt_ht_bearing + 360) %% 360  
# hist(wind_angle_dif_flt_ht, breaks = 72)

points.info$wind_angle_dif_flt_ht <- wind_angle_dif_flt_ht

points.info$cross_wind_flt_ht <- points.info$ecmwf_wind_10m_speed_flt_ht*sin(rad(wind_angle_dif_flt_ht))

points.info$head_wind_flt_ht <- points.info$ecmwf_wind_10m_speed_flt_ht*cos(rad(wind_angle_dif_flt_ht))

# hist(points.info$cross_wind_flt_ht)
# hist(points.info$head_wind_flt_ht)
# 
# i <- 1000
# 
# plot(points.info$ecmwf_wind_10m_u_flt_ht[i]~
#      points.info$ecmwf_wind_10m_v_flt_ht[i],
#      xlim = c(-25,25), ylim = c(-25,25)
# )
# points(
#   points.info$va_u_flt_ht[i]~
#   points.info$va_v_flt_ht[i], col="red"
# )
# points(
#   points.info$vg_u[i]~
#   points.info$vg_v[i], col = "blue"
#   
# )
# points.info$cross_wind_flt_ht[i]
# points.info$head_wind_flt_ht[i]
# grid()
# abline(v=0)
# abline(h=0)

# And relative to track, not heading
wind_angle_dif_track <- ((points.info$ecmwf_wind_10m_dir - points.info$direction_common)+360)%%360 
# hist(wind_angle_dif_track, breaks = 72)
points.info$wind_angle_dif_track <- wind_angle_dif_track

points.info$track_cross_wind_10m <- points.info$ecmwf_wind_10m_speed*sin(rad(wind_angle_dif_track))

points.info$track_head_wind_10m <- points.info$ecmwf_wind_10m_speed*cos(rad(wind_angle_dif_track))

# hist(points.info$track_cross_wind_10m)
# hist(points.info$track_head_wind_10m)

# points.info$ecmwf_wind_10m_speed_1m
# new
points.info$track_cross_wind_1m <- points.info$ecmwf_wind_10m_speed_1m*sin(rad(wind_angle_dif_track))

points.info$track_head_wind_1m <- points.info$ecmwf_wind_10m_speed_1m*cos(rad(wind_angle_dif_track))


points.info$track_cross_wind_2m <- points.info$ecmwf_wind_10m_speed_2m*sin(rad(wind_angle_dif_track))

points.info$track_head_wind_2m <- points.info$ecmwf_wind_10m_speed_2m*cos(rad(wind_angle_dif_track))


points.info$track_cross_wind_5m <- points.info$ecmwf_wind_10m_speed_5m*sin(rad(wind_angle_dif_track))

points.info$track_head_wind_5m <- points.info$ecmwf_wind_10m_speed_5m*cos(rad(wind_angle_dif_track))


# 
# hist(points.info$track_cross_wind_1m)
# hist(points.info$track_head_wind_1m)



# 
# i <- 10000
# 
# plot(points.info$ecmwf_wind_10m_u_flt_ht[i]~
#      points.info$ecmwf_wind_10m_v_flt_ht[i],
#      xlim = c(-25,25), ylim = c(-25,25)
# )
# points(
#   points.info$va_u_flt_ht[i]~
#   points.info$va_v_flt_ht[i], col="red"
# )
# points(
#   points.info$vg_u[i]~
#   points.info$vg_v[i], col = "blue"
#   
# )
# # Relative to heading
# points.info$cross_wind_flt_ht[i]
# points.info$head_wind_flt_ht[i]
# 
# # Relative to track
# (points.info$track_cross_wind_1m[i])
# (points.info$track_head_wind_1m[i])
# grid()
# abline(v=0)
# abline(h=0)




# hist(points.info$track_cross_wind_10m - points.info$track_cross_wind_1m)


# wind_angle_dif_track_flt_ht <- points.info$ecmwf_wind_10m_dir - points.info$va_flt_ht_bearing 


points.info$track_cross_wind_flt_ht <- points.info$ecmwf_wind_10m_speed_flt_ht*sin(rad(wind_angle_dif_track))

points.info$track_head_wind_flt_ht <- points.info$ecmwf_wind_10m_speed_flt_ht*cos(rad(wind_angle_dif_track))

# hist(points.info$track_head_wind_flt_ht)


# Relative to goal ------
# And relative to goal, not heading or heading
# Goal direction
# ?geosphere::bearingRhumb

points.info$goal_dir <- geosphere::bearingRhumb(
  as.matrix(dplyr::select(points.info, longitude, latitude)),
  as.matrix(dplyr::select(points.info, longitude.nest, latitude.nest)))

# hist(points.info$goal_dir)
# hist(points.info$direction_common)
# library(circular)
# goal.circ <- as.circular(points.info$goal_dir, type="directions", units = "degrees",
#                          rotation = "clock")
# plot(goal.circ)
# head(goal.circ)
# head(points.info$goal_dir)
# 
# res25 <- density(goal.circ, bw=25, control.circular=list(units="degrees"))
# plot(res25, shrink=1.2)

wind_angle_dif_goal <- (360 +( points.info$ecmwf_wind_10m_dir - points.info$goal_dir) )%%360
points.info$wind_angle_dif_goal <- wind_angle_dif_goal

# hist(wind_angle_dif_goal[points.info$species == "murre"], breaks = 72)
# hist(((wind_angle_dif_goal + 360) %% 360), breaks = 72)
# hist(((wind_angle_dif_track + 360) %% 360), breaks = 72)
# 
# plot(((wind_angle_dif_goal + 360) %% 360)~((wind_angle_dif_track + 360) %% 360))

# rad(-180)
# rad(180)
points.info$goal_cross_wind_10m <- points.info$ecmwf_wind_10m_speed*sin(rad(wind_angle_dif_goal))

points.info$goal_head_wind_10m <- points.info$ecmwf_wind_10m_speed*cos(rad(wind_angle_dif_goal))

# hist(points.info$goal_cross_wind_10m)
# hist(points.info$goal_head_wind_10m)

# points.info$ecmwf_wind_10m_speed_1m
# new
points.info$goal_cross_wind_1m <- points.info$ecmwf_wind_10m_speed_1m*sin(rad(wind_angle_dif_goal))

points.info$goal_head_wind_1m <- points.info$ecmwf_wind_10m_speed_1m*cos(rad(wind_angle_dif_goal))


points.info$goal_cross_wind_2m <- points.info$ecmwf_wind_10m_speed_2m*sin(rad(wind_angle_dif_goal))

points.info$goal_head_wind_2m <- points.info$ecmwf_wind_10m_speed_2m*cos(rad(wind_angle_dif_goal))


points.info$goal_cross_wind_5m <- points.info$ecmwf_wind_10m_speed_5m*sin(rad(wind_angle_dif_goal))

points.info$goal_head_wind_5m <- points.info$ecmwf_wind_10m_speed_5m*cos(rad(wind_angle_dif_goal))

# # 
# # 
# hist(points.info$goal_cross_wind_1m[points.info$species == "murre"])
# hist(points.info$goal_head_wind_1m[points.info$species == "murre"])
# # 
# hist(points.info$goal_cross_wind_1m[points.info$species == "gull"])
# hist(points.info$goal_head_wind_1m[points.info$species == "gull"])


# hist(points.info$goal_cross_wind_10m - points.info$goal_cross_wind_1m)


# wind_angle_dif_goal_flt_ht <- points.info$ecmwf_wind_10m_dir - points.info$va_flt_ht_bearing 


points.info$goal_cross_wind_flt_ht <- points.info$ecmwf_wind_10m_speed_flt_ht*sin(rad(wind_angle_dif_goal))

points.info$goal_head_wind_flt_ht <- points.info$ecmwf_wind_10m_speed_flt_ht*cos(rad(wind_angle_dif_goal))

# hist(points.info$goal_dir[points.info$species == "murre"])
# hist(points.info$goal_dir[points.info$species == "gull"])
# 
# hist(points.info$direction_common[points.info$species == "murre"])
# hist(points.info$direction_common[points.info$species == "gull"])
# 
# # Why are some track & goal cross-winds at 90 degrees?
# 
# plot(points.info$goal_cross_wind_flt_ht ~
#        points.info$track_cross_wind_flt_ht)
# 
# plot(points.info$goal_head_wind_flt_ht ~
#      points.info$track_head_wind_flt_ht)
# 
# points.info.x <- filter(points.info, goal_cross_wind_flt_ht >5 &
#                           track_cross_wind_flt_ht < -5)
# 
# # plot(points.info.x$goal_cross_wind_flt_ht ~
#        # points.info.x$track_cross_wind_flt_ht)
# # hist(points.info.x$speed_2d)
# 
# points.info.x <- filter(points.info, flight_id_combined == points.info.x$flight_id_combined[i])
# points.info.x$goal_dir
# i <- 10
# 
# points.info.x$goal_cross_wind_flt_ht[i]
# points.info.x$track_cross_wind_flt_ht[i]
# f <- points.info$flight_id_combined == points.info.x$flight_id_combined[i]
# plot(points.info.x$latitude~points.info.x$longitude)
# points(points.info.x$latitude[c(25:29)]~
#        points.info.x$longitude[c(25:29)], col = "red")
# 
# points(points.info.x$latitude[c(2:13)]~points.info.x$longitude[c(2:13)],
#      col = "red")
# points(points.info.x$latitude[5]~points.info.x$longitude[5],
#      col = "red")
# points(points.info.x$latitude[6]~points.info.x$longitude[6],
#        col = "blue")
# 
# plot(points.info.x$goal_dir~
#      points.info.x$direction_common
# )
# all(points.all$latitude == points.info$latitude)
# plot(points.info.x$goal_dir)
# plot(points.info.x$direction_common)
# plot(points.info.x$ecmwf_wind_10m_dir)
# points.info.x$flight_id_combined
# 
# plot(points.info.x$ecmwf_wind_10m_u_flt_ht[i]~
#        points.info.x$ecmwf_wind_10m_v_flt_ht[i],
#      xlim = c(-25,25), ylim = c(-25,25)
# )
# points(
#   points.info.x$va_u_flt_ht[i]~
#     points.info.x$va_v_flt_ht[i], col="red"
# )
# points(
#   points.info.x$vg_u[i]~
#     points.info.x$vg_v[i], col = "blue"
#   
# )
# points.info.x$cross_wind_flt_ht[i]
# points.info.x$head_wind_flt_ht[i]
# grid()
# abline(v=0)
# abline(h=0)
# 


# hist(points.info$goal_head_wind_flt_ht)


# Wind effect -----
points.info$wind_effect_10m <- points.info$speed_2d - points.info$va_10m
points.info$wind_effect_flt_ht <- points.info$speed_2d - points.info$va_flt_ht

# hist(points.info$wind_effect_10m)
# hist(points.info$wind_effect_flt_ht)
# hist(points.info$wind_effect_flt_ht - points.info$wind_effect_10m, breaks = 100)
# mean(points.info$wind_effect_flt_ht - points.info$wind_effect_10m, na.rm = TRUE)
# median(points.info$wind_effect_flt_ht - points.info$wind_effect_10m, na.rm = TRUE)
# 
# m <- points.info$species == "murre"
# g <- points.info$species == "gull"
# hist(points.info$wind_effect_flt_ht[m] - points.info$wind_effect_10m[m], breaks = 100)
# mean(points.info$wind_effect_flt_ht[m] - points.info$wind_effect_10m[m], na.rm = TRUE)
# hist(points.info$wind_effect_flt_ht[g] - points.info$wind_effect_10m[g], breaks = 100)
# mean(points.info$wind_effect_flt_ht[g] - points.info$wind_effect_10m[g], na.rm = TRUE)



# names(points.info)

# Altitude filter ----------
points.info$altitude_filter_included <- TRUE

# IGU (2014 & 2015)
points.info$altitude_filter_included[
  points.info$device_type == "igu"  & !igu_2009 &
  (points.info$MSVs_QCN %in% c(128,136,151,161,163,164,166,
                              176,177,178,179,180,181,183,
                              192,193,194,195,196,198,199,
                              200,202,203))] <- FALSE
# UvA BiTs - Murres
points.info$altitude_filter_included[
  points.info$device_type == "uva"  & points.info$species == "murre" &
    points.info$satellites_used <8] <- FALSE

# UvA BiTs - Gulls - with no satellite info
points.info$altitude_filter_included[
  points.info$device_type == "uva"  & points.info$species == "gull" &
    is.na(points.info$satellites_used) &
    points.info$positiondop >3] <- FALSE

# UvA BiTs - Gulls - with satellite info
points.info$altitude_filter_included[
  points.info$device_type == "uva"  & points.info$species == "gull" &
    !is.na(points.info$satellites_used) &
    points.info$satellites_used <7] <- FALSE

# summary(points.info$altitude_filter_included)


# Output as new table ----
# Save points data without the flight columns (can add those again later if needed by merge)
# names(points.info)
points.detailed <- points.info[,c(1:22,46:ncol(points.info))]

# summary(is.na(points.detailed$alpha_10m))
# summary(is.na(points.detailed$alpha_flt_ht))
# names(points.detailed)[2] <- "device_info_serial"

save(points.detailed, file = "points.detailed.RData")

# Output to csv
write.table(points.detailed, file = "points_detailed.csv", col.names = TRUE,
            row.names = FALSE, sep = ",")

# points.detailed$va_flt_5m_bearing
