# Calculations based on wind etc to get Va, alpha, cross/ tail wind components,
# and wind at flight height (using wind-shear calculations)

# Load in flight and point data ----
load("flights.RData")
load("points_all.RData")


# Connect point data and flight data ----
# ?merge
points.info <- merge(points.all, flights, by = "flight_id_combined")


# Create new direction column, with common format -----
hist(points.info$direction[points.info$device_type == "uva"])
hist(points.info$direction[points.info$device_type == "igu"])
summary(is.na(points.info$direction[points.info$device_type == "igu"]))


points.info$direction_common <- points.info$direction

# Correct UvA to 0 - 360 range
points.info$direction_common[points.info$device_type == "uva"] <- 
  points.info$direction[points.info$device_type == "uva"] %% 360
hist(points.info$direction_common)


# Calculate bearings for 2009 IGU data

igu_2009 <- points.info$date_time < as.POSIXct("2010-01-01 00:00:00", tz = "UTC")
summary(igu_2009)

library(fossil)

n <- sum(igu_2009)
dir.igu <- earth.bear(points.info$longitude[igu_2009][-n],
                      points.info$latitude[igu_2009][-n],
                      points.info$longitude[igu_2009][-1],
                      points.info$latitude[igu_2009][-1]
                      )
dir.igu.2009 <- c(dir.igu, NA)
hist(dir.igu.2009)

points.info$direction_common[igu_2009] <- dir.igu.2009

flights.murres <- unique(points.info$flight_id_combined[igu_2009])

for(i in 1:length(flights.murres)){
  np <- length(points.info$direction_common[points.info$flight_id_combined == flights.murres[i]])
  points.info$direction_common[points.info$flight_id_combined == flights.murres[i]][np] <- NA
}

hist(points.info$direction_common)
summary(is.na(points.info$direction_common))

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
hist(points.info$altitude_callib[gulls.uva], breaks = 1000, xlim = c(-20,100))

# Correction for murre UvA: +1.28
murre.uva <- points.info$device_type == "uva" & points.info$species == "murre"
points.info$altitude_callib[murre.uva] <- points.info$altitude[murre.uva] +1.28
hist(points.info$altitude_callib[murre.uva], breaks = 1000, xlim = c(-20,100))


# Correction for murre IGU: -0.18
murre.igu <- points.info$device_type == "igu" & points.info$species == "murre"
points.info$altitude_callib[murre.igu] <- points.info$altitude[murre.igu] -0.18
hist(points.info$altitude_callib[murre.igu], breaks = 1000, xlim = c(-20,100))

# Difference reduced between the two tags, though still significant at p<0.05 with simple t-test
t.test(points.info$altitude_callib[murre.igu], points.info$altitude_callib[murre.uva])
t.test(points.info$altitude[murre.igu], points.info$altitude[murre.uva])


# see all altitudes
hist(points.info$altitude_callib, breaks = 1000, xlim = c(-50,200))
summary(points.info$altitude_callib)
extremes_1 <- (quantile(points.info$altitude_callib, c(0.005, 0.995), na.rm = TRUE))
abline(v=extremes_1, col = "red", lwd = 2)

points.info$altitude_callib_extm <- points.info$altitude_callib
points.info$altitude_callib_extm[points.info$altitude_callib < extremes_1[1]] <- NA
points.info$altitude_callib_extm[points.info$altitude_callib > extremes_1[2]] <- NA


hist(points.info$altitude_callib_extm, breaks = 100)

# For wind at flight altitude calculations set altitudes below 0.5 to 0.5
points.info$altitude_callib_extm_05 <- points.info$altitude_callib_extm
points.info$altitude_callib_extm_05[points.info$altitude_callib_extm < 0.5] <- 0.5
hist(points.info$altitude_callib_extm_05)


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

# 50m
points.info$ecmwf_wind_10m_v_50m <- wind.shear(points.info$ecmwf_wind_10m_v,
                                              50,
                                              points.info$ecmwf_surf_roughness)

# wind gradient ratio
points.info$ecmwf_wind_10m_v_gradient_01_50_ratio <- 
  points.info$ecmwf_wind_10m_v_50m/ points.info$ecmwf_wind_10m_v_1m

points.info$ecmwf_wind_10m_v_gradient_01_50_dif <- 
  abs(points.info$ecmwf_wind_10m_v_50m) - abs(points.info$ecmwf_wind_10m_v_1m)


hist(points.info$ecmwf_wind_10m_v_gradient_01_50_dif)
hist(points.info$ecmwf_wind_10m_v_gradient_01_50_ratio, breaks = 1000,
     xlim = c(0,2))

# mean(points.info$ecmwf_surf_roughness)

# For u

points.info$ecmwf_wind_10m_u_flt_ht <- wind.shear(points.info$ecmwf_wind_10m_u,
                                                  points.info$altitude_callib_extm_05,
                                                  points.info$ecmwf_surf_roughness)
# 1m
points.info$ecmwf_wind_10m_u_1m <- wind.shear(points.info$ecmwf_wind_10m_u,
                                              1,
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

hist(abs(points.info$ecmwf_wind_10m_u_gradient_01_50_dif))
hist(points.info$ecmwf_wind_10m_u_gradient_01_50_ratio, breaks = 1000,
     xlim = c(0,2))

# Calculate scalar wind values and wind direction -----

# Originaly written in file 'wind_dir_speed.R' in R project 'lbbg_gps'
wind.dir.speed <- function(uwind10, vwind10){
  # This function calculates the wind speed and direction based on the u
  # v wind vectors
  
  if(is.na(uwind10) | is.na(vwind10)) return(t(c(NA,NA))) else {
    
    #Wind speed Pythagoras theorem
    wind.speed <- sqrt((uwind10 * uwind10) + (vwind10 * vwind10))
    
    # Calculate direction in radians (0 - 90 deg)
    dir <- atan(abs(uwind10/ vwind10))
    
    #   atan(1)
    #   atan(0.5)
    #   dir <- atan(0.5)
    #   ?atan
    # Direction in degrees (0 - 90)
    dir <- dir * 180 / pi
    
    # Make into bearing from North
    if(uwind10 > 0 && vwind10 < 0){
      wind.dir <- (180 - dir)
    }else if(uwind10 < 0 && vwind10 < 0){
      wind.dir <- (dir + 180)
    }else if(uwind10 < 0 && vwind10 > 0){
      wind.dir <- (360 - dir)
    }else   wind.dir <- (dir)
    
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

hist(points.info$ecmwf_wind_10m_dir)
hist(points.info$ecmwf_wind_10m_speed)

# wind flight height
points.info$ecmwf_wind_10m_speed_flt_ht <- t(mapply(wind.dir.speed,
                   points.info$ecmwf_wind_10m_u_flt_ht,
                   points.info$ecmwf_wind_10m_v_flt_ht))[,1]

# Wind 1
points.info$ecmwf_wind_10m_speed_1m <- t(mapply(wind.dir.speed,
                                                    points.info$ecmwf_wind_10m_u_1m,
                                                    points.info$ecmwf_wind_10m_v_1m))[,1]

# wind 50
points.info$ecmwf_wind_10m_speed_50m <- t(mapply(wind.dir.speed,
                                                points.info$ecmwf_wind_10m_u_50m,
                                                points.info$ecmwf_wind_10m_v_50m))[,1]


# wind gradient ratio
points.info$ecmwf_wind_10m_speed_gradient_01_50_ratio <- 
  points.info$ecmwf_wind_10m_speed_50m/ points.info$ecmwf_wind_10m_speed_1m

points.info$ecmwf_wind_10m_speed_gradient_01_50_dif <- 
  (points.info$ecmwf_wind_10m_speed_50m) - points.info$ecmwf_wind_10m_speed_1m

hist(points.info$ecmwf_wind_10m_speed_gradient_01_50_dif)
hist(points.info$ecmwf_wind_10m_speed_gradient_01_50_ratio, xlim = c(0,10), breaks = 1000)

# **Component calculations -----
calc_hypotenuse <- function(a,b){
  h <- sqrt((a*a) + (b*b))
  return(h)
}

library(CircStats)

# Va and Vg vectors ------
# ?cos
hist(points.info$speed_2d, xlim = c(0,25), breaks = 1000)
# Vg in u and v directions
points.info$vg_v <- points.info$speed_2d*(cos(rad(points.info$direction_common)))
hist(points.info$vg_v, xlim = c(-50,50), breaks = 400)

# 
# a1 <- points.info$speed_2d[test.na][2]*(cos(rad(points.info$direction_common[test.na][2])))
# 
# b1 <- points.info$speed_2d[test.na][2]*(sin(rad(points.info$direction_common[test.na][2])))
# 
# calc_hypotenuse(a1,b1)

# (cos(rad(c(0,45,90,270,180))))

points.info$vg_u <- points.info$speed_2d*(sin(rad(points.info$direction_common)))
hist(points.info$vg_u, xlim = c(-50,50), breaks = 100)

# Va in u and v directions
points.info$va_v_10m <- points.info$vg_v - points.info$ecmwf_wind_10m_v
hist(points.info$va_v_10m, xlim = c(-50,50), breaks = 400)

points.info$va_u_10m <- points.info$vg_u - points.info$ecmwf_wind_10m_u


points.info$va_v_1m <- points.info$vg_v - points.info$ecmwf_wind_10m_v_1m
# hist(points.info$va_v_10m, xlim = c(-50,50), breaks = 400)

points.info$va_u_1m <- points.info$vg_u - points.info$ecmwf_wind_10m_u_1m


points.info$va_v_flt_ht <- points.info$vg_v - points.info$ecmwf_wind_10m_v_flt_ht
hist(points.info$va_v_flt_ht, xlim = c(-50,50), breaks = 50)

points.info$va_u_flt_ht <- points.info$vg_u - points.info$ecmwf_wind_10m_u_flt_ht
hist(points.info$va_u_flt_ht, xlim = c(-50,50), breaks = 50)

# Va scalar components
points.info$va_flt_ht <- calc_hypotenuse(points.info$va_u_flt_ht,
                                         points.info$va_v_flt_ht)
points.info$va_10m <- calc_hypotenuse(points.info$va_u_10m,
                                         points.info$va_v_10m)
hist(points.info$va_10m, xlim = c(0,50), breaks = 1000)
hist(points.info$va_flt_ht, xlim = c(0,50), breaks = 100)


points.info$va_1m <- calc_hypotenuse(points.info$va_u_1m,
                                      points.info$va_v_1m)
hist(points.info$va_1m, xlim = c(0,50), breaks = 1000)

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

hist(points.info$alpha_1m)
hist((points.info$alpha_flt_ht))
hist((points.info$alpha_10m))

# Cross wind calculations -----
wind_angle_dif_10m <- points.info$ecmwf_wind_10m_dir -points.info$va_flt_10m_bearing 
hist(wind_angle_dif_10m, breaks = 72)

points.info$cross_wind_10m <- points.info$ecmwf_wind_10m_speed*sin(rad(wind_angle_dif_10m))

points.info$head_wind_10m <- points.info$ecmwf_wind_10m_speed*cos(rad(wind_angle_dif_10m))

hist(points.info$cross_wind_10m)
hist(points.info$head_wind_10m)


# Add for flight height
wind_angle_dif_flt_ht <- points.info$ecmwf_wind_10m_dir - points.info$va_flt_ht_bearing 
hist(wind_angle_dif_flt_ht, breaks = 72)

points.info$cross_wind_flt_ht <- points.info$ecmwf_wind_10m_speed_flt_ht*sin(rad(wind_angle_dif_flt_ht))

points.info$head_wind_flt_ht <- points.info$ecmwf_wind_10m_speed_flt_ht*cos(rad(wind_angle_dif_flt_ht))

hist(points.info$cross_wind_flt_ht)
hist(points.info$head_wind_flt_ht)


# And relative to track, not heading
wind_angle_dif_track <- points.info$ecmwf_wind_10m_dir - points.info$direction_common 
hist(wind_angle_dif_track, breaks = 72)


points.info$track_cross_wind_10m <- points.info$ecmwf_wind_10m_speed*sin(rad(wind_angle_dif_track))

points.info$track_head_wind_10m <- points.info$ecmwf_wind_10m_speed*cos(rad(wind_angle_dif_track))

hist(points.info$track_cross_wind_10m)
hist(points.info$track_head_wind_10m)

# points.info$ecmwf_wind_10m_speed_1m
# new
points.info$track_cross_wind_1m <- points.info$ecmwf_wind_10m_speed_1m*sin(rad(wind_angle_dif_track))

points.info$track_head_wind_1m <- points.info$ecmwf_wind_10m_speed_1m*cos(rad(wind_angle_dif_track))

hist(points.info$track_cross_wind_1m)
hist(points.info$track_head_wind_1m)

# hist(points.info$track_cross_wind_10m - points.info$track_cross_wind_1m)


# wind_angle_dif_track_flt_ht <- points.info$ecmwf_wind_10m_dir - points.info$va_flt_ht_bearing 


points.info$track_cross_wind_flt_ht <- points.info$ecmwf_wind_10m_speed_flt_ht*sin(rad(wind_angle_dif_track))

points.info$track_head_wind_flt_ht <- points.info$ecmwf_wind_10m_speed_flt_ht*cos(rad(wind_angle_dif_track))

hist(points.info$track_head_wind_flt_ht)


# Wind effect -----
points.info$wind_effect_10m <- points.info$speed_2d - points.info$va_10m
points.info$wind_effect_flt_ht <- points.info$speed_2d - points.info$va_flt_ht

hist(points.info$wind_effect_10m)
hist(points.info$wind_effect_flt_ht)
hist(points.info$wind_effect_flt_ht - points.info$wind_effect_10m, breaks = 100)
mean(points.info$wind_effect_flt_ht - points.info$wind_effect_10m, na.rm = TRUE)
median(points.info$wind_effect_flt_ht - points.info$wind_effect_10m, na.rm = TRUE)

m <- points.info$species == "murre"
g <- points.info$species == "gull"
hist(points.info$wind_effect_flt_ht[m] - points.info$wind_effect_10m[m], breaks = 100)
mean(points.info$wind_effect_flt_ht[m] - points.info$wind_effect_10m[m], na.rm = TRUE)
hist(points.info$wind_effect_flt_ht[g] - points.info$wind_effect_10m[g], breaks = 100)
mean(points.info$wind_effect_flt_ht[g] - points.info$wind_effect_10m[g], na.rm = TRUE)



names(points.info)

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

summary(points.info$altitude_filter_included)


# Output as new table ----
# Save points data without the flight columns (can add those again later if needed by merge)
# names(points.info)
points.detailed <- points.info[,c(1:22,46:96)]

# summary(is.na(points.detailed$alpha_10m))
# summary(is.na(points.detailed$alpha_flt_ht))
names(points.detailed)[2] <- "device_info_serial"

save(points.detailed, file = "points.detailed.RData")

# Output to csv
write.table(points.detailed, file = "points_detailed.csv", col.names = TRUE,
            row.names = FALSE, sep = ",")
