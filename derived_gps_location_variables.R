# Calculations based on wind etc to get Va, alpha, cross/ tail wind components,
# and wind at flight height (using wind-shear calculations)

# Load in flight and point data ----
load("flights.RData")
load("points_all.RData")

# Connect point data and flight data ----
# ?merge
points.info <- merge(points.all, flights, by = "flight_id_combined")

# ** Wind shear calculations -----
# Flight height for calculations (NA for extremes, and 1 m for <0.5 m)

# Altitude values ------

points.info$altitude_callib <- NULL

# Correction for gull UvA: +2.820093
gulls.uva <- points.info$device_type == "uva" & points.info$species == "gull"
points.info$altitude_callib[gulls.uva] <- points.info$altitude[gulls.uva] +2.820093
hist(points.info$altitude_callib[gulls.uva], breaks = 1000, xlim = c(-20,100))

# Correction for murre UvA: +1.279747
murre.uva <- points.info$device_type == "uva" & points.info$species == "murre"
points.info$altitude_callib[murre.uva] <- points.info$altitude[murre.uva] +1.279747
hist(points.info$altitude_callib[murre.uva], breaks = 1000, xlim = c(-20,100))


# Correction for murre IGU: -0.07854506
murre.igu <- points.info$device_type == "igu" & points.info$species == "murre"
points.info$altitude_callib[murre.igu] <- points.info$altitude[murre.igu] -0.07854506
hist(points.info$altitude_callib[murre.igu], breaks = 1000, xlim = c(-20,100))

# Difference reduced between the two tags, though still significant at p<0.05 with simple t-test
t.test(points.info$altitude_callib[murre.igu], points.info$altitude_callib[murre.uva])
t.test(points.info$altitude[murre.igu], points.info$altitude[murre.uva])


# see all altitudes
hist(points.info$altitude_callib, breaks = 1000, xlim = c(-50,200))
summary(points.info$altitude_callib)
extremes_1 <- (quantile(points.info$altitude_callib, c(0.005, 0.995)))
abline(v=extremes_1, col = "red", lwd = 2)

points.info$altitude_callib_extm <- points.info$altitude_callib
points.info$altitude_callib_extm[points.info$altitude_callib < extremes_1[1]] <- NA
points.info$altitude_callib_extm[points.info$altitude_callib > extremes_1[2]] <- NA

hist(points.info$altitude_callib_extm)

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
  points.info$ecmwf_wind_10m_v_50m - points.info$ecmwf_wind_10m_v_1m


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
  points.info$ecmwf_wind_10m_u_50m - points.info$ecmwf_wind_10m_u_1m


hist(points.info$ecmwf_wind_10m_u_gradient_01_50_dif)
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


# **Component calculations -----
calc_hypotenuse <- function(a,b){
  h <- sqrt((a*a) + (b*b))
}

library(CircStats)

# Va and Vg vectors ------
# ?cos
# Vg in u and v directions
points.info$vg_v <- points.info$speed_2d*(cos(rad(points.info$direction)))
hist(points.info$vg_v, xlim = c(-50,50), breaks = 100)

points.info$vg_u <- points.info$speed_2d*(sin(rad(points.info$direction)))
hist(points.info$vg_u, xlim = c(-50,50), breaks = 100)

# Va in u and v directions
points.info$va_v_10m <- points.info$vg_v - points.info$ecmwf_wind_10m_v
hist(points.info$va_v_10m, xlim = c(-50,50), breaks = 400)

points.info$va_u_10m <- points.info$vg_u - points.info$ecmwf_wind_10m_u

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

# Va bear
points.info$va_flt_ht_bearing <- t(mapply(wind.dir.speed,
         points.info$va_u_flt_ht,
         points.info$va_v_flt_ht))[,2]

points.info$va_flt_10m_bearing <- t(mapply(wind.dir.speed,
                                          points.info$va_u_10m,
                                          points.info$va_v_10m))[,2]

# Alpha calculation ------

# alpha angle component
solve_alpha <- function(t, h, w){
  # Use law of cosines
  alpha <- acos(-1*(((w*w)-(t*t)-(h*h))/(2*h*t)))
  
  # Set a sign for alpha
  z <- t-h
  alpha <- sign(z) * alpha
  
    return(alpha)
}

# ?sign()

points.info$alpha_flt_ht <- solve_alpha(points.info$speed_2d,
                          points.info$va_flt_ht,
                          points.info$ecmwf_wind_10m_v_flt_ht)

points.info$alpha_10m <- solve_alpha(points.info$speed_2d,
                                        points.info$va_flt_ht,
                                        points.info$ecmwf_wind_10m_v)

hist(deg(alpha_test))

# Cross wind calculations -----
wind_angle_dif_10m <- points.info$va_flt_10m_bearing - points.info$ecmwf_wind_10m_dir
hist(wind_angle_dif_10m, breaks = 72)

points.info$cross_wind_10m <- points.info$ecmwf_wind_10m_speed*cos(rad(wind_angle_dif_10m))

points.info$head_wind_10m <- points.info$ecmwf_wind_10m_speed*sin(rad(wind_angle_dif_10m))

hist(points.info$cross_wind_10m)
hist(points.info$head_wind_10m)


# Add for flight height
wind_angle_dif_flt_ht <- points.info$va_flt_ht_bearing - points.info$ecmwf_wind_10m_dir
hist(wind_angle_dif_flt_ht, breaks = 72)

points.info$cross_wind_flt_ht <- points.info$ecmwf_wind_10m_speed_flt_ht*cos(rad(wind_angle_dif_flt_ht))

points.info$head_wind_flt_ht <- points.info$ecmwf_wind_10m_speed_flt_ht*sin(rad(wind_angle_dif_flt_ht))

hist(points.info$cross_wind_flt_ht)
hist(points.info$head_wind_flt_ht)


# And relative to track, not heading
#****** something wrong with 'direction' column currently - need to check before proceeding!
wind_angle_dif_track <- points.info$ - points.info$ecmwf_wind_10m_dir
hist(wind_angle_dif_track, breaks = 72)

x <- wind_angle_dif_track < -360

cbind(points.info$direction[x], points.info$ecmwf_wind_10m_dir[x])

hist(points.info$direction)

points.info$cross_wind_10m <- points.info$ecmwf_wind_10m_speed*cos(rad(wind_angle_dif_10m))

points.info$head_wind_10m <- points.info$ecmwf_wind_10m_speed*sin(rad(wind_angle_dif_10m))

hist(points.info$cross_wind_10m)
hist(points.info$head_wind_10m)

