# Calculations based on wind etc to get Va, alpha, cross/ tail wind components,
# and wind at flight height (using wind-shear calculations)

# Load in flight and point data ----
load("flights.RData")
load("points_all.RData")

# Connect point data and flight data ----
# ?merge
points.info <- merge(points.all, flights, by = "flight_id_combined")

# Wind shear calculations -----
# Flight height for calculations (NA for extremes, and 1 m for <0.5 m)

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


# Wind speed at flight height

# Wind speed at 5 and 50 m and differnce between (some index of wind gradient)