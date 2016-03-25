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
sql_query <- paste("SELECT gps_ee_tracking_speed_limited.device_info_serial, gps_ee_tracking_speed_limited.date_time, gps_ee_tracking_speed_limited.latitude, gps_ee_tracking_speed_limited.longitude, gps_ee_tracking_speed_limited.altitude, gps_ee_tracking_speed_limited.h_accuracy, gps_ee_tracking_speed_limited.v_accuracy, gps_ee_tracking_speed_limited.satellites_used, gps_ee_tracking_speed_limited.positiondop
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
sql_query <- paste("SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_gps_points_igu.elev, guillemots_gps_points_igu.ehpe, guillemots_gps_points_igu.timeout, guillemots_gps_points_igu.MSVs_QCN, guillemots_gps_points_igu.sat_n
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





# ***** Altitude precision estimation -------

plot_quantile <- function(x, main = ""){
  quantiles <- quantile(abs(x), seq(0.01,0.99,0.01), na.rm = TRUE)
  plot(c(1:99)~quantiles, xlab = "Altitude error (m)", ylab = "Proportion of locations  (%)",
       type = "l", las = 1,
       main = main)
  abline(v = quantiles[c(50,90,95)], lwd = 2, lty = 2, col = "dark grey")
  text(quantiles[c(50,90,95)], 5,
       paste("<",format(round(quantiles[c(50,90,95)], 1), nsmall = 1), "m"),
       col = "grey20")
  text(quantiles[c(50,90,95)], 10, c("50 %", "90 %", "95 %"),
       col = "grey20")
  
  perc_val <- c((c(1:99)[quantiles>5][1]), (c(1:99)[quantiles>10][1]))
  abline(h = perc_val, lwd = 2, lty = 2, col = "red")
  text(quantiles[97],perc_val+3, paste(perc_val, "%  <", c(5,10), "m"),
       col = "red")
}

# plot(x_n~x_max)

# UvA gulls tags ----
hist(points.uva.gulls$altitude[points.uva.gulls$altitude > -50 & points.uva.gulls$altitude < 150], breaks = 1000, xlim = c(-50,50))
abline(v = c(median(points.uva.gulls$altitude, na.rm = TRUE), mean(points.uva.gulls$altitude, na.rm = TRUE)), col = c("red", "blue"))
extremes <- points.uva.gulls$altitude < -100 | points.uva.gulls$altitude > 100
summary(extremes)
abline(v = c(median(points.uva.gulls$altitude[!extremes], na.rm = TRUE), mean(points.uva.gulls$altitude[!extremes], na.rm = TRUE)), col = c("red", "blue"), lty = 2)
summary(points.uva.gulls$altitude - mean(points.uva.gulls$altitude, na.rm = TRUE))


# Re-centred on zero
gull.uva.alt <- points.uva.gulls$altitude
extremes <- points.uva.gulls$altitude < -100 | points.uva.gulls$altitude > 100
gull.uva.alt.0 <- gull.uva.alt - mean(gull.uva.alt[!extremes], na.rm = TRUE)
hist(gull.uva.alt.0, breaks = 1000, xlim = c(-50,50))
# range(gull.uva.alt.0, na.rm = TRUE)
plot_quantile(gull.uva.alt.0, main = "UvA GPS tags - gulls")


# UvA murre tags -----
hist(points.uva.murres$altitude[points.uva.murres$altitude > -50 & points.uva.murres$altitude < 150], breaks = 1000, xlim = c(-50,50))
abline(v = c(median(points.uva.murres$altitude, na.rm = TRUE), mean(points.uva.murres$altitude, na.rm = TRUE)), col = c("red", "blue"))
extremes <- points.uva.murres$altitude < -100 | points.uva.murres$altitude > 100
summary(extremes)
abline(v = c(median(points.uva.murres$altitude[!extremes], na.rm = TRUE), mean(points.uva.murres$altitude[!extremes], na.rm = TRUE)), col = c("red", "blue"), lty = 2)
summary(points.uva.murres$altitude - mean(points.uva.murres$altitude, na.rm = TRUE))


# Re-centred on zero
murre.uva.alt <- points.uva.murres$altitude
extremes <- points.uva.murres$altitude < -100 | points.uva.murres$altitude > 100
murre.uva.alt.0 <- gull.uva.alt - mean(murre.uva.alt[!extremes], na.rm = TRUE)
hist(murre.uva.alt.0, breaks = 1000, xlim = c(-50,50))
# range(gull.uva.alt.0, na.rm = TRUE)
plot_quantile(murre.uva.alt.0, main = "UvA GPS tags - murres")



# Igu murre tags -----
hist(points.igu$elev[points.igu$elev > -50 & points.igu$elev < 150], breaks = 100, xlim = c(-50,50))
abline(v = c(median(points.igu$elev, na.rm = TRUE), mean(points.igu$elev, na.rm = TRUE)), col = c("red", "blue"))
extremes <- points.igu$elev < -100 | points.igu$elev > 100
summary(extremes)
abline(v = c(median(points.igu$elev[!extremes], na.rm = TRUE), mean(points.igu$elev[!extremes], na.rm = TRUE)), col = c("red", "blue"), lty = 2)
summary(points.igu$elev - mean(points.igu$elev, na.rm = TRUE))


# Re-centred on zero
murre.igu.alt <- points.igu$elev
extremes <- points.igu$elev < -100 | points.igu$elev > 100
murre.igu.alt.0 <- murre.igu.alt - mean(murre.igu.alt[!extremes], na.rm = TRUE)
hist(murre.uva.alt.0, breaks = 1000, xlim = c(-50,50))
# range(gull.uva.alt.0, na.rm = TRUE)
plot_quantile(murre.igu.alt.0, main = "IGU GPS tags - murres")
# 
# pdf("Surface_location_error_cumulation_plots.pdf")
# dev.off()



# ** Filtering altitude by quality ----
# See what variables we have currently with GPS assembled df
load("points.detailed.RData")


# Filter IGU locations
hist(points.igu$ehpe, xlim = c(0,200), breaks = 100)

# Remove points from 2009 (where additional variables are unavailable)
igu_2009 <- points.igu$date_time < as.POSIXct("2010-01-01 00:00:00", tz = "UTC")

points.igu <- points.igu[!igu_2009,]

murre.igu.alt <- points.igu$elev
extremes <- points.igu$elev < -100 | points.igu$elev > 100
murre.igu.alt.0 <- murre.igu.alt - mean(murre.igu.alt[!extremes], na.rm = TRUE)


error.df <- cbind.data.frame("ehpe_lim" = numeric(0), "p50" = numeric(0),
                             "p75" = numeric(0), "p95" = numeric(0),
                             "n" = numeric(0), "prop" = numeric(0))
ehpe_crit <- 0
for(i in 1:500){
  error.df[i,c(2:4)] <- quantile(abs(murre.igu.alt.0[points.igu$ehpe < ehpe_crit]), c(0.5,0.75, 0.95), na.rm = TRUE)
  error.df[i,1] <- ehpe_crit
  error.df[i,5] <- sum(points.igu$ehpe < ehpe_crit, na.rm = TRUE)
  error.df[i,6] <- error.df[i,5]/length(murre.igu.alt.0[!is.na(murre.igu.alt.0)])
  ehpe_crit <- ehpe_crit + 0.2
}

par(mfrow=c(2,1))
plot(error.df$p75~error.df$ehpe_lim, ylim = c(0,100), type = "l")
plot(error.df$prop~error.df$ehpe_lim, type = "l")

# summary(points.igu$ehpe < 6.2)



p_obs <- NULL
q95_error <- NULL
n <- length(murre.igu.alt.0)
v_ac <- c(1:100)
for(i in 1:100){
  p_obs[i] <- sum(points.igu$ehpe < v_ac[i], na.rm = TRUE)/n
  q95_error[i] <- quantile(abs(murre.igu.alt.0[points.igu$ehpe < v_ac[i]]), 0.95, na.rm = TRUE)
}
par(mfrow = c(2,1))
plot(q95_error~v_ac, ylim = c(0,100))
plot(p_obs~v_ac)



# Satellites
hist(points.igu$sat_n, breaks = 100)
error.df <- cbind.data.frame("sat_n" = numeric(0), "p50" = numeric(0),
                             "p75" = numeric(0), "p95" = numeric(0),
                             "n" = numeric(0), "prop" = numeric(0))
sat_n_test <- 0
for(i in 1:13){
  error.df[i,c(2:4)] <- quantile(abs(murre.igu.alt.0[points.igu$sat_n >= sat_n_test]), c(0.5,0.75, 0.95), na.rm = TRUE)
  error.df[i,1] <- sat_n_test
  # ehpe_crit <- ehpe_crit + 0.2
  error.df[i,5] <- sum(points.igu$sat_n >= sat_n_test, na.rm = TRUE)
  error.df[i,6] <- error.df[i,5]/length(murre.igu.alt.0[!is.na(murre.igu.alt.0)])
  sat_n_test <- sat_n_test + 1
}

par(mfrow=c(2,1))
plot(error.df$p95~error.df$sat_n, ylim = c(0,100), type = "l")
plot(error.df$prop~error.df$sat_n, type = "l")

boxplot(abs(murre.igu.alt.0)~points.igu$sat_n, ylim = c(0,30))



boxplot(abs(murre.igu.alt.0)~points.igu$MSVs_QCN, ylim = c(0,30))


plot(abs(murre.igu.alt.0)~points.igu$sat_n, ylim = c(0,50))
plot(abs(murre.igu.alt.0)~points.igu$timeout, ylim = c(0,50))

hist(abs(murre.igu.alt.0), xlim = c(0,100), breaks = 10000)
plot(ecdf(abs(murre.igu.alt.0)), xlim = c(0,100))



plot(as.factor(points.igu$sat_n))



# MSVs_QCN thing
hist(points.igu$MSVs_QCN, breaks = 100)
error.df <- cbind.data.frame("MSVs_QCN" = numeric(0), "p50" = numeric(0),
                             "p75" = numeric(0), "p95" = numeric(0),
                             "n" = numeric(0), "prop" = numeric(0))
MSVs_QCN_levels <- unique(points.igu$MSVs_QCN)
for(i in 1:length(MSVs_QCN_levels)){
  MSVs_QCN_val <- MSVs_QCN_levels[i]
  error.df[i,c(2:4)] <- quantile(abs(murre.igu.alt.0[points.igu$MSVs_QCN == MSVs_QCN_val]), c(0.5,0.75, 0.95), na.rm = TRUE)
  error.df[i,1] <- MSVs_QCN_val
  # ehpe_crit <- ehpe_crit + 0.2
  error.df[i,5] <- sum(points.igu$MSVs_QCN == MSVs_QCN_val, na.rm = TRUE)
  error.df[i,6] <- error.df[i,5]/length(murre.igu.alt.0[!is.na(murre.igu.alt.0)])
  # sat_n_test <- sat_n_test + 1
}

par(mfrow=c(2,1))
plot(log(error.df$p95)~error.df$MSVs_QCN)
plot(error.df$prop~error.df$MSVs_QCN)

f <- (error.df$p75) < 8
fp <- points.igu$MSVs_QCN %in% MSVs_QCN_levels[f]

summary(fp)

par(mfrow=c(1,1))
plot_quantile(murre.igu.alt.0[fp])


pdf("igu_gps_MSVs_QCN_effect.pdf")
plot_quantile(murre.igu.alt.0, main = "All")
plot_quantile(murre.igu.alt.0[fp], main = "MSVs_QCN filter")
dev.off()






pdf("igu_gps_ehpe_effect.pdf")
plot_quantile(murre.igu.alt.0[points.igu$ehpe < 100], main = "ehpe <100")
plot_quantile(murre.igu.alt.0[points.igu$ehpe < 50], main = "ehpe <50")
plot_quantile(murre.igu.alt.0[points.igu$ehpe < 40], main = "ehpe <40")
plot_quantile(murre.igu.alt.0[points.igu$ehpe < 35], main = "ehpe <35")
plot_quantile(murre.igu.alt.0[points.igu$ehpe < 30], main = "ehpe <30")
plot_quantile(murre.igu.alt.0[points.igu$ehpe < 25], main = "ehpe <25")
plot_quantile(murre.igu.alt.0[points.igu$ehpe < 20], main = "ehpe <20")
plot_quantile(murre.igu.alt.0[points.igu$ehpe < 15], main = "ehpe <15")
dev.off()




pdf("igu_gps_sat_n_effect.pdf")
plot_quantile(murre.igu.alt.0[points.igu$sat_n >= 3], main = "N sat >= 3")
plot_quantile(murre.igu.alt.0[points.igu$sat_n >= 4], main = "N sat >= 4")
plot_quantile(murre.igu.alt.0[points.igu$sat_n >= 5], main = "N sat >= 5")
plot_quantile(murre.igu.alt.0[points.igu$sat_n >= 6], main = "N sat >= 6")
plot_quantile(murre.igu.alt.0[points.igu$sat_n >= 7], main = "N sat >= 7")
plot_quantile(murre.igu.alt.0[points.igu$sat_n >= 8], main = "N sat >= 8")
plot_quantile(murre.igu.alt.0[points.igu$sat_n >= 9], main = "N sat >= 9")
plot_quantile(murre.igu.alt.0[points.igu$sat_n >= 10], main = "N sat >= 10")

dev.off()



# Error on uva gull tags

hist(points.uva.gulls$v_accuracy, breaks = 100)


pdf("uva_gulls_v_accuracy_effect.pdf")
plot_quantile(gull.uva.alt.0[points.uva.gulls$v_accuracy < 100], main = "v_accuracy <100")
plot_quantile(gull.uva.alt.0[points.uva.gulls$v_accuracy < 50], main = "v_accuracy <50")
plot_quantile(gull.uva.alt.0[points.uva.gulls$v_accuracy < 40], main = "v_accuracy <40")
plot_quantile(gull.uva.alt.0[points.uva.gulls$v_accuracy < 30], main = "v_accuracy <30")
plot_quantile(gull.uva.alt.0[points.uva.gulls$v_accuracy < 20], main = "v_accuracy <20")
plot_quantile(gull.uva.alt.0[points.uva.gulls$v_accuracy < 10], main = "v_accuracy <10")
plot_quantile(gull.uva.alt.0[points.uva.gulls$v_accuracy < 7.5], main = "v_accuracy <7.5")
plot_quantile(gull.uva.alt.0[points.uva.gulls$v_accuracy < 5], main = "v_accuracy <5")
plot_quantile(gull.uva.alt.0[points.uva.gulls$v_accuracy < 4], main = "v_accuracy <4")
plot_quantile(gull.uva.alt.0[points.uva.gulls$v_accuracy < 3], main = "v_accuracy <3")
dev.off()

p_obs <- NULL
q95_error <- NULL
n <- length(gull.uva.alt.0)
v_ac <- c(1:100)
for(i in 1:100){
  p_obs[i] <- sum(points.uva.gulls$v_accuracy < v_ac[i], na.rm = TRUE)/n
  q95_error[i] <- quantile(abs(gull.uva.alt.0[points.uva.gulls$v_accuracy < v_ac[i]]), 0.95, na.rm = TRUE)
}
par(mfrow = c(2,1))
plot(q95_error~v_ac)
plot(p_obs~v_ac)


# Error on murre UVA tags
pdf("uva_murres_v_accuracy_effect.pdf")
plot_quantile(murre.uva.alt.0[points.uva.murres$v_accuracy < 100], main = "v_accuracy <100")
plot_quantile(murre.uva.alt.0[points.uva.murres$v_accuracy < 50], main = "v_accuracy <50")
plot_quantile(murre.uva.alt.0[points.uva.murres$v_accuracy < 40], main = "v_accuracy <40")
plot_quantile(murre.uva.alt.0[points.uva.murres$v_accuracy < 30], main = "v_accuracy <30")
plot_quantile(murre.uva.alt.0[points.uva.murres$v_accuracy < 20], main = "v_accuracy <20")
plot_quantile(murre.uva.alt.0[points.uva.murres$v_accuracy < 10], main = "v_accuracy <10")
plot_quantile(murre.uva.alt.0[points.uva.murres$v_accuracy < 7.5], main = "v_accuracy <7.5")
plot_quantile(murre.uva.alt.0[points.uva.murres$v_accuracy < 5], main = "v_accuracy <5")
plot_quantile(murre.uva.alt.0[points.uva.murres$v_accuracy < 4], main = "v_accuracy <4")
plot_quantile(murre.uva.alt.0[points.uva.murres$v_accuracy < 3], main = "v_accuracy <3")
dev.off()

p_obs <- NULL
q95_error <- NULL
n <- length(murre.uva.alt.0)
v_ac <- c(1:100)
for(i in 1:100){
  p_obs[i] <- sum(points.uva.murres$v_accuracy < v_ac[i], na.rm = TRUE)/n
  q95_error[i] <- quantile(abs(murre.uva.alt.0[points.uva.murres$v_accuracy < v_ac[i]]), 0.95, na.rm = TRUE)
}
par(mfrow = c(2,1))
plot(q95_error~v_ac)
plot(p_obs~v_ac)