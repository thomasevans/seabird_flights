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
sql_query <- paste("SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_gps_points_igu.elev, guillemots_gps_points_igu.ehpe, guillemots_gps_points_igu.timeout, guillemots_gps_points_igu.MSVs_QCN, guillemots_gps_points_igu.sat_n, guillemots_gps_points_igu.timeout
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







# Code for transparent colours ----
# Code from https://github.com/mylesmharrison/colorRampPaletteAlpha/blob/master/colorRampPaletteAlpha.R
# Hight-lighted by blog post: http://www.everydayanalytics.ca/2014/03/colorRampPalette-alpha-in-R.html
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

# Map GPS locations --------
# needed to plot maps
library(maps)


# Plot base map
load("SWE_adm0.RData")


# Set limits
c.xlim <- c(17.0, 18.1)

c.ylim <- c(56.7, 57.8)

# ?png
png(filename = "surface_locations_map.png",
    width = 9, height = 4, units = "in", pointsize = 8,
    bg = "white", res = 600, family = "", restoreConsole = TRUE,
    type = c("cairo-png"))


# 3 maps side by side 
par(mfrow=c(1,3))

# Plot base map
plot(gadm, xlim = c.xlim,
     ylim = c.ylim, col="grey", bg = "white",
     main = "Gulls - UvA BiTs GPS")

points(points.uva.gulls$longitude, points.uva.gulls$latitude,
       col = addalpha("black", alpha = 0.2))


# Scale bar and axis
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
map.scale(x,y,ratio = FALSE, col="black",col.lab="black")
box(col="black",lwd=2)
axis(side=(1), las=1, col="black", col.axis="black")
axis(side=(2), las=1, col="black", col.axis="black")

# Plot base map
plot(gadm, xlim = c.xlim,
     ylim = c.ylim, col="grey", bg = "white",
     main = "Murres - UvA BiTs GPS")

points(points.uva.murres$longitude, points.uva.murres$latitude,
       col = addalpha("black", alpha = 0.2))


# Scale bar and axis
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
map.scale(x,y,ratio = FALSE, col="black",col.lab="black")
box(col="black",lwd=2)
axis(side=(1), las=1, col="black", col.axis="black")
axis(side=(2), las=1, col="black", col.axis="black")




# Scale bar and axis
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
map.scale(x,y,ratio = FALSE, col="black",col.lab="black")
box(col="black",lwd=2)
axis(side=(1), las=1, col="black", col.axis="black")
axis(side=(2), las=1, col="black", col.axis="black")

# Plot base map
plot(gadm, xlim = c.xlim,
     ylim = c.ylim, col="grey", bg = "white",
     main = "Murres - IGU GPS")

points(points.igu$longitude, points.igu$latitude,
       col = addalpha("black", alpha = 0.2))


# Scale bar and axis
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
map.scale(x,y,ratio = FALSE, col="black",col.lab="black")
box(col="black",lwd=2)
axis(side=(1), las=1, col="black", col.axis="black")
axis(side=(2), las=1, col="black", col.axis="black")


dev.off()

# Altitude calibration -------
gull.uva.alt.quant <- quantile(points.uva.gulls$altitude, c(0.025,0.975),
                               na.rm = TRUE)
gull.uva.alt.mean <- mean(points.uva.gulls$altitude[points.uva.gulls$altitude >= gull.uva.alt.quant[1] &
                                                      points.uva.gulls$altitude <= gull.uva.alt.quant[2]],
                          na.rm = TRUE)

murre.uva.alt.quant <- quantile(points.uva.murres$altitude, c(0.025,0.975),
                                na.rm = TRUE)
murre.uva.alt.mean <- mean(points.uva.murres$altitude[points.uva.murres$altitude >= murre.uva.alt.quant[1] &
                                                        points.uva.murres$altitude <= murre.uva.alt.quant[2]],
                           na.rm = TRUE)


murre.igu.alt.quant <- quantile(points.igu$elev, c(0.025,0.975),
                                na.rm = TRUE)
murre.igu.alt.mean <- mean(points.igu$elev[points.igu$elev >= murre.igu.alt.quant[1] &
                                             points.igu$elev <= murre.igu.alt.quant[2]],
                           na.rm = TRUE)

win.metafile(filename = "altitude_variation_hist.wmf", width = 9, height = 4)
par(mfrow=c(1,3))
hist(points.uva.gulls$altitude[points.uva.gulls$altitude >= -100 &
                                 points.uva.gulls$altitude <= 100], xlim= c(-50,40),
     breaks = 100,
     main = "Gulls - UvA BiTs GPS",
     ylab = "N GPS locations (2 m interval)",
     xlab = "GPS recorded altitude (m)")
abline(v=gull.uva.alt.quant, lty = 2, col = "dark grey")
abline(v=gull.uva.alt.mean, lty = 2, col = "red", lwd = 2)

hist(points.uva.murres$altitude[points.uva.murres$altitude >= -100 &
                                  points.uva.murres$altitude <= 100], xlim= c(-50,50),
     breaks = 100,
     main = "Murres - UvA BiTs GPS",
     ylab = "N GPS locations (2 m interval)",
     xlab = "GPS recorded altitude (m)")
abline(v=murre.uva.alt.quant, lty = 2, col = "dark grey")
abline(v=murre.uva.alt.mean, lty = 2, col = "red", lwd = 2)

hist(points.igu$elev[points.igu$elev >= -100 &
                       points.igu$elev <= 100], xlim= c(-100,100),
     breaks = 100,
     main = "Murres - IGU GPS",
     ylab = "N GPS locations (2 m interval)",
     xlab = "GPS recorded altitude (m)")
abline(v=murre.igu.alt.quant, lty = 2, col = "dark grey")
abline(v=murre.igu.alt.mean, lty = 2, col = "red", lwd = 2)

dev.off()



# Altitude precision pre-filter ------

plot_quantile <- function(x, main = "", xlim = NULL){
  quantiles <- quantile(abs(x), seq(0.01,0.99,0.01), na.rm = TRUE)
  plot(c(1:99)~quantiles, xlab = "Altitude error (m)", ylab = "Proportion of locations  (%)",
       type = "l", las = 1,
       main = main, xlim = xlim)
  abline(v = quantiles[c(50,90,95)], lwd = 2, lty = 2, col = "dark grey")
  text(quantiles[c(50,90,95)], 5,
       paste("<",format(round(quantiles[c(50,90,95)], 1), nsmall = 1), "m"),
       col = "grey20")
  text(quantiles[c(50,90,95)], 10, c("50 %", "90 %", "95 %"),
       col = "grey20")
  
  perc_val <- c((c(1:99)[quantiles>5][1]), (c(1:99)[quantiles>10][1]))
  abline(h = perc_val, lwd = 2, lty = 2, col = "red")
  if(is.null(xlim)){
    text(quantiles[97],perc_val+3, paste(perc_val, "%  <", c(5,10), "m"),
         col = "red")
  } else {
    text(xlim[2]-20,perc_val+3, paste(perc_val, "%  <", c(5,10), "m"),
         col = "red")
  }

}




win.metafile(filename = "altitude_precision_quantile.wmf", width = 9, height = 4)
par(mfrow=c(1,3))
plot_quantile(gull.uva.alt.0,
              main = "Gulls - UvA BiTs GPS")

plot_quantile(murre.uva.alt.0,
              main = "Murres - UvA BiTs GPS")

plot_quantile(murre.igu.alt.0,
              main = "Murres - IGU GPS")
dev.off()


# New altitudes ----
gull.uva.alt.0 <- points.uva.gulls$altitude - gull.uva.alt.mean
murre.uva.alt.0 <- points.uva.murres$altitude - murre.uva.alt.mean
murre.igu.alt.0 <- points.igu$elev - murre.igu.alt.mean


# Altitude filtering ------

win.metafile(filename = "altitude_precision_sat_n_ext.wmf", width = 9, height = 9)

# UvA devices

points.uva.gulls_with_sat <- points.uva.gulls[!is.na(points.uva.gulls$satellites_used),]

quantiles <- perc_val <- n_loc <- p_loc <- sat_number <- NULL
sat_n <- 3
for(i in 1:10){
  f <- points.uva.gulls_with_sat$satellites_used >= sat_n
  quantiles <- quantile(abs(gull.uva.alt.0[!is.na(points.uva.gulls$satellites_used)][f]), seq(0.01,0.99,0.01), na.rm = TRUE)
  perc_val[i] <- c(1:99)[quantiles>5][1]
  sat_number[i] <- sat_n
  n_loc[i] <- sum(f, na.rm = TRUE)
  p_loc[i] <- n_loc[i]/nrow(points.uva.gulls_with_sat)
  sat_n <- sat_n + 1
  
}

gull_sat <- cbind.data.frame(sat_number,perc_val,n_loc,p_loc)

par(mfrow=c(2,2))
par(mar = c(5,5,2,5))
plot(gull_sat$perc_val~gull_sat$sat_number,
     ylab = "Proportion of locations with error <5 m (%)",
     xlab = "Minimum number of satellites",
     type = "l", las = 1, lwd = 2,
     main = "Gulls - UvA BiTs GPS")
par(new = TRUE)
plot(gull_sat$p_loc*100~gull_sat$sat_number,  axes=F, ylab = "", xlab = "", col = "red",
     type = "l", las = 1, lwd = 2, lty = 2)
axis(side = 4, col = "red", las = 1, col.axis = "red")
# ?axis
mtext(side = 4, line = 3, 'Proportion of GPS locations retained (%)',
      col = "red", cex = 0.66)

# summary(as.factor(points.uva.gulls$satellites_used))/nrow(points.uva.gulls)

# unique(points.uva.gulls$device_info_serial[is.na(points.uva.gulls$satellites_used)])
#


# Murres - UvA

points.uva.murres_with_sat <- points.uva.murres[!is.na(points.uva.murres$satellites_used),]

quantiles <- perc_val <- n_loc <- p_loc <- sat_number <- NULL
sat_n <- 3
for(i in 1:10){
  f <- points.uva.murres_with_sat$satellites_used >= sat_n
  quantiles <- quantile(abs(murre.uva.alt.0[!is.na(points.uva.murres$satellites_used)][f]), seq(0.01,0.99,0.01), na.rm = TRUE)
  perc_val[i] <- c(1:99)[quantiles>5][1]
  sat_number[i] <- sat_n
  n_loc[i] <- sum(f, na.rm = TRUE)
  p_loc[i] <- n_loc[i]/nrow(points.uva.murres_with_sat)
  sat_n <- sat_n + 1
  
}

murre_sat <- cbind.data.frame(sat_number,perc_val,n_loc,p_loc)

# par(mfrow=c(1,1))
par(mar = c(5,5,2,5))
plot(murre_sat$perc_val~murre_sat$sat_number, ylab = "Proportion of locations with error <5 m (%)", xlab = "Minimum number of satellites",
     type = "l", las = 1, lwd = 2,
     main = "Murres - UvA BiTs GPS")
par(new = TRUE)
plot(murre_sat$p_loc*100~murre_sat$sat_number,  axes=F, ylab = "", xlab = "", col = "red",
     type = "l", las = 1, lwd = 2, lty = 2)
axis(side = 4, col = "red", las = 1, col.axis = "red")
# ?axis
mtext(side = 4, line = 3, 'Proportion of GPS locations retained (%)',
      col = "red", cex = 0.66)

# summary(as.factor(points.uva.murres$satellites_used))/nrow(points.uva.murres)

# unique(points.uva.murres$device_info_serial[is.na(points.uva.murres$satellites_used)])




# Murres - IGU

points.igu_with_sat <- points.igu[!is.na(points.igu$sat_n),]

quantiles <- perc_val <- n_loc <- p_loc <- sat_number <- NULL
sat_n <- 3
for(i in 1:10){
  f <- points.igu_with_sat$sat_n >= sat_n
  quantiles <- quantile(abs(murre.igu.alt.0[!is.na(points.igu$sat_n)][f]), seq(0.01,0.99,0.01), na.rm = TRUE)
  perc_val[i] <- c(1:99)[quantiles>10][1]
  sat_number[i] <- sat_n
  n_loc[i] <- sum(f, na.rm = TRUE)
  p_loc[i] <- n_loc[i]/nrow(points.igu_with_sat)
  sat_n <- sat_n + 1
  
}

murre_sat <- cbind.data.frame(sat_number,perc_val,n_loc,p_loc)

# par(mfrow=c(1,1))
par(mar = c(5,5,2,5))
plot(murre_sat$perc_val~murre_sat$sat_number,
     ylab = "Proportion of locations with error <5 m (%)",
     xlab = "Minimum number of satellites", las = 1, lwd = 2,
     type = "l",
     main = "Murres - IGU GPS")
par(new = TRUE)
plot(murre_sat$p_loc*100~murre_sat$sat_number,  axes=F, ylab = "", xlab = "", col = "red",
     type = "l", las = 1, lwd = 2, lty = 2)
axis(side = 4, col = "red", las = 1, col.axis = "red")
# ?axis
mtext(side = 4, line = 3, 'Proportion of GPS locations retained (%)',
      col = "red", cex = 0.66)
# ?mtext


quantiles <- perc_val <- n_loc <- p_loc <- sat_number <- NULL
sat_n <- 3
for(i in 1:10){
  f <- points.igu_with_sat$sat_n == sat_n
  quantiles <- quantile(abs(murre.igu.alt.0[!is.na(points.igu$sat_n)][f]), seq(0.01,0.99,0.01), na.rm = TRUE)
  perc_val[i] <- c(1:99)[quantiles>10][1]
  sat_number[i] <- sat_n
  n_loc[i] <- sum(f, na.rm = TRUE)
  p_loc[i] <- n_loc[i]/nrow(points.igu_with_sat)
  sat_n <- sat_n + 1
  
}

murre_sat <- cbind.data.frame(sat_number,perc_val,n_loc,p_loc)

# par(mfrow=c(1,1))
par(mar = c(5,5,2,5))
plot(murre_sat$perc_val~murre_sat$sat_number,
     ylab = "Proportion of locations with error <5 m (%)",
     xlab = "Number of satellites", las = 1, lwd = 2,
     type = "l",
     main = "Murres - IGU GPS")
par(new = TRUE)
plot(murre_sat$p_loc*100~murre_sat$sat_number,  axes=F, ylab = "", xlab = "", col = "red",
     type = "l", las = 1, lwd = 2, lty = 2)
axis(side = 4, col = "red", las = 1, col.axis = "red")
# ?axis
mtext(side = 4, line = 3, 'Proportion of GPS locations (%)',
      col = "red", cex = 0.66)

dev.off()




# EHPE/ EVPE ---------
win.metafile(filename = "altitude_precision_ehpe.wmf", width = 9, height = 4)

# UvA devices
# hist(points.uva.gulls$v_accuracy, xlim = c(0,50), breaks = 100)
# hist(points.uva.murres$v_accuracy)
# hist(points.igu$ehpe)

thresh <- c(1:100)

# points.uva.gulls_with_sat <- points.uva.gulls[!is.na(points.uva.gulls$satellites_used),]

quantiles <- perc_val <- n_loc <- p_loc <- sat_number <- NULL
sat_n <- 3
for(i in 1:100){
  f <- points.uva.gulls$v_accuracy <= thresh[i]
  quantiles <- quantile(abs(gull.uva.alt.0[f]), seq(0.01,0.99,0.01), na.rm = TRUE)
  perc_val[i] <- c(1:99)[quantiles>5][1]
  # thresh_l[i] <- sat_n
  n_loc[i] <- sum(f, na.rm = TRUE)
  p_loc[i] <- n_loc[i]/nrow(points.uva.gulls)
  sat_n <- sat_n + 1
  
}

gull_sat <- cbind.data.frame(thresh,perc_val,n_loc,p_loc)

par(mfrow=c(1,3))
par(mar = c(5,5,2,5))
plot(gull_sat$perc_val~gull_sat$thresh,
     ylab = "Proportion of locations with error <5 m (%)",
     xlab = "EVPE error upper threshold (m)",
     type = "l", las = 1, lwd = 2,
     main = "Gulls - UvA BiTs GPS")
par(new = TRUE)
plot(gull_sat$p_loc*100~gull_sat$thresh,  axes=F, ylab = "", xlab = "", col = "red",
     type = "l", las = 1, lwd = 2, lty = 2)
axis(side = 4, col = "red", las = 1, col.axis = "red")
# ?axis
mtext(side = 4, line = 3, 'Proportion of GPS locations retained (%)',
      col = "red", cex = 0.66)

# summary(as.factor(points.uva.gulls$satellites_used))/nrow(points.uva.gulls)

# unique(points.uva.gulls$device_info_serial[is.na(points.uva.gulls$satellites_used)])
#


# Murres - UvA

quantiles <- perc_val <- n_loc <- p_loc <- sat_number <- NULL
# sat_n <- 3
for(i in 1:100){
  f <- points.uva.murres$v_accuracy <= thresh[i]
  quantiles <- quantile(abs(murre.uva.alt.0[f]), seq(0.01,0.99,0.01), na.rm = TRUE)
  perc_val[i] <- c(1:99)[quantiles>5][1]
  # thresh_l[i] <- sat_n
  n_loc[i] <- sum(f, na.rm = TRUE)
  p_loc[i] <- n_loc[i]/nrow(points.uva.murres)
  sat_n <- sat_n + 1
  
}

murre_sat <- cbind.data.frame(thresh,perc_val,n_loc,p_loc)

# par(mfrow=c(1,3))
par(mar = c(5,5,2,5))
plot(murre_sat$perc_val~murre_sat$thresh,
     ylab = "Proportion of locations with error <5 m (%)",
     xlab = "EVPE error upper threshold (m)",
     type = "l", las = 1, lwd = 2,
     main = "Murres - UvA BiTs GPS")
par(new = TRUE)
plot(murre_sat$p_loc*100~murre_sat$thresh,  axes=F, ylab = "", xlab = "", col = "red",
     type = "l", las = 1, lwd = 2, lty = 2)
axis(side = 4, col = "red", las = 1, col.axis = "red")
# ?axis
mtext(side = 4, line = 3, 'Proportion of GPS locations retained (%)',
      col = "red", cex = 0.66)



# Murres - IGU
points.igu_with_sat <- points.igu[!is.na(points.igu$sat_n),]

quantiles <- perc_val <- n_loc <- p_loc <- sat_number <- NULL
# sat_n <- 3
for(i in 1:100){
  f <- points.igu_with_sat$ehpe <= thresh[i]
  quantiles <- quantile(abs(murre.igu.alt.0[!is.na(points.igu$sat_n)][f]), seq(0.01,0.99,0.01), na.rm = TRUE)
  perc_val[i] <- c(1:99)[quantiles>5][1]
  # thresh_l[i] <- sat_n
  n_loc[i] <- sum(f, na.rm = TRUE)
  p_loc[i] <- n_loc[i]/nrow(points.igu_with_sat)
  sat_n <- sat_n + 1
  
}

murre_sat <- cbind.data.frame(thresh,perc_val,n_loc,p_loc)

# par(mfrow=c(1,3))
par(mar = c(5,5,2,5))
plot(murre_sat$perc_val~murre_sat$thresh,
     ylab = "Proportion of locations with error <5 m (%)",
     xlab = "EHPE error upper threshold (m)",
     type = "l", las = 1, lwd = 2,
     main = "Murres - IGU GPS")
par(new = TRUE)
plot(murre_sat$p_loc*100~murre_sat$thresh,  axes=F, ylab = "", xlab = "", col = "red",
     type = "l", las = 1, lwd = 2, lty = 2)
axis(side = 4, col = "red", las = 1, col.axis = "red")
# ?axis
mtext(side = 4, line = 3, 'Proportion of GPS locations retained (%)',
      col = "red", cex = 0.66)


dev.off()

# DOP -------
win.metafile(filename = "altitude_precision_dop.wmf", width = 8, height = 5)

# UvA devices
# hist(points.uva.gulls$positiondop, xlim = c(0,10), breaks = 100)
# hist(points.uva.murres$v_accuracy)
# hist(points.igu$ehpe)

thresh <- seq(1,10,0.1)

# points.uva.gulls_with_sat <- points.uva.gulls[!is.na(points.uva.gulls$satellites_used),]

quantiles <- perc_val <- n_loc <- p_loc <- NULL
# sat_n <- 3
for(i in 1:91){
  f <- points.uva.gulls$positiondop <= thresh[i]
  quantiles <- quantile(abs(gull.uva.alt.0[f]), seq(0.01,0.99,0.01), na.rm = TRUE)
  perc_val[i] <- c(1:99)[quantiles>5][1]
  # thresh_l[i] <- sat_n
  n_loc[i] <- sum(f, na.rm = TRUE)
  p_loc[i] <- n_loc[i]/nrow(points.uva.gulls)
  # sat_n <- sat_n + 1
  
}

gull_sat <- cbind.data.frame(thresh,perc_val,n_loc,p_loc)

par(mfrow=c(1,2))
par(mar = c(5,5,2,5))
plot(gull_sat$perc_val~gull_sat$thresh,
     ylab = "Proportion of locations with error <5 m (%)",
     xlab = "Dilution of precision",
     type = "l", las = 1, lwd = 2,
     main = "Gulls - UvA BiTs GPS")
par(new = TRUE)
plot(gull_sat$p_loc*100~gull_sat$thresh,  axes=F, ylab = "", xlab = "", col = "red",
     type = "l", las = 1, lwd = 2, lty = 2)
axis(side = 4, col = "red", las = 1, col.axis = "red")
# ?axis
mtext(side = 4, line = 3, 'Proportion of GPS locations retained (%)',
      col = "red")

# summary(as.factor(points.uva.gulls$satellites_used))/nrow(points.uva.gulls)

# unique(points.uva.gulls$device_info_serial[is.na(points.uva.gulls$satellites_used)])
#


# Murres - UvA

quantiles <- perc_val <- n_loc <- p_loc <- NULL
# sat_n <- 3
for(i in 1:91){
  f <- points.uva.murres$positiondop <= thresh[i]
  quantiles <- quantile(abs(murre.uva.alt.0[f]), seq(0.01,0.99,0.01), na.rm = TRUE)
  perc_val[i] <- c(1:99)[quantiles>5][1]
  # thresh_l[i] <- sat_n
  n_loc[i] <- sum(f, na.rm = TRUE)
  p_loc[i] <- n_loc[i]/nrow(points.uva.murres)
  # sat_n <- sat_n + 1
  
}

murre_sat <- cbind.data.frame(thresh,perc_val,n_loc,p_loc)

# par(mfrow=c(1,2))
par(mar = c(5,5,2,5))
plot(murre_sat$perc_val~murre_sat$thresh,
     ylab = "Proportion of locations with error <5 m (%)",
     xlab = "Dilution of precision",
     type = "l", las = 1, lwd = 2,
     main = "Murres - UvA BiTs GPS")
par(new = TRUE)
plot(murre_sat$p_loc*100~murre_sat$thresh,  axes=F, ylab = "", xlab = "", col = "red",
     type = "l", las = 1, lwd = 2, lty = 2)
axis(side = 4, col = "red", las = 1, col.axis = "red")
# ?axis
mtext(side = 4, line = 3, 'Proportion of GPS locations retained (%)',
      col = "red")

dev.off()



# IGU devices only ------
# Look at:
# MSCs_QCN
f <- !is.na(points.igu$MSVs_QCN)
par(mfrow=c(1,1))
boxplot(abs(murre.igu.alt.0[f])~as.factor(points.igu$MSVs_QCN[f]),
        ylim = c(0,100),
        )
abline(h = 5, lty = 2, lwd = 2, col = "red")






summary(as.factor(points.igu$MSVs_QCN[f]))

library(plyr)

# ci_fun_high(data_df$y[data_df$group=="GPS_first"])
points.igu.msv <- points.igu[f,]
points.igu.msv$alt_0_abs <- abs(murre.igu.alt.0[f])
z <- ddply(points.igu.msv, .(as.factor(MSVs_QCN)), summarise,
           quan_50 = quantile(abs(alt_0_abs), 0.5, na.rm = TRUE),
           mean = mean(alt_0_abs, na.rm = TRUE),
           sd = sd(alt_0_abs, na.rm = TRUE),
           n = length(alt_0_abs))
names(z)[1] <- "MSVs_QCN"

z$thresh_test <- z$quan_50 < 4
sum(z$n[z$thresh_test])/sum(z$n)

z$`as.factor(MSVs_QCN)`[!thresh_test]

points.igu.msv.merge <- merge(points.igu.msv, z[,c(1,6)], by = "MSVs_QCN")

library(ggplot2)
# ?ggsave
ggplot(data = points.igu.msv.merge, aes(x = as.factor(MSVs_QCN), y = abs(alt_0_abs),
                                  col = as.factor(thresh_test))) +
  geom_jitter(alpha = 0.3,size = 1) +
  geom_boxplot(alpha = 0) +
  ylim(0,100) +
  theme_bw() +
  geom_abline(intercept = 4, alpha = 0.5, lty = 2, slope = 0, lwd = 1, col = "black") +
  labs(title = 'Murres - UvA BiTs GPS',
       x = 'MSVs_QCN',
       y = 'Altitude error (m)') +
  guides(colour=FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("MSVs_QCN_quality.png",
       width = 8, height = 5, units = "in")


plot_quantile(points.igu.msv.merge$alt_0_abs[points.igu.msv.merge$thresh_test])
plot_quantile(points.igu.msv.merge$alt_0_abs[!points.igu.msv.merge$thresh_test])
plot_quantile(points.igu.msv.merge$alt_0_abs)


# Timeout thing
hist(points.igu$timeout, breaks = 1000)

plot(points.igu$timeout,abs(murre.igu.alt.0))


plot(points.igu$timeout,points.igu$sat_n)


x <- points.igu[points.igu$timeout==250 & !is.na(points.igu$timeout),]
# Points already labelled by MSVs_QCN thing





# Final filters ------
# Murres - IGU 2009
igu_2009 <- points.igu$date_time < as.POSIXct("2010-01-01 00:00:00", tz = "UTC")
# hist(abs(murre.uva.alt.0[igu_2009]))
quantiles <- quantile(abs(murre.igu.alt.0[igu_2009]), seq(0.01,0.99,0.001), na.rm = TRUE)
seq(0.01,0.99,0.001)[quantiles>5][1]
seq(0.01,0.99,0.001)[quantiles>10][1]
plot_quantile(abs(murre.igu.alt.0[igu_2009]), xlim = c(0,100))


# Murres - IGU others
igu_2009 <- points.igu$date_time < as.POSIXct("2010-01-01 00:00:00", tz = "UTC")

#Pre-filter
quantiles <- quantile(abs(points.igu.msv.merge$alt_0_abs), seq(0.01,0.99,0.001), na.rm = TRUE)
seq(0.01,0.99,0.001)[quantiles>5][1]
seq(0.01,0.99,0.001)[quantiles>10][1]

# Post filter
quantiles <- quantile(abs(points.igu.msv.merge$alt_0_abs[points.igu.msv.merge$thresh_test]), seq(0.01,0.99,0.001), na.rm = TRUE)
seq(0.01,0.99,0.001)[quantiles>5][1]
seq(0.01,0.99,0.001)[quantiles>10][1]

sum(z$n[z$thresh_test])/sum(z$n)
#65% <5m   83.4% retained


# Murres - UvA BiTs
summary(as.factor(points.uva.murres$satellites_used))


#Pre-filter
quantiles <- quantile(abs(murre.uva.alt.0), seq(0.01,0.99,0.001), na.rm = TRUE)
seq(0.01,0.99,0.001)[quantiles>5][1]

# Post filter
quantiles <- quantile(abs(murre.uva.alt.0[points.uva.murres$satellites_used >= 8]), seq(0.01,1,0.001), na.rm = TRUE)
seq(0.01,1,0.001)[quantiles>5][1]
seq(0.01,1,0.001)[quantiles>10][1]
sum(points.uva.murres$satellites_used >= 8)/length(points.uva.murres$satellites_used)



# Gulls - UvA BiTs - no sat
summary(as.factor(points.uva.gulls$satellites_used))
points.uva.gulls_no_sat <- points.uva.gulls[is.na(points.uva.gulls$satellites_used),]
unique(points.uva.gulls_no_sat$device_info_serial)


z <- ddply(points.uva.gulls_no_sat, .(as.factor(device_info_serial)), summarise,
           date_min = min(date_time, na.rm = TRUE),
           date_max = max(date_time, na.rm = TRUE),
           n = length(device_info_serial))


#Pre-filter
quantiles <- quantile(abs(gull.uva.alt.0[is.na(points.uva.gulls$satellites_used)]), seq(0.01,0.99,0.001), na.rm = TRUE)
seq(0.01,0.99,0.001)[quantiles>5][1]

# Post filter
quantiles <- quantile(abs(gull.uva.alt.0[is.na(points.uva.gulls$satellites_used)][
  points.uva.gulls_no_sat$positiondop <=3
]), seq(0.01,0.99,0.001), na.rm = TRUE)
seq(0.01,0.99,0.001)[quantiles>5][1]
seq(0.01,0.99,0.001)[quantiles>10][1]
sum(points.uva.gulls_no_sat$positiondop <=3)/length(points.uva.gulls_no_sat$positiondop)


# Gulls - UvA BiTs - with sat
summary(as.factor(points.uva.gulls$satellites_used))
points.uva.gulls_inc_sat <- points.uva.gulls[!is.na(points.uva.gulls$satellites_used),]
unique(points.uva.gulls_inc_sat$device_info_serial)


z <- ddply(points.uva.gulls_inc_sat, .(as.factor(device_info_serial)), summarise,
           date_min = min(date_time, na.rm = TRUE),
           date_max = max(date_time, na.rm = TRUE),
           n = length(device_info_serial))
z

#Pre-filter
quantiles <- quantile(abs(gull.uva.alt.0[!is.na(points.uva.gulls$satellites_used)]), seq(0.01,0.99,0.001), na.rm = TRUE)
seq(0.01,0.99,0.001)[quantiles>5][1]
seq(0.01,0.99,0.001)[quantiles>10][1]

# Post filter
quantiles <- quantile(abs(gull.uva.alt.0[!is.na(points.uva.gulls$satellites_used)][
  points.uva.gulls_inc_sat$satellites_used >=7
  ]), seq(0.01,0.99,0.001), na.rm = TRUE)
seq(0.01,0.99,0.001)[quantiles>5][1]
sum(points.uva.gulls_inc_sat$satellites_used >=7)/length(points.uva.gulls_inc_sat$satellites_used)


# Final precision post-filtering -----

win.metafile(filename = "altitude_precision_quantile_post_filter.wmf", width = 9, height = 4)
par(mfrow=c(1,3))
plot_quantile(gull.uva.alt.0[(
  !is.na(points.uva.gulls$satellites_used) & points.uva.gulls$satellites_used >=7)|(
    is.na(points.uva.gulls$satellites_used) & points.uva.gulls$positiondop <=3
  )],
              main = "Gulls - UvA BiTs GPS")

plot_quantile(murre.uva.alt.0[points.uva.murres$satellites_used >= 8],
              main = "Murres - UvA BiTs GPS")

plot_quantile(murre.igu.alt.0[(
  igu_2009)|(
    !igu_2009 & !(points.igu$MSVs_QCN %in% c(128, 136, 151, 161, 163, 164, 166, 176, 177, 178, 179, 180, 181, 183, 192, 193, 194, 195, 196, 198, 199, 200, 202, 203))
  )],
              main = "Murres - IGU GPS")
dev.off()




#







# Device specific offsets??? ------

# Plot device altitude distributions (boxplots)
par(mfrow=c(1,1))
plot(points.uva.gulls$altitude~as.factor(points.uva.gulls$device_info_serial),
     ylim = c(-25,25),
     xlab = "Device ID",
     ylab = "GPS recorded altitude (m)")
abline(h = -2.84, lwd = 1.5, col = "red")

par(mfrow=c(1,1))
plot(points.uva.murres$altitude~as.factor(points.uva.murres$device_info_serial),
     ylim = c(-25,25),
     xlab = "Device ID",
     ylab = "GPS recorded altitude (m)")
abline(h = -1.28, lwd = 1.5, col = "red")


par(mfrow=c(1,1))
plot(points.igu$elev~as.factor(points.igu$device_info_serial),
     ylim = c(-50,50),
     xlab = "Device ID",
     ylab = "GPS recorded altitude (m)")
abline(h = -0.18, lwd = 1.5, col = "red")

summary(as.factor(points.igu$device_info_serial))




ci_fun_low <- function(ix){
  n <- length(ix)
  sd_calc <- sd(ix)
  e <- qt(0.975,df=n-1)*sd_calc/sqrt(n)
  out.val <- mean(ix)-e
  return(out.val)
}

ci_fun_high <- function(ix){
  n <- length(ix)
  sd_calc <- sd(ix)
  e <- qt(0.975,df=n-1)*sd_calc/sqrt(n)
  out.val <- mean(ix)+e
  return(out.val)
}
# ?qt

mean_95_fun <- function(x){
  qs <- quantile(x, c(0.025,0.975),
                 na.rm = TRUE)
  means.x <- mean(x[x >= qs[1] & x <= qs[2]], na.rm = TRUE)
  return(means.x)
}

library(plyr)

# ci_fun_high(data_df$y[data_df$group=="GPS_first"])
z <- ddply(points.uva.gulls, .(as.factor(device_info_serial)), summarise,
           ymin = ci_fun_low(altitude),
           ymax = ci_fun_high(altitude),
           mean = mean(altitude, na.rm = TRUE),
           sd = sd(altitude, na.rm = TRUE),
           mean_95 = mean_95_fun(altitude),
           n = length(altitude))
names(z)[1] <- "device_info_serial"

test.df <- merge(points.uva.gulls[,c(1,2,5)], z[,c(1,6)], by = "device_info_serial")

gulls.uva.new.alt <- test.df$altitude - test.df$mean_95
hist(gulls.uva.new.alt, breaks = 1000, xlim = c(-50,50))



mod1 <- lm(points.uva.gulls$altitude~as.factor(points.uva.gulls$device_info_serial))
summary(mod1)



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

par(mfrow=c(2,1))
plot_quantile(points.uva.gulls$altitude)
plot_quantile(gulls.uva.new.alt)

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