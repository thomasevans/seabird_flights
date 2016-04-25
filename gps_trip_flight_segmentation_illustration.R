

# Load in data from database ----
# GPS points with trip_id etc.
# For device_info_serial #803
# Period: 2013-07-14 to 2013-07-17 (whole days)


# Datbase functions
# Required library
library(RODBC)

# Establish a connection to the databases
gull.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

points <- sqlQuery(gull.db, query= gsub("\n", " ", "SELECT gps_uva_tracking_speed_3d_limited.device_info_serial, gps_uva_tracking_speed_3d_limited.date_time, gps_uva_tracking_speed_3d_limited.latitude, gps_uva_tracking_speed_3d_limited.longitude, gps_uva_tracking_speed_3d_limited.altitude, gps_uva_tracking_speed_3d_limited.speed_3d, lund_gps_parameters.nest_gc_dist, lund_gps_parameters.inst_ground_speed, lund_gps_parameters.pos_type, lund_gps_parameters.loc_type, lund_gps_parameters.trip_id, lund_gps_parameters.flight_class, lund_gps_parameters.flight_id
FROM gps_uva_tracking_speed_3d_limited INNER JOIN lund_gps_parameters ON (gps_uva_tracking_speed_3d_limited.date_time = lund_gps_parameters.date_time) AND (gps_uva_tracking_speed_3d_limited.device_info_serial = lund_gps_parameters.device_info_serial)
                                        WHERE (((gps_uva_tracking_speed_3d_limited.device_info_serial)=803) AND ((gps_uva_tracking_speed_3d_limited.date_time) Between #7/16/2013# And #7/18/2013#))
                                        ORDER BY gps_uva_tracking_speed_3d_limited.device_info_serial, gps_uva_tracking_speed_3d_limited.date_time;
                                        "))




# Map all locations ----
# All locations, no colour (black)



# needed to plot maps
library(maps)
library(mapproj)
library(raster) 

# Plot base map
load("SWE_adm0.RData")


# Set limits
c.xlim <- range(points$longitude)
dif    <- c.xlim[2] - c.xlim[1]
dif    <- dif *.25
c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))

c.ylim <- range(points$latitude)
dif    <- c.ylim[2] - c.ylim[1]
dif    <- dif *.25
c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))

# Set global par
par(ps = 14, cex = 1.5, cex.lab = 2)

svg("map_illustration_figure_segmentation_1.svg",
    width = 5, height = 5, family = "serif")
# ?cairo_ps
# Plot base map
par(mfrow = c(1,1))
par(mar=c(2, 3, 1, 1) + 0.1)   

# str(gadm)

range.x <-  c.xlim[2]-c.xlim[1]
range.y <-  c.ylim[2]-c.ylim[1]
dif.co <- max(c(range.x,range.y))

gadm_clip <- crop(gadm, extent(c.xlim[1]-1*(dif.co),
                               c.xlim[2]+1*(dif.co),
                               c.ylim[1]-1*(dif.co),
                               c.ylim[2]+1*(dif.co)))


plot(gadm_clip, xlim = c.xlim,
     ylim = c.ylim, col="grey", bg = "white",
     main = "")
grid()


# Code from https://github.com/mylesmharrison/colorRampPaletteAlpha/blob/master/colorRampPaletteAlpha.R
# Hight-lighted by blog post: http://www.everydayanalytics.ca/2014/03/colorRampPalette-alpha-in-R.html
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}


# Lines between points
# colour by included - black, excluded - red according to truncation thing
line.col <- rep("black", nrow(points))
# line.col[points.sub$included_points == FALSE] <- "red"
line.lty <- rep(1, nrow(points))
# line.lty[points.sub$included_points == FALSE] <- 3
segments(points$longitude[-1], points$latitude[-1],
         points$longitude[-nrow(points)], points$latitude[-nrow(points)],
         lwd = 1, lty = line.lty, col = line.col)



# Points during flight:
# Colour - rainbow colour by time - with alpha channel (lighter for excluded??)
points.col <- rainbow(nrow(points))

# points.col.alpha <- addalpha(points.col, alpha = 0.5)
# points.col.alpha[points$included_points == FALSE] <- addalpha(
#   points.col[points.sub$included_points == FALSE], alpha = 0.2)


# Size - by altitude
# points.size <- 2*points.sub$altitude_callib/max(points.sub$altitude_callib) + 1

points(points$longitude, points$latitude, cex = 0.7,
       col = "black", bg = addalpha("black", 0.2), pch = 21)



# Borders scale etc
# Scale bar and axis
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/20
map.scale(x,y, ratio = FALSE,
          col="black", col.lab="black",
          relwidth = 0.3)
box(col="black",lwd=2)
axis(side=(1), las=1, col="black", col.axis="black")
axis(side=(2), las=1, col="black", col.axis="black")

# ?map.scale

legend("topleft", "(a)", bty="n", cex = 1.2) 

dev.off()











# Map only trip locations ------
#  with unique colours for each trip

# 9 colours from http://colorbrewer2.org/
# col.trips <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')

library(RColorBrewer)

trip_ids <- unique(points$trip_id)
n_trips <- length(trip_ids)-1

trip_col <- brewer.pal(n_trips,"Spectral")
# display.brewer.pal(n_trips,"Spectral")
trip_col <- c("black", trip_col)
trip_col_alpha_03 <- addalpha(trip_col,0.3)
trip_col_alpha_07 <- addalpha(trip_col,0.7)

df.col <- cbind.data.frame(trip_ids,trip_col,trip_col_alpha_03,trip_col_alpha_07)
# names(df.col)[1] <- "trip_id"
points <- merge(x = points, y = df.col,
                by.x = "trip_id",
                by.y = "trip_ids")
points <- points[order(points$date_time),]


# # ?merge
# names(points)
# names(df.col)


svg("map_illustration_figure_segmentation_2.svg",
    width = 5, height = 5, family = "serif")
# ?cairo_ps
# Plot base map
par(mfrow = c(1,1))
par(mar=c(2, 3, 1, 1) + 0.1)   


plot(gadm_clip, xlim = c.xlim,
     ylim = c.ylim, col="grey", bg = "white",
     main = "")
grid()


# Lines between points
segments(points$longitude[-1], points$latitude[-1],
         points$longitude[-nrow(points)], points$latitude[-nrow(points)],
         lwd = 2, lty = 1, col = points$trip_col_alpha_07)


points(points$longitude, points$latitude, cex = 0.5,
       col = addalpha("black",0.5), bg = points$trip_col_alpha_07, pch = 21)



# Borders scale etc
# Scale bar and axis
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/20
map.scale(x,y, ratio = FALSE,
          col="black", col.lab="black",
          relwidth = 0.3)
box(col="black",lwd=2)
axis(side=(1), las=1, col="black", col.axis="black")
axis(side=(2), las=1, col="black", col.axis="black")

# ?map.scale

legend("topleft", "(b)", bty="n", cex = 1.2) 

dev.off()








# Map of focus trip only ----
# Indicate flight points somehow...

points.trip <- points[points$trip_id == 3178,]

# Set limits
c.xlim <- range(points.trip$longitude)
dif    <- c.xlim[2] - c.xlim[1]
dif    <- dif *.25
c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))

c.ylim <- range(points.trip$latitude)
dif    <- c.ylim[2] - c.ylim[1]
dif    <- dif *.25
c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))

# Set global par
par(ps = 14, cex = 1.5, cex.lab = 2)

# svg("map_illustration_figure_segmentation_1.svg",
#     width = 5, height = 5, family = "serif")
# # ?cairo_ps
# # Plot base map
# par(mfrow = c(1,1))
# par(mar=c(2, 3, 1, 1) + 0.1)   
# 
# # str(gadm)

range.x <-  c.xlim[2]-c.xlim[1]
range.y <-  c.ylim[2]-c.ylim[1]
dif.co <- max(c(range.x,range.y))

gadm_clip <- crop(gadm, extent(c.xlim[1]-1*(dif.co),
                               c.xlim[2]+1*(dif.co),
                               c.ylim[1]-1*(dif.co),
                               c.ylim[2]+1*(dif.co)))


svg("map_illustration_figure_segmentation_3.svg",
    width = 5, height = 5, family = "serif")
# ?cairo_ps
# Plot base map
par(mfrow = c(1,1))
par(mar=c(2, 3, 1, 1) + 0.1)   


plot(gadm_clip, xlim = c.xlim,
     ylim = c.ylim, col="grey", bg = "white",
     main = "")
grid()



# Lines between points
segments(points.trip$longitude[-1], points.trip$latitude[-1],
         points.trip$longitude[-nrow(points.trip)], points.trip$latitude[-nrow(points.trip)],
         lwd = 2, lty = 1, col = "black")

# 
# type.col <- rep("red", nrow(points.trip))
# type.col[points.trip$flight_class == 1] <- "black"

points(points.trip$longitude, points.trip$latitude, cex = 1,
       col = addalpha("black",0.5), bg = rainbow(nrow(points.trip)), pch = 21)



# Borders scale etc
# Scale bar and axis
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/20
map.scale(x,y, ratio = FALSE,
          col="black", col.lab="black",
          relwidth = 0.3)
box(col="black",lwd=2)
axis(side=(1), las=1, col="black", col.axis="black")
axis(side=(2), las=1, col="black", col.axis="black")

# ?map.scale

legend("topleft", "(c)", bty="n", cex = 1.2) 

dev.off()







# Combine plots below into single output figure - same dimensions as single map
# Plot of displacemnt (for trip recognition) ------
svg("map_illustration_figure_segmentation_4.svg",
    width = 5, height = 5, family = "serif")
par(mfrow = c(2,1),cex=1)

par(mar=c(4,5,1,2))   


# Panel A - Height (GPS + calibrated)
# - horizontal line to indicate median value during flight
plot(points$nest_gc_dist~points$date_time,
     pch = 21, bg = points$trip_col_alpha_07,
     type = "n",
     xlab = "Hour of day",
     ylab = "Displacement (km)",
     cex.lab = 1.3,
     # cex = 1.5,
     xaxt = "n",
     las = 1)


axis.POSIXct(1, at=seq(min(points$date_time), max(points$date_time), by="6 hours"),
             format = "%H")
# ?axis.Date
# axis(1, seq(points$date_time[1],20,1800), format(points$date_time, "%H"), cex.axis = .7)
# ?format
grid()
points(points$nest_gc_dist~points$date_time,
       type = "l")

points(points$nest_gc_dist~points$date_time,
       # col = points$trip_col_alpha_07,
       col = addalpha("black",0.5), bg = points$trip_col_alpha_07, pch = 21)

legend("topleft", "(d)", bty="n", cex = 1.2) 


# Horizontal line for cut-off for trips

# Plot of speed over time for focal trip ----
# Could colour all points by rainbow thing for correspondance with map
par(mar=c(4,5,1,2))   


# Panel A - Height (GPS + calibrated)
# - horizontal line to indicate median value during flight
plot(points.trip$inst_ground_speed~points.trip$date_time,
     pch = 21, bg = points$trip_col_alpha_07,
     type = "n",
     xlab = "Time (hh:mm)",
     ylab = expression("Ground speed ("~ms^{-1}~")"),
     cex.lab = 1.3,
     # cex = 1.5,
     las = 1)
grid()
points(points.trip$inst_ground_speed~points.trip$date_time,
       type = "l")

points(points.trip$inst_ground_speed~points.trip$date_time,
       # col = points$trip_col_alpha_07,
       col = addalpha("black",0.5), bg = rainbow(nrow(points.trip)), pch = 21)
abline(v = points.trip$date_time[14]+150, lwd = 2, lty = 2)
legend("topleft", "(e)", bty="n", cex = 1.2) 

dev.off()
# High-light inward flight

