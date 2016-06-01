# Example figure for methods of flight analysis


# Load in data -------

# Points
load("points.detailed.incl.RData")

# Summary data
load("flight_details.RData")


# **** Figures -----

# Pick example flight -----
flight_id <- "g37478"

points.sub <- points.df[points.df$flight_id_combined == flight_id,]
flight.details.sub <- flight.details[flight.details$flight_id_combined == flight_id,]

# example point
pid <- 11

# 1. Flight map ------
# Base map with land on

# needed to plot maps
library(maps)
library(mapproj)
library(raster) 

# Plot base map
load("SWE_adm0.RData")


# Set limits
c.xlim <- range(points.sub$longitude)
dif    <- c.xlim[2] - c.xlim[1]
dif    <- dif *.25
c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))

c.ylim <- range(points.sub$latitude)
dif    <- c.ylim[2] - c.ylim[1]
dif    <- dif *.25
c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))

# Set global par
par(ps = 14, cex = 1.5, cex.lab = 2)

  svg(paste("map_illustration_figure_", flight_id, ".svg", sep = ""),
      width = 7, height = 9, family = "serif")
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
  
  # Circle showing distance buffer around island, with centre shown
  
  # Adapted code from SO answer on plotting circles on projected maps
  # http://stackoverflow.com/a/29133886/1172358
  plotCircle <- function(LonDec, LatDec, Km,
                         lty= 1, lwd= 1, col.fill = NA,
                         col.line = "black") {
    
    #LatDec = latitude in decimal degrees of the center of the circle
    #LonDec = longitude in decimal degrees
    #Km = radius of the circle in kilometers
    ER <- 6371 #Mean Earth radius in kilometers. Change this to 3959 and you will have your function working in miles.
    AngDeg <- seq(1:360) #angles in degrees 
    Lat1Rad <- LatDec*(pi/180)#Latitude of the center of the circle in radians
    Lon1Rad <- LonDec*(pi/180)#Longitude of the center of the circle in radians
    AngRad <- AngDeg*(pi/180)#angles in radians
    Lat2Rad <-asin(sin(Lat1Rad)*cos(Km/ER)+cos(Lat1Rad)*sin(Km/ER)*cos(AngRad)) #Latitude of each point of the circle rearding to angle in radians
    Lon2Rad <- Lon1Rad+atan2(sin(AngRad)*sin(Km/ER)*cos(Lat1Rad),cos(Km/ER)-sin(Lat1Rad)*sin(Lat2Rad))#Longitude of each point of the circle rearding to angle in radians
    Lat2Deg <- Lat2Rad*(180/pi)#Latitude of each point of the circle rearding to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
    Lon2Deg <- Lon2Rad*(180/pi)#Longitude of each point of the circle rearding to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
    polygon(Lon2Deg,Lat2Deg,lty=lty, lwd=lwd, col = col.fill,
            border = col.line)
  }
  
  
  karlso.cen.long   <-  17.972088
  karlso.cen.lat    <-  57.284804
  
  SK.loc.proj <- mapproject(karlso.cen.long,karlso.cen.lat)
  
  points(karlso.cen.long, karlso.cen.lat, pch = 4, cex = 2, col = "red",
         lwd = 2)
  
  plotCircle(karlso.cen.long,karlso.cen.lat,2,
             lwd = 2, col.line = "red", lty = 2,
             col.fill = NA)
  
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
  line.col <- rep("black", nrow(points.sub))
  line.col[points.sub$included_points == FALSE] <- "red"
  line.lty <- rep(1, nrow(points.sub))
  line.lty[points.sub$included_points == FALSE] <- 3
  segments(points.sub$longitude[-1], points.sub$latitude[-1],
           points.sub$longitude[-nrow(points.sub)], points.sub$latitude[-nrow(points.sub)],
           lwd = 2, lty = line.lty, col = line.col)
  
  
  
  # Points during flight:
  # Colour - rainbow colour by time - with alpha channel (lighter for excluded??)
  points.col <- rainbow(nrow(points.sub))
  
  points.col.alpha <- addalpha(points.col, alpha = 0.8)
  points.col.alpha[points.sub$included_points == FALSE] <- addalpha(
    points.col[points.sub$included_points == FALSE], alpha = 0.4)
  
  
  # Size - by altitude
  points.size <- 2*points.sub$altitude_callib/max(points.sub$altitude_callib) + 1
  
  points(points.sub$longitude, points.sub$latitude, cex = points.size,
         col = points.col.alpha, bg = points.col.alpha, pch = 21)
  # ?points
  
  # Mark example point representing aproximately a median point/ 
  # representative location - maybe an X
  points(points.sub$longitude[pid], points.sub$latitude[pid], pch = 4, cex = 3, col = "black",
         lwd = 2)
  
  
  
  
  
  
  # Borders scale etc
  # Scale bar and axis
  # x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
  # y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/20
  map.scale(ratio = FALSE,
            col="black", col.lab="black",
            relwidth = 0.3)
  box(col="black",lwd=2)
  axis(side=(1), las=1, col="black", col.axis="black")
  axis(side=(2), las=1, col="black", col.axis="black")
  
  # ?map.scale
  
  legend("topleft", "(a)", bty="n", cex = 1.2) 
  
  dev.off()
  

# 2. Multi-panel figure of components over time -----

# For all vertical lines to indicate section of flight included
# No x-axis for A + B, but common range as for C, so can use common X-axis

  # pdf(paste("gps_plot_test_illustration_", flight_id, ".pdf", sep = ""), width = 4, height = 9)
  svg(paste("gps_plot_test_illustration_", flight_id, ".svg", sep = ""),
      width = 5, height = 9, family = "serif")
  # ?pdf
  par(mfrow = c(3,1),cex=1)
  
  par(mar=c(0,5,1,2))   
  
  
# Panel A - Height (GPS + calibrated)
# - horizontal line to indicate median value during flight
  plot(points.sub$altitude_callib~ points.sub$date_time,
       pch = 21, bg = points.col,
       type = "n",
       xlab = "Time (minutes)",
       ylab = "Altitude (m)",
       cex.lab = 1.3,
       # cex = 1.5,
       las = 1,
       xaxt = "n",
       ylim = c(0,(max(points.sub$altitude_callib) + 0.2*max(points.sub$altitude_callib)))
       )
  grid()
  segments(points.sub$date_time[-1], points.sub$altitude_callib[-1],
           points.sub$date_time[-nrow(points.sub)],
           points.sub$altitude_callib[-nrow(points.sub)],
           lwd = 2, lty = line.lty, col = line.col)
  points(points.sub$altitude_callib~ points.sub$date_time,
         pch = 21, bg = points.col, cex = 1.5)
  abline(h = flight.details.sub$altitude_callib_extm_05,
         lwd = 2, col = "dark grey", lty = 2)
  points(points.sub$altitude_callib[pid]~ points.sub$date_time[pid], pch = 4, cex = 3, col = "black",
         lwd = 1.5)
  
  legend("topleft", "(b)", bty="n", cex = 1.2) 
  
  
  # This is the same - so OK
  # abline(h=median(points.sub$altitude_callib[points.sub$included_points == TRUE]))
  
# Panel B - Vg (GPS speed) over time
  par(mar=c(0,5,1,2))         # no top spacing
  
# - common time axis
# - horizontal line to indicate median value during flight
  plot(points.sub$speed_2d~ points.sub$date_time,
       pch = 21, bg = points.col,
       # cex = 1.5,
       type = "n",
       xlab = "Time (minutes)",
       ylab = expression("Ground speed ("~ms^{-1}~")"),
       las = 1,
       cex.lab = 1.3,
       ylim = c(0,(max(points.sub$speed_2d) + 0.2*max(points.sub$speed_2d))),
       xaxt = "n")
  grid()
  segments(points.sub$date_time[-1], points.sub$speed_2d[-1],
           points.sub$date_time[-nrow(points.sub)],
           points.sub$speed_2d[-nrow(points.sub)],
           lwd = 2, lty = line.lty, col = line.col)
  points(points.sub$speed_2d~ points.sub$date_time,
         pch = 21, bg = points.col,
         cex = 1.5
         )
  points(points.sub$speed_2d[pid]~ points.sub$date_time[pid], pch = 4, cex = 3, col = "black",
         lwd = 1.5)
  
#   sqrt((flight.details.sub$vg_v*flight.details.sub$vg_v)+(
#  flight.details.sub$vg_u*flight.details.sub$vg_u
#  ))
  # Add line for median ground speed
    abline(h =  flight.details.sub$vg, lwd = 2, col = "dark grey", lty = 2)
    
    legend("topleft", "(c)", bty="n", cex = 1.2) 
    
  # Recalculated median
#   abline(h=median(points.sub$speed_2d[points.sub$included_points == TRUE]))
#   abline(h=mean(points.sub$speed_2d[points.sub$included_points == TRUE]))
#     abline(h = 2, lwd = 2, col = "red", lty = 2)

  # abline(h=median(points.sub$speed_2d))
  
  
# Panel C -  distance from island over time
# - plot distance over time

  source("deg.dist.R")
  par(mar=c(5,5,1,2))         
  # ?par
  # Calculate distance from island
  island_dist <- deg.dist(karlso.cen.long, karlso.cen.lat,
           points.sub$longitude, points.sub$latitude,
           km = TRUE)
  
    plot(island_dist~ points.sub$date_time,
       pch = 21, bg = points.col,
       # cex = 1.5,
       type = "n",
       xlab = "Time (hh:mm)",
       ylab = "Displacement (km)",
       cex.lab = 1.3,
       las = 1,
       ylim = c(0,(max(island_dist) + 0.2*max(island_dist))),
       )
  grid()
  segments(points.sub$date_time[-1], island_dist[-1],
           points.sub$date_time[-nrow(points.sub)],
           island_dist[-nrow(points.sub)],
           lwd = 2, lty = line.lty, col = line.col)
  points(island_dist~ points.sub$date_time,
         pch = 21, bg = points.col, cex = 1.5)
  
  points(island_dist[pid]~ points.sub$date_time[pid], pch = 4, cex = 3, col = "black",
         lwd = 1.5)
  
  # Add line for median ground speed
  abline(h = 2, lwd = 2, col = "red", lty = 2)
  legend("topleft", "(d)", bty="n", cex = 1.2) 
  

  dev.off()
  
# 3. Wind shear illustration ------
# Altitude vs. wind-speed - for example track

  altitude_increments <- seq(0.1, 100, 0.1)
  
  
  # wind speed at 10 m reference altitude
  wind.shear <- function(wind10, alt, roughness){
    a <- log(alt/roughness)
    b <- log(10/roughness)
    c <- a/b
    wind10*c  
  }
  # This equation from: Ragheb, M. (2012). Wind Shear, Roughness Classes and Turbine Energy Production.
  # http://mragheb.com/NPRE%20475%20Wind%20Power%20Systems/Wind%20Shear%20Roughness%20Classes%20and%20Turbine%20Energy%20Production.pdf
  
  wind.speed <- wind.shear(points.sub$ecmwf_wind_10m_speed[pid],
                           altitude_increments,
                           points.sub$ecmwf_surf_roughness[pid])
  
  
  # pdf(paste("wind_shear_example_test_illustration_", flight_id, ".pdf", sep = ""), width = 5, height = 4)
  
  svg(paste("wind_shear_example_test_illustration_", flight_id, ".svg", sep = ""),
      width = 5, height = 4, family = "serif")
  
  
  par(mfrow = c(1,1))
  par(mar=c(5, 5, 1, 1) + 0.1)   
  # par(mar=c(2, 3, 1, 1) + 0.1)   
  
  plot(wind.speed,altitude_increments,
       ylab = "Altitude (m)",
       xlab = expression("Wind speed ("~ms^{-1}~")"),
       pch = 21, bg = points.col,
       type = "n",
       las = 1,
       lwd = 3,
       # cex = 1.5,
       cex.lab = 1.3
       # cex.axis = 1.2
       )
    grid()
    
#     points(wind.speed,altitude_increments,
#            lwd = 3, type = "l")
#     
    # Add sea-surface
    alt_sea <- sin(seq(-pi, pi, 0.5))
    alt_sea <- rep(alt_sea,100)
    wind.speed_sea <- seq(0,10,0.01)
    # wind.speed_sea <- rep(wind.speed_sea,10)
  
    points(0.4*alt_sea[1:length(wind.speed_sea)]~ wind.speed_sea,
           col = addalpha("light blue", 0.8), lwd = 3, type = "l")
    
    
    points(wind.speed,altitude_increments,
         lwd = 3,
         # new = FALSE,
         type = "l"
    )
    
    
    abline(h = points.sub$altitude_callib[pid], lwd = 2,
           lty = 2, col = "#1b9e77")
    abline(v = points.sub$ecmwf_wind_10m_speed_flt_ht[pid], lwd = 2,
           lty = 2, col = "#1b9e77")
    
    abline(h = 10, lwd = 2,
           lty = 2, col = "dark grey")
    abline(v = points.sub$ecmwf_wind_10m_speed[pid], lwd = 2,
           lty = 2, col = "dark grey")
    legend("topleft", "(f)", bty="n", cex = 1.2)
    # ?text
    text(3.5
         ,points.sub$altitude_callib[pid]+ 3,
         "Flight height", col = "#1b9e77",
         pos = 4)
    text(3.5
         ,13,
         "Reference height (10 m)", col = "dark grey",
         pos = 4)
    
    dev.off()
    
    # ?pi
# Wind speed on x-axis (so that altitude is on y-axis)

# Height on y-axis, with range 0-100 m

# Indicate 10 m height, with dashed lines - for reference altitude

# Indicate actual flight altitude and calculated wind-speed for that point
# dashed lines - as for reference altitude


# 4. Velocity components ----
# similar to figure 2 in Tarroux et al.
# Could use common notation, for simplicity etc.

    library(CircStats)
    
    # pid <- 4
    
    
    # pdf(paste("vector_decompose_illustration_", flight_id, ".pdf", sep = ""), width = 5, height = 5)
    
    svg(paste("vector_decompose_illustration_", flight_id, ".svg", sep = ""),
        width = 5, height = 5, family = "serif")
    
    par(mfrow = c(1,1))
    par(mar=c(3, 3, 1, 1) + 0.1)   
    
    dummy_x <- dummy_y <- seq(-15,15,1)
    plot(dummy_y~dummy_x, type = "n",
         # cex.lab = 1.5,
         las = 1,
         xlab = "",
         ylab = "",
         xlim = c(-15,10),
         ylim = c(-5,20))
    grid()    
    
    abline(lwd = 2, col = "dark grey", h = 0)    
    abline(lwd = 2, col = "dark grey", v = 0)    
    
    # ?arrows
    
    # Vg
    arrows(0, 0, points.sub$vg_u[pid], points.sub$vg_v[pid],
           lwd = 2,
           length = 0.15,
           col = "#7570b3")
    text(points.sub$vg_u[pid] + sign(points.sub$vg_u[pid])* 1,
         points.sub$vg_v[pid] + sign(points.sub$vg_v[pid])* 1,
         "Vg", col = "#7570b3")
    
    # Vw
    arrows(0, 0, points.sub$ecmwf_wind_10m_u_flt_ht[pid],
           points.sub$ecmwf_wind_10m_v_flt_ht[pid],
           lwd = 2,
           length = 0.15,
           col = "#1b9e77")
    text(points.sub$ecmwf_wind_10m_u_flt_ht[pid] + sign(points.sub$ecmwf_wind_10m_u_flt_ht[pid])*1,
         points.sub$ecmwf_wind_10m_v_flt_ht[pid] + sign(points.sub$ecmwf_wind_10m_v_flt_ht[pid])*1, "Vw",
         col = "#1b9e77")
    
    
    # Va
    arrows(0, 0, points.sub$va_u_flt_ht[pid],
           points.sub$va_v_flt_ht[pid],
           lwd = 2,
           length = 0.15,
           col = "#d95f02",
           lty = 2)
    text( points.sub$va_u_flt_ht[pid] + sign(points.sub$va_u_flt_ht[pid])*1,
          points.sub$va_v_flt_ht[pid] + sign(points.sub$va_v_flt_ht[pid])*1, "Va",
         col = "#d95f02")
    
    arrows(points.sub$ecmwf_wind_10m_u_flt_ht[pid],
           points.sub$ecmwf_wind_10m_v_flt_ht[pid],
           points.sub$vg_u[pid], points.sub$vg_v[pid],
           lwd = 2,
           length = 0.15,
           col = "#d95f02",
           lty = 2)
    
    # Cross-wind
    arrows(0,0,
           (sin(rad(points.sub$va_flt_ht_bearing[pid]+90)))*points.sub$cross_wind_flt_ht[pid],
           (cos(rad(points.sub$va_flt_ht_bearing[pid]+90)))*points.sub$cross_wind_flt_ht[pid],
           
           lwd = 2,
           length = 0.15,
           col = "#1b9e77",
           lty = 2)
    
    text(((sin(rad(points.sub$va_flt_ht_bearing[pid]+90)))*points.sub$cross_wind_flt_ht[pid] + sign((sin(rad(points.sub$va_flt_ht_bearing[pid]+90)))*points.sub$cross_wind_flt_ht[pid])*1),
         ((cos(rad(points.sub$va_flt_ht_bearing[pid]+90)))*points.sub$cross_wind_flt_ht[pid]
          +sign((cos(rad(points.sub$va_flt_ht_bearing[pid]+90)))*points.sub$cross_wind_flt_ht[pid])*1), expression("Vw"["c"]^"H"),
         col = "#1b9e77")
    
    # Relative to track
    arrows(0,0,
           (sin(rad(points.sub$va_flt_ht_bearing[pid])))*points.sub$head_wind_flt_ht[pid],
           (cos(rad(points.sub$va_flt_ht_bearing[pid])))*points.sub$head_wind_flt_ht[pid],
           lwd = 2,
           length = 0.15,
           col = "#1b9e77",
           lty = 2)
    
    text(((sin(rad(points.sub$va_flt_ht_bearing[pid])))*points.sub$head_wind_flt_ht[pid] + sign((sin(rad(points.sub$va_flt_ht_bearing[pid])))*points.sub$head_wind_flt_ht[pid])*1),
         ((cos(rad(points.sub$va_flt_ht_bearing[pid])))*points.sub$head_wind_flt_ht[pid]
          +sign((cos(rad(points.sub$va_flt_ht_bearing[pid])))*points.sub$head_wind_flt_ht[pid])*1), expression("Vw"["s"]^"H"),
         col = "#1b9e77")
    
    # points.sub$track_head_wind_flt_ht
    
    # points.sub$
    
      vg.dir <- 90 - deg(atan2(points.sub$vg_v[pid],
                               points.sub$vg_u[pid]))
    
    # flight.details$vg_v
    # warnings()
    fun2 <- function(x){
      if(is.na(x))return(NA) else{
        if(x<0)return(360 + x) else{
          return(x)
        }
      }
    }
    # fun2(NA)
    vg.dir <- sapply(vg.dir, fun2)
      
    arrows(0,0,
           (sin(rad(vg.dir)))*points.sub$track_head_wind_flt_ht[pid],
           (cos(rad(vg.dir)))*points.sub$track_head_wind_flt_ht[pid],
           lwd = 2,
           length = 0.15,
           col = "red",
           lty = 3)
    
    text(((sin(rad(points.sub$va_flt_ht_bearing[pid])))*points.sub$head_wind_flt_ht[pid] + sign((sin(rad(points.sub$va_flt_ht_bearing[pid])))*points.sub$head_wind_flt_ht[pid])*1),
         ((cos(rad(points.sub$va_flt_ht_bearing[pid])))*points.sub$head_wind_flt_ht[pid]
          +sign((cos(rad(points.sub$va_flt_ht_bearing[pid])))*points.sub$head_wind_flt_ht[pid])*1), expression("Vw"["s"]^"H"),
         col = "#1b9e77")
    
    
    arrows(0,0,
           (sin(rad(vg.dir + 90)))*points.sub$track_head_wind_flt_ht[pid],
           (cos(rad(vg.dir + 90)))*points.sub$track_head_wind_flt_ht[pid],
           
           lwd = 2,
           length = 0.15,
           col = "#1b9e77",
           lty = 2)
    
    text(((sin(rad(points.sub$va_flt_ht_bearing[pid]+90)))*points.sub$cross_wind_flt_ht[pid] + sign((sin(rad(points.sub$va_flt_ht_bearing[pid]+90)))*points.sub$cross_wind_flt_ht[pid])*1),
         ((cos(rad(points.sub$va_flt_ht_bearing[pid]+90)))*points.sub$cross_wind_flt_ht[pid]
          +sign((cos(rad(points.sub$va_flt_ht_bearing[pid]+90)))*points.sub$cross_wind_flt_ht[pid])*1), expression("Vw"["c"]^"H"),
         col = "#1b9e77")
    
    
    # "Vw"["s"]^"H +"
    text(c(-14,2,2,9),c(1,20,-5,1),
         c("West", "North", "South", "East"),
         col = "dark grey")
    legend("topleft", "(e)", bty="n", cex = 1.2) 
    
#     # install.packages("plotrix")
#     library(plotrix)
    
    
    text(1.4,1.8, expression(alpha), cex = 1.2)
    # draw.arc(x=1,y=NULL,radius=.5)
    # points.sub$vg_u[pid], points.sub$vg_v[pid]
    # points.sub$ecmwf_wind_10m_u_flt_ht[pid],
    # points.sub$ecmwf_wind_10m_v_flt_ht[pid]
    xspline(x = c(0.3*points.sub$ecmwf_wind_10m_u_flt_ht[pid],
                  0.11*points.sub$ecmwf_wind_10m_u_flt_ht[pid],
                  0.15*points.sub$vg_u[pid]),
            y= c(0.3*points.sub$ecmwf_wind_10m_v_flt_ht[pid],
                 0.125*points.sub$vg_v[pid],
                 0.15*points.sub$vg_v[pid]),
            shape = -1,
            lwd = 2)
    
    text(-0.8,2, expression(beta), cex = 1.2)

    xspline(x = c(0.20*points.sub$ecmwf_wind_10m_u_flt_ht[pid],
                  0,
                  0.12*points.sub$va_u_flt_ht[pid]),
            y= c(0.20*points.sub$ecmwf_wind_10m_v_flt_ht[pid],
                 0.125*points.sub$va_v_flt_ht[pid],
                 0.12*points.sub$va_v_flt_ht[pid]),
            shape = -1,
            lty = 2,
            lwd = 2)
    
    
    dev.off()
    
# Do for same example flight as all above.



# **** Output figure -----

# Could either combine all, using layout etc, or print 4 panels sepperatly then combine
# with another program, e.g. illustrator, GIMP etc.
# Best option is probably 4 sepperatly, then can combine to single pdf using
# Inkscape, following workflow described here:
# http://b.nanes.org/figures/

