# Script to truncate flights to remove non-directed initial parts of flights
# and to remove final parts of flights when birds approach island


# Load in data -----

# GPS point data
load("points.detailed.RData")



# Flight summary data
load("flights.RData")


# needed to plot maps
library(maps)


# Plot base map
load("SWE_adm0.RData")

points.detailed$point_id <- 1:nrow(points.detailed)

# Truncation thing (find first point to include) --------

# Code to calculate distances
source("deg.dist.R")

# To interpolate flight trajectories to common time interval
library("adehabitatLT")


# Make these into ltraj thing from adehabitat -----
# Treat each flight as a 'burst'
points_all.ltraj <- as.ltraj(points.detailed[,5:4], points.detailed$date_time,
                                  points.detailed$flight_id_combined,
                                  burst = points.detailed$flight_id_combined,
                                  typeII = TRUE)

# plot(points_all.ltraj[2])


# Resample to new fixed time interval ------
# Process with redisltraj from adehabitatLT
# 100 s
points.100 <- redisltraj(points_all.ltraj, 100, type = "time")

# plot(points.100[600])

# Convert data back to data.frame ----
points.100.df <- ld(points.100)


# Change radian angles to degrees
deg.relangle <- deg(points.100.df$rel.angle)


points.df.100 <- points.100.df[,c(12,3,2,1,10)]
names(points.df.100) <- c("flight_id_combined", "date_time", "latitude", "longitude", "turn_angle_rad")
points.df.100$turn_angle_deg <- deg.relangle


# Number of flights
n_flights <- nrow(flights)

# i <- 45

# i <- c(1:n_flights)[flights$flight_id_combined == "g2953"]
# i <- 1
flight.details <- list()
points.included <- list()
pdf("flight_plots_final2.pdf")
# For each flight do:
for(i in 1:n_flights){
  # for(i in 1:100){
    
  # Subset original GPS data
  points.original <- points.detailed[points.detailed$flight_id_combined == flights$flight_id_combined[i],]
  
  # Subset resampled GPS data
  pointsx <- points.df.100[points.df.100$flight_id_combined == flights$flight_id_combined[i],]
  
  # Number of GPS locations
  n <- nrow(pointsx)
  
  # Make an index of GPS points
  points.index <- 1:n
  

  
  # Distance from island centre
  karlso.cen.long   <-  17.972088
  karlso.cen.lat    <-  57.284804
  
  island.dist <- deg.dist(karlso.cen.long, karlso.cen.lat,
                          pointsx$longitude, pointsx$latitude,
                          km = FALSE)
  # plot(island.dist)
  # plot(pointsx$latitude~pointsx$longitude)
#   
  # Find first point < 2 km from island
  island.buffer <- island.dist <2000
  if(sum(island.buffer) >=1){
    last.point <- min(points.index[island.buffer])
  } else {last.point <- max(points.index)}
  
  
  
  # Time interval between GPS locations
  time_interval <- as.numeric(difftime(pointsx$date_time[2:n],pointsx$date_time[1:n-1], units = "secs"))
  time_interval <- c(0, time_interval)
  
  # Velocity relative to island centre
  d.dist <- island.dist[2:n] - island.dist[1:(n-1)]
  d.dist <- c(0, d.dist)
  # plot(island.dist)
  # abline(h = 5000)
  d.speed <- d.dist/ time_interval
  # plot(d.speed)
  d.speed <- d.speed*-1
  
  #Change in speed from previous points
  d.dif <- function(ia, ds = ds){
    mean(ds[(ia ):(ia + 2)]) / mean(ds[(ia - 1):(ia - 3)])
  }
  
  thresh <- 0.3
  # plot(x)
  if((last.point)>6){
      x <- rev(sapply(c(3:(length(d.speed[last.point:1])-3)), d.dif, ds = d.speed[last.point:1]))
      if(is.infinite(x[1])){ x[1] <- 1}
      x <- c(x,1,1,1)
      # length(x) == n
      z <- x > thresh
      
      z[is.na(z)] <- TRUE
      first.point <- max((1:length(z))[!z])
      if(is.infinite(first.point)){first.point <- 1
      }
    }else {first.point <- 1}
  
  
  points2include <- c(first.point:last.point)
  np <- length(points2include)
  
  s50 <- as.difftime(50, units = "secs")
  # Real points
  start_timex <- pointsx$date_time[points2include[1]] - s50
  end_timex <- pointsx$date_time[points2include[np]] + s50
  
  points.original$include <- FALSE
  points.original$include[points.original$date_time > start_timex &
                            points.original$date_time < end_timex] <- TRUE
  points2includex <- c(1:nrow(points.original))[points.original$include]
  
  # If penultimate point is more than 5 km from the island, exlude flight
  island.distx <- deg.dist(karlso.cen.long, karlso.cen.lat,
                           points.original$longitude, points.original$latitude,
                           km = FALSE)
  # test.df <- points.original[points2includex,]
  npx <- length(points2includex)
  
  if(npx>2){
  dcrit <- 2000*mean(deg.dist(points.original$longitude[points2includex][-npx],
                           points.original$latitude[points2includex][-npx],
                           points.original$longitude[points2includex][-1],
                           points.original$latitude[points2includex][-1]))
  if(dcrit > 14000)dcrit <- 14000
  if(dcrit < 7000)dcrit <- 7000
  }else{dcrit <- 7000}
  
  # island.distx[points2includex]
  if(npx>1){
    if(island.distx[points2includex[npx]] > dcrit){
      points2includex <- NULL
      np <- 0
    } else{np <- npx}
  } else {np <- 0}
  
  if(np>1){
    start_timex <- points.original$date_time[points2includex[1]]
    end_timex <- points.original$date_time[points2includex[npx]]
    include_flight <- TRUE
  } else {start_timex <- end_timex <- NA
  include_flight <- FALSE}

  
  flight.info <- cbind.data.frame(points.original$flight_id_combined[1],
                                  start_timex,
                                  end_timex,
                                  include_flight)
  label.points <- rep(FALSE,nrow(points.original))
  label.points[points2includex] <- TRUE
  
  flight.details[[i]] <- flight.info
  
  points.included[[i]] <- cbind.data.frame(points.original$date_time,
                                           points.original$device_info_serial,
                                           points.original$point_id,
                                           label.points)
  
  # Plot data
  
  # Set limits
  c.xlim <- range(points.original$longitude)
  dif    <- c.xlim[2] - c.xlim[1]
  dif    <- dif *.15
  c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))
  
  c.ylim <- range(points.original$latitude)
  dif    <- c.ylim[2] - c.ylim[1]
  dif    <- dif *.15
  c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))
  
  # Plot base map
  plot(gadm, xlim = c.xlim,
       ylim = c.ylim, col="grey", bg = "white",
       main = paste(points.original$flight_id_combined[1], "  included: ", include_flight))
  nx <- nrow(points.original)
  # Add points
  segments(points.original$longitude[-1], points.original$latitude[-1], points.original$longitude[-nx], points.original$latitude[-nx])
  points(points.original$latitude~points.original$longitude)
  points(points.original$latitude[!label.points]~points.original$longitude[!label.points],
         col = "red")
  points(karlso.cen.long, karlso.cen.lat, pch = 4, cex = 2, col = "blue")
  
  # Scale bar and axis
  x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
  y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
  map.scale(x,y,ratio = FALSE, col="black",col.lab="black")
  box(col="black",lwd=2)
  axis(side=(1), las=1, col="black", col.axis="black")
  axis(side=(2), las=1, col="black", col.axis="black")
  
  
}
# warnings()
dev.off()







flights.details.df <- do.call(rbind , flight.details)
names(flights.details.df) <- c("flight_id_combined", "date_time_include_start",
                               "date_time_include_end", "include_flight")

points.included.df <- do.call(rbind , points.included)
names(points.included.df) <- c("date_time", "device_info_serial",
                               "point_id", "included_points")
# Are all flights represented in the table?
all(flights$flight_id_combined %in% as.character(flights.details.df$flight_id_combined))
all(points.detailed$point_id %in% points.included.df$point_id)

# summary(points.included.df$included_points)

# summary(points.included.df$date_time == points.detailed$date_time)


# Merge with current tables -------
str(points.included.df)
str(points.detailed)

points.included.df$device_info_serial <- as.character(points.included.df$device_info_serial)

points.df <- merge(points.detailed, points.included.df,
                   by = "point_id")

all(points.df$device_info_serial.x == points.df$device_info_serial.y)
all(points.df$date_time.x == points.df$date_time.y)

names(points.df)[3:4] <- c("device_info_serial", "date_time")
points.df <- points.df[,-c(67,68)]
# points.df <- points.df[!is.na(points.df$device_info_serial),]
points.df <- points.df[order(points.df$device_info_serial,points.df$date_time),]

flights.df <- merge(flights, flights.details.df, by = "flight_id_combined")
names(flights.df)

summary(flights.df$include_flight)


# Output the data -----

# Points data
save(points.df, file = "points.detailed.incl.RData")

# Output to csv
write.table(points.df, file = "points_detailed_included.csv", col.names = TRUE,
            row.names = FALSE, sep = ",")

# Flight summary data
save(flights.df, file = "flights.detailed.incl.RData")

# Output to csv
write.table(flights.df, file = "flights_detailed_included.csv", col.names = TRUE,
            row.names = FALSE, sep = ",")
