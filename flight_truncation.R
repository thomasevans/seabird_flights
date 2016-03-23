# Script to truncate flights to remove non-directed initial parts of flights
# and to remove final parts of flights when birds approach island


# Load in data -----

# GPS point data
load("points.detailed.RData")



# Flight summary data
load("flights.RData")

# 
# points.info <- merge(points.detailed, flights, by = "flight_id_combined")
# 
# 
# 
# test.df <- points.info[points.info$species == "murre" & points.info$device_type == "igu",]
# test.df.x <- unique(test.df)

# needed to plot maps
library(maps)


# Plot base map
load("SWE_adm0.RData")

points.detailed$point_id <- 1:nrow(points.detailed)

# Truncation thing (find first point to include) --------

# Code to calculate distances
source("deg.dist.R")

# Number of flights
n_flights <- nrow(flights)

# i <- 45


i <- c(1:n_flights)[flights$flight_id_combined == "g3005"]

flight.details <- list()
points.included <- list()
pdf("flight_plots4.pdf")
# For each flight do:
for(i in 1:n_flights){
  # for(i in 1:10){
    
  # Subset GPS data
  pointsx <- points.detailed[points.detailed$flight_id_combined == flights$flight_id_combined[i],]
  
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
  
  d.speed <- d.dist/ time_interval
  # plot(d.speed)
  
  #Change in speed from previous points
  d.dif <- function(ia, ds = ds){
    mean(ds[(ia ):(ia + 2)]) / mean(ds[(ia - 1):(ia - 3)])
  }
  
  thresh <- 0.3
  
  if((last.point)>5){
      x <- rev(sapply(c(3:(length(d.speed[last.point:1])-2)), d.dif, ds = d.speed[last.point:1]))
      if(is.infinite(x[1])){ x[1] <- 1}
      x <- c(x,1,1,1,1)
      # length(x) == n
      z <- x>0.3
      
      z[is.na(z)] <- TRUE
      first.point <- max((1:length(z))[!z])
      if(is.infinite(first.point)){first.point <- 1
      }
    }else {first.point <- 1}
  
  
  points2include <- c(first.point:last.point)
  np <- length(points2include)
  
  # If penultimate point is more than 5 km from the island, exlude flight
  if(island.dist[points2include[np-1]] > 7000){
    points2include <- rep(FALSE,n)
    np <- 0
    
  }
  
  if(np>1){
    start_time <- pointsx$date_time[points2include[1]]
    end_time <- pointsx$date_time[points2include[np]]
    include_flight <- TRUE
  } else {start_time <- end_time <- NA
  include_flight <- FALSE}

  
  flight.info <- cbind.data.frame(pointsx$flight_id_combined[1],
                                  start_time,
                                  end_time,
                                  include_flight)
  label.points <- rep(FALSE,n)
  label.points[points2include] <- TRUE
  
  flight.details[[i]] <- flight.info
  
  points.included[[i]] <- cbind.data.frame(pointsx$date_time,
                                           pointsx$device_info_serial,
                                           pointsx$point_id,
                                           label.points,
                                           d.dist)
  
  # Plot data
  
  # Set limits
  c.xlim <- range(pointsx$longitude)
  dif    <- c.xlim[2] - c.xlim[1]
  dif    <- dif *.15
  c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))
  
  c.ylim <- range(pointsx$latitude)
  dif    <- c.ylim[2] - c.ylim[1]
  dif    <- dif *.15
  c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))
  
  # Plot base map
  plot(gadm, xlim = c.xlim,
       ylim = c.ylim, col="grey", bg = "white",
       main = paste(pointsx$flight_id_combined[1]))
  
  # Add points
  segments(pointsx$longitude[-1], pointsx$latitude[-1], pointsx$longitude[-n], pointsx$latitude[-n])
  points(pointsx$latitude~pointsx$longitude)
  points(pointsx$latitude[!label.points]~pointsx$longitude[!label.points],
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

dev.off()

flights.details.df <- do.call(rbind , flight.details)
names(flights.details.df) <- c("flight_id_combined", "date_time_include_start",
                               "date_time_include_end", "include_flight")

points.included.df <- do.call(rbind , points.included)
names(points.included.df) <- c("date_time", "device_info_serial",
                               "point_id", "included_points",
                               "dist_sk_centre_m")
# Are all flights represented in the table?
all(flights$flight_id_combined %in% as.character(flights.details.df$flight_id_combined))
all(points.detailed$point_id %in% points.included.df$point_id)

summary(points.included.df$included_points)

summary(points.included.df$date_time == points.detailed$date_time)


# Merge with current tables -------
str(points.included.df)
str(points.detailed)

points.included.df$device_info_serial <- as.character(points.included.df$device_info_serial)

points.df <- merge(points.detailed, points.included.df,
                   by = "point_id")

points.df <- points.df[order(c(points.df$device_info_serial,points.df$date_time)),]

# points.df2 <- points.df[!is.na(points.df$date_time),]

# ?merge

# Output the data -----

# Points data


# Flight summary data