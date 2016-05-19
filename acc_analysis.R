# Analysing wing-beat frequency/ activity from Acc data


# Load in data ------

# Acc data
# Individual ACC records
load("acc.dat.df.Rdata")

# Acc events (i.e. sequences)
load("acc.rec.df.Rdata")

# Load flight summary data
load("flight_details.RData")

# GPS data
load("points.detailed.RData")

# Subset above (only those with acc data)
flights.acc <- flight.details[flight.details$flight_id_combined %in% acc.dat.df$flight_id_combined,]

# Merge command??
points.acc <- merge(points.detailed, acc.rec.df, by = c("device_info_serial", "date_time"))
# ?merge



# Look at some examples ----

# Acc records where gull in flight:
points.acc$vg <- sqrt(points.acc$vg_v*points.acc$vg_v + points.acc$vg_u*points.acc$vg_u)

hist(points.acc$vg)

points.acc_vg3 <- points.acc$vg >3

# i <- 150
z <- NULL
# hist(z, breaks = 100)
for(i in 1:sum(points.acc_vg3, na.rm = TRUE)){
  
  for(i in 1:2000){
    
  device_info_i <- points.acc$device_info_serial[points.acc_vg3][i]
  date_time_i <- points.acc$date_time[points.acc_vg3][i]
  
  
  acc_dat.i <- acc.dat.df[acc.dat.df$device_info_serial == device_info_i &
                            acc.dat.df$date_time == date_time_i,]
  
  
  # plot(acc_dat.i$z_acceleration, type = "l")
  
  if(any(is.na(acc_dat.i$z_acceleration))) {z[i] <- NA }else{
    
    
    z_acc_cen <- acc_dat.i$z_acceleration - mean(abs(acc_dat.i$z_acceleration))
    # plot(z_acc_cen, type = "l")
    
    z_acc_cen_norm <- z_acc_cen/max(abs(z_acc_cen))
    # plot(z_acc_cen_norm, type = "l")
    # ?spec.ar
    x <- spec.ar(z_acc_cen, log="no", plot = FALSE)
    
    max.spec <- max(unlist(x[['spec']]))
    freq.max <- unlist(x[['spec']]) == max.spec
    # str(x['freq'])
    freq <- unlist(x['freq'])[freq.max[1]]
    spec.freq <- freq*20
    z[i] <- spec.freq
    
  }
  
  
}
# summary(is.na(z))
  
  hist(z, breaks = 1000)
