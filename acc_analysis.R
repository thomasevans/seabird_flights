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

i <- 150
z <- NULL
# hist(z, breaks = 100)
for(i in 1:nrow(points.acc)){
  
  for(i in 8097:nrow(points.acc)){
    
  
    
  device_info_i <- points.acc$device_info_serial[i]
  date_time_i <- points.acc$date_time[i]
  speed_high <- points.acc$vg[i] >3
  if(is.na(speed_high)) speed_high <- FALSE
  
  acc_dat.i <- acc.dat.df[acc.dat.df$device_info_serial == device_info_i &
                            acc.dat.df$date_time == date_time_i,]
  
  
  # plot(acc_dat.i$z_acceleration, type = "l")
  # Don't calculate if missing Acc data, the Vg <3 or there is no variation in the z acc measure
  if(any(is.na(acc_dat.i$z_acceleration)) | !speed_high | var(acc_dat.i$z_acceleration) == 0) {z[i] <- NA }else{
    
    
    z_acc_cen <- acc_dat.i$z_acceleration - mean(abs(acc_dat.i$z_acceleration))
    # plot(z_acc_cen, type = "l")
    
    z_acc_cen_norm <- z_acc_cen/max(abs(z_acc_cen))
    plot(z_acc_cen_norm, type = "l")
    
    
    svg("gull_acc_example_raw.svg",
        width = 5, height = 4, family = "serif")
    # ?pdf
    par(ps = 14, cex = 1.5, cex.lab = 2)
    par(mfrow = c(1,1),cex=1)
    t <- as.difftime((acc_dat.i$index-1)/20, units = "secs")
    
    par(mar=c(5,7,1,2))
    plot(z_acc_cen_norm ~ t , type = "b",
         ylab = paste("Acceleration (scaled)\nZ-component"),
         xlab = "Time (s)",
         las = 1,
         cex.lab = 1.3,
         lty = 2,
         ylim = c(-1,1))
    grid()
    legend("topleft", "(a)", bty="n", cex = 1.2) 
    dev.off()
    
    # ?spec.ar
    x <- spec.ar(z_acc_cen, log="no", plot = FALSE)
    
    max.spec <- max(unlist(x[['spec']]))
    freq.max <- unlist(x[['spec']]) == max.spec
    # str(x['freq'])
    freq <- unlist(x['freq'])[freq.max][1]  # First maxima (if there are multiple maxima of same value[rare])
    spec.freq <- freq*20  # multiply by sampling interval (Hz)
    z[i] <- spec.freq
    
    
    # Plot AR thing
    value <- unlist(x[['spec']])[,1]
    # str(value)
    freq.v <- unlist(x['freq'])*20
    
    svg("gull_acc_example.svg",
        width = 5, height = 3, family = "serif")
    # ?pdf
    par(ps = 14, cex = 1.5, cex.lab = 2)
    par(mfrow = c(1,1),cex=1)
    
    par(mar=c(5,5,1,2))   
    
    plot(value/1000000~freq.v, type = "l",
         ylab = expression("AR spectrum"~(x10^6)),
         xlab = "Frequency (Hz)",
         las = 1,
         cex.lab = 1.3)
    abline(v = spec.freq, lty = 2)
    legend("topleft", "(c)", bty="n", cex = 1.2) 
    dev.off()
    
    
  }
  
  
}
# summary(is.na(z))
  

  
  # Output file
  save(points.acc, file = "points.acc.calc.RData")
  

  
  
  # Resume here
  load("acc_workspace_20160520.Rdata")
  
  # Add to dataframe
  points.acc$wing_beat_freq <- z
  
  # Have a look at the data -----
  
  # i
  
  
  
  
  svg("gull_acc_hist_all.svg",
      width = 5, height = 4, family = "serif")
  # ?pdf
  par(ps = 14, cex = 1.5, cex.lab = 2)
  par(mfrow = c(1,1),cex=1)
  
  par(mar=c(5,6,1,2))   
  
  hist(z, breaks = 100,
       ylab = "Number of\nacceleration samples",
       main = "",
       xlab = "AR peak frequency",
       las = 1,
       cex.lab = 1.3)
  
  #   plot(value~freq.v, type = "l",
  #        ylab = "AR spectrum",
  #        xlab = "Frequency (Hz)",
  #        las = 1,
  #        cex.lab = 1.3)
  # abline(v = spec.freq, lty = 2)
  legend("topleft", "(e)", bty="n", cex = 1.2) 
  dev.off()
  
  
  svg("gull_acc_hist_flight.svg",
      width = 5, height = 4, family = "serif")
  # ?pdf
  par(ps = 14, cex = 1.5, cex.lab = 2)
  par(mfrow = c(1,1),cex=1)
  
  par(mar=c(5,6,1,2))   
  
  hist(z[z > 2 & z < 5], breaks = 150,
       ylab = "Number of\nacceleration samples",
       main = "",
       xlab = "AR peak frequency",
       las = 1,
       cex.lab = 1.3)
  
  #   plot(value~freq.v, type = "l",
  #        ylab = "AR spectrum",
  #        xlab = "Frequency (Hz)",
  #        las = 1,
  #        cex.lab = 1.3)
  # abline(v = spec.freq, lty = 2)
  legend("topleft", "(g)", bty="n", cex = 1.2) 
  dev.off()
  
  
  hist(z, breaks = 200,
       xlab ="wing-beat frequency")
median(z, na.rm = TRUE)
  
f <- !is.na(z) & z >2 & z<5
summary(f)
plot(points.acc$va_flt_ht[f],z[f])
  
plot(points.acc$head_wind_flt_ht[f],z[f])
anova(aov(z[f]~points.acc$head_wind_flt_ht[f]))
plot(z[f]~as.factor(points.acc$device_info_serial[f]))



# Next ---

library(plyr)
# Get median values for each flight

# points.acc$
  # ?ddply
  
mean.range <- function(x, min.x, max.x){
  x2 <- x[x>min.x & x <max.x]
  mean(x2, na.rm = TRUE)
}

sd.range <- function(x, min.x, max.x){
  x2 <- x[x>min.x & x <max.x]
  sd(x2, na.rm = TRUE)
}

median.range <- function(x, min.x, max.x){
  x2 <- x[x>min.x & x <max.x]
  mean(x2, na.rm = TRUE)
}

n.range <- function(x, min.x, max.x){
  x2 <- x[x>min.x & x <max.x]
  sum(!is.na(x2))
}

flight.summary <- ddply(points.acc, .(device_info_serial, flight_id_combined.x),
                        summarise,
      mean = mean(wing_beat_freq, na.rm = TRUE),
      sd = sd(wing_beat_freq, na.rm = TRUE),
      median = median(wing_beat_freq, na.rm = TRUE),
      mean.f = mean.range(wing_beat_freq, 2, 5),
      sd.f = sd.range(wing_beat_freq, 2, 5),
      median.f = median.range(wing_beat_freq, 2, 5),
      n = sum(!is.na(wing_beat_freq)),
      n.f = n.range(wing_beat_freq, 2, 5)
      )
flight.summary$prop_flap <- flight.summary$n.f/flight.summary$n

par(mfrow=c(1,1))
hist(flight.summary$prop_flap, breaks = 20)
hist(flight.summary$n, breaks = 100)
hist(flight.summary$median, breaks = 100)
sum(flight.summary$n)
hist(flight.summary$median.f, breaks = 20)
unique(flight.summary$device_info_serial)



par(mfrow=c(4,1))
f <- points.acc$flight_id_combined.x == "g2861"
plot(points.acc$vg[f]~points.acc$date_time[f])
plot(points.acc$altitude_callib[f]~points.acc$date_time[f])
plot(points.acc$wing_beat_freq[f]~points.acc$date_time[f])


# Combine above with ring_number
ring_numbers <- unique(data.frame(flight.details$device_info_serial, flight.details$ring_number))
names(ring_numbers) <- c("device_info_serial", "ring_number")

flight.summary.ring_number <- merge(flight.summary, ring_numbers, by = "device_info_serial")

plot(flight.summary.ring_number$median.f~ as.factor(as.character(flight.summary.ring_number$ring_number)))

plot(flight.summary.ring_number$median~ as.factor(as.character(flight.summary.ring_number$ring_number)))

# warnings()

# Get summary statistics for individuals (with and without restricted range)
# - median
# - mean (sd)
# - 95% CI??
# ** May (should) exlude very low and high values (possibly gliding??)
# Get number of useful Acc locations per flight too (for some idea of reliability)
# N flights with acc data per individual too

bird.summary <- ddply(flight.summary.ring_number, .(ring_number),
                        summarise,
                        mean = mean(median, na.rm = TRUE),
                        sd = sd(median, na.rm = TRUE),
                        median = median(median, na.rm = TRUE),
                        mean.f = mean.range(median.f, 2, 5),
                        sd.f = sd.range(median.f, 2, 5),
                        median.f = median.range(median.f, 2, 5),
                        # n = sum(!is.na("median")),
                        # n.f = sum(!is.na("median.f")),
                        n = length(ring_number)
)

bird.acc.dep_per <- ddply(flight.summary.ring_number, .(ring_number,
                                                        device_info_serial),
                      summarise,
                      acc.mean = mean(median, na.rm = TRUE),
                      acc.sd = sd(median, na.rm = TRUE),
                      acc.median = median(median, na.rm = TRUE),
                      acc.mean.f = mean.range(median.f, 2, 5),
                      acc.sd.f = sd.range(median.f, 2, 5),
                      acc.median.f = median.range(median.f, 2, 5),
                      # n = sum(!is.na("median")),
                      # n.f = sum(!is.na("median.f")),
                      acc.n = length(ring_number)
)

# summary(is.na(flight.summary.ring_number$median))

flight.details$date_time_include_end
deployments.all <- ddply(flight.details, .(ring_number, device_info_serial, species),
                         summarise,
                         n = length(ring_number),
                         date.first = min(date_time_include_start),
                         date.final = max(date_time_include_end),
                         date.per = ceiling(difftime(date.final, date.first,
                                             units = "days")),
                         nf = sum(altitude_filter_n >= 1),
                         va_10m_mean = mean(va_10m, na.rm = TRUE),
                         va_10m_sd = sd(va_10m, na.rm = TRUE),
                         va_10m_median = median(va_10m, na.rm = TRUE),
                         # Va at flight height with altitude filter
                         va_fltht_mean = mean(va_flt_ht_alt_filter, na.rm = TRUE),
                         va_fltht_sd = sd(va_flt_ht_alt_filter, na.rm = TRUE),
                         va_fltht_median = median(va_flt_ht_alt_filter, na.rm = TRUE),
                         vg_mean = mean(vg, na.rm = TRUE),
                         vg_sd = sd(vg, na.rm = TRUE),
                         vg_median = median(vg, na.rm = TRUE),
                         alt_mean = mean(altitude_callib_extm_no_filter, na.rm = TRUE),
                         alt_sd = sd(altitude_callib_extm_no_filter, na.rm = TRUE),
                         alt_median = median(altitude_callib_extm_no_filter, na.rm = TRUE),
                         alt_filt_mean = mean(altitude_callib_extm, na.rm = TRUE),
                         alt_filt_sd = sd(altitude_callib_extm, na.rm = TRUE),
                         alt_filt_median = median(altitude_callib_extm, na.rm = TRUE)
# flight.details$altitude_callib_extm_no_filter
)

deployments.all$n - deployments.all$nf

deployments.all.acc <- merge(deployments.all, bird.acc.dep_per,
                             by = c("ring_number", "device_info_serial"),
                             all = TRUE
)


write.table(deployments.all.acc,
            file = "deployments_acc.csv",
            sep = ",",
            row.names = FALSE,
            col.names = TRUE)



# ?difftime

# Combine above with morphometric data (wing-area etc) -----


# Then need to do somthing for the guillemots

