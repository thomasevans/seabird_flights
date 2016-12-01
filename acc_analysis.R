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


load("acc_workspace_20160520.Rdata")

# Add to dataframe
points.acc$wing_beat_freq <- z


# Compare with flights Franc and Julien are analysing -------
library(dplyr)

flight_ids <- c( 436, 564, 571, 5225, 5285, 5917, 14477, 16542, 16606, 20104, 24234, 30960, 31210, 31321, 31599, 31861, 32591, 38745)
flight_ids_combined <- paste("g", flight_ids, sep = "")

# flight_ids_combined <- unique(flight.details$flight_id_combined)

points.acc_sub <- filter(points.acc, flight_id_combined.x %in% flight_ids_combined)

# summary(points.acc$flight_id_combined.x %in% flight_ids_combined)

fligths.with.acc <- unique(points.acc_sub$flight_id_combined.x)
# [1] "g16542" "g16606" "g20104" "g24234" "g30960" "g31599" "g31861"

pdf("flight.acc2.pdf")
for(i in 1:length(fligths.with.acc)){
  points.acc_sub <- filter(points.acc, flight_id_combined.x == fligths.with.acc[i])
  plot(points.acc_sub$wing_beat_freq~points.acc_sub$date_time, ylim = c(2.5,4),
       main = paste(fligths.with.acc[i]))

}
dev.off()


plot(points.acc_sub$wing_beat_freq~points.acc_sub$date_time, ylim = c(2.5,4),
     main = paste(fligths.with.acc[i]))
t_5min <- seq.POSIXt(points.acc_sub$date_time[1],
           points.acc_sub$date_time[length(points.acc_sub$device_info_serial)],
           by = "600 sec")
abline(v=t_5min)

?seq.POSIXt
points.acc_sub <- filter(points.acc, flight_id_combined.x == "g29664")
plot(points.acc_sub$wing_beat_freq~points.acc_sub$date_time, ylim = c(3,4),
     main = paste(fligths.with.acc[i]))

plot(points.acc_sub$latitude~points.acc_sub$longitude)

flights.with.acc.details <- filter(flight.details, flight_id_combined %in% fligths.with.acc)
hist(flights.with.acc.details$duration/60, breaks = 100)


# Summaries of wingbeat frequency during flights ------
# Summarise mean + median wing beat frequency for each 10 minute section of flight
# Plus standardize these numbers by flight mean/median (i.e. divide by)


flight_ids_combined <- unique(flight.details$flight_id_combined)

points.acc_sub <- filter(points.acc, flight_id_combined.x %in% flight_ids_combined)
fligths.with.acc <- unique(points.acc_sub$flight_id_combined.x)

flight.details.acc_sub <- filter(flight.details, flight_id_combined %in% fligths.with.acc)
# summary(points.acc$flight_id_combined.x %in% flight_ids_combined)

flight.details.acc_sub <- filter(flight.details.acc_sub, duration > 30*60 &
                           duration <120*60)
fligths.with.acc <- unique(flight.details.acc_sub$flight_id_combined)

points.acc_sub <- filter(points.acc, flight_id_combined.x %in% fligths.with.acc)




flight.acc.list <- list()

# i <- 1
for(i in 1:length(fligths.with.acc)){
  points.acc_sub_new <- filter(points.acc_sub, flight_id_combined.x == fligths.with.acc[i])
  t_5min <- seq.POSIXt(points.acc_sub_new$date_time[1],
                       points.acc_sub_new$date_time[length(points.acc_sub_new$device_info_serial)],
                       by = "600 sec")
  trip_id <- fligths.with.acc[i]
  n <- length(t_5min) - 1
  t_min_mid <- seq(5,by = 10, length.out = n)
  
  wing_beat_mean <- NA
  wing_beat_median <- NA
  n_samples <- NA
  n_samples_na <- NA
  # ix <- 1
  for(ix in 1:n){
    points.acc_sub.wingbeat <- filter(points.acc_sub_new, date_time >= t_5min[ix] &
                                        date_time < t_5min[ix+1])
    if(nrow(points.acc_sub.wingbeat) == 0){points.acc_sub.wingbeat[1,] <- NA}
    wing_beat_mean[ix] <- mean(points.acc_sub.wingbeat$wing_beat_freq, na.rm = TRUE)
    wing_beat_median[ix] <- median(points.acc_sub.wingbeat$wing_beat_freq, na.rm = TRUE)
    n_samples[ix] <- length(points.acc_sub.wingbeat$wing_beat_freq)
    n_samples_na[ix] <- sum(is.na(points.acc_sub.wingbeat$wing_beat_freq))
    
  }
  wing_beat_mean_standard <- wing_beat_mean/mean(points.acc_sub_new$wing_beat_freq,
                                                 na.rm = TRUE)
  wing_beat_median_standard <- wing_beat_median/median(points.acc_sub_new$wing_beat_freq,
                                                 na.rm = TRUE)
  
  df_tab <- cbind.data.frame(trip_id, n, t_min_mid,
                             wing_beat_mean, wing_beat_median,
                             wing_beat_mean_standard,
                             wing_beat_median_standard,
                             n_samples, n_samples_na)
  flight.acc.list[[i]] <- df_tab
  
}

flight.acc.df <- do.call(rbind.data.frame, flight.acc.list)


plot(flight.acc.df$wing_beat_median_standard~flight.acc.df$t_min_mid)

plot(flight.acc.df$wing_beat_median_standard~flight.acc.df$t_min_mid,
     ylim = c(0.5, 1.5))

boxplot(flight.acc.df$wing_beat_median_standard~flight.acc.df$t_min_mid,
     ylim = c(0.9, 1.1))



library(ggplot2)

p <- ggplot(flight.acc.df, aes(x = as.factor(t_min_mid), y = wing_beat_median_standard)) 
p <- p + geom_boxplot() 
p + ylim(0.9,1.1)



p <- ggplot(flight.acc.df, aes(x = as.factor(t_min_mid), y = wing_beat_median)) 
p <- p + geom_boxplot() 
p + ylim(2.8,3.7)

summary(flight.acc.df$wing_beat_median_new)


## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

flight.acc.df$wing_beat_median_standard_fixed <- flight.acc.df$wing_beat_median_standard

flight.acc.df$wing_beat_median_standard_fixed[is.infinite(flight.acc.df$wing_beat_median_standard_fixed)] <- NA
flight.acc.df$wing_beat_median_standard_fixed[flight.acc.df$wing_beat_median_standard_fixed == 0] <- NA

tgc <- summarySE(flight.acc.df, measurevar="wing_beat_median_standard_fixed", groupvars=c("t_min_mid"))

# Use 95% confidence interval instead of SEM
ggplot(tgc, aes(x=t_min_mid, y=wing_beat_median_standard_fixed)) + 
  geom_errorbar(aes(ymin=wing_beat_median_standard_fixed-ci, ymax=wing_beat_median_standard_fixed+ci), width=.1) +
  geom_line() +
  geom_point() +
  ylim(0.95,1.1)







# Example acc data for one flight --------
# [1] "g16542" "g16606" "g20104" "g24234" "g30960" "g31599" "g31861"

flight_id_ex <- "g31861"

acc.raw <- filter(acc.dat.df, flight_id_combined == flight_id_ex)

acc.proc <- filter(points.acc, flight_id_combined.x == flight_id_ex)


# Az = (Mz-Oz)/Sz

acc.raw$x_acceleration_g <- (acc.raw$x_acceleration - -251)/1325
acc.raw$y_acceleration_g <- (acc.raw$y_acceleration - -91)/1339
acc.raw$z_acceleration_g <- (acc.raw$z_acceleration - 130)/1317

acc.flight.points.g <- summarise(group_by(acc.raw, date_time),
                             
                x_acc_g_mean = mean(x_acceleration_g, na.rm = TRUE),
                x_acc_g_pDBA = sum(abs(x_acceleration_g - x_acc_g_mean)),
                y_acc_g_mean = mean(y_acceleration_g, na.rm = TRUE),
                y_acc_g_pDBA = sum(abs(y_acceleration_g - y_acc_g_mean)),
                z_acc_g_mean = mean(z_acceleration_g, na.rm = TRUE),
                z_acc_g_pDBA = sum(abs(z_acceleration_g - z_acc_g_mean)),
                acc_g_ODBA = sum(x_acc_g_pDBA, y_acc_g_pDBA, z_acc_g_pDBA)
                
)
# 
# par(mfrow=c(3,1))
# plot(acc.flight.points.g$acc_g_ODBA~acc.flight.points.g$date_time,
#      type = "b")
# points(acc.flight.points.g$x_acc_g_pDBA~acc.flight.points.g$date_time,
#        col = "red",
#        type = "b")
# points(acc.flight.points.g$y_acc_g_pDBA~acc.flight.points.g$date_time,
#        col = "blue",
#        type = "b")
# points(acc.flight.points.g$z_acc_g_pDBA~acc.flight.points.g$date_time,
#        col = "grey",
#        type = "b")
# 
# plot(acc.proc$wing_beat_freq~acc.proc$date_time,
#      type = "b")

x <- merge(acc.proc, acc.flight.points.g, by="date_time")
x$odba_per_beat <- x$acc_g_ODBA/(2*x$wing_beat_freq)

# plot(x$odba_per_beat~x$date_time)



pdf(paste(flight_id_ex, ".pdf", sep = ""))
par(mfrow = c(5,1))

par(mar=c(0,4,0,4))         

# Speed over time
plot(x$speed_2d ~ x$date_time,
     #        xlab = "Time",
     #        ylab = expression("Speed ms"^{-1}),
     ylab = "",
     ylim = c(0,max(x$speed_2d, na.rm = TRUE) + 1),
     axes = FALSE)

n <- nrow(x)
segments(x0 = x$date_time[1:(n-1)],
         x1 = x$date_time[-1],
         y0 = x$speed_2d[1:(n-1)],
         y1 = x$speed_2d[-1],
         lwd = 1,
         col = "red")
points(x$speed_2d ~ x$date_time, pch = 21, bg = "white")

axis(4, las = 1)              # y-axis
box()
grid()
title(ylab = expression("Speed ms"^{-1}), line = 1)

par(mar=c(0,4,0,4))         # no top spacing
plot(x$altitude~x$date_time,
     ylab = "",
     ylim = c(-20,max(x$altitude , na.rm = TRUE) +1),
     axes = FALSE)
segments(x0 = x$date_time[1:(n-1)],
         x1 = x$date_time[-1],
         y0 = x$altitude[1:(n-1)],
         y1 = x$altitude[-1],
         lwd = 1,
         col = "red")
points(x$altitude ~ x$date_time, pch = 21, bg = "white")
axis(2,las = 1)                # y-axis
title(ylab = "Height (m)", line = 2.5)
grid()
box()

par(mar=c(0,4,0,4))         # no top spacing

plot(x$acc_g_ODBA~x$date_time,
     type = "b",
     ylab = "",
     axes = FALSE)
points(x$x_acc_g_pDBA~x$date_time,
       col = "red",
       type = "b")
points(x$y_acc_g_pDBA~x$date_time,
       col = "blue",
       type = "b")
points(x$z_acc_g_pDBA~x$date_time,
       col = "grey",
       type = "b")
axis(4,las = 1)              # y-axis
title(ylab = "pDBA/ ODBA", line = 1)
box()
grid()


par(mar=c(0,4,0,4))         # no top spacing

plot(x$wing_beat_freq~x$date_time,
     type = "b",
     ylab = "",
     axes = FALSE)

axis(4,las = 1)              # y-axis
title(ylab = "Wingbeat freq (Hz)", line = 1)
box()
grid()

par(mar=c(3,4,0,4))         

plot(x$odba_per_beat~x$date_time,
     ylab = "ODBA/wingbeat",
     type = "b",
     # log = "y",
     las = 1
)
grid()
title(xlab = "Time", line = 2)
dev.off()



acc.raw_sub <- filter(acc.raw, date_time == x$date_time[6])

pdf("acc_example.pdf")
par(mfrow=(c(3,1)))
plot(acc.raw_sub$x_acceleration_g, type = "b", ylab = "x (g)")
plot(acc.raw_sub$z_acceleration_g, type = "b", ylab = "z (g)")
plot(acc.raw_sub$y_acceleration_g, type = "b", ylab = "y (g)")
dev.off()

# "g5285" %in% flight.details$flight_id_combined
julien.flights.vector <- c("g436", "g564", "g571", "g5225", "g5285", "g5917", "g14477", "g16542", "g16606", "g20104", "g24234", "g30960", "g31210", "g31321", "g31599", "g31861", "g32591", "g38745")
flights.julien <- filter(flight.details, flight_id_combined %in% julien.flights.vector)
flights.julien.details <- select(flights.julien, c(flight_id_combined,
                                                   flight_id,
                                                   ring_number,
                                                   device_info_serial))



points.julien <- filter(points.detailed, flight_id_combined %in% julien.flights.vector)

flight.julien_wind <- summarise(group_by(points.julien, flight_id_combined),
                                 
                           wind_u_10m_mean = mean(ecmwf_wind_10m_u),
                           wind_v_10m_mean = mean(ecmwf_wind_10m_u),
                           lat_start = first(latitude),
                           lat_end = last(latitude),
                           long_start = first(longitude),
                           long_end = last(longitude)
                                 
)







# All flights for Julien ------
flight.tab <- read.csv("D:/Dropbox/LBBG_flight_height/R_files/flights_information.csv")
flight.tab <- flight.tab[-c(8,10),]
julien.flights.vector.num <- as.numeric(sub("g", "", julien.flights.vector))

all(julien.flights.vector.num %in% flight.tab$flight_id)

str(flight.tab)

flight.tab$start_time <- as.POSIXct(flight.tab$start_time, tz="UTC",
                                     format = "%Y-%m-%d %H:%M:%S")
flight.tab$end_time <- as.POSIXct(flight.tab$end_time, tz="UTC",
                                   format = "%Y-%m-%d %H:%M:%S")

points.list <- list()

for(i in 1:nrow(flight.tab)){
  
  sql_query <- paste("SELECT DISTINCT gps_ee_tracking_speed_limited_local.device_info_serial, gps_ee_tracking_speed_limited_local.date_time, gps_ee_tracking_speed_limited_local.latitude, gps_ee_tracking_speed_limited_local.longitude, gps_ee_tracking_speed_limited_local.altitude, gps_ee_tracking_speed_limited_local.speed_2d, gps_ee_tracking_speed_limited_local.direction, gps_ee_tracking_speed_limited_local.h_accuracy, gps_ee_tracking_speed_limited_local.v_accuracy, gps_ee_tracking_speed_limited_local.satellites_used, gps_ee_tracking_speed_limited_local.positiondop, move_bank_variables_all.wind_v_10m, move_bank_variables_all.surface_roughness, move_bank_variables_all.temperature_2m, move_bank_variables_all.wind_u_10m
FROM gps_ee_track_session_limited_local, gps_ee_tracking_speed_limited_local INNER JOIN move_bank_variables_all ON (gps_ee_tracking_speed_limited_local.date_time = move_bank_variables_all.date_time) AND (Val(gps_ee_tracking_speed_limited_local.device_info_serial) = move_bank_variables_all.device_info_serial)
                     WHERE (((gps_ee_tracking_speed_limited_local.device_info_serial)= '",
                     flight.tab$device_info_serial[i],
                     "') AND ((gps_ee_tracking_speed_limited_local.date_time)>=#",
                     flight.tab$start_time[i],
                     "# And (gps_ee_tracking_speed_limited_local.date_time)<=#",
                     flight.tab$end_time[i],
                     "#) AND ((gps_ee_track_session_limited_local.device_info_serial)=",
                     flight.tab$device_info_serial[i],
                     "));",
                     sep = "")
  
  points <- sqlQuery(gps.db, query= gsub("\n", " ", sql_query))
  points$flight_id <- flight.tab$flight_id[i]
  points.list[i] <- list(points)
  
  
}


points.all_new <- do.call(rbind , points.list)




flight.julien_wind <- summarise(group_by(points.all_new, flight_id),
                                
                                wind_u_10m_mean = mean(wind_u_10m),
                                wind_v_10m_mean = mean(wind_v_10m),
                                lat_start = first(latitude),
                                lat_end = last(latitude),
                                long_start = first(longitude),
                                long_end = last(longitude)
                                
)



wind.dir.speed <- wind.dir.speed(flight.julien_wind$wind_u_10m_mean, flight.julien_wind$wind_v_10m_mean)

flight.julien_wind <- cbind.data.frame(flight.julien_wind, wind.dir.speed)

library(geosphere)

flight.julien_wind$direction_rhumb <- bearingRhumb(cbind.data.frame(flight.julien_wind$long_start, flight.julien_wind$lat_start),
             cbind.data.frame(flight.julien_wind$long_end, flight.julien_wind$lat_end))


flight.julien_wind$direction_rhumb - flight.julien_wind$wind.dir
library(compiler)

rowMin <- function(x, ind) apply(x, ind, min)
rowMin(cbind(c(1:10)))

flight.julien_wind$wind_angle <-  apply(cbind.data.frame(((flight.julien_wind$direction_rhumb - flight.julien_wind$wind.dir + 360) %% 360),
    ((flight.julien_wind$wind.dir - flight.julien_wind$direction_rhumb + 360) %% 360)),1,min)

write.csv(flight.julien_wind, file = "trajectories_wind.csv")
