


# Connect to DB ----
library(RODBC)

murre.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')
gull.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


# Get table of UvA flights -----

sql_query <- "SELECT guillemots_gps_flights.flight_id, guillemots_gps_flights.ring_number, guillemots_gps_flights.deploy_id, guillemots_gps_flights.device_info_serial, guillemots_gps_flights.start_time, guillemots_gps_flights.end_time, guillemots_gps_flights.n_points, guillemots_gps_flights.col_dist_start, guillemots_gps_flights.col_dist_end, guillemots_gps_flights.col_dist_dif, guillemots_gps_flights.col_dist_max, guillemots_gps_flights.trip_id, guillemots_gps_flights.duration, guillemots_gps_flights.dist_straight, guillemots_gps_flights.p2p2_dist, guillemots_gps_flights.speed_median, guillemots_gps_flights.type, guillemots_gps_flights.trip_flight_n, guillemots_gps_flights.flight_type, guillemots_gps_flights.flight_trip_n
FROM guillemots_gps_flights
WHERE (((guillemots_gps_flights.device_type)='uva'))
ORDER BY guillemots_gps_flights.device_info_serial, guillemots_gps_flights.start_time;"

flights <- sqlQuery(murre.db, query= gsub("\n", " ", sql_query))


# Get murre UvA deployment details -----
sql_query <- "SELECT guillemots_track_session.device_info_serial, guillemots_track_session.ring_number, guillemots_track_session.start_date, guillemots_track_session.end_date
FROM guillemots_track_session
WHERE (((guillemots_track_session.device_type)='uva'));"

deployments <- sqlQuery(murre.db, query= gsub("\n", " ", sql_query))



# Get Acc data for within deployments -----

z <- 1
# str
# i <- 1
acc.ls <- list()

n <- nrow(deployments)
for(i in 1:n){
  
  
  sql_query <- paste("SELECT DISTINCT gps_ee_acceleration_limited.device_info_serial, gps_ee_acceleration_limited.date_time, gps_ee_acceleration_limited.index, gps_ee_acceleration_limited.x_acceleration, gps_ee_acceleration_limited.y_acceleration, gps_ee_acceleration_limited.z_acceleration
FROM gps_ee_acceleration_limited
                     WHERE (((gps_ee_acceleration_limited.device_info_serial)= ",
                     as.numeric(deployments$device_info_serial[i]),
                     ") AND ((gps_ee_acceleration_limited.date_time)>=#",
                     deployments$start_date[i],
                     "# And (gps_ee_acceleration_limited.date_time)<=#",
                     deployments$end_date[i],
                     "#))
                     ORDER BY gps_ee_acceleration_limited.device_info_serial, gps_ee_acceleration_limited.date_time, gps_ee_acceleration_limited.index;",
                     sep = "")
  
  acc_rec <- sqlQuery(gull.db, query= gsub("\n", " ", sql_query))
  
  # hist(acc_rec$y_acceleration)
  
  if(nrow(acc_rec)>0){
    
    # acc_rec$flight_id_combined  <- flights$flight_id[i]
    acc_rec$ring_number <- deployments$ring_number[i]
    # acc_rec$deploy_id <- deployments$deploy_id[i]
    
    acc.ls[z] <- list(acc_rec)
    
    z <- z + 1
    
  }
  
  
}


acc.dat.df <- do.call(rbind , acc.ls)

unique(acc.dat.df$device_info_serial)


# Make table of Acc periods ------

# Table with start date/time, device_info_serial, ring_number
acc.measures <- unique.data.frame(acc.dat.df[,c(1,2,7)])


summary(as.factor(acc.measures$ring_number))

# Extract frequency (AR) for each (z-axis only) -----
# i <- 1
z <- NULL
# hist(z, breaks = 100)
# SLOW!!! about 2h I think?? For ca. 50,000 acc sample sections
for(i in 1:nrow(acc.measures)){
  # ptm <- proc.time()
  # for(i in 1:200){
    
  i <- idx[p2][600]
  
  
    device_info_i <- acc.measures$device_info_serial[i]
    date_time_i <- acc.measures$date_time[i]
    
    acc_dat.i <- acc.dat.df[acc.dat.df$device_info_serial == device_info_i &
                              acc.dat.df$date_time == date_time_i,]
    
    # Make sure this is ordered
    acc_dat.i <- acc_dat.i[order(acc_dat.i$index),]
   
      z_acc_cen <- acc_dat.i$z_acceleration - mean(abs(acc_dat.i$z_acceleration), na.rm = TRUE)
      # plot(z_acc_cen, type = "l")
      
      z_acc_cen_norm <- z_acc_cen/max(abs(z_acc_cen), na.rm = TRUE)
     
      
      t <- as.difftime((acc_dat.i$index-1)/20, units = "secs")
      # str(t)
      
      # str(acc_dat.i)
      
      
      
      svg("murre_acc_example_raw.svg",
          width = 5, height = 4, family = "serif")
      # ?pdf
      par(ps = 14, cex = 1.5, cex.lab = 2)
      par(mfrow = c(1,1),cex=1)
      
      par(mar=c(5,7,1,2))
       plot(z_acc_cen_norm ~ t , type = "b",
           ylab = paste("Acceleration (scaled)\nZ-component"),
           xlab = "Time (s)",
           las = 1,
           cex.lab = 1.3,
           lty = 2,
           ylim = c(-1,1))
       grid()
       legend("topleft", "(b)", bty="n", cex = 1.2) 
       dev.off()
       
       
       # ?spec.ar
       # spec.ar(z_acc_cen_norm, log="no", plot = TRUE)
      
       # ?as.difftime
       # ?spec.ar
      # ?plot
      
      y <- length(z_acc_cen_norm)
      
      if(any(is.na(z_acc_cen_norm))){
        y <- c(1:length(z_acc_cen_norm))[is.na(z_acc_cen_norm)]
        y <- y[1]-1        
      }

      
      if(y < 10){      z[i] <- NA
      n[i] <- NA }else{
        
        if(any(is.na(z_acc_cen_norm))){
          z_acc_cen_norm <- z_acc_cen_norm[1:(y[1]-1)]
          
        }
        
        x <- spec.ar(z_acc_cen_norm, log="no", plot = TRUE)
        # ?spec.ar
        max.spec <- max(unlist(x[['spec']]))
        freq.max <- unlist(x[['spec']]) == max.spec
        # str(x['freq'])
        freq <- unlist(x['freq'])[freq.max][1]  # First maxima (if there are multiple maxima of same value[rare])
        spec.freq <- freq*20  # multiply by sampling interval (Hz)
        z[i] <- spec.freq
        n[i] <- length(z_acc_cen_norm)
        
        # Plot AR thing
        value <- unlist(x[['spec']])[,1]
        # str(value)
        freq.v <- unlist(x['freq'])*20
        
        
        
        
        svg("murre_acc_example.svg",
            width = 5, height = 3, family = "serif")
        # ?pdf
        par(ps = 14, cex = 1.5, cex.lab = 2)
        par(mfrow = c(1,1),cex=1)
        
        par(mar=c(5,5,1,2))   
        
        plot(value~freq.v, type = "l",
             ylab = "AR spectrum",
             xlab = "Frequency (Hz)",
             las = 1,
             cex.lab = 1.3)
        abline(v = spec.freq, lty = 2)
        legend("topleft", "(d)", bty="n", cex = 1.2) 
        dev.off()
        
      }
   

    
  }
  # summary(is.na(z))
#  tp <-  proc.time() - ptm
#  tp
#  (nrow(acc.measures)/200)*tp/60
#   

  # Add to dataframe
  acc.measures$wing_beat_freq <- z
  acc.measures$n_acc_samples <- n

  
  
  
  
  par(mfrow= c(1,1))
  
  
  
  svg("murre_acc_hist_all.svg",
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
       cex.lab = 1.3,
       ylim = c(0,6000))
  
#   plot(value~freq.v, type = "l",
#        ylab = "AR spectrum",
#        xlab = "Frequency (Hz)",
#        las = 1,
#        cex.lab = 1.3)
  # abline(v = spec.freq, lty = 2)
  legend("topleft", "(f)", bty="n", cex = 1.2) 
  dev.off()
  
  
  svg("murre_acc_hist_flight.svg",
      width = 5, height = 4, family = "serif")
  # ?pdf
  par(ps = 14, cex = 1.5, cex.lab = 2)
  par(mfrow = c(1,1),cex=1)
  
  par(mar=c(5,6,1,2))   
  
  hist(z[acc.measures$wing_beat_freq > 4 & acc.measures$wing_beat_freq < 10], breaks = 50,
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
  legend("topleft", "(h)", bty="n", cex = 1.2) 
  dev.off()
  
  
  
  summary(is.na(z))
  summary(is.na(n))

  hist(z[z>6], breaks = 200)
  hist(z[z<6], breaks = 400)
  
  summary(z == 10)
  
  idx <- c(1:nrow(acc.measures))
  p1 <- acc.measures$wing_beat_freq > 1.5 & acc.measures$wing_beat_freq < 2
  p2 <- acc.measures$wing_beat_freq > 4 & acc.measures$wing_beat_freq < 10
  
  i <- idx[!p1][100]
  
  
  # acc.measures
    
  acc.measures$wing_beat_freq_f <- acc.measures$wing_beat_freq
  
  # Apparent artefact of large peak in frequency of 10 Hz, likely deriving from sampling interval (20 Hz)
  acc.measures$wing_beat_freq_f[acc.measures$wing_beat_freq == 10] <- NA
  
  # 'Innactive points', 0 Hz
  acc.measures$wing_beat_freq_f[acc.measures$wing_beat_freq == 0] <- NA
  

  hist(acc.measures$wing_beat_freq_f, breaks = 100)  
# Connect with GPS ????
  
  
# Label those within flights ----
  acc.measures$flight_id <- NULL
  
  for(i in 1:nrow(flights)){
    
    
    device_info_i <- flights$device_info_serial[i]
    date_time_start_i <- flights$start_time[i]
    date_time_end_i <- flights$start_time[i]
    flight_id_i <- flights$flight_id[i]
    
    acc.measures$flight_id[
      acc.measures$device_info_serial == device_info_i &
        acc.measures$date_time >= date_time_start_i &
        acc.measures$date_time <= date_time_end_i
    ] <- flight_id_i
    
    
    
  }
  summary(as.factor(acc.measures$flight_id))
  
  
  hist(acc.measures$wing_beat_freq[!is.na(acc.measures$flight_id)])
  # acc.measures$ring_number[!is.na(acc.measures$flight_id)]
  
  
  # Individual summaries -----
  
  library(plyr)
  
  
  
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
    median(x2, na.rm = TRUE)
  }
  
  n.range <- function(x, min.x, max.x){
    x2 <- x[x>min.x & x <max.x]
    sum(!is.na(x2))
  }

  
  
  bird.summary <- ddply(acc.measures, .(ring_number, device_info_serial),
                          summarise,
                          mean = mean(wing_beat_freq, na.rm = TRUE),
                          sd = sd(wing_beat_freq, na.rm = TRUE),
                          median = median(wing_beat_freq, na.rm = TRUE),
                          mean.f = mean.range(wing_beat_freq, 6, 10),
                          sd.f = sd.range(wing_beat_freq, 6, 10),
                          median.f = median.range(wing_beat_freq, 6, 10),
                          n = sum(!is.na(wing_beat_freq)),
                          n.f = n.range(wing_beat_freq, 6, 10),
                          date.start = min(date_time),
                        date.end = max(date_time),
                        days = difftime(date.end,date.start, units = "days"),
                        day.int = ceiling(days),
                        samples.med = median(n_acc_samples, na.rm = TRUE),
                        samples.mean = mean(n_acc_samples, na.rm = TRUE)
  )
  
  write.csv(bird.summary, file = "murre_acc_dat.csv", row.names = FALSE)
# ?write.csv  
  
  # ?difftime
  