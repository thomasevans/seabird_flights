# Does Wingbeat frequency vary during flights and is there a consistant pattern?

library(dplyr)

# Load in data ---
load("acc_workspace_20160520.Rdata")

# Add to dataframe
points.acc$wing_beat_freq <- z

unique(points.acc$flight_id_combined.x)


# Load weather data combined with acc data -----
library(RODBC)
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# Get data
gps.points <- sqlQuery(gps.db,
                       query =
                         "SELECT lund_points_flight_acc_wingbeat_table.*, move_bank_variables_all.temperature_2m
FROM lund_points_flight_acc_wingbeat_table INNER JOIN move_bank_variables_all ON (lund_points_flight_acc_wingbeat_table.date_time = move_bank_variables_all.date_time) AND (lund_points_flight_acc_wingbeat_table.device_info_serial = move_bank_variables_all.device_info_serial);
                       "
                       ,as.is = TRUE)

str(gps.points)
gps.points$date_time <-  as.POSIXct(strptime(gps.points$date_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))
gps.points$ecmwf_temp_2m_C <- gps.points$ecmwf_temp_2m - 273.15
hist(gps.points$ecmwf_temp_2m_C)



# Prepare data.frame ------

flights_to_include <- unique(gps.points$flight_id_combinedx)

# i <- 1

flight_point_list <- list()
for(i in 1:length(flights_to_include)){
  flight_id <- flights_to_include[i]
  flight.points <- filter(gps.points, flight_id_combinedx == flight_id)
  device_info_id <- flight.points$device_info_serial[1]
  wing_beats <- flight.points$wing_beat_freq
  temp_c <- flight.points$ecmwf_temp_2m_C
  date_time <- flight.points$date_time
  t <- flight.points$date_time - flight.points$date_time[1]
  t <- as.numeric(t)

  
  # Altitude
  alt_change <- flight.points$altitude_callib[2:nrow(flight.points)] - flight.points$altitude_callib[1:nrow(flight.points)-1]
  t_dif <- t[2:nrow(flight.points)] - t[1:nrow(flight.points)-1]
  alt_change_rate <- alt_change/t_dif  
  
  # Combine to df
  df.flight <- cbind.data.frame(flight_id, device_info_id,
                                date_time, t, temp_c, wing_beats)
  df.flight <- df.flight[-1,]
  df.flight <- cbind.data.frame(df.flight, alt_change, alt_change_rate)
  flight_point_list[[i]] <- df.flight
# names(df.flight)
  }

flight_point_df <- do.call(rbind.data.frame, flight_point_list)

fun.wingbeat <- function(x){
  if(is.na(x)) return(NA)
  if(x <2.5) return(NA)
  if(x >4.99) return(NA)
  return(x)
}

flight_point_df$wing_beats_restric <- sapply(flight_point_df$wing_beats,
                                             fun.wingbeat)

summary(is.na(flight_point_df$wing_beats_restric ))


# Summary information for foraging trips
individ_summary <- summarise(group_by(flight_point_df,
                                      device_info_id),
                      
                     wing_beat_mean = mean(wing_beats_restric, na.rm = TRUE),
                     wing_beat_median = median(wing_beats_restric, na.rm = TRUE),
                     wing_beat_sd = sd(wing_beats_restric, na.rm = TRUE),
                     n = n(),
                     n_no_na = sum(!is.na(wing_beats_restric))
)

flight_point_df_temp <- merge(flight_point_df, individ_summary, by = "device_info_id")

flight_point_df$wing_beats_restric_centred <- flight_point_df$wing_beats_restric - flight_point_df_temp$wing_beat_mean
flight_point_df$wing_beats_restric_centred_scaled <- flight_point_df$wing_beats_restric_centred/ flight_point_df_temp$wing_beat_sd

hist(flight_point_df$wing_beats_restric_centred_scaled, breaks = 100)

hist(flight_point_df$alt_change_rate, breaks = 100)

# Run analysis on prepared data.frame ---------

library(lme4)
library(arm)
library(lattice)
library(MuMIn)

flight_point_df_sub <- filter(flight_point_df, t_min < 100 )

models.gull <- list()
str(flight_point_df)

flight_point_df$t_min <- flight_point_df$t/60

models.gull[[1]] <- lmer(wing_beats_restric_centred_scaled ~
                           poly(t_min,2,raw=T) *  temp_c +
                           alt_change_rate +
                           (1|device_info_id/flight_id),
                         data = flight_point_df_sub)

models.gull[[2]] <- lmer(wing_beats_restric_centred_scaled ~
                           poly(t_min,2,raw=T) +  temp_c +
                           alt_change_rate +
                            (1|device_info_id/flight_id),
                          data = flight_point_df_sub)
# summary(models.gull[[1]])
models.gull[[3]] <- lmer(wing_beats_restric_centred_scaled ~
                           poly(t_min,2,raw=T) +  alt_change_rate +
                           (1|device_info_id/flight_id),
                         data = flight_point_df_sub)

models.gull[[4]] <- lmer(wing_beats_restric_centred_scaled ~
                           temp_c + alt_change_rate +
                           (1|device_info_id/flight_id),
                         data = flight_point_df_sub)


models.gull[[5]] <- lmer(wing_beats_restric_centred_scaled ~
                           poly(t_min,2,raw=T) +  
                           (1|device_info_id/flight_id),
                         data = flight_point_df_sub)

models.gull[[6]] <- lmer(wing_beats_restric_centred_scaled ~
                           temp_c + 
                           (1|device_info_id/flight_id),
                         data = flight_point_df_sub)

models.gull[[7]] <- lmer(wing_beats_restric_centred_scaled ~
                           alt_change_rate +
                           (1|device_info_id/flight_id),
                         data = flight_point_df_sub)

models.gull[[8]] <- lmer(wing_beats_restric_centred_scaled ~
                           t_min +  
                           (1|device_info_id/flight_id),
                         data = flight_point_df_sub)

models.gull[[9]] <- lmer(wing_beats_restric_centred_scaled ~
                           t_min *  temp_c +
#                            alt_change_rate +
                           (1|device_info_id/flight_id),
                         data = flight_point_df_sub)


models.gull[[9]] <- lmer(wing_beats_restric_centred_scaled ~
                           1 +
                           (1|device_info_id/flight_id),
                         data = flight_point_df_sub)

models.gull[[10]] <- lmer(wing_beats_restric_centred_scaled ~
                           poly(t_min,3,raw=T) *  temp_c +
                           alt_change_rate +
                           (1|device_info_id/flight_id),
                         data = flight_point_df_sub)

models.gull[[11]] <- lmer(wing_beats_restric_centred_scaled ~
                            poly(t_min,3,raw=T) +  temp_c +
                            alt_change_rate +
                            (1|device_info_id/flight_id),
                          data = flight_point_df_sub)

models.gull[[12]] <- lmer(wing_beats_restric_centred_scaled ~
                            poly(t_min,2,raw=T) +  temp_c +
                            alt_change_rate +
                            (1|device_info_id/flight_id),
                          data = flight_point_df_sub)

z <- length(models.gull)
models.gull.stdz <- list()
# models.gull.stdz[[4]] <- models.gull[[4]]
for(i in 1:z){
  models.gull.stdz[[i]] <- standardize(models.gull[[i]], standardize.y=FALSE)
}

models.gull.aicc <- sapply(models.gull.stdz, AICc)
models.gull.aicc.dif <- models.gull.aicc-min(models.gull.aicc)
models.gull.r2m <- sapply(models.gull.stdz, r.squaredGLMM)
t(models.gull.r2m)

models.gull.fit.df <- cbind.data.frame(c(1:z),models.gull.aicc,
                                       models.gull.aicc.dif,
                                       t(models.gull.r2m))
names(models.gull.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")
models.gull.fit.df

library(ggplot2)

p <- ggplot(flight_point_df_sub, aes(x = t_min, y = wing_beats_restric_centred_scaled,
                                     group = flight_id )) 
# p <- p + geom_point(alpha = 0.2) 
# p <- p + geom_line(alpha = 0.1)
# p <- p + geom_smooth(col= "black", method = "lm", alpha = 0.8, se = FALSE)
p <- p +  geom_line(stat="smooth",method = "lm", se = FALSE,
                    col = "black", alpha = 0.5)
p <- p + ylim(-3,3)
p + geom_hline(yintercept=0, lwd = 1, col = "blue")
# ?geom_abline

summary(models.gull[[10]])





library(pbkrtest)

KRSumFun <- function(object, objectDrop, ...) {
  krnames <- c("ndf","ddf","Fstat","p.value","F.scaling")
  r <- if (missing(objectDrop)) {
    setNames(rep(NA,length(krnames)),krnames)
  } else {
    krtest <- KRmodcomp(object,objectDrop)
    unlist(krtest$stats[krnames])
  }
  attr(r,"method") <- c("Kenward-Roger via pbkrtest package")
  r
}

# P values for gull model
drop1(models.gull[[10]], test="user",sumFun=KRSumFun)


mean(flight_point_df_sub$temp_c)
mod.data <- cbind.data.frame(t_min=c(1:100),
                             temp_c = 10,
                             alt_change_rate = 0)

df.pred <- predict(models.gull[[10]],
              REform=NA,
              newdata=mod.data)

plot(df.pred~mod.data$t_min)
points(df.pred~mod.data$t_min, col = "blue")

# Single term deletions
# 
# Model:
#   wing_beats_restric_centred_scaled ~ t_min + (1 | device_info_id/flight_id)
# Method: 
#   Kenward-Roger via pbkrtest package
# 
# 
# ndf    ddf  Fstat    p.value F.scaling
# <none>                                       
#   t_min    1 7209.1 56.428 6.5324e-14         1


# With first ten minutes excluded - where ascending flight may be expected
# Single term deletions
# 
# Model:
#   wing_beats_restric_centred_scaled ~ t_min + (1 | device_info_id/flight_id)
# Method: 
#   Kenward-Roger via pbkrtest package
# 
# 
# ndf  ddf  Fstat   p.value F.scaling
# <none>                                    
#   t_min    1 5572 9.4415 0.0021315         1


# Confidence intervals + coeficients
lbbg_model_va_coef <- summary(models.gull[[10]])$coef[, 1]
lbbg_model_va_ci <- confint(models.gull[[10]], method="Wald")
lbbg_model_va_par_df <- cbind.data.frame(lbbg_model_va_coef,lbbg_model_va_ci[-c(1:3),])
lbbg_model_va_par_df


# With first 10 minutes excluded:
#             lbbg_model_va_coef        2.5 %        97.5 %
#   (Intercept)        0.004523765 -0.075779339  0.0848268702
# t_min             -0.002052488 -0.003359311 -0.0007456647