# Flight model calculations and figures
# Using Marco's flight model


# Load in morphological data for birds ------
birds <- read.csv("deployments_details.csv")


# Load Marco's afpt package and other required packages -----
library(afpt)
library(plyr)

# Assemble data -----
gulls <- birds$species == "gull"
gulls.males <- birds$sex.new == "M"  & birds$species == "gull"
gulls.females <- birds$sex.new == "F"  & birds$species == "gull"

murres <- birds$species == "murre"
murres.males <- birds$sex.new == "M"  & birds$species == "murre"
murres.females <- birds$sex.new == "F" & birds$species == "murre"

# Use individual specific wingbeat frequencies for the gulls that have them
# Means for all others
gull.wingbeat.mean <- mean(birds$acc.median.f[gulls], na.rm = TRUE)
gull.wingbeat.sd <- sd(birds$acc.median.f[gulls], na.rm = TRUE)

gull.male.wingbeat.mean <- mean(birds$acc.median.f[gulls & males], na.rm = TRUE)
gull.male.wingbeat.sd <- sd(birds$acc.median.f[gulls & males], na.rm = TRUE)

gull.female.wingbeat.mean <- mean(birds$acc.median.f[gulls & females], na.rm = TRUE)
gull.female.wingbeat.sd <- sd(birds$acc.median.f[gulls & females], na.rm = TRUE)


# Means for all murres
murre.wingbeat.mean <- mean(birds$acc.median.f[murres], na.rm = TRUE)
murre.wingbeat.sd <- sd(birds$acc.median.f[murres], na.rm = TRUE)

murre.male.wingbeat.mean <- mean(birds$acc.median.f[murres & males], na.rm = TRUE)
murre.male.wingbeat.sd <- sd(birds$acc.median.f[murres & males], na.rm = TRUE)

murre.female.wingbeat.mean <- mean(birds$acc.median.f[murres & females], na.rm = TRUE)
murre.female.wingbeat.sd <- sd(birds$acc.median.f[murres & females], na.rm = TRUE)


# Then calculate also for means of gulls - male, gulls - female, gulls - both, and murres

birds.means2 <- ddply(birds, .(species, sex.new),
                      summarise,
                     massTotal = mean(weight.kg., na.rm = TRUE),
                     wingSpan = mean(photo_wing_span, na.rm = TRUE),
                     wingArea = mean(wing_area.m2., na.rm = TRUE),
                     wingbeatFrequency = mean(acc.median.f, na.rm = TRUE)
                   
)

spp.means <- ddply(birds, .(species),
                     summarise,
                     massTotal = mean(weight.kg., na.rm = TRUE),
                     wingSpan = mean(photo_wing_span, na.rm = TRUE),
                     wingArea = mean(wing_area.m2., na.rm = TRUE),
                     wingbeatFrequency = mean(acc.median.f, na.rm = TRUE)
               )

birds.means2$name <- paste(birds.means2$species, birds.means2$sex.new, sep = "_")
spp.means$name <- spp.means$species

# Combine
birds.means <- rbind.data.frame(spp.means[,2:6], birds.means2[,3:7])




# Individual bird table ----
birds_df <- data.frame(
  massTotal = birds$weight.kg.,
  wingSpan = birds$photo_wing_span,
  wingArea = birds$wing_area.m2.,
  wingbeatFrequency = birds$acc.median.f,
  name = birds$ring_number,
  species = birds$species,
  sex = birds$sex.new
)


birds_df$acc_data <- !is.na(birds_df$wingbeatFrequency)

# Put wing-beat freq values in if missing
birds_df$wingbeatFrequency[!birds_df$acc_data & birds$species == "gull" &
                             birds_df$sex == "M"] <- birds.means[3,4]
birds_df$wingbeatFrequency[!birds_df$acc_data & birds$species == "gull" &
                             birds_df$sex == "F"] <- birds.means[2,4]

# birds_df$wingbeatFrequency[!birds_df$acc_data & birds$species == "murre"] <- birds.means[4,4]


birds_df$wingbeatFrequency[!birds_df$acc_data & birds$species == "murre" &
                             birds_df$sex == "M"] <- birds.means[6,4]
birds_df$wingbeatFrequency[!birds_df$acc_data & birds$species == "murre" &
                             birds_df$sex == "F"] <- birds.means[5,4]

birds_df$wingbeatFrequency[is.na(birds_df$wingbeatFrequency) &
                             birds$species == "murre"] <- birds.means[2,4]


# Calculate Vmr and Vmp ------

birds_list <- Bird(
  massTotal = birds_df$massTotal,
  wingSpan = birds_df$wingSpan,
  wingArea = birds_df$wingArea,
  wingbeatFrequency = birds_df$wingbeatFrequency,
  name = birds_df$name
)


birds_means_list <- Bird(
  massTotal = birds.means$massTotal,
  wingSpan = birds.means$wingSpan,
  wingArea = birds.means$wingArea,
  wingbeatFrequency = birds.means$wingbeatFrequency,
  name = birds.means$name
)
str(birds_means_list)



birds_mr <- list()
for(i in 1:nrow(birds_means_list)){
  
  # fun_powerchem <- function(speed)computeChemicalPower(computeFlappingPower(birds_means_list[i,]),birds_means_list[i,])
  # maximumRangeSpeed.chem <- findMaximumRangeSpeed(fun_powerchem, 3, 30)
  birds_mr[[i]] <- findMaximumRangeSpeed(birds_means_list[i,]) 
  
}

# str(birds_mr)

birds_mr_df <- do.call(rbind , birds_mr)



birds_list_filtered <- birds_list[!(is.na(birds_list$wingSpan)|
                                       is.na(birds_list$wingArea)|
                                       is.na(birds_list$massTotal)),]
birds_all_mr <- list()
for(i in 1:nrow(birds_list_filtered)){
  
#   fun_powerchem <- function(speed)computeChemicalPower(computeFlappingPower(birds_list_filtered[i,],speed),birds_list_filtered[i,])
#   maximumRangeSpeed.chem <- findMaximumRangePower(fun_powerchem, 3, 30)
#   birds_all_mr[[i]] <- maximumRangeSpeed.chem[1,] 
  birds_all_mr[[i]] <- findMaximumRangeSpeed(birds_list_filtered[i,]) 
  
  
}

# str(birds_mr)

birds_all_mr_df <- do.call(rbind , birds_all_mr)


# Vmp


birds_mp <- list()
for(i in 1:nrow(birds_means_list)){
  
#   fun_poweraero <- function(speed)computeFlappingPower(birds_means_list[i,],speed)
# 
#     minimumPowerSpeed.aero <- findMinimumPower(fun_poweraero, 3, 30)
  
  birds_mp[[i]] <- findMinimumPowerSpeed(birds_means_list[i,])  
  
}

# str(birds_mr)

birds_mp_df <- do.call(rbind , birds_mp)



birds_list_filtered <- birds_list[!(is.na(birds_list$wingSpan)|
                                      is.na(birds_list$wingArea)|
                                      is.na(birds_list$massTotal)),]
birds_all_mp <- list()
for(i in 1:nrow(birds_list_filtered)){
  
  
#   fun_poweraero <- function(speed)computeFlappingPower(birds_list_filtered[i,],speed)
#   
#   minimumPowerSpeed.aero <- findMinimumPower(fun_poweraero, 3, 30)
  
  birds_all_mp[[i]] <- findMinimumPowerSpeed(birds_list_filtered[i,])

}


birds_all_mp_df <- do.call(rbind , birds_all_mp)



# Combine these with original tables
birds_df$vmr <- NA
birds_df$vmr[!(is.na(birds_list$wingSpan)|
                 is.na(birds_list$wingArea)|
                 is.na(birds_list$massTotal))] <- birds_all_mr_df$speed

birds_df$vmp <- NA
birds_df$vmp[!(is.na(birds_list$wingSpan)|
                 is.na(birds_list$wingArea)|
                 is.na(birds_list$massTotal))] <- birds_all_mp_df$speed


birds.means$vmr <- birds_mr_df$speed

birds.means$vmp <- birds_mp_df$speed






# Wind calculations --------


# i <- 1
wind.speed <- seq(0,12.0, 0.5)
wind.dir <- seq(0, 180, 5)

winds <- expand.grid(wind.speed, wind.dir)

# Exclude winds which cannot be worked out
winds_10 <- winds[!(winds[,1] >= 8  &  winds[,2] > 45 & winds[,2] < 135),]

# plot(winds_10[,2], winds_10[,1])

# Gulls
  fun_powerchem <- function(speed)computeChemicalPower(computeFlappingPower(birds_means_list[1,],speed),birds_means_list[1,])
  maximumRangeSpeed.chem <- findMaximumRangePower(fun_powerchem,
                                                  3, 30, windSpeed = winds_10[,1],
                                                  windDir = winds_10[,2])
  gull.mean.wind <- cbind.data.frame(maximumRangeSpeed.chem, winds_10)


  
  fun_powerchem <- function(speed)computeChemicalPower(computeFlappingPower(birds_means_list[4,],speed),birds_means_list[4,])
  maximumRangeSpeed.chem <- findMaximumRangePower(fun_powerchem,
                                                  3, 30, windSpeed = winds[,1],
                                                  windDir = winds[,2])
  murre.mean.wind <- cbind.data.frame(maximumRangeSpeed.chem, winds)
  
  
# Rename wind columns
names(murre.mean.wind)[23:24] <- c("wind_speed", "wind_dir")
names(gull.mean.wind)[23:24] <- c("wind_speed", "wind_dir")





  # Calculate wind components (relative to heading) -----
  
  # Required for trig functions
  library(CircStats)

  # Gulls
  gull.mean.wind$va_vw_angle <- deg(asin((
    gull.mean.wind$wind_speed*(sin(rad(gull.mean.wind$wind_dir)))
    )/gull.mean.wind$speed)) + gull.mean.wind$wind_dir
  
  gull.mean.wind$Vw.c <-  gull.mean.wind$wind_speed*sin(rad(gull.mean.wind$va_vw_angle))
  gull.mean.wind$Vw.s <-  gull.mean.wind$wind_speed*cos(rad(gull.mean.wind$va_vw_angle))
  
  
  plot(gull.mean.wind$Vw.c~ gull.mean.wind$Vw.s, col = gull.mean.wind$speed,
       xlim = c(-10,10), ylim = c(0,10))
  
  

  # Murres
  murre.mean.wind$va_vw_angle <- deg(asin((
    murre.mean.wind$wind_speed*(sin(rad(murre.mean.wind$wind_dir)))
    )/murre.mean.wind$speed)) + murre.mean.wind$wind_dir
  
  murre.mean.wind$Vw.c <-  murre.mean.wind$wind_speed*sin(rad(murre.mean.wind$va_vw_angle))
  murre.mean.wind$Vw.s <-  murre.mean.wind$wind_speed*cos(rad(murre.mean.wind$va_vw_angle))
  
  plot(murre.mean.wind$Vw.c~ murre.mean.wind$Vw.s, col = murre.mean.wind$speed,
       xlim = c(-10,10), ylim = c(0,10))

  
  
  # Calculate wind components relative to track ----
  
  gull.mean.wind$Vw.c_track <-  gull.mean.wind$wind_speed*sin(rad(gull.mean.wind$wind_dir))
  gull.mean.wind$Vw.s_track <-  gull.mean.wind$wind_speed*cos(rad(gull.mean.wind$wind_dir))
  
  plot(gull.mean.wind$Vw.c_track~ gull.mean.wind$Vw.s_track, col = gull.mean.wind$speed,
       xlim = c(-10,10), ylim = c(0,10))
  
  
  
  murre.mean.wind$Vw.c_track <-  murre.mean.wind$wind_speed*sin(rad(murre.mean.wind$wind_dir))
  murre.mean.wind$Vw.s_track <-  murre.mean.wind$wind_speed*cos(rad(murre.mean.wind$wind_dir))
  
  plot(murre.mean.wind$Vw.c_track~ murre.mean.wind$Vw.s_track, col = murre.mean.wind$speed,
       xlim = c(-10,10), ylim = c(0,10))
  
  
  
  
  # Calculate Power-curves (for mean murre and mean gull) -------
  
  flightperf.murre <- computeFlightPerformance(birds_means_list[4,])
  flightperf.gull <- computeFlightPerformance(birds_means_list[1,])
  
  
  
  powercurve.murre <- flightperf.murre$powercurve
  powercurve.gull <- flightperf.gull$powercurve
  par(mar=c(3.1,3.1,0.4,3.1),mgp=c(1.9,.7,0),cex=0.75)
  with(powercurve.murre , plot( speed, power.aero, type='b', 
                          xlab=NA, ylab=NA, xlim=c(0,28), ylim = c(0,35)))
  with(powercurve.gull , points( speed, power.aero, type='b', 
                          col = "red"))
  mtext(side = 1, line = 2,'Airspeed (m/s)')
  mtext(side = 2, line = 2,'Aerodynamic power (W)')

  
  
  # Summary stats for tables -----
  birds.means
  
  
  fun_powerchem <- function(speed)computeChemicalPower(computeFlappingPower(birds_means_list[1,],speed),birds_means_list[1,])
  gull.f.wind <- findMaximumRangePower(fun_powerchem,
                                                  3, 30, windSpeed = c(-5,5))
  
  fun_powerchem <- function(speed)computeChemicalPower(computeFlappingPower(birds_means_list[2,],speed),birds_means_list[2,])
  gull.m.wind <- findMaximumRangePower(fun_powerchem,
                                       3, 30, windSpeed = c(-5,5))
  
  fun_powerchem <- function(speed)computeChemicalPower(computeFlappingPower(birds_means_list[3,],speed),birds_means_list[3,])
  gull.all.wind <- findMaximumRangePower(fun_powerchem,
                                       3, 30, windSpeed = c(-5,5))
  
  fun_powerchem <- function(speed)computeChemicalPower(computeFlappingPower(birds_means_list[4,],speed),birds_means_list[4,])
  murre.wind <- findMaximumRangePower(fun_powerchem,
                                       3, 30, windSpeed = c(-5,5))
  
  
  # Birds_all
  birds_details <- cbind.data.frame(birds_list_filtered, birds_all_mp_df[,1], birds_all_mr_df[,1])
  names(birds_details)[21:22] <- c("Vmp", "Vmr")
  
  write.csv(birds_details, row.names = FALSE, file = "birds_predictions_flight.csv")
  
  
  
# **** Various plot ------  
  library("ggplot2")
  library("scales")
  library(RColorBrewer)               #for brewer.pal()
  library(cowplot)
  library(akima)
  
  theme_new <- theme_bw(base_size = 14, base_family = "serif") +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.key.size =   unit(2, "lines"),
          legend.key = element_rect(colour =NA),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text.align = 0,
          legend.key.width = unit(3, "lines"),
          legend.title = element_blank()
    )
  
  
  # Airspeed prediction plots -----
  
  # Murres - according to track ----

  str(murre.mean.wind)
  
  v <- seq(5, 30, 0.5) 
  murre.mean.wind$z2 <- findInterval(murre.mean.wind$speed, v)
  murre.mean.wind$z3 <- v[murre.mean.wind$z2]
  
  
  murre_gg <- data.frame(
    x = murre.mean.wind$Vw.s_track,
    y = murre.mean.wind$Vw.c_track,
    z = murre.mean.wind$speed,
    z2 = murre.mean.wind$z3
  )
  
  
  # install.packages("akima")
  # Using tip from SO answer: http://stackoverflow.com/a/19339663/1172358
  fld <- with(murre_gg, interp(x = x, y = y, z = z, duplicate = "strip",
                               nx = 100, ny = 100))
  # ?interp
  
  
  
  df <- melt(fld$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Va")
  df$x <- fld$x[df$x]
  df$y <- fld$y[df$y]

  
  lab.1 <- expression(atop("Vw"["s"]^"+"~"","Tail-wind"))
  lab.2 <- expression(atop("Vw"["s"]^"-"~"","Head-wind"))

 p <-  ggplot(data = df, aes(x = x, y = y, z = Va)) +
    geom_tile(aes(fill = Va)) +
    stat_contour(colour = "grey20", lty = 2) +
    scale_x_continuous(expand=c(0,0), limits = c(-10,10))+
    scale_y_continuous(expand=c(0,0), limits = c(0,10))+
    coord_fixed() +
    # scale_fill_continuous(name = "Va",
                          # low = "white", high = "blue") 
    scale_fill_gradient2(low = muted("blue"), mid = "white",
                       high = muted("red"), midpoint = 20, space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
    labs( x = expression("Vw"["s"]~~~~"Wind assitance ("~ms^{-1}~")"),
          y = expression("Vw"["c"]~~~~"Cross wind ("~ms^{-1}~")"),
          fill = expression("Va ("~ms^{-1}~")"),
          parse = TRUE) +
   theme_new +
   theme(legend.title = element_text(size = 14)) +
   theme(legend.position = "top")+
   theme(legend.key.size = unit(0.25, "inch")) +
    annotate("text", label = c(paste(lab.1),paste(lab.2)),
             x = c( 8, -7.5),
             y = c( 1.5, 1.5),
             parse=TRUE,
             colour = "grey40",
             size = 4,
             vjust = 0.5) 
  p <- p + annotate("text",  x= -9,
                    y = 9, label = "(b)",
                    vjust = 1, hjust=0, size = 5)
  p
  ggsave("va_murre_model_prediction_track.svg", width = 5, height = 4, units = "in")
  
  
    # Gull - according to track -------
  str(gull.mean.wind)
  
  v <- seq(5, 30, 0.5) 
  gull.mean.wind$z2 <- findInterval(gull.mean.wind$speed, v)
  gull.mean.wind$z3 <- v[gull.mean.wind$z2]
  
  
  gull_gg <- data.frame(
    x = gull.mean.wind$Vw.s_track,
    y = gull.mean.wind$Vw.c_track,
    z = gull.mean.wind$speed,
    z2 = gull.mean.wind$z3
  )
  
  
  # install.packages("akima")
  # Using tip from SO answer: http://stackoverflow.com/a/19339663/1172358
  fld <- with(gull_gg, interp(x = x, y = y, z = z, duplicate = "strip",
                              nx = 100, ny = 100))
  # ?interp
  
  
  
  df <- melt(fld$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Va")
  df$x <- fld$x[df$x]
  df$y <- fld$y[df$y]
  
  lab.1 <- expression(atop("Vw"["s"]^"+"~"","Tail-wind"))
  lab.2 <- expression(atop("Vw"["s"]^"-"~"","Head-wind"))
  
  p <-  ggplot(data = df, aes(x = x, y = y, z = Va)) +
    geom_tile(aes(fill = Va)) +
    stat_contour(colour = "grey20", lty = 2) +
    scale_x_continuous(expand=c(0,0), limits = c(-10,10))+
    scale_y_continuous(expand=c(0,0), limits = c(0,7.5))+
    coord_fixed() +
    # scale_fill_continuous(name = "Va",
    # low = "white", high = "blue") 
    scale_fill_gradient2(low = muted("blue"), mid = "white",
                         high = muted("red"), midpoint = 12.7, space = "Lab",
                         na.value = "grey50", guide = "colourbar") +
    labs( x = expression("Vw"["s"]~~~~"Wind assitance ("~ms^{-1}~")"),
          y = expression("Vw"["c"]~~~~"Cross wind ("~ms^{-1}~")"),
          fill = expression("Va ("~ms^{-1}~")"),
          parse = TRUE) +
    theme_new +
    theme(legend.title = element_text(size = 14)) +
    theme(legend.position = "top")+
    theme(legend.key.size = unit(0.25, "inch")) +
    annotate("text", label = c(paste(lab.1),paste(lab.2)),
             x = c( 8, -7.5),
             y = c( 1.5, 1.5),
             parse=TRUE,
             colour = "grey40",
             size = 4,
             vjust = 0.5) 
  p <- p + annotate("text",  x= -9,
                    y = 7, label = "(a)",
                    vjust = 1, hjust=0, size = 5)
  p
  ggsave("va_gull_model_prediction_track.svg", width = 5, height = 4, units = "in")
  
  
  # Murre - relative to Va vector -----
  
  
  str(murre.mean.wind)
  
#   v <- seq(5, 30, 0.5) 
#   murre.mean.wind$z2 <- findInterval(murre.mean.wind$speed, v)
#   murre.mean.wind$z3 <- v[murre.mean.wind$z2]
  
  
  murre_gg <- data.frame(
    x = murre.mean.wind$Vw.s,
    y = murre.mean.wind$Vw.c,
    z = murre.mean.wind$speed,
    z2 = murre.mean.wind$z3
  )
  
  
  # install.packages("akima")
  # Using tip from SO answer: http://stackoverflow.com/a/19339663/1172358
  fld <- with(murre_gg, interp(x = x, y = y, z = z, duplicate = "strip",
                               nx = 100, ny = 100))
  # ?interp
  
  
  
  df <- melt(fld$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Va")
  df$x <- fld$x[df$x]
  df$y <- fld$y[df$y]
  
  
  lab.1 <- expression(atop("Vw"["s"]^"+"~"","Tail-wind"))
  lab.2 <- expression(atop("Vw"["s"]^"-"~"","Head-wind"))
  
  p <-  ggplot(data = df, aes(x = x, y = y, z = Va)) +
    geom_tile(aes(fill = Va)) +
    stat_contour(colour = "grey20", lty = 2) +
    scale_x_continuous(expand=c(0,0), limits = c(-10,10))+
    scale_y_continuous(expand=c(0,0), limits = c(0,10))+
    coord_fixed() +
    # scale_fill_continuous(name = "Va",
    # low = "white", high = "blue") 
    scale_fill_gradient2(low = muted("blue"), mid = "white",
                         high = muted("red"), midpoint = 20, space = "Lab",
                         na.value = "grey50", guide = "colourbar") +
    labs( x = expression("Vw"["s"]~~~~"Wind assitance ("~ms^{-1}~")"),
          y = expression("Vw"["c"]~~~~"Cross wind ("~ms^{-1}~")"),
          fill = expression("Va ("~ms^{-1}~")"),
          parse = TRUE) +
    theme_new +
    theme(legend.title = element_text(size = 14)) +
    theme(legend.position = "top")+
    theme(legend.key.size = unit(0.25, "inch")) +
    annotate("text", label = c(paste(lab.1),paste(lab.2)),
             x = c( 8, -7.5),
             y = c( 1.5, 1.5),
             parse=TRUE,
             colour = "grey40",
             size = 4,
             vjust = 0.5) 
  p <- p + annotate("text",  x= -9,
                    y = 9, label = "(d)",
                    vjust = 1, hjust=0, size = 5)
  p
  ggsave("va_murre_model_prediction_heading.svg", width = 5, height = 4, units = "in")
  
  
  # Gull - according to heading -------
  str(gull.mean.wind)
  
#   v <- seq(5, 30, 0.5) 
#   gull.mean.wind$z2 <- findInterval(gull.mean.wind$speed, v)
#   gull.mean.wind$z3 <- v[gull.mean.wind$z2]
#   
  
  gull_gg <- data.frame(
    x = gull.mean.wind$Vw.s,
    y = gull.mean.wind$Vw.c,
    z = gull.mean.wind$speed,
    z2 = gull.mean.wind$z3
  )
  
  
  # install.packages("akima")
  # Using tip from SO answer: http://stackoverflow.com/a/19339663/1172358
  fld <- with(gull_gg, interp(x = x, y = y, z = z, duplicate = "strip",
                              nx = 100, ny = 100))
  # ?interp
  
  
  
  df <- melt(fld$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Va")
  df$x <- fld$x[df$x]
  df$y <- fld$y[df$y]
  
  lab.1 <- expression(atop("Vw"["s"]^"+"~"","Tail-wind"))
  lab.2 <- expression(atop("Vw"["s"]^"-"~"","Head-wind"))
  
  p <-  ggplot(data = df, aes(x = x, y = y, z = Va)) +
    geom_tile(aes(fill = Va)) +
    stat_contour(colour = "grey20", lty = 2) +
    scale_x_continuous(expand=c(0,0), limits = c(-10,10))+
    scale_y_continuous(expand=c(0,0), limits = c(0,8))+
    coord_fixed() +
    # scale_fill_continuous(name = "Va",
    # low = "white", high = "blue") 
    scale_fill_gradient2(low = muted("blue"), mid = "white",
                         high = muted("red"), midpoint = 12.7, space = "Lab",
                         na.value = "grey50", guide = "colourbar") +
    labs( x = expression("Vw"["s"]~~~~"Wind assitance ("~ms^{-1}~")"),
          y = expression("Vw"["c"]~~~~"Cross wind ("~ms^{-1}~")"),
          fill = expression("Va ("~ms^{-1}~")"),
          parse = TRUE) +
    theme_new +
    theme(legend.title = element_text(size = 14)) +
    theme(legend.position = "top")+
    theme(legend.key.size = unit(0.25, "inch")) +
    annotate("text", label = c(paste(lab.1),paste(lab.2)),
             x = c( 8, -7.5),
             y = c( 1.5, 1.5),
             parse=TRUE,
             colour = "grey40",
             size = 4,
             vjust = 0.5) 
  p <- p + annotate("text",  x= -9,
                    y = 7.5, label = "(c)",
                    vjust = 1, hjust=0, size = 5)
  p
  ggsave("va_gull_model_prediction_heading.svg", width = 5, height = 4, units = "in")
  
  
  
  # Power curve plot -----
  
  # If resuming from here:
  load("flight_models.RData")
  
  powercurve.murre <- flightperf.murre$powercurve
  powercurve.gull <- flightperf.gull$powercurve
  
  
  par(mar=c(3.1,3.1,0.4,3.1),mgp=c(1.9,.7,0),cex=0.75)
  with(powercurve.murre , plot( speed, power.aero, type='b', 
                                xlab=NA, ylab=NA, xlim=c(0,28), ylim = c(0,35)))
  with(powercurve.gull , points( speed, power.aero, type='b', 
                                 col = "red"))
  mtext(side = 1, line = 2,'Airspeed (m/s)')
  mtext(side = 2, line = 2,'Aerodynamic power (W)')
  
  

  svg("flight_power_curves_new.svg",
      width = 5, height = 4, family = "serif")
  
  par(mfrow = c(1,1))
  par(mar=c(4, 4, 1, 1) + 0.1)   
  
  plot(powercurve.murre$power.aero ~
         powercurve.murre$speed, type = "n",
       # cex.lab = 1.5,
       las = 1,
       xlab = expression("Airspeed ("~italic(Va)~~ms^{-1}~")"),
       ylab = "Aerodynamic power (W)",
       cex.lab = 1.3,
       xlim = c(0, 28),
       ylim = c(0, 35),
       yaxs = "i",
       xaxs = "i")
  grid()    
  
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  cols.new <- gg_color_hue(2)
  cols.new <- rev(cols.new)
  
  points(powercurve.murre$power.aero~powercurve.murre$speed,
         type = "l", lwd = 2, col = cols.new[2])
  
  points(powercurve.gull$power.aero~powercurve.gull$speed,
         type = "l", lwd = 2, col = cols.new[1])
  
  
  points(birds_mr[[1]]$speed,computeFlappingPower(birds_means_list[1,],
                                                  birds_mr[[1]]$speed)[2][1,],
         bg = cols.new[1], pch = 21)
  segments(0,0,birds_mr[[1]]$speed,computeFlappingPower(birds_means_list[1,],
                                                        birds_mr[[1]]$speed)[2][1,],
           lty = 2, cols.new[1])
  segments(birds_mr[[1]]$speed,computeFlappingPower(birds_means_list[1,],
                                                        birds_mr[[1]]$speed)[2][1,],
           birds_mr[[1]]$speed,0,
           lty = 2, cols.new[1])
  
  points(birds_mr[[4]]$speed,computeFlappingPower(birds_means_list[4,],
                                                  birds_mr[[4]]$speed)[2][1,],
         bg = cols.new[2], pch = 21)
  segments(0,0,birds_mr[[4]]$speed,computeFlappingPower(birds_means_list[4,],
                                                        birds_mr[[4]]$speed)[2][1,],
           lty = 2, cols.new[2])
  segments(birds_mr[[4]]$speed,computeFlappingPower(birds_means_list[4,],
                                                    birds_mr[[4]]$speed)[2][1,],
           birds_mr[[4]]$speed,0,
           lty = 2, cols.new[2])
  
  points(birds_mp[[1]]$speed,computeFlappingPower(birds_means_list[1,],
                                                  birds_mp[[1]]$speed)[2][1,],
         bg = cols.new[1], pch = 21)
  segments(birds_mp[[1]]$speed,computeFlappingPower(birds_means_list[1,],
                                  birds_mp[[1]]$speed)[2][1,],birds_mp[[1]]$speed,0,
           lty = 3, cols.new[1])
  segments(birds_mp[[1]]$speed,computeFlappingPower(birds_means_list[1,],
                                                    birds_mp[[1]]$speed)[2][1,],
           0,computeFlappingPower(birds_means_list[1,],
                                                    birds_mp[[1]]$speed)[2][1,],
           lty = 3, cols.new[1])
  
  points(birds_mp[[4]]$speed,computeFlappingPower(birds_means_list[4,],
                                                  birds_mp[[4]]$speed)[2][1,],
         bg = cols.new[2], pch = 21)
  segments(birds_mp[[4]]$speed,computeFlappingPower(birds_means_list[4,],
                                                    birds_mp[[4]]$speed)[2][1,],birds_mp[[4]]$speed,0,
           lty = 3, cols.new[2])
  segments(birds_mp[[4]]$speed,computeFlappingPower(birds_means_list[4,],
                                                    birds_mp[[4]]$speed)[2][1,],
           0,computeFlappingPower(birds_means_list[4,],
                                  birds_mp[[4]]$speed)[2][1,],
           lty = 3, cols.new[2])
  
  
  text(14.3, 6,
       "Vmr", col = cols.new[1])
  text(10.8, 4.5,
       "Vmp", col = cols.new[1])
  text(21.3, 20,
       "Vmr", col = cols.new[2])
  text(16.8, 17,
       "Vmp", col = cols.new[2])
  
  box()
  legend("topleft", "(a)", bty="n", cex = 1.2) 
  
  dev.off()
  
  
  
  
  # For poster ------
  svg("flight_power_curves_new_poster.svg",
      width = 5, height = 4, family = "serif")
  cairo_pdf("flight_power_curves_new_poster.pdf",
      width = 5, height = 4, family = "serif")
  par(mfrow = c(1,1))
  par(mar=c(4, 4, 1, 1) + 0.1)   
  
  plot(powercurve.murre$power.aero ~
         powercurve.murre$speed, type = "n",
       # cex.lab = 1.5,
       las = 1,
       xlab = expression("Airspeed ("~italic(Va)~~ms^{-1}~")"),
       ylab = "Aerodynamic power (W)",
       cex.lab = 1.3,
       # lty = 0,
       xlim = c(-5, 18),
       ylim = c(0, 12),
       yaxs = "i",
       xaxs = "i")
  grid()    
  
  abline(v=0)
  abline(h=0)
  
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  cols.new <- gg_color_hue(2)
  cols.new <- rev(cols.new)
  
  # points(powercurve.murre$power.aero~powercurve.murre$speed,
  #        type = "l", lwd = 2, col = cols.new[2])
  # 
  points(powercurve.gull$power.aero~powercurve.gull$speed,
         type = "l", lwd = 3, col = "grey10")
  
  
  points(birds_mr[[1]]$speed,
         computeFlappingPower(birds_means_list[1,],
                                                  birds_mr[[1]]$speed)[3][1,],
         bg = "grey30", pch = 21)
  segments(0,0,birds_mr[[1]]$speed,computeFlappingPower(birds_means_list[1,],
                                                        birds_mr[[1]]$speed)[3][1,],
           lty = 2,lwd = 2, "grey30")
  segments(birds_mr[[1]]$speed,computeFlappingPower(birds_means_list[1,],
                                                    birds_mr[[1]]$speed)[3][1,],
           birds_mr[[1]]$speed,0,
           lty = 2,lwd = 2, "grey30")
  
  # points(birds_mr[[4]]$speed,computeFlappingPower(birds_means_list[4,],
  #                                                 birds_mr[[4]]$speed)[3][1,],
  #        bg = cols.new[2], pch = 21)
  # segments(0,0,birds_mr[[4]]$speed,computeFlappingPower(birds_means_list[4,],
  #                                                       birds_mr[[4]]$speed)[3][1,],
  #          lty = 2, cols.new[2])
  # segments(birds_mr[[4]]$speed,computeFlappingPower(birds_means_list[4,],
  #                                                   birds_mr[[4]]$speed)[3][1,],
  #          birds_mr[[4]]$speed,0,
  #          lty = 2, cols.new[2])
  
  # Tail wind
  
  vmr_head <- findMaximumRangeSpeed(birds_means_list[1,], windSpeed = 5, windDir = 180)[2:3]
  vmr_head_p <- computeFlappingPower(birds_means_list[1,],
                                     vmr_head[[1]])[3][1,]
  
  
  
  points(vmr_head[[1]],
         vmr_head_p,
         bg = "#ef8a62", pch = 21)
  segments(5,0,vmr_head[[1]],vmr_head_p,
           # vmr_head[[1]],
           lty = 2,lwd = 2, "#ef8a62")
  segments(vmr_head[[1]],vmr_head_p,
           vmr_head[[1]],0,
           lty = 2,lwd = 2, "#ef8a62")
  
  
  vmr_tail <- findMaximumRangeSpeed(birds_means_list[1,], windSpeed = 5, windDir = 0)[2:3]
  vmr_tail_p <- computeFlappingPower(birds_means_list[1,],
                                     vmr_tail[[1]])[3][1,]
  
  points(vmr_tail[[1]],
         vmr_tail_p,
         bg = "#67a9cf", pch = 21)
  segments(-5,0,vmr_tail[[1]],vmr_tail_p,
           # vmr_head[[1]],
           lty = 2,lwd = 2, "#67a9cf")
  segments(vmr_tail[[1]],vmr_tail_p,
           vmr_tail[[1]],0,
           lty = 2,lwd = 2,"#67a9cf")
  
  
  # 
  # points(birds_mp[[1]]$speed,computeFlappingPower(birds_means_list[1,],
  #                                                 birds_mp[[1]]$speed)[2][1,],
  #        bg = cols.new[1], pch = 21)
  # segments(birds_mp[[1]]$speed,computeFlappingPower(birds_means_list[1,],
  #                                                   birds_mp[[1]]$speed)[2][1,],birds_mp[[1]]$speed,0,
  #          lty = 3, cols.new[1])
  # segments(birds_mp[[1]]$speed,computeFlappingPower(birds_means_list[1,],
  #                                                   birds_mp[[1]]$speed)[2][1,],
  #          0,computeFlappingPower(birds_means_list[1,],
  #                                 birds_mp[[1]]$speed)[2][1,],
  #          lty = 3, cols.new[1])
  # 
  # points(birds_mp[[4]]$speed,computeFlappingPower(birds_means_list[4,],
  #                                                 birds_mp[[4]]$speed)[2][1,],
  #        bg = cols.new[2], pch = 21)
  # segments(birds_mp[[4]]$speed,computeFlappingPower(birds_means_list[4,],
  #                                                   birds_mp[[4]]$speed)[2][1,],birds_mp[[4]]$speed,0,
  #          lty = 3, cols.new[2])
  # segments(birds_mp[[4]]$speed,computeFlappingPower(birds_means_list[4,],
  #                                                   birds_mp[[4]]$speed)[2][1,],
  #          0,computeFlappingPower(birds_means_list[4,],
  #                                 birds_mp[[4]]$speed)[2][1,],
  #          lty = 3, cols.new[2])
  
  
  text(13.8, 6,
       expression("V"["mr"]^"0"), col = "grey30")
  # text(10.8, 4.5,
       # "Vmp", col = cols.new[1])
  # text(21.3, 20,
  #      expression("V"["mr"]^"0"), col = cols.new[2])
  # 
  
  # expression("Vw"["s"]+~"(tail-wind)")
  text(12.5, 5,
       expression("V"["mr"]^"+5"), col = "#67a9cf")
  
  text(16.2, 8,
       expression("V"["mr"]^"-5"), col = "#ef8a62")
  
  # text(16.8, 17,
       # "Vmp", col = cols.new[2])
  
  # box()
  # legend("topleft", "(a)", bty="n", cex = 1.2) 
  
  dev.off()
  
  
  
  # Displaying predicted, and actual speeds ----
  
  # Code for transparent colours 
  # Code from https://github.com/mylesmharrison/colorRampPaletteAlpha/blob/master/colorRampPaletteAlpha.R
  # Hight-lighted by blog post: http://www.everydayanalytics.ca/2014/03/colorRampPalette-alpha-in-R.html
  addalpha <- function(colors, alpha=1.0) {
    r <- col2rgb(colors, alpha=T)
    # Apply alpha
    r[4,] <- alpha*255
    r <- r/255.0
    return(rgb(r[1,], r[2,], r[3,], r[4,]))
  }
  
  
  svg("flight_speeds_predicted_recorded_va_track_model_new_goal_models.svg",
      width = 5, height = 6, family = "serif")
  
  par(mfrow = c(1,1))
  par(mar=c(4, 7.5, 1, 1) + 0.1)   
  
  plot(powercurve.murre$power.aero ~
         powercurve.murre$speed, type = "n",
       # cex.lab = 1.5,
       las = 1,
       xlab = expression("Speed ("~ms^{-1}~")"),
       ylab = "",
       cex.lab = 1.3,
       xlim = c(5, 28),
       ylim = c(0, 12),
       yaxt = "n",
       yaxs = "i",
       xaxs = "i")
  grid(ny = 9)    
  # ?grid
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  cols.new <- gg_color_hue(2)
  cols.new <- rev(cols.new)
  cols.new.05 <- addalpha(cols.new, 0.6)
  cols.new.08 <- addalpha(cols.new, 0.6)
  

  # Gulls Vmr
  points(birds_details$Vmr[1:19], rep(10, 19),
         col = cols.new.08[1],
         cex = 1.5)
  points(birds_mr[[1]]$speed, 10,
         bg = cols.new.05[1], pch = 21,
         cex = 2)
  
  # Murres Vmr
  points(birds_details$Vmr[20:46], rep(5, 46-19),
         col = cols.new.08[2],
         cex = 1.5)
  points(birds_mr[[4]]$speed, 5,
         bg = cols.new.05[2], pch = 21,
         cex = 2)
  
  # Gulls Vmp
  points(birds_details$Vmp[1:19], rep(9, 19),
         col = cols.new.08[1],
         cex = 1.5)
  points(birds_mp[[1]]$speed, 9,
         bg = cols.new.05[1], pch = 21,
         cex = 2)
  
  # Murres Vmp
  points(birds_details$Vmp[20:46], rep(4, 46-19),
         col = cols.new.08[2],
         cex = 1.5)
  points(birds_mp[[4]]$speed, 4,
         bg = cols.new.05[2], pch = 21,
         cex = 2)
  
  # Labels
  mtext("Lesser Black-\nbacked Gulls", side = 2,
        las = 1, at = 11, line = 5, adj = 0.2)
  mtext("Vmr", side = 2,
        las = 1, at = 10, line = 4, adj = 0.2)
  mtext("Vmp", side = 2,
        las = 1, at = 9, line = 4, adj = 0.2)
  mtext("Va", side = 2,
        las = 1, at = 8, line = 4, adj = 0.2)
  mtext("Vg (calm)", side = 2,
        las = 1, at = 7, line = 4, adj = 0.2)
  
  
  mtext("Common \nMurres", side = 2,
        las = 1, at = 6, line = 5, adj = 0.2)
  mtext("Vmr", side = 2,
        las = 1, at = 5, line = 4, adj = 0.2)
  mtext("Vmp", side = 2,
        las = 1, at = 4, line = 4, adj = 0.2)
  mtext("Va", side = 2,
        las = 1, at = 3, line = 4, adj = 0.2)
  # mtext("Va*", side = 2,
        # las = 1, at = 2, line = 4, adj = 0.2)
  mtext("Vg (calm)", side = 2,
        las = 1, at = 2, line = 4, adj = 0.2)
  
  
  # va - gulls
  # mean: 11.19277
  
  # Individuals: 11.87982 11.73128 11.06866 10.70988 10.85327 11.19412 11.69940 11.26912 11.11638 10.99355 11.45010 11.10744 11.15412 11.45894 11.03225 10.36595
  
  
# points(c(11.87982,11.73128,11.06866,10.70988,10.85327,11.19412,11.69940,11.26912,11.11638,10.99355,11.45010,11.10744,11.15412,11.45894,11.03225,10.36595),rep(5,length(c(11.87982,11.73128,11.06866,10.70988,10.85327,11.19412,11.69940,11.26912,11.11638,10.99355,11.45010,11.10744,11.15412,11.45894,11.03225,10.36595))),
#          col = cols.new.08[1],
#          cex = 1.5)
#   points(11.19277, 5,
#          bg = cols.new.05[1], pch = 21,
#          cex = 2)
  
  # Track - Va for gulls
  points(c(11.70413,11.48595,11.04980,10.58815,10.57708,10.68146,10.99835,10.84219,11.07618,10.94113,11.00763,11.06298,11.12828,11.05650,10.48532,10.25406
),rep(8,length(c(11.70413,11.48595,11.04980,10.58815,10.57708,10.68146,10.99835,10.84219,11.07618,10.94113,11.00763,11.06298,11.12828,11.05650,10.48532,10.25406

))),
         col = cols.new.08[1],
         cex = 1.5)
  points(10.9337, 8,
         bg = cols.new.05[1], pch = 21,
         cex = 2)
  
  
  # Va - murres
  # mean:14.83481
  
  # Individuals: 15.90540 15.65118 17.22575 13.11338 13.95448 15.02935 14.78943 15.82419 16.64745 15.05879 12.70207 12.60923 13.46966 15.52329 16.59419 14.74537 15.46680 14.00277 13.19658 14.88626 15.13534
#   
# points(c(15.90540,15.65118,17.22575,13.11338,13.95448,15.02935,14.78943,15.82419,16.64745,15.05879,12.70207,12.60923,13.46966,15.52329,16.59419,14.74537,15.46680,14.00277,13.19658,14.88626,15.13534),rep(1,length(c(15.90540,15.65118,17.22575,13.11338,13.95448,15.02935,14.78943,15.82419,16.64745,15.05879,12.70207,12.60923,13.46966,15.52329,16.59419,14.74537,15.46680,14.00277,13.19658,14.88626,15.13534))),
#          col = cols.new.08[2],
#          cex = 1.5)
#   points(14.83481, 1,
#          bg = cols.new.05[2], pch = 21,
#          cex = 2)
  
  # Goal model - murres airspeed
  points(c(14.89787,13.96929,16.01708,13.04651,13.32695,13.04037,13.49781,14.67886,15.33522,14.27410,11.49553,11.13253,11.89644,14.61107,14.77792,13.54308,14.63569,13.17930,12.49256,13.99551,14.01750),
         rep(3,length(c(14.89787,13.96929,16.01708,13.04651,13.32695,13.04037,13.49781,14.67886,15.33522,14.27410,11.49553,11.13253,11.89644,14.61107,14.77792,13.54308,14.63569,13.17930,12.49256,13.99551,14.01750))),
         col = cols.new.08[2],
         cex = 1.5)
  points(13.79754, 3,
         # bg = "blue", pch = 21,
         bg = cols.new.05[2], pch = 21,
         cex = 2)
#   points(13.86643, 2,
#          bg = cols.new.05[2], pch = 23,
#          cex = 2)
#   
#   # Using fixed altitude (10m still model 7)
#   points(c(16.21770,15.60502,17.45417,14.35015,14.78810,14.35910,15.26232,15.75134,16.38967,15.62196,13.11194,12.72520,13.18415,15.82789,16.13410,15.55406,15.51106,14.14780,13.45607,15.14789,15.44444),rep(2,length(c(16.21770,15.60502,17.45417,14.35015,14.78810,14.35910,15.26232,15.75134,16.38967,15.62196,13.11194,12.72520,13.18415,15.82789,16.13410,15.55406,15.51106,14.14780,13.45607,15.14789,15.44444))),
#          col = cols.new.08[2],
#          cex = 1.5)
#   points(15.04972, 2,
#          bg = cols.new.05[2], pch = 21,
#          cex = 2)
#   points(13.86643, 2,
#          bg = cols.new.05[2], pch = 23,
#          cex = 2)
  
  # Gulls Vg under calm conditions
  # Vg (wind < 2ms-1)
  points(c(13.671850,14.937996,11.455421,11.188226,9.191042,11.956423,11.169780,11.666191,10.789200,11.267608,11.626858,11.247204,9.491904,10.553425),rep(7,length(c(13.671850,14.937996,11.455421,11.188226,9.191042,11.956423,11.169780,11.666191,10.789200,11.267608,11.626858,11.247204,9.491904,10.553425
))),
         col = cols.new.08[1],
         cex = 1.5)
  points(11.44379, 7,
         bg = cols.new.05[1], pch = 21,
         cex = 2)
  
  
#   # Track model
#   points(c(14.66591,13.86643,15.52335,11.65145,13.17248,13.79279,13.25963,14.88026,15.76323,14.39630,11.57383,11.64208,12.05237,14.58168,15.45520,13.96711,14.63981,12.85711,11.86864,13.93918,13.75917),rep(1,length(c(14.66591,13.86643,15.52335,11.65145,13.17248,13.79279,13.25963,14.88026,15.76323,14.39630,11.57383,11.64208,12.05237,14.58168,15.45520,13.96711,14.63981,12.85711,11.86864,13.93918,13.75917))),
#          col = cols.new.08[2],
#          cex = 1.5)
#   points(13.68133, 1,
#          bg = cols.new.05[2], pch = 21,
#          cex = 2)
  
  # Murres
  # Vg (low wind <2 ms-1)
  points(c(15.31111,15.05556,14.50000,11.86111,15.90005,10.44444
),rep(2,length(c(15.31111,15.05556,14.50000,11.86111,15.90005,10.44444
))),
         col = cols.new.08[2],
         cex = 1.5)
  points(13.84538, 2,
         bg = cols.new.05[2], pch = 21,
         cex = 2)
  # Median
  points(14.77778, 2,
         bg = cols.new.05[2], pch = 23,
         cex = 2)
  
  
  legend("topleft", "(b)", bty="n", cex = 1.2) 
  
  dev.off()
  