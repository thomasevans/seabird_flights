# Flight model calculations and figures
# Using Marco's flight model


# Load in morphological data for birds ------
birds <- read.csv("deployments_details_export.csv")


# Load Marco's afpt package and other required packages -----
library(afpt)
library(plyr)

# Assemble data -----
gulls <- birds$species == "gull"
males <- birds$sex.morph. == "M"
females <- birds$sex.morph. == "F"

murres <- birds$species == "murre"

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



# Then calculate also for means of gulls - male, gulls - female, gulls - both, and murres

birds.means2 <- ddply(birds, .(species, sex.morph.),
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

birds.means2$name <- paste(birds.means2$species, birds.means2$sex.morph., sep = "_")
spp.means$name <- spp.means$species

# Combine
birds.means <- rbind.data.frame(spp.means[1,2:6], birds.means2[,3:7])




# Individual bird table ----
birds_df <- data.frame(
  massTotal = birds$weight.kg.,
  wingSpan = birds$photo_wing_span,
  wingArea = birds$wing_area.m2.,
  wingbeatFrequency = birds$acc.median.f,
  name = birds$ring_number,
  species = birds$species,
  sex = birds$sex.morph.
)


birds_df$acc_data <- !is.na(birds_df$wingbeatFrequency)

# Put wing-beat freq values in if missing
birds_df$wingbeatFrequency[!birds_df$acc_data & birds$species == "gull" &
                             birds_df$sex == "M"] <- birds.means[3,4]
birds_df$wingbeatFrequency[!birds_df$acc_data & birds$species == "gull" &
                             birds_df$sex == "F"] <- birds.means[2,4]
birds_df$wingbeatFrequency[!birds_df$acc_data & birds$species == "murre"] <- birds.means[4,4]


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
  
  fun_powerchem <- function(speed)computeChemicalPower(computeFlappingPower(birds_means_list[i,],speed),birds_means_list[i,])
  maximumRangeSpeed.chem <- findMaximumRangePower(fun_powerchem, 3, 30)
  birds_mr[[i]] <- maximumRangeSpeed.chem[1,]  
  
}

# str(birds_mr)

birds_mr_df <- do.call(rbind , birds_mr)



birds_list_filtered <- birds_list[!(is.na(birds_list$wingSpan)|
                                       is.na(birds_list$wingArea)|
                                       is.na(birds_list$massTotal)),]
birds_all_mr <- list()
for(i in 1:nrow(birds_list_filtered)){
  
  fun_powerchem <- function(speed)computeChemicalPower(computeFlappingPower(birds_list_filtered[i,],speed),birds_list_filtered[i,])
  maximumRangeSpeed.chem <- findMaximumRangePower(fun_powerchem, 3, 30)
  birds_all_mr[[i]] <- maximumRangeSpeed.chem[1,]  
  
}

# str(birds_mr)

birds_all_mr_df <- do.call(rbind , birds_all_mr)


# Vmp


birds_mp <- list()
for(i in 1:nrow(birds_means_list)){
  
  fun_poweraero <- function(speed)computeFlappingPower(birds_means_list[i,],speed)

    minimumPowerSpeed.aero <- findMinimumPower(fun_poweraero, 3, 30)
  
  birds_mp[[i]] <- minimumPowerSpeed.aero[1,]  
  
}

# str(birds_mr)

birds_mp_df <- do.call(rbind , birds_mp)



birds_list_filtered <- birds_list[!(is.na(birds_list$wingSpan)|
                                      is.na(birds_list$wingArea)|
                                      is.na(birds_list$massTotal)),]
birds_all_mp <- list()
for(i in 1:nrow(birds_list_filtered)){
  
  
  fun_poweraero <- function(speed)computeFlappingPower(birds_list_filtered[i,],speed)
  
  minimumPowerSpeed.aero <- findMinimumPower(fun_poweraero, 3, 30)
  
  birds_all_mp[[i]] <- minimumPowerSpeed.aero[1,] 

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
       xlab = expression("Airspeed ("~ms^{-1}~")"~~"Va"),
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
  
  
  svg("flight_speeds_predicted_recorded_va_track_model_new.svg",
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
       ylim = c(0, 11),
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
  

  
  points(birds_details$Vmr[1:19], rep(9, 19),
         col = cols.new.08[1],
         cex = 1.5)
  points(birds_mr[[1]]$speed, 9,
         bg = cols.new.05[1], pch = 21,
         cex = 2)
  
  points(birds_details$Vmr[20:46], rep(4, 46-19),
         col = cols.new.08[2],
         cex = 1.5)
  points(birds_mr[[4]]$speed, 4,
         bg = cols.new.05[2], pch = 21,
         cex = 2)
  
  points(birds_details$Vmp[1:19], rep(8, 19),
         col = cols.new.08[1],
         cex = 1.5)
  points(birds_mp[[1]]$speed, 8,
         bg = cols.new.05[1], pch = 21,
         cex = 2)
  
  points(birds_details$Vmp[20:46], rep(3, 46-19),
         col = cols.new.08[2],
         cex = 1.5)
  points(birds_mp[[4]]$speed, 3,
         bg = cols.new.05[2], pch = 21,
         cex = 2)
  
  # ?mtext
  mtext("Lesser Black-\nbacked Gulls", side = 2,
        las = 1, at = 10, line = 5, adj = 0.2)
  mtext("Vmr", side = 2,
        las = 1, at = 9, line = 4, adj = 0.2)
  mtext("Vmp", side = 2,
        las = 1, at = 8, line = 4, adj = 0.2)
  mtext("Va", side = 2,
        las = 1, at = 7, line = 4, adj = 0.2)
  mtext("Vg (calm)", side = 2,
        las = 1, at = 6, line = 4, adj = 0.2)
  
  
  mtext("Common \nMurres", side = 2,
        las = 1, at = 5, line = 5, adj = 0.2)
  mtext("Vmr", side = 2,
        las = 1, at = 4, line = 4, adj = 0.2)
  mtext("Vmp", side = 2,
        las = 1, at = 3, line = 4, adj = 0.2)
  mtext("Va", side = 2,
        las = 1, at = 2, line = 4, adj = 0.2)
  mtext("Vg (calm)", side = 2,
        las = 1, at = 1, line = 4, adj = 0.2)
  
  
  # va - gulls
  # mean: 11.19277
  
  # Individuals: 11.87982 11.73128 11.06866 10.70988 10.85327 11.19412 11.69940 11.26912 11.11638 10.99355 11.45010 11.10744 11.15412 11.45894 11.03225 10.36595
  
  
# points(c(11.87982,11.73128,11.06866,10.70988,10.85327,11.19412,11.69940,11.26912,11.11638,10.99355,11.45010,11.10744,11.15412,11.45894,11.03225,10.36595),rep(5,length(c(11.87982,11.73128,11.06866,10.70988,10.85327,11.19412,11.69940,11.26912,11.11638,10.99355,11.45010,11.10744,11.15412,11.45894,11.03225,10.36595))),
#          col = cols.new.08[1],
#          cex = 1.5)
#   points(11.19277, 5,
#          bg = cols.new.05[1], pch = 21,
#          cex = 2)
  
  # Track
  points(c(11.91885,11.36472,11.28023,10.56731,10.94953,10.57653,11.33638,11.14906,10.97801,10.88252,11.26848,11.16238,11.47210,10.95305,10.45212,10.26821
),rep(7,length(c(11.91885,11.36472,11.28023,10.56731,10.94953,10.57653,11.33638,11.14906,10.97801,10.88252,11.26848,11.16238,11.47210,10.95305,10.45212,10.26821
))),
         col = cols.new.08[1],
         cex = 1.5)
  points(11.03622, 7,
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
  
  # Track model
  points(c(14.66591,13.86643,15.52335,11.65145,13.17248,13.79279,13.25963,14.88026,15.76323,14.39630,11.57383,11.64208,12.05237,14.58168,15.45520,13.96711,14.63981,12.85711,11.86864,13.93918,13.75917),rep(2,length(c(14.66591,13.86643,15.52335,11.65145,13.17248,13.79279,13.25963,14.88026,15.76323,14.39630,11.57383,11.64208,12.05237,14.58168,15.45520,13.96711,14.63981,12.85711,11.86864,13.93918,13.75917))),
         col = cols.new.08[2],
         cex = 1.5)
  points(13.68133, 2,
         bg = cols.new.05[2], pch = 21,
         cex = 2)
  
  # Vg (wind < 2ms-1)
  points(c(13.671850,14.937996,11.455421,11.188226,9.191042,11.956423,11.169780,11.666191,10.789200,11.267608,11.626858,11.247204,9.491904,10.553425),rep(6,length(c(13.671850,14.937996,11.455421,11.188226,9.191042,11.956423,11.169780,11.666191,10.789200,11.267608,11.626858,11.247204,9.491904,10.553425
))),
         col = cols.new.08[1],
         cex = 1.5)
  points(11.44379, 6,
         bg = cols.new.05[1], pch = 21,
         cex = 2)
  
#   # Track model
#   points(c(14.66591,13.86643,15.52335,11.65145,13.17248,13.79279,13.25963,14.88026,15.76323,14.39630,11.57383,11.64208,12.05237,14.58168,15.45520,13.96711,14.63981,12.85711,11.86864,13.93918,13.75917),rep(1,length(c(14.66591,13.86643,15.52335,11.65145,13.17248,13.79279,13.25963,14.88026,15.76323,14.39630,11.57383,11.64208,12.05237,14.58168,15.45520,13.96711,14.63981,12.85711,11.86864,13.93918,13.75917))),
#          col = cols.new.08[2],
#          cex = 1.5)
#   points(13.68133, 1,
#          bg = cols.new.05[2], pch = 21,
#          cex = 2)
  
  # Vg (low wind <2 ms-1)
  points(c(15.31111,15.05556,14.50000,11.86111,15.90005,10.44444
),rep(1,length(c(15.31111,15.05556,14.50000,11.86111,15.90005,10.44444
))),
         col = cols.new.08[2],
         cex = 1.5)
  points(13.84538, 1,
         bg = cols.new.05[2], pch = 21,
         cex = 2)
  
  legend("topleft", "(b)", bty="n", cex = 1.2) 
  
  dev.off()
  