# Flight model calculations and figures
# Using Marco's flight model


# Load in morphological data for birds ------
birds <- read.csv("deployments_details_export.csv")


# Load Marco's afpt package and other required packages -----
library(afpt)
library(plyr)
# library(dplyr)

# Assemble data -----
gulls <- birds$species == "gull"
males <- birds$sex.morph. == "M"
females <- birds$sex.morph. == "F"

murres <- birds$species == "murre"
# summary(gulls)
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
#   
#   fun_powerchem <- function(speed)computeChemicalPower(computeFlappingPower(birds_list_filtered[i,],speed),birds_list_filtered[i,])
#   maximumRangeSpeed.chem <- findMaximumRangePower(fun_powerchem, 3, 30)
#   birds_all_mp[[i]] <- maximumRangeSpeed.chem[1,]  
  
}

# str(birds_mr)

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
wind.speed <- seq(-12.0,12.0, 1)
wind.dir <- seq(0,180,2)

winds <- expand.grid(wind.speed, wind.dir)

# Gulls
  fun_powerchem <- function(speed)computeChemicalPower(computeFlappingPower(birds_means_list[1,],speed),birds_means_list[1,])
  maximumRangeSpeed.chem <- findMaximumRangePower(fun_powerchem,
                                                  3, 30, windSpeed = winds[,1],
                                                  windDir = winds[,2])
  gull.mean.wind <- cbind.data.frame(maximumRangeSpeed.chem, winds)


  
  fun_powerchem <- function(speed)computeChemicalPower(computeFlappingPower(birds_means_list[4,],speed),birds_means_list[4,])
  maximumRangeSpeed.chem <- findMaximumRangePower(fun_powerchem,
                                                  3, 30, windSpeed = winds[,1],
                                                  windDir = winds[,2])
  murre.mean.wind <- cbind.data.frame(maximumRangeSpeed.chem, winds)
  
  
  plot(murre.mean.wind$Var1, murre.mean.wind$Var2, col = murre.mean.wind$speed)

  
  plot(gull.mean.wind$Var1, gull.mean.wind$Var2, col = gull.mean.wind$speed)
  
# str(birds_mr)

# birds_mr_wind_df <- do.call(rbind , birds_mr_wind)
gull.mean.wind <- gull.mean.wind[gull.mean.wind$Var1 >= 0,]
murre.mean.wind <- murre.mean.wind[murre.mean.wind$Var1 >= 0,]

names(murre.mean.wind)[23:24] <- c("wind_speed", "wind_dir")
names(gull.mean.wind)[23:24] <- c("wind_speed", "wind_dir")


plot(murre.mean.wind$wind_speed~ murre.mean.wind$wind_dir, col = murre.mean.wind$speed)


plot(gull.mean.wind$wind_speed~ gull.mean.wind$wind_dir, col = gull.mean.wind$speed)



library(CircStats)

  # Calculate wind components (relative to heading)
  
  # Gulls
gull.mean.wind$va_vw_angle <- deg(asin((gull.mean.wind$wind_speed*(sin(rad(gull.mean.wind$wind_dir))))/gull.mean.wind$speed)) + gull.mean.wind$wind_dir
  
  gull.mean.wind$Vw.c <-  gull.mean.wind$wind_speed*sin(rad(gull.mean.wind$va_vw_angle))
  gull.mean.wind$Vw.s <-  gull.mean.wind$wind_speed*cos(rad(gull.mean.wind$va_vw_angle))
  
  
  plot(gull.mean.wind$Vw.c~gull.mean.wind$Vw.s, col = gull.mean.wind$speed,
       xlim = c(-10,10), ylim = c(0,10))
  
  
  # Murres
  murre.mean.wind$va_vw_angle <- deg(asin((murre.mean.wind$wind_speed*(sin(rad(murre.mean.wind$wind_dir))))/murre.mean.wind$speed)) + murre.mean.wind$wind_dir
  
  murre.mean.wind$Vw.c <-  murre.mean.wind$wind_speed*sin(rad(murre.mean.wind$va_vw_angle))
  murre.mean.wind$Vw.s <-  murre.mean.wind$wind_speed*cos(rad(murre.mean.wind$va_vw_angle))
  
  f <- murre.mean.wind$Vw.s == 2
  
  summary(f)
  
  plot(murre.mean.wind$speed~ murre.mean.wind$Vw.c )
  
  
  (murre.mean.wind$wind_speed*murre.mean.wind$wind_speed) - ((murre.mean.wind$Vw.c*murre.mean.wind$Vw.c) + (murre.mean.wind$Vw.s*murre.mean.wind$Vw.s))
  
  
  plot(murre.mean.wind$Vw.c~murre.mean.wind$Vw.s, col = murre.mean.wind$speed,
       xlim = c(-10,10), ylim = c(0,10))
  # abline(v= se)
  grid()
  
  # plot(gull.mean.wind$va_vw_angle,gull.mean.wind$speed )
  