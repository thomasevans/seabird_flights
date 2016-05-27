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



# Adapt this:
tailwind <- seq(-10.0,10.0)
flightperf_wind_gull <- adply(
  tailwind,
  1,
  function(tailwind)findMaximumRangePower(
    function(speed)computeChemicalPower(
      computeFlappingPower(bird,speed),
      bird
    ),
    lower=5,
    upper=30,
    windSpeed=tailwind
  )
)
