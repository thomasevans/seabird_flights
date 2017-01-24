# New flight calculations using updated package

# Load package
library(afpt)


# Load in morphological data for birds ------
birds <- read.csv("deployments_details.csv")



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

gull.male.wingbeat.mean <- mean(birds$acc.median.f[gulls.males], na.rm = TRUE)
gull.male.wingbeat.sd <- sd(birds$acc.median.f[gulls.males], na.rm = TRUE)

gull.female.wingbeat.mean <- mean(birds$acc.median.f[gulls.females], na.rm = TRUE)
gull.female.wingbeat.sd <- sd(birds$acc.median.f[gulls.females], na.rm = TRUE)


# Means for all murres
murre.wingbeat.mean <- mean(birds$acc.median.f[murres], na.rm = TRUE)
murre.wingbeat.sd <- sd(birds$acc.median.f[murres], na.rm = TRUE)

murre.male.wingbeat.mean <- mean(birds$acc.median.f[murres.males], na.rm = TRUE)
murre.male.wingbeat.sd <- sd(birds$acc.median.f[murres.males], na.rm = TRUE)

murre.female.wingbeat.mean <- mean(birds$acc.median.f[murres.females], na.rm = TRUE)
murre.female.wingbeat.sd <- sd(birds$acc.median.f[murres.females], na.rm = TRUE)


# Then calculate also for means of gulls - male, gulls - female, gulls - both, and murres

birds.means2 <- ddply(birds, .(species, sex.new),
                      summarise,
                      massTotal = mean(weight.kg., na.rm = TRUE),
                      wingSpan = mean(photo_wing_span/100, na.rm = TRUE),
                      wingArea = mean(wing_area.m2., na.rm = TRUE),
                      wingbeatFrequency = mean(acc.median.f, na.rm = TRUE)
                      
)

spp.means <- ddply(birds, .(species),
                   summarise,
                   massTotal = mean(weight.kg., na.rm = TRUE),
                   wingSpan = mean(photo_wing_span/100, na.rm = TRUE),
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
  wingSpan = birds$photo_wing_span/100,
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



# Flight performance
computeFlightPerformance(Bird(birds_df[1,], type = "other"))

# warnings()



# Wing freq expected (Pennycuick 1996)
func.wing.beat.freq <- function(m, g = 9.8, b, S){
  (m^(3/8))*(g^0.5)*(b^(-23/24))*(S^(-1/3))*(1.23^(-3/8))  
}

# gulls f
func.wing.beat.freq(m = 0.71, b = 1.34, S = 0.164)

# gulls m
func.wing.beat.freq(m = 0.76, b = 1.44, S = 0.186)

# gulls all
func.wing.beat.freq(m = 0.74, b = 1.40, S = 0.178)



# murre f
func.wing.beat.freq(m = 0.89, b = 0.76, S = 0.062)

# murre m
func.wing.beat.freq(m = 0.92, b = 0.74, S = 0.061)

# murre all
func.wing.beat.freq(m = 0.90, b = 0.75, S = 0.061)


birds_df_filtered <- birds_df[!(is.na(birds_df$wingSpan)|
                                      is.na(birds_df$wingArea)|
                                      is.na(birds_df$massTotal)),]


# Optimal speeds
vmr <- vmp <- vmr_5_tail <- vmr_5_head <- NULL
for(i in 1:46){
  x <- computeFlightPerformance(Bird(birds_df_filtered[i,], type = "other"))
  vmp[i] <- (x$table$speed)[2]
  vmr[i] <- (x$table$speed)[3]
  
  vmr_5_tail[i] <- unlist(findMaximumRangeSpeed(Bird(birds_df_filtered[i,], type = "other"),
                                          windSpeed=5)[2])
  vmr_5_head[i] <- unlist(findMaximumRangeSpeed(Bird(birds_df_filtered[i,], type = "other"),
                                          windSpeed=-5)[2])
}

optimal.speed.df <- cbind.data.frame(birds_df_filtered$name, vmp, vmr,
                                     vmr_5_tail, vmr_5_head)
names(optimal.speed.df) <- c("ring_number", "Vmp", "Vmr",
                             "vmr_5_tail", "vmr_5_head")

write.csv(optimal.speed.df, file = "optimal_speeds.csv")





# For bird means -----

# Optimal speeds
vmr <- vmp <- vmr_5_tail <- vmr_5_head <- NULL
for(i in 1:7){
  x <- computeFlightPerformance(Bird(birds.means[i,], type = "other"))
  vmp[i] <- (x$table$speed)[2]
  vmr[i] <- (x$table$speed)[3]
  
  vmr_5_tail[i] <- unlist(findMaximumRangeSpeed(Bird(birds.means[i,], type = "other"),
                                                windSpeed=5)[2])
  vmr_5_head[i] <- unlist(findMaximumRangeSpeed(Bird(birds.means[i,], type = "other"),
                                                windSpeed=-5)[2])
}

optimal.speed.df <- cbind.data.frame(birds.means$name, vmp, vmr,
                                     vmr_5_tail, vmr_5_head)
names(optimal.speed.df) <- c("ring_number", "Vmp", "Vmr",
                             "vmr_5_tail", "vmr_5_head")

write.csv(optimal.speed.df, file = "optimal_speeds_birds_means.csv")
