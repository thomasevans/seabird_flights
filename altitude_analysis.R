
# Analysis of determinants of flight altitude


# Load in data -----

# Load flight summary data
load("flight_details.RData")

# Make data subsets ----

# Add species name column
flight.details$sp_name <- flight.details$species
flight.details$sp_name[flight.details$species == "gull"] <- "Lesser black-backed gull"
flight.details$sp_name[flight.details$species == "murre"] <- "Common murre"
flight.details$sp_name <- as.factor(flight.details$sp_name)
# levels(flight.details$sp_name)

# For using wind @10 m (i.e. ignoring altitude)
flight_10m <- flight.details

# For using altitude data - calculated wind at flight height
# (excluding those with low Q altitude values)
flight_alt_ok <- flight.details[flight.details$altitude_filter_n >= 1,]

# Using altitude data - but all - calculated wind at flight height
flight_alt <- flight.details




# Data plotting set-up ----
library(ggplot2)
library(cowplot)

# Make a common theme for the plots
# Black + white, serif font, similar size to other base graphics
# Probably slightly larger text for axis text than the default
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


# ********** Statistical analysis -----
library(lme4)
library(arm)
library(lattice)
library(MuMIn)





# ***** Gulls -------
# * Flight height with filter -----

# Make km version of distance
flight_alt_ok$trunc_seg_dist_a_b_km <- flight_alt_ok$trunc_seg_dist_a_b / 1000
hist(flight_alt_ok$trunc_seg_dist_a_b_km)

# Type of wind assistance
flight_alt_ok$head_wind_10m_alt_filter_type <- sign(flight_alt_ok$head_wind_10m_alt_filter)
summary(as.factor(flight_alt_ok$head_wind_10m_alt_filter_type))
flight_alt_ok$head_wind_10m_alt_filter_type <- factor(
  flight_alt_ok$head_wind_10m_alt_filter_type, levels = c(-1, 1),
  labels = c("head", "tail")
)
summary(flight_alt_ok$head_wind_10m_alt_filter_type)

# Wind assistance absolute value
flight_alt_ok$head_wind_10m_alt_filter_abs<- abs(flight_alt_ok$head_wind_10m_alt_filter)
hist(flight_alt_ok$head_wind_10m_alt_filter_abs)


# Type of wind side
flight_alt_ok$cross_wind_10m_alt_filter_type <- sign(flight_alt_ok$cross_wind_10m_alt_filter)
summary(as.factor(flight_alt_ok$cross_wind_10m_alt_filter_type))
flight_alt_ok$cross_wind_10m_alt_filter_type <- factor(
  flight_alt_ok$cross_wind_10m_alt_filter_type, levels = c(-1, 1),
  labels = c("to left", "to right")
)
summary(flight_alt_ok$cross_wind_10m_alt_filter_type)

# Wind assistance absolute value
flight_alt_ok$cross_wind_10m_alt_filter_abs<- abs(flight_alt_ok$cross_wind_10m_alt_filter)
hist(flight_alt_ok$cross_wind_10m_alt_filter_abs)

# Species subsets
flight_alt_ok_gull <- flight_alt_ok[flight_alt_ok$species == "gull",]
flight_alt_ok_murre <- flight_alt_ok[flight_alt_ok$species == "murre",]


# Fit possible models (18):
models.gull <- list()


models.gull[[14]] <- glmer(altitude_callib_extm ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_10m_alt_filter_abs*cross_wind_10m_alt_filter_type +
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                             + (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[17]] <- glmer(altitude_callib_extm ~
                             
                             cross_wind_10m_alt_filter_abs*cross_wind_10m_alt_filter_type +
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type
                           + (1|ring_number),
                           data = flight_alt_ok_gull)


models.gull[[16]] <- glmer(altitude_callib_extm ~
                             
                             cross_wind_10m_alt_filter_abs + cross_wind_10m_alt_filter_type +
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type
                           + (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[15]] <- glmer(altitude_callib_extm ~
                             
                             cross_wind_10m_alt_filter_type +
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type
                           + (1|ring_number),
                           data = flight_alt_ok_gull)

# models.gull[[13]] <- glmer(altitude_callib_extm ~
#                              
#                              head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type
#                            + (1|ring_number),
#                            data = flight_alt_ok_gull)

models.gull[[13]] <- glmer(altitude_callib_extm ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_10m_alt_filter_abs+cross_wind_10m_alt_filter_type +
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                             + (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[12]] <- glmer(altitude_callib_extm ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_10m_alt_filter_type +
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                             + (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[11]] <- glmer(altitude_callib_extm ~
                             trunc_seg_dist_a_b_km +
                             
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type 
                             + (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[10]] <- glmer(altitude_callib_extm ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_10m_alt_filter_abs*cross_wind_10m_alt_filter_type
                           
                           + (1|ring_number),
                           data = flight_alt_ok_gull)


models.gull[[9]] <- glmer(altitude_callib_extm ~
                            trunc_seg_dist_a_b_km +
                            cross_wind_10m_alt_filter_abs+cross_wind_10m_alt_filter_type +
                            (1|ring_number),
                          data = flight_alt_ok_gull)


models.gull[[8]] <- glmer(altitude_callib_extm ~
                            trunc_seg_dist_a_b_km +
                            cross_wind_10m_alt_filter_type +
                            (1|ring_number),
                          data = flight_alt_ok_gull)


models.gull[[7]] <- glmer(altitude_callib_extm ~
                            head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                            + (1|ring_number),
                          data = flight_alt_ok_gull)


models.gull[[6]] <- glmer(altitude_callib_extm ~
                            
                            cross_wind_10m_alt_filter_abs*cross_wind_10m_alt_filter_type +
                            (1|ring_number),
                          data = flight_alt_ok_gull)

models.gull[[5]] <- glmer(altitude_callib_extm ~
                            
                            cross_wind_10m_alt_filter_abs+cross_wind_10m_alt_filter_type 
                          + (1|ring_number),
                          data = flight_alt_ok_gull)


models.gull[[4]] <- glmer(altitude_callib_extm ~
                            cross_wind_10m_alt_filter_type +
                            (1|ring_number),
                          data = flight_alt_ok_gull)

models.gull[[3]] <- glmer(altitude_callib_extm ~
                            
                            cross_wind_10m_alt_filter_abs
                          + (1|ring_number),
                          data = flight_alt_ok_gull)

models.gull[[2]] <- glmer(altitude_callib_extm ~
                            trunc_seg_dist_a_b_km +
                            (1|ring_number),
                          data = flight_alt_ok_gull)

models.gull[[1]] <- glmer(altitude_callib_extm ~ 1 +
                            (1|ring_number),
                          data = flight_alt_ok_gull)
#


# Standardize models
models.gull.stdz <- list()
models.gull.stdz[[1]] <- models.gull[[1]]
for(i in 2:17){
  models.gull.stdz[[i]] <- standardize(models.gull[[i]], standardize.y=FALSE)
}

models.gull.aicc <- sapply(models.gull.stdz, AICc)
models.gull.aicc.dif <- models.gull.aicc-min(models.gull.aicc)
models.gull.r2m <- sapply(models.gull.stdz, r.squaredGLMM)
t(models.gull.r2m)

models.gull.fit.df <- cbind.data.frame(c(1:17),models.gull.aicc,
                                       models.gull.aicc.dif,
                                       t(models.gull.r2m))
names(models.gull.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")


# * Flight_height_unfiltered ------
# Make km version of distance
flight_alt$trunc_seg_dist_a_b_km <- flight_alt$trunc_seg_dist_a_b / 1000
hist(flight_alt$trunc_seg_dist_a_b_km)

# Type of wind assistance
flight_alt$head_wind_10m_type <- sign(flight_alt$head_wind_10m)
summary(as.factor(flight_alt$head_wind_10m_type))
flight_alt$head_wind_10m_type <- factor(
  flight_alt$head_wind_10m_type, levels = c(-1, 1),
  labels = c("head", "tail")
)
summary(flight_alt$head_wind_10m_type)

# Wind assistance absolute value
flight_alt$head_wind_10m_abs<- abs(flight_alt$head_wind_10m)
hist(flight_alt$head_wind_10m_abs)


# Type of wind side
flight_alt$cross_wind_10m_type <- sign(flight_alt$cross_wind_10m)
summary(as.factor(flight_alt$cross_wind_10m_type))
flight_alt$cross_wind_10m_type <- factor(
  flight_alt$cross_wind_10m_type, levels = c(-1, 1),
  labels = c("to left", "to right")
)
summary(flight_alt$cross_wind_10m_type)

# Wind assistance absolute value
flight_alt$cross_wind_10m_abs<- abs(flight_alt$cross_wind_10m)
hist(flight_alt$cross_wind_10m_abs)

# Species subsets
flight_alt_gull <- flight_alt[flight_alt$species == "gull",]
flight_alt_murre <- flight_alt[flight_alt$species == "murre",]


# Fit possible models (18):
models.gull.flt_ht_no_filter <- list()


# 
# flight_alt_ok_gull$altitude_callib_extm_no_filter
# flight_alt_ok$cross_wind_flt_
models.gull.flt_ht_no_filter[[14]] <- glmer(altitude_callib_extm_no_filter ~
                             trunc_seg_dist_a_b_km +
                               cross_wind_10m_abs*cross_wind_10m_type +
                               head_wind_10m_abs*head_wind_10m_type +
                             + (1|ring_number),
                           data = flight_alt_gull)

models.gull.flt_ht_no_filter[[17]] <- glmer(altitude_callib_extm_no_filter ~
                             
                             cross_wind_10m_abs*cross_wind_10m_type +
                             head_wind_10m_abs*head_wind_10m_type
                           + (1|ring_number),
                           data = flight_alt_gull)


models.gull.flt_ht_no_filter[[16]] <- glmer(altitude_callib_extm_no_filter ~
                             
                             cross_wind_10m_abs + cross_wind_10m_type +
                             head_wind_10m_abs*head_wind_10m_type
                           + (1|ring_number),
                           data = flight_alt_gull)

models.gull.flt_ht_no_filter[[15]] <- glmer(altitude_callib_extm_no_filter ~
                             
                             cross_wind_10m_type +
                             head_wind_10m_abs*head_wind_10m_type
                           + (1|ring_number),
                           data = flight_alt_gull)

# models.gull.flt_ht_no_filter[[13]] <- glmer(altitude_callib_extm_no_filter ~
#                              
#                              head_wind_10m_abs*head_wind_10m_type
#                            + (1|ring_number),
#                            data = flight_alt_gull)

models.gull.flt_ht_no_filter[[13]] <- glmer(altitude_callib_extm_no_filter ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_10m_abs+cross_wind_10m_type +
                             head_wind_10m_abs*head_wind_10m_type +
                             + (1|ring_number),
                           data = flight_alt_gull)

models.gull.flt_ht_no_filter[[12]] <- glmer(altitude_callib_extm_no_filter ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_10m_type +
                             head_wind_10m_abs*head_wind_10m_type +
                             + (1|ring_number),
                           data = flight_alt_gull)

models.gull.flt_ht_no_filter[[11]] <- glmer(altitude_callib_extm_no_filter ~
                             trunc_seg_dist_a_b_km +
                             
                             head_wind_10m_abs*head_wind_10m_type 
                           + (1|ring_number),
                           data = flight_alt_gull)

models.gull.flt_ht_no_filter[[10]] <- glmer(altitude_callib_extm_no_filter ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_10m_abs*cross_wind_10m_type
                           
                           + (1|ring_number),
                           data = flight_alt_gull)


models.gull.flt_ht_no_filter[[9]] <- glmer(altitude_callib_extm_no_filter ~
                            trunc_seg_dist_a_b_km +
                            cross_wind_10m_abs+cross_wind_10m_type +
                            (1|ring_number),
                          data = flight_alt_gull)


models.gull.flt_ht_no_filter[[8]] <- glmer(altitude_callib_extm_no_filter ~
                            trunc_seg_dist_a_b_km +
                            cross_wind_10m_type +
                            (1|ring_number),
                          data = flight_alt_gull)


models.gull.flt_ht_no_filter[[7]] <- glmer(altitude_callib_extm_no_filter ~
                            head_wind_10m_abs*head_wind_10m_type +
                            + (1|ring_number),
                          data = flight_alt_gull)


models.gull.flt_ht_no_filter[[6]] <- glmer(altitude_callib_extm_no_filter ~
                            
                            cross_wind_10m_abs*cross_wind_10m_type +
                            (1|ring_number),
                          data = flight_alt_gull)

models.gull.flt_ht_no_filter[[5]] <- glmer(altitude_callib_extm_no_filter ~
                            
                            cross_wind_10m_abs+cross_wind_10m_type 
                          + (1|ring_number),
                          data = flight_alt_gull)


models.gull.flt_ht_no_filter[[4]] <- glmer(altitude_callib_extm_no_filter ~
                            cross_wind_10m_type +
                            (1|ring_number),
                          data = flight_alt_gull)

models.gull.flt_ht_no_filter[[3]] <- glmer(altitude_callib_extm_no_filter ~
                            
                            cross_wind_10m_abs
                          + (1|ring_number),
                          data = flight_alt_gull)

models.gull.flt_ht_no_filter[[2]] <- glmer(altitude_callib_extm_no_filter ~
                            trunc_seg_dist_a_b_km +
                            (1|ring_number),
                          data = flight_alt_gull)

models.gull.flt_ht_no_filter[[1]] <- glmer(altitude_callib_extm_no_filter ~ 1 +
                            (1|ring_number),
                          data = flight_alt_gull)
#


# Standardize models
models.gull.flt_ht_no_filter.stdz <- list()
models.gull.flt_ht_no_filter.stdz[[1]] <- models.gull.flt_ht_no_filter[[1]]
for(i in 2:17){
  models.gull.flt_ht_no_filter.stdz[[i]] <- standardize(models.gull.flt_ht_no_filter[[i]],
                                                        standardize.y=FALSE)
}

models.gull.flt_ht_no_filter.aicc <- sapply(models.gull.flt_ht_no_filter.stdz, AICc)
mmodels.gull.flt_ht_no_filter.aicc.dif <- models.gull.flt_ht_no_filter.aicc-
  min(models.gull.flt_ht_no_filter.aicc)

models.gull.flt_ht_no_filter.r2m <- sapply(models.gull.flt_ht_no_filter.stdz,
                          r.squaredGLMM)
t(models.gull.flt_ht_no_filter.r2m)

models.gull.flt_ht_no_filter.fit.df <- cbind.data.frame(c(1:17),
                                                        models.gull.flt_ht_no_filter.aicc,
                                                        mmodels.gull.flt_ht_no_filter.aicc.dif,
                                       t(models.gull.flt_ht_no_filter.r2m))
names(models.gull.flt_ht_no_filter.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")


# * merge gull glmm summary table -----

names(models.gull.flt_ht_no_filter.fit.df) <- paste(
  "no_filter_flt_ht_", names(models.gull.flt_ht_no_filter.fit.df), sep = "")

names(models.gull.fit.df) <- paste(
  "filter_flt_ht_", names(models.gull.fit.df), sep = "")

# Combine
models.gull.fit.all <- cbind(models.gull.fit.df,
                             models.gull.flt_ht_no_filter.fit.df[,2:5])

write.csv(models.gull.fit.all, file = "gull_alt_model_fit_table.csv")
#




# ***** Common murres ---------


# * Flight height with filter -----

# sort(flight_alt_ok_murre$altitude_callib_extm)
flight_alt_ok_murre_original <- flight_alt_ok_murre
flight_alt_ok_murre <- flight_alt_ok_murre_original[
  !(flight_alt_ok_murre_original$flight_id_combined %in% c("m1228", "m623",
                                                           "m490", "m499",
                                                           "m515", "m655",
                                                           "m643")),]

# Transformations of altitude:
# hist(flight_alt_ok_murre$altitude_callib_extm)
# flight_alt_ok_murre$altitude_callib_extm <- log(flight_alt_ok_murre$altitude_callib_extm - min(flight_alt_ok_murre$altitude_callib_extm, na.rm = TRUE) + 1)
# 
# flight_alt_ok_murre$altitude_callib_extm <- sqrt(flight_alt_ok_murre$altitude_callib_extm - min(flight_alt_ok_murre$altitude_callib_extm, na.rm = TRUE) + 1)

# flight_alt_ok_murre <- flight_alt_ok_murre[flight_alt_ok_murre$altitude_callib_extm < 50,]

# hist(flight_alt_ok_murre_original$altitude_callib_extm, breaks = 40)

# Fit possible models (18):
models.murre <- list()


models.murre[[14]] <- glmer(altitude_callib_extm ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_10m_alt_filter_abs*cross_wind_10m_alt_filter_type +
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                             + (1|ring_number),
                           data = flight_alt_ok_murre)


models.murre.new <- glmer(altitude_callib_extm ~
                              trunc_seg_dist_a_b_km +
                              ecmwf_wind_10m_speed_alt_filter
                              + (1|ring_number),
                            data = flight_alt_ok_murre)
# hist(flight_alt_ok_murre$)



models.murre[[17]] <- glmer(altitude_callib_extm ~
                             
                             cross_wind_10m_alt_filter_abs*cross_wind_10m_alt_filter_type +
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type
                           + (1|ring_number),
                           data = flight_alt_ok_murre)


models.murre[[16]] <- glmer(altitude_callib_extm ~
                             
                             cross_wind_10m_alt_filter_abs + cross_wind_10m_alt_filter_type +
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type
                           + (1|ring_number),
                           data = flight_alt_ok_murre)

models.murre[[15]] <- glmer(altitude_callib_extm ~
                             
                             cross_wind_10m_alt_filter_type +
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type
                           + (1|ring_number),
                           data = flight_alt_ok_murre)

# models.murre[[13]] <- glmer(altitude_callib_extm ~
#                              
#                              head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type
#                            + (1|ring_number),
#                            data = flight_alt_ok_murre)

models.murre[[13]] <- glmer(altitude_callib_extm ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_10m_alt_filter_abs+cross_wind_10m_alt_filter_type +
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                             + (1|ring_number),
                           data = flight_alt_ok_murre)

models.murre[[12]] <- glmer(altitude_callib_extm ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_10m_alt_filter_type +
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                             + (1|ring_number),
                           data = flight_alt_ok_murre)

models.murre[[11]] <- glmer(altitude_callib_extm ~
                             trunc_seg_dist_a_b_km +
                             
                             head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type 
                           + (1|ring_number),
                           data = flight_alt_ok_murre)

models.murre[[10]] <- glmer(altitude_callib_extm ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_10m_alt_filter_abs*cross_wind_10m_alt_filter_type
                           
                           + (1|ring_number),
                           data = flight_alt_ok_murre)


models.murre[[9]] <- glmer(altitude_callib_extm ~
                            trunc_seg_dist_a_b_km +
                            cross_wind_10m_alt_filter_abs+cross_wind_10m_alt_filter_type +
                            (1|ring_number),
                          data = flight_alt_ok_murre)


models.murre[[8]] <- glmer(altitude_callib_extm ~
                            trunc_seg_dist_a_b_km +
                            cross_wind_10m_alt_filter_type +
                            (1|ring_number),
                          data = flight_alt_ok_murre)


models.murre[[7]] <- glmer(altitude_callib_extm ~
                            head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                            + (1|ring_number),
                          data = flight_alt_ok_murre)


models.murre[[6]] <- glmer(altitude_callib_extm ~
                            
                            cross_wind_10m_alt_filter_abs*cross_wind_10m_alt_filter_type +
                            (1|ring_number),
                          data = flight_alt_ok_murre)

models.murre[[5]] <- glmer(altitude_callib_extm ~
                            
                            cross_wind_10m_alt_filter_abs+cross_wind_10m_alt_filter_type 
                          + (1|ring_number),
                          data = flight_alt_ok_murre)


models.murre[[4]] <- glmer(altitude_callib_extm ~
                            cross_wind_10m_alt_filter_type +
                            (1|ring_number),
                          data = flight_alt_ok_murre)

models.murre[[3]] <- glmer(altitude_callib_extm ~
                            
                            cross_wind_10m_alt_filter_abs
                          + (1|ring_number),
                          data = flight_alt_ok_murre)

models.murre[[2]] <- glmer(altitude_callib_extm ~
                            trunc_seg_dist_a_b_km +
                            (1|ring_number),
                          data = flight_alt_ok_murre)

models.murre[[1]] <- glmer(altitude_callib_extm ~ 1 +
                            (1|ring_number),
                          data = flight_alt_ok_murre)
#


# Standardize models
models.murre.stdz <- list()
models.murre.stdz[[1]] <- models.murre[[1]]
for(i in 2:17){
  models.murre.stdz[[i]] <- standardize(models.murre[[i]], standardize.y=FALSE)
}
# models.murre.stdz[[14]] <- standardize(models.murre[[14]], standardize.y=FALSE)

models.murre.aicc <- sapply(models.murre.stdz, AICc)
models.murre.aicc.dif <- models.murre.aicc-min(models.murre.aicc)
models.murre.r2m <- sapply(models.murre.stdz, r.squaredGLMM)
t(models.murre.r2m)

models.murre.fit.df <- cbind.data.frame(c(1:17),models.murre.aicc,
                                       models.murre.aicc.dif,
                                       t(models.murre.r2m))
names(models.murre.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")

plot(models.murre[[14]])
qqmath(models.murre[[14]])


models.murre.new.stdz <- standardize(models.murre.new, standardize.y=FALSE)

plot(models.murre.new.stdz)
qqmath(models.murre.new.stdz)

r.squaredGLMM(models.murre.new.stdz)
AICc(models.murre.new.stdz)
AICc(models.murre[[14]])
# * Flight_height_unfiltered ------



# Fit possible models (18):
models.murre.flt_ht_no_filter <- list()

flight_alt_murre.original <- flight_alt_murre
flight_alt_murre <- flight_alt_murre.original[
  !(flight_alt_murre.original$flight_id_combined %in% c("m1228", "m623",
                                                        "m490", "m499",
                                                        "m515", "m655",
                                                        "m643")),]
# 
# flight_alt_ok_murre_original <- flight_alt_ok_murre
# flight_alt_ok_murre <- flight_alt_ok_murre_original[
#   !(flight_alt_ok_murre_original$flight_id_combined %in% c("m1228", "m623", "m781", "m490", "m499", "m655", "m643")),]

# 
# flight_alt_ok_murre$altitude_callib_extm_no_filter
# flight_alt_ok$cross_wind_flt_
models.murre.flt_ht_no_filter[[14]] <- glmer(altitude_callib_extm_no_filter ~
                                              trunc_seg_dist_a_b_km +
                                              cross_wind_10m_abs*cross_wind_10m_type +
                                              head_wind_10m_abs*head_wind_10m_type +
                                              + (1|ring_number),
                                            data = flight_alt_murre)

models.murre.flt_ht_no_filter[[17]] <- glmer(altitude_callib_extm_no_filter ~
                                              
                                              cross_wind_10m_abs*cross_wind_10m_type +
                                              head_wind_10m_abs*head_wind_10m_type
                                            + (1|ring_number),
                                            data = flight_alt_murre)


models.murre.flt_ht_no_filter[[16]] <- glmer(altitude_callib_extm_no_filter ~
                                              
                                              cross_wind_10m_abs + cross_wind_10m_type +
                                              head_wind_10m_abs*head_wind_10m_type
                                            + (1|ring_number),
                                            data = flight_alt_murre)

models.murre.flt_ht_no_filter[[15]] <- glmer(altitude_callib_extm_no_filter ~
                                              
                                              cross_wind_10m_type +
                                              head_wind_10m_abs*head_wind_10m_type
                                            + (1|ring_number),
                                            data = flight_alt_murre)

# models.murre.flt_ht_no_filter[[13]] <- glmer(altitude_callib_extm_no_filter ~
#                              
#                              head_wind_10m_abs*head_wind_10m_type
#                            + (1|ring_number),
#                            data = flight_alt_murre)

models.murre.flt_ht_no_filter[[13]] <- glmer(altitude_callib_extm_no_filter ~
                                              trunc_seg_dist_a_b_km +
                                              cross_wind_10m_abs+cross_wind_10m_type +
                                              head_wind_10m_abs*head_wind_10m_type +
                                              + (1|ring_number),
                                            data = flight_alt_murre)

models.murre.flt_ht_no_filter[[12]] <- glmer(altitude_callib_extm_no_filter ~
                                              trunc_seg_dist_a_b_km +
                                              cross_wind_10m_type +
                                              head_wind_10m_abs*head_wind_10m_type +
                                              + (1|ring_number),
                                            data = flight_alt_murre)

models.murre.flt_ht_no_filter[[11]] <- glmer(altitude_callib_extm_no_filter ~
                                              trunc_seg_dist_a_b_km +
                                              
                                              head_wind_10m_abs*head_wind_10m_type 
                                            + (1|ring_number),
                                            data = flight_alt_murre)

models.murre.flt_ht_no_filter[[10]] <- glmer(altitude_callib_extm_no_filter ~
                                              trunc_seg_dist_a_b_km +
                                              cross_wind_10m_abs*cross_wind_10m_type
                                            
                                            + (1|ring_number),
                                            data = flight_alt_murre)


models.murre.flt_ht_no_filter[[9]] <- glmer(altitude_callib_extm_no_filter ~
                                             trunc_seg_dist_a_b_km +
                                             cross_wind_10m_abs+cross_wind_10m_type +
                                             (1|ring_number),
                                           data = flight_alt_murre)


models.murre.flt_ht_no_filter[[8]] <- glmer(altitude_callib_extm_no_filter ~
                                             trunc_seg_dist_a_b_km +
                                             cross_wind_10m_type +
                                             (1|ring_number),
                                           data = flight_alt_murre)


models.murre.flt_ht_no_filter[[7]] <- glmer(altitude_callib_extm_no_filter ~
                                             head_wind_10m_abs*head_wind_10m_type +
                                             + (1|ring_number),
                                           data = flight_alt_murre)


models.murre.flt_ht_no_filter[[6]] <- glmer(altitude_callib_extm_no_filter ~
                                             
                                             cross_wind_10m_abs*cross_wind_10m_type +
                                             (1|ring_number),
                                           data = flight_alt_murre)

models.murre.flt_ht_no_filter[[5]] <- glmer(altitude_callib_extm_no_filter ~
                                             
                                             cross_wind_10m_abs+cross_wind_10m_type 
                                           + (1|ring_number),
                                           data = flight_alt_murre)


models.murre.flt_ht_no_filter[[4]] <- glmer(altitude_callib_extm_no_filter ~
                                             cross_wind_10m_type +
                                             (1|ring_number),
                                           data = flight_alt_murre)

models.murre.flt_ht_no_filter[[3]] <- glmer(altitude_callib_extm_no_filter ~
                                             
                                             cross_wind_10m_abs
                                           + (1|ring_number),
                                           data = flight_alt_murre)

models.murre.flt_ht_no_filter[[2]] <- glmer(altitude_callib_extm_no_filter ~
                                             trunc_seg_dist_a_b_km +
                                             (1|ring_number),
                                           data = flight_alt_murre)

models.murre.flt_ht_no_filter[[1]] <- glmer(altitude_callib_extm_no_filter ~ 1 +
                                             (1|ring_number),
                                           data = flight_alt_murre)
#


# Standardize models
models.murre.flt_ht_no_filter.stdz <- list()
models.murre.flt_ht_no_filter.stdz[[1]] <- models.murre.flt_ht_no_filter[[1]]
for(i in 2:17){
  models.murre.flt_ht_no_filter.stdz[[i]] <- standardize(models.murre.flt_ht_no_filter[[i]],
                                                        standardize.y=FALSE)
}

models.murre.flt_ht_no_filter.aicc <- sapply(models.murre.flt_ht_no_filter.stdz, AICc)
mmodels.murre.flt_ht_no_filter.aicc.dif <- models.murre.flt_ht_no_filter.aicc-
  min(models.murre.flt_ht_no_filter.aicc)

models.murre.flt_ht_no_filter.r2m <- sapply(models.murre.flt_ht_no_filter.stdz,
                                           r.squaredGLMM)
t(models.murre.flt_ht_no_filter.r2m)

models.murre.flt_ht_no_filter.fit.df <- cbind.data.frame(c(1:17),
                                                        models.murre.flt_ht_no_filter.aicc,
                                                        mmodels.murre.flt_ht_no_filter.aicc.dif,
                                                        t(models.murre.flt_ht_no_filter.r2m))
names(models.murre.flt_ht_no_filter.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")


# * merge murre glmm summary table -----

names(models.murre.flt_ht_no_filter.fit.df) <- paste(
  "no_filter_flt_ht_", names(models.murre.flt_ht_no_filter.fit.df), sep = "")

names(models.murre.fit.df) <- paste(
  "filter_flt_ht_", names(models.murre.fit.df), sep = "")

# Combine
models.murre.fit.all <- cbind(models.murre.fit.df,
                             models.murre.flt_ht_no_filter.fit.df[,2:5])

write.csv(models.murre.fit.all, file = "murre_alt_model_fit_table_influential_points_removed.csv")
#


# *********** Coef + p-values -----------
# Calculate p values for dropping of terms from lowest AIC model -----
murre_model_va <- models.murre[[14]]
lbbg_model_va <- models.gull[[14]]

murre_model_va <- models.murre.new


lbbg_model_va <- models.gull.flt_ht_no_filter[[14]]


murre_model_va <- models.murre.flt_ht_no_filter[[14]]




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
drop1(lbbg_model_va, test="user", sumFun=KRSumFun)

# p values for murre model
drop1(murre_model_va, test="user", sumFun=KRSumFun)

# drop1(models.gull.stdz[[14]],test="user",sumFun=KRSumFun)

# Cite: Halekoh, U., and Højsgaard, S. (2014). A kenward-roger approximation and parametric bootstrap methods for tests in linear mixed models–the R package pbkrtest. Journal of Statistical Software 59, 1–30.


# "The result could reported as a Kenward-Roger corrected test with F(1, 118.5) = 16.17, p = .0001024"
# From: https://seriousstats.wordpress.com/tag/kenward-roger-approximation/


# Confidence intervals + coeficients
lbbg_model_va_coef <- summary(lbbg_model_va)$coef[, 1]
lbbg_model_va_ci <- confint(lbbg_model_va, method="Wald")
lbbg_model_va_par_df <- cbind.data.frame(lbbg_model_va_coef,lbbg_model_va_ci[-c(1:2),])

# show(lbbg_model_va_par_df)

murre_model_va_coef <- summary(murre_model_va)$coef[, 1]
murre_model_va_ci <- confint(murre_model_va, method="Wald")
murre_model_va_par_df <- cbind.data.frame(murre_model_va_coef,murre_model_va_ci[-c(1:2),])


plot(murre_model_va)
qqmath(murre_model_va)

#

# Influential points in murre data ----
install.packages("influence.ME")

library(influence.ME)
infl <- influence(models.murre[[14]], obs = TRUE)
(cooks.distance(infl))>0.06
plot(infl, which = "cook")
alt_na_rem <- !is.na(flight_alt_ok_murre$altitude_callib_extm)
exclude.flights <- flight_alt_ok_murre$flight_id_combined[alt_na_rem][(cooks.distance(infl))>0.06]
# "m1228" "m623" 

exclude.flights2 <- flight_alt_ok_murre$flight_id_combined[(cooks.distance(infl))>0.06]
# "m490" "m499" "m515" "m655"

exclude.flights3 <- flight_alt_ok_murre$flight_id_combined[(cooks.distance(infl))>0.5]
# "m643"

plot(models.murre[[14]])
qqmath(models.murre[[14]])


models.murre.stdz[[14]]


infl <- influence(lbbg_model_va, obs = TRUE)
# (cooks.distance(infl))>0.06
plot(infl, which = "cook")

plot(lbbg_model_va)
qqmath(lbbg_model_va)





# Make forest plot to illustrate final model -----
library(cowplot)

murre_model_va <- models.murre[[14]]
lbbg_model_va <- models.gull[[14]]

# Adapted from: https://gist.github.com/dsparks/4332698

mod_gull_frame <- data.frame(Variable = rownames(summary(lbbg_model_va)$coef),
                             Coefficient = summary(lbbg_model_va)$coef[, 1],
                             CI_low = confint(lbbg_model_va, method="Wald")[-c(1:2), 1],
                             CI_high = confint(lbbg_model_va, method="Wald")[-c(1:2), 2],
                             modelName = "Lesser black-backed gulls")
mod_murre_frame <- data.frame(Variable = rownames(summary(murre_model_va)$coef),
                              Coefficient = summary(murre_model_va)$coef[, 1],
                              CI_low = confint(murre_model_va, method="Wald")[-c(1:2), 1],
                              CI_high = confint(murre_model_va, method="Wald")[-c(1:2), 2],
                              modelName = "Common murres")
# Combine these data.frames
allModelFrame <- data.frame(rbind(mod_gull_frame, mod_murre_frame))  

str(allModelFrame)
allModelFrame$modelName <-  factor(allModelFrame$modelName,
                                   levels(allModelFrame$modelName)[c(2,1)])

levels(allModelFrame$Variable)


# lab.1 <- expression("Vw"["s"]+~"(tail-wind)")
# lab.2 <- expression("Vw"["s"]-~"(head-wind)")
# lab.3 <- expression("Vw"["c"]~"Absolute speed")
# lab.4 <- expression("Vw"["c"]-~"(to left)")
# parse=TRUE

zp1 <- ggplot(allModelFrame, aes(colour = modelName))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = Variable,
                                ymin = CI_low,
                                ymax = CI_high),
                            lwd = 1, position = position_dodge(width = 1/2),
                            show.legend = FALSE)
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient,
                                 ymin = CI_low,
                                 ymax = CI_high),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE",
                             show.legend = TRUE)
zp1 <- zp1 + scale_x_discrete("",
                              labels = c("Intercept",
                                expression("Vw"["c"]~" speed"),
                                         expression("Vw"["c"]~" speed: type (to right)"),
                                         expression("Vw"["c"]~" type (to right)"),
                                         expression("Vw"["s"]~" speed"),
                                         expression("Vw"["s"]~" speed: type (tail-wind)"),
                                         expression("Vw"["s"]~" type (tail-wind)"),
                                         "Distance (km)"
                              ))

# expression("Vw"["s"]+~"(tail-wind)")

zp1 <- zp1 + coord_flip() 
# zp1 <- zp1 + theme_new + ylim(-4,2.5)
# zp1 <- zp1 + scale_y_continuous(breaks = seq(-4, 4, 1),
#                                 minor_breaks = seq(-4, 4, 0.5),
#                                 limits = c(-4, 2.5))
# ?scale_y_continuous
# zp1 <- zp1 + theme(axis.text = element_text(size = 12))
zp1 <- zp1 +  theme_new
zp1 <- zp1 + theme(legend.position = c(1, 1))
zp1 <- zp1 + labs(x = "", y = expression("Coefficient   ("~Delta~"Altitude ["~m~"])"))
zp1 <- zp1 +
  annotate("text",  x= layer_scales(zp1)$x$range$range[8],
           y = layer_scales(zp1)$y$range$range[1], label = "(a)",
           vjust=-1, hjust=0, size = 5)
zp1
ggsave(zp1, filename = "alt_model_coef_fig.svg", width = 8, height = 8,
       units = "in")
# ?ggsave






# Figures to illustrate model predictions ------
# Similar style to fig 4 or sup file 4 in Sapir et al. 2014 study

# y - cross-wind, x - head-tail wind
# colour plot showing model predictions as colour field
# Actual points with same colour scale overplotted
# See http://docs.ggplot2.org/0.9.3.1/scale_gradient2.html
# - can use + scale_colour_gradient2(space="Lab")

# To make contour plot in ggplot with discreet categories
# - See: http://stackoverflow.com/a/27571412/1172358


wind.side <- seq(-10,10,.1)
wind.assist <- seq(-10,10,.1)
dist.median <- median(flight_alt_ok_gull$trunc_seg_dist_a_b_km)

gg <- expand.grid(x=wind.assist,y=wind.side)


lbbg.new.data.df <- cbind.data.frame(
  trunc_seg_dist_a_b_km = dist.median,
  cross_wind_10m_alt_filter_abs = abs(gg$y),
  cross_wind_10m_alt_filter_type = factor(
    sign(gg$y), levels = c(-1, 1),
    labels = c("to left", "to right")),
  head_wind_10m_alt_filter_abs = abs(gg$x),
  head_wind_10m_alt_filter_type = factor(
    sign(gg$x), levels = c(-1, 1),
    labels = c("head", "tail"))
)



pred.va <- predict(lbbg_model_va,
                   newdata = lbbg.new.data.df,
                   re.form=NA)
hist(pred.va)


gg$z <- with(gg,pred.va)      # need long format for ggplot
summary(is.na(gg$z))
range(gg$z, na.rm = TRUE)
library(RColorBrewer)               #for brewer.pal()
library(scales)



v <- seq(-15, 150, 5) 
gg$z2 <- findInterval(gg$z, v)
gg$z3 <- v[gg$z2]


flight_alt_ok_gg <- data.frame(
  x = flight_alt_ok_gull$head_wind_10m_alt_filter,
  y = flight_alt_ok_gull$cross_wind_10m_alt_filter,
  z = flight_alt_ok_gull$altitude_callib_extm,
  z2 = findInterval(flight_alt_ok_gull$altitude_callib_extm, v)
)


lab.1 <- expression("Vw"["s"]+~"(tail-wind)")
lab.2 <- expression("Vw"["s"]-~"(head-wind)")
lab.3 <- expression("Vw"["c"]+~"(to right)")
lab.4 <- expression("Vw"["c"]-~"(to left)")


p <- ggplot(gg,aes(x,y)) + 
  geom_raster(aes(fill=z3))+
  scale_fill_gradient2(low = muted("blue"), mid = "white",
                       high = muted("red"), midpoint = median(
                         flight_alt_ok_gg$z, na.rm = TRUE
                       ), space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  coord_fixed() +
  geom_point(data = flight_alt_ok_gg,
             aes(fill = z), 
             shape=21, alpha=1,na.rm=T, size=3) +
  labs( x = expression("Wind assitance ("~ms^{-1}~")"),
        y = expression("Cross wind ("~ms^{-1}~")"),
        fill = "Altitude (m)",
        parse = TRUE) +
  theme_new +
  theme(legend.title = element_text(size = 14)) +
  annotate("text", label = c(paste(lab.3),paste(lab.4),paste(lab.1),paste(lab.2)),
           x = c(1, 1 , 8, -6),
           y = c(-10, 9, 0, 0),
           parse=TRUE,
           colour = "black",
           size = 5,
           vjust = 0) 
p <- p + annotate("text",  x= -9,
                  y = 9, label = "(b)",
                  vjust=-1, hjust=0, size = 5)
p
ggsave("Alt_gull_predication.svg", width = 8, height = 8, units = "in")




# For murres ----

wind.side <- seq(-10,10,.1)
wind.assist <- seq(-10,10,.1)
dist.median <- median(flight_alt_ok_murre$trunc_seg_dist_a_b_km)

gg <- expand.grid(x=wind.assist,y=wind.side)


murre.new.data.df <- cbind.data.frame(
  trunc_seg_dist_a_b_km = dist.median,
  cross_wind_10m_alt_filter_abs = abs(gg$y),
  cross_wind_10m_alt_filter_type = factor(
    sign(gg$y), levels = c(-1, 1),
    labels = c("to left", "to right")),
  head_wind_10m_alt_filter_abs = abs(gg$x),
  head_wind_10m_alt_filter_type = factor(
    sign(gg$x), levels = c(-1, 1),
    labels = c("head", "tail"))
)



pred.va <- predict(murre_model_va,
                   newdata = murre.new.data.df,
                   re.form=NA)
hist(pred.va)


gg$z <- with(gg,pred.va)      # need long format for ggplot
summary(is.na(gg$z))
range(gg$z, na.rm = TRUE)
library(RColorBrewer)               #for brewer.pal()
library(scales)


# range(gg$z, na.rm = TRUE)
v <- seq(-20, 45, 2.5) 
gg$z2 <- findInterval(gg$z, v)
gg$z3 <- v[gg$z2]

range(flight_alt_ok_murre$altitude_callib_extm[f])

f <- !is.na(flight_alt_ok_murre$altitude_callib_extm)
flight_alt_ok_gg <- data.frame(
  x = flight_alt_ok_murre$head_wind_10m_alt_filter[f],
  y = flight_alt_ok_murre$cross_wind_10m_alt_filter[f],
  z = flight_alt_ok_murre$altitude_callib_extm[f],
  z2 = findInterval(flight_alt_ok_murre$altitude_callib_extm[f], v)
)
# summary(is.na(flight_alt_ok_murre$altitude_callib_extm))

lab.1 <- expression("Vw"["s"]+~"(tail-wind)")
lab.2 <- expression("Vw"["s"]-~"(head-wind)")
lab.3 <- expression("Vw"["c"]+~"(to right)")
lab.4 <- expression("Vw"["c"]-~"(to left)")


p <- ggplot(gg,aes(x,y)) + 
  geom_raster(aes(fill=z3))+
  scale_fill_gradient2(low = muted("blue"), mid = "white",
                       high = muted("red"), midpoint = median(
                         flight_alt_ok_gg$z, na.rm = TRUE
                       ), space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  coord_fixed() +
  geom_point(data = flight_alt_ok_gg,
             aes(fill = z), 
             shape=21, alpha=1,na.rm=T, size=3) +
  labs( x = expression("Wind assitance ("~ms^{-1}~")"),
        y = expression("Cross wind ("~ms^{-1}~")"),
        fill = "Altitude (m)",
        parse = TRUE) +
  theme_new +
  theme(legend.title = element_text(size = 14)) +
  annotate("text", label = c(paste(lab.3),paste(lab.4),paste(lab.1),paste(lab.2)),
           x = c(1, 1 , 8, -6),
           y = c(-10, 9, 0, 0),
           parse=TRUE,
           colour = "black",
           size = 5,
           vjust = 0) 
p <- p + annotate("text",  x= -9,
                  y = 9, label = "(b)",
                  vjust=-1, hjust=0, size = 5)
p
ggsave("Alt_murre_predication.svg", width = 8, height = 8, units = "in")









