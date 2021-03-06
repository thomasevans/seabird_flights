
# Analysis of determinants of flight altitude
# Analysing for wind with respect to track direction

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


theme_new_rt_legend <- theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = "right",
        legend.justification = c(1, 1),
        legend.key.size =   unit(2, "lines"),
        legend.key = element_rect(colour =NA),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text.align = 0,
        legend.key.width = unit(2, "lines"),
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
flight_alt_ok$head_wind_10m_alt_filter_type <- sign(flight_alt_ok$track_head_wind_10m_alt_filter)
summary(as.factor(flight_alt_ok$head_wind_10m_alt_filter_type))
flight_alt_ok$head_wind_10m_alt_filter_type <- factor(
  flight_alt_ok$head_wind_10m_alt_filter_type, levels = c(-1, 1),
  labels = c("head", "tail")
)
summary(flight_alt_ok$head_wind_10m_alt_filter_type)

# Wind assistance absolute value
flight_alt_ok$head_wind_10m_alt_filter_abs<- abs(flight_alt_ok$track_head_wind_10m_alt_filter)
hist(flight_alt_ok$head_wind_10m_alt_filter_abs)



# Type of wind side
flight_alt_ok$cross_wind_10m_alt_filter_type <- sign(flight_alt_ok$track_cross_wind_10m_alt_filter)
summary(as.factor(flight_alt_ok$cross_wind_10m_alt_filter_type))
flight_alt_ok$cross_wind_10m_alt_filter_type <- factor(
  flight_alt_ok$cross_wind_10m_alt_filter_type, levels = c(-1, 1),
  labels = c("to left", "to right")
)
summary(flight_alt_ok$cross_wind_10m_alt_filter_type)

# Wind assistance absolute value
flight_alt_ok$cross_wind_10m_alt_filter_abs<- abs(flight_alt_ok$track_cross_wind_10m_alt_filter)
hist(flight_alt_ok$cross_wind_10m_alt_filter_abs)

# Species subsets
flight_alt_ok_gull <- flight_alt_ok[flight_alt_ok$species == "gull",]
flight_alt_ok_murre <- flight_alt_ok[flight_alt_ok$species == "murre",]


# Fit possible models (8):
models.gull <- list()


models.gull[[8]] <- glmer(altitude_callib_extm ~
                            trunc_seg_dist_a_b_km +
                            cross_wind_10m_alt_filter_abs +
                            head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                            (1|ring_number),
                          data = flight_alt_ok_gull)


models.gull[[7]] <- glmer(altitude_callib_extm ~
                            cross_wind_10m_alt_filter_abs +
                            head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                            (1|ring_number),
                          data = flight_alt_ok_gull)


models.gull[[6]] <- glmer(altitude_callib_extm ~
                            trunc_seg_dist_a_b_km +
                            cross_wind_10m_alt_filter_abs +
                            (1|ring_number),
                          data = flight_alt_ok_gull)

models.gull[[5]] <- glmer(altitude_callib_extm ~
                            trunc_seg_dist_a_b_km +
                            head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                            (1|ring_number),
                          data = flight_alt_ok_gull)


models.gull[[4]] <- glmer(altitude_callib_extm ~
                            head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                            (1|ring_number),
                          data = flight_alt_ok_gull)

models.gull[[3]] <- glmer(altitude_callib_extm ~
                            cross_wind_10m_alt_filter_abs +
                            (1|ring_number),
                          data = flight_alt_ok_gull)

models.gull[[2]] <- glmer(altitude_callib_extm ~
                            trunc_seg_dist_a_b_km +
                            (1|ring_number),
                          data = flight_alt_ok_gull)

models.gull[[1]] <- glmer(altitude_callib_extm ~
                            1+
                            (1|ring_number),
                          data = flight_alt_ok_gull)
#


# Standardize models
models.gull.stdz <- list()
models.gull.stdz[[1]] <- models.gull[[1]]
for(i in 2:8){
  models.gull.stdz[[i]] <- standardize(models.gull[[i]], standardize.y=FALSE)
}

models.gull.aicc <- sapply(models.gull.stdz, AICc)
models.gull.aicc.dif <- models.gull.aicc-min(models.gull.aicc)
# mods_inc2 <- c(1, 2, 3, 7, 11, 18, 19, 20, 21, 22)
# models.gull.aicc.dif2 <- models.gull.aicc-min(models.gull.aicc[mods_inc2])

models.gull.r2m <- sapply(models.gull.stdz, r.squaredGLMM)
t(models.gull.r2m)

models.gull.fit.df <- cbind.data.frame(c(1:8),models.gull.aicc,
                                       models.gull.aicc.dif,
                                       t(models.gull.r2m))
names(models.gull.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")


# * Flight_height_unfiltered ------
# Make km version of distance
flight_alt$trunc_seg_dist_a_b_km <- flight_alt$trunc_seg_dist_a_b / 1000
hist(flight_alt$trunc_seg_dist_a_b_km)

# Type of wind assistance
flight_alt$head_wind_10m_type <- sign(flight_alt$track_head_wind_10m)
summary(as.factor(flight_alt$head_wind_10m_type))
flight_alt$head_wind_10m_type <- factor(
  flight_alt$head_wind_10m_type, levels = c(-1, 1),
  labels = c("head", "tail")
)
summary(flight_alt$head_wind_10m_type)

# Wind assistance absolute value
flight_alt$head_wind_10m_abs<- abs(flight_alt$track_head_wind_10m)
hist(flight_alt$head_wind_10m_abs)


# Type of wind side
flight_alt$cross_wind_10m_type <- sign(flight_alt$track_cross_wind_10m)
summary(as.factor(flight_alt$cross_wind_10m_type))
flight_alt$cross_wind_10m_type <- factor(
  flight_alt$cross_wind_10m_type, levels = c(-1, 1),
  labels = c("to left", "to right")
)
summary(flight_alt$cross_wind_10m_type)

# Wind assistance absolute value
flight_alt$cross_wind_10m_abs<- abs(flight_alt$track_cross_wind_10m)
hist(flight_alt$cross_wind_10m_abs)

# Species subsets
flight_alt_gull <- flight_alt[flight_alt$species == "gull",]
flight_alt_murre <- flight_alt[flight_alt$species == "murre",]


# Fit possible models (8):
models.gull.flt_ht_no_filter <- list()

models.gull.flt_ht_no_filter[[8]] <- glmer(altitude_callib_extm_no_filter ~
                            trunc_seg_dist_a_b_km +
                              cross_wind_10m_abs +
                              head_wind_10m_abs*head_wind_10m_type +
                              (1|ring_number),
                          data = flight_alt_gull)


models.gull.flt_ht_no_filter[[7]] <- glmer(altitude_callib_extm_no_filter ~
                                             cross_wind_10m_abs +
                                             head_wind_10m_abs*head_wind_10m_type +
                                             (1|ring_number),
                                           data = flight_alt_gull)


models.gull.flt_ht_no_filter[[6]] <- glmer(altitude_callib_extm_no_filter ~
                                             trunc_seg_dist_a_b_km +
                                             cross_wind_10m_abs +
                                             (1|ring_number),
                                           data = flight_alt_gull)

models.gull.flt_ht_no_filter[[5]] <- glmer(altitude_callib_extm_no_filter ~
                                             trunc_seg_dist_a_b_km +
                                             head_wind_10m_abs*head_wind_10m_type +
                                             (1|ring_number),
                                           data = flight_alt_gull)


models.gull.flt_ht_no_filter[[4]] <- glmer(altitude_callib_extm_no_filter ~
                                             head_wind_10m_abs*head_wind_10m_type +
                                             (1|ring_number),
                                           data = flight_alt_gull)

models.gull.flt_ht_no_filter[[3]] <- glmer(altitude_callib_extm_no_filter ~
                                             cross_wind_10m_abs +
                                             (1|ring_number),
                                           data = flight_alt_gull)

models.gull.flt_ht_no_filter[[2]] <- glmer(altitude_callib_extm_no_filter ~
                                             trunc_seg_dist_a_b_km +
                                             (1|ring_number),
                                           data = flight_alt_gull)

models.gull.flt_ht_no_filter[[1]] <- glmer(altitude_callib_extm_no_filter ~
                                             1 +
                                             (1|ring_number),
                                           data = flight_alt_gull)
#


# Standardize models
models.gull.flt_ht_no_filter.stdz <- list()
models.gull.flt_ht_no_filter.stdz[[1]] <- models.gull.flt_ht_no_filter[[1]]
for(i in 2:8){
  models.gull.flt_ht_no_filter.stdz[[i]] <- standardize(models.gull.flt_ht_no_filter[[i]],
                                                        standardize.y=FALSE)
}

models.gull.flt_ht_no_filter.aicc <- sapply(models.gull.flt_ht_no_filter.stdz, AICc)
mmodels.gull.flt_ht_no_filter.aicc.dif <- models.gull.flt_ht_no_filter.aicc-
  min(models.gull.flt_ht_no_filter.aicc)

models.gull.flt_ht_no_filter.r2m <- sapply(models.gull.flt_ht_no_filter.stdz,
                          r.squaredGLMM)
t(models.gull.flt_ht_no_filter.r2m)

models.gull.flt_ht_no_filter.fit.df <- cbind.data.frame(c(1:8),
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

write.csv(models.gull.fit.all, file = "gull_alt_model_fit_table_TRACK_new.csv")
#




# ***** Common murres ---------


# * Flight height with filter -----

# sort(flight_alt_ok_murre$altitude_callib_extm)
flight_alt_ok_murre_original <- flight_alt_ok_murre
flight_alt_ok_murre <- flight_alt_ok_murre_original[
  !(flight_alt_ok_murre_original$flight_id_combined %in% c("m1228", "m623",
                                                           "m515", "m676",
                                                           "m495", "m774")),]
# flight_alt_ok_murre <- flight_alt_ok_murre_original


# Fit possible models (8):
models.murre <- list()


models.murre[[8]] <- glmer(altitude_callib_extm ~
                          trunc_seg_dist_a_b_km +
                          cross_wind_10m_alt_filter_abs +
                          head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                          (1|ring_number),
                          data = flight_alt_ok_murre)


models.murre[[7]] <- glmer(altitude_callib_extm ~
                            cross_wind_10m_alt_filter_abs +
                            head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                             (1|ring_number),
                           data = flight_alt_ok_murre)


models.murre[[6]] <- glmer(altitude_callib_extm ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_10m_alt_filter_abs +
                             (1|ring_number),
                           data = flight_alt_ok_murre)

models.murre[[5]] <- glmer(altitude_callib_extm ~
                            trunc_seg_dist_a_b_km +
                            head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                             (1|ring_number),
                           data = flight_alt_ok_murre)


models.murre[[4]] <- glmer(altitude_callib_extm ~
                          head_wind_10m_alt_filter_abs*head_wind_10m_alt_filter_type +
                          (1|ring_number),
                           data = flight_alt_ok_murre)

models.murre[[3]] <- glmer(altitude_callib_extm ~
                             cross_wind_10m_alt_filter_abs +
                             (1|ring_number),
                           data = flight_alt_ok_murre)

models.murre[[2]] <- glmer(altitude_callib_extm ~
                             trunc_seg_dist_a_b_km +
                             (1|ring_number),
                           data = flight_alt_ok_murre)

models.murre[[1]] <- glmer(altitude_callib_extm ~
                            1 +
                             (1|ring_number),
                           data = flight_alt_ok_murre)
#


# Standardize models
models.murre.stdz <- list()
models.murre.stdz[[1]] <- models.murre[[1]]
for(i in 2:8){
  models.murre.stdz[[i]] <- standardize(models.murre[[i]], standardize.y=FALSE)
}

models.murre.aicc <- sapply(models.murre.stdz, AICc)
models.murre.aicc.dif <- models.murre.aicc-min(models.murre.aicc)
models.murre.r2m <- sapply(models.murre.stdz, r.squaredGLMM)
t(models.murre.r2m)

models.murre.fit.df <- cbind.data.frame(c(1:8),models.murre.aicc,
                                       models.murre.aicc.dif,
                                       t(models.murre.r2m))

names(models.murre.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")

plot(models.murre[[8]])
qqmath(models.murre[[8]])



# * Flight_height_unfiltered ------



# Fit possible models (8):

flight_alt_murre.original <- flight_alt_murre
flight_alt_murre <- flight_alt_murre.original[
  !(flight_alt_murre.original$flight_id_combined %in% c("m1228", "m623",
                                                        "m515", "m676",
                                                        "m495", "m774")),]
# flight_alt_murre <- flight_alt_murre.original 

models.murre.flt_ht_no_filter <- list()




models.murre.flt_ht_no_filter[[8]] <- glmer(altitude_callib_extm_no_filter ~
                                             trunc_seg_dist_a_b_km +
                                              cross_wind_10m_abs +
                                             head_wind_10m_abs*head_wind_10m_type +
                                             (1|ring_number),
                                           data = flight_alt_murre)


models.murre.flt_ht_no_filter[[7]] <- glmer(altitude_callib_extm_no_filter ~
                                              cross_wind_10m_abs +
                                              head_wind_10m_abs*head_wind_10m_type +
                                              (1|ring_number),
                                            data = flight_alt_murre)


models.murre.flt_ht_no_filter[[6]] <- glmer(altitude_callib_extm_no_filter ~
                                              trunc_seg_dist_a_b_km +
                                              cross_wind_10m_abs +
                                              (1|ring_number),
                                            data = flight_alt_murre)

models.murre.flt_ht_no_filter[[5]] <- glmer(altitude_callib_extm_no_filter ~
                                              trunc_seg_dist_a_b_km +
                                              head_wind_10m_abs*head_wind_10m_type +
                                              (1|ring_number),
                                            data = flight_alt_murre)


models.murre.flt_ht_no_filter[[4]] <- glmer(altitude_callib_extm_no_filter ~
                                              head_wind_10m_abs*head_wind_10m_type +
                                              (1|ring_number),
                                            data = flight_alt_murre)

models.murre.flt_ht_no_filter[[3]] <- glmer(altitude_callib_extm_no_filter ~
                                              cross_wind_10m_abs +
                                              (1|ring_number),
                                            data = flight_alt_murre)

models.murre.flt_ht_no_filter[[2]] <- glmer(altitude_callib_extm_no_filter ~
                                              trunc_seg_dist_a_b_km +
                                              (1|ring_number),
                                            data = flight_alt_murre)

models.murre.flt_ht_no_filter[[1]] <- glmer(altitude_callib_extm_no_filter ~
                                             1 +
                                              (1|ring_number),
                                            data = flight_alt_murre)
#


# Standardize models
models.murre.flt_ht_no_filter.stdz <- list()
models.murre.flt_ht_no_filter.stdz[[1]] <- models.murre.flt_ht_no_filter[[1]]
for(i in 2:8){
  models.murre.flt_ht_no_filter.stdz[[i]] <- standardize(models.murre.flt_ht_no_filter[[i]],
                                                        standardize.y=FALSE)
}

models.murre.flt_ht_no_filter.aicc <- sapply(models.murre.flt_ht_no_filter.stdz, AICc)
models.murre.flt_ht_no_filter.aicc.dif <- models.murre.flt_ht_no_filter.aicc-
  min(models.murre.flt_ht_no_filter.aicc)

models.murre.flt_ht_no_filter.r2m <- sapply(models.murre.flt_ht_no_filter.stdz,
                                           r.squaredGLMM)
t(models.murre.flt_ht_no_filter.r2m)


models.murre.flt_ht_no_filter.fit.df <- cbind.data.frame(c(1:8),
                                                        models.murre.flt_ht_no_filter.aicc,
                                                        models.murre.flt_ht_no_filter.aicc.dif,
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

# write.csv(models.murre.fit.all, file = "murre_alt_model_fit_table_TRACK_new.csv")

write.csv(models.murre.fit.all, file = "murre_alt_model_fit_table_influential_points_removed_TRACK_new.csv")
#


# anova(models.murre[[14]],models.murre[[13]])


# *********** Coef + p-values -----------
# Calculate p values for dropping of terms from lowest AIC model -----
murre_model_va <- models.murre[[8]]
lbbg_model_va <- models.gull[[8]]

# murre_model_va <- models.murre.new


lbbg_model_va <- models.gull.flt_ht_no_filter[[8]]


murre_model_va <- models.murre.flt_ht_no_filter[[8]]


anova(models.gull[[14]], models.gull[[13]])
anova(models.gull[[1]], models.gull[[13]])

anova(models.murre[[14]], models.murre[[13]])
anova(models.murre[[5]], models.murre[[8]])
anova(models.murre.flt_ht_no_filter[[5]], models.murre.flt_ht_no_filter[[8]])


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


plot(lbbg_model_va)
qqmath(lbbg_model_va)

#

# Influential points in murre data ----
# install.packages("influence.ME")

library(influence.ME)
infl <- influence(models.murre[[21]], obs = TRUE)
(cooks.distance(infl))>0.06
plot(infl, which = "cook")
alt_na_rem <- !is.na(flight_alt_ok_murre$altitude_callib_extm)
exclude.flights <- flight_alt_ok_murre$flight_id_combined[alt_na_rem][(cooks.distance(infl))>0.06]
# "m1228" "m623" 

x <- (cooks.distance(infl))>0.06
exclude.flights2 <- flight_alt_ok_murre$flight_id_combined[alt_na_rem][(cooks.distance(infl))>0.06]
# "m515" "m676"


exclude.flights3 <- flight_alt_ok_murre$flight_id_combined[alt_na_rem][(cooks.distance(infl))>0.12]
# "m774"

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

murre_model_va <- models.murre[[5]]
lbbg_model_va <- models.gull[[8]]

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
# allModelFrame <- mod_gull_frame
# allModelFrame <- mod_murre_frame
allModelFrame <- allModelFrame[-c(1,7),]

str(allModelFrame)
allModelFrame$modelName <-  factor(allModelFrame$modelName,
                                   levels(allModelFrame$modelName)[c(2,1)])

levels(allModelFrame$Variable)


# lab.1 <- expression("Vw"["s"]+~"(tail-wind)")
# lab.2 <- expression("Vw"["s"]-~"(head-wind)")
# lab.3 <- expression("Vw"["c"]~"Absolute speed")
# lab.4 <- expression("Vw"["c"]-~"(to left)")
# parse=TRUE

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols.new <- gg_color_hue(2)
cols.new <- rev(cols.new)
# plot(1,1, col = cols.new[2])

zp1 <- ggplot(allModelFrame, aes(colour = modelName))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = Variable,
                                ymin = CI_low,
                                ymax = CI_high),
                            # colour = cols.new,
                            lwd = 1, position = position_dodge(width = 1/2),
                            show.legend = FALSE)
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient,
                                 ymin = CI_low,
                                 ymax = CI_high),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE",
                             # colour = cols.new,
                             show.legend = TRUE)
zp1 <- zp1 + scale_x_discrete("",
                              labels = c(expression("Vw"["c"]~" speed"),
                                         # expression("Vw"["c"]~" speed: type (to right)"),
                                         # expression("Vw"["c"]~" type (to right)"),
                                         expression("Vw"["s"]~" speed"),
                                         expression(atop("Vw"["s"]~" speed:",
                                          "type (tail-wind)")),
                                         expression(atop("Vw"["s"]~" ",
                                          "type (tail-wind)")),
                                         "Distance (km)"
                              ))

# expression("Vw"["s"]+~"(tail-wind)")

# zp1 <- zp1 + coord_flip() 
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
  annotate("text",  x= layer_scales(zp1)$x$range$range[1],
           y = layer_scales(zp1)$y$range$range[2], label = "(a)",
           vjust=1, hjust=2, size = 5)
zp1
ggsave(zp1, filename = "alt_model_coef_fig_combined_track.svg", width = 6, height = 8,
       units = "in")

ggsave(zp1, filename = "alt_model_coef_fig_combined_track_horizontal_new.svg", width = 12, height = 4,
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


wind.side <- seq(0,10,.1)
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



v <- seq(-30, 60, 5) 
gg$z2 <- findInterval(gg$z, v)
gg$z3 <- v[gg$z2]


flight_alt_ok_gg <- data.frame(
  x = flight_alt_ok_gull$head_wind_10m_alt_filter,
  y = abs(flight_alt_ok_gull$cross_wind_10m_alt_filter),
  z = flight_alt_ok_gull$altitude_callib_extm,
  z2 = findInterval(flight_alt_ok_gull$altitude_callib_extm, v)
)

#expression("Vw"["c"]^"+")
lab.1 <- expression(atop("Vw"["s"]^"+"~"","Tail-wind"))
lab.2 <- expression(atop("Vw"["s"]^"-"~"","Head-wind"))
# lab.3 <- expression(atop("Vw"["c"]^"-"~"","To left"))
lab.4 <- expression(atop("Vw"["c"]~"","Cross-wind"))


p <- ggplot(gg,aes(x,y)) + 
  geom_raster(aes(fill=z3))+
  scale_fill_gradient2(low = muted("blue"), mid = "white",
                       high = muted("red"), midpoint = median(
                         flight_alt_ok_gg$z, na.rm = TRUE
                       ), space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
#   scale_x_continuous(expand=c(0,0))+
#   scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0), breaks = seq(-10, 10, 2.5))+
  scale_y_continuous(expand=c(0,0), breaks = c(0,2.5,5,7.5,10), limits = c(0, 12))+
  coord_fixed() +
  geom_point(data = flight_alt_ok_gg[complete.cases(flight_alt_ok_gg),],
             aes(fill = z), 
             shape=21, alpha=1,na.rm=T, size=1.5) +
  labs( x = expression("Vw"["s"]~~~~"Wind assitance ("~ms^{-1}~")"),
        y = expression("Vw"["c"]~~~~"Cross wind ("~ms^{-1}~")"),
        fill = "Altitude\n(m)",
        parse = TRUE) +
  # theme_new +
  theme_new_rt_legend +
  theme(legend.title = element_text(size = 14)) +
  annotate("text", label = c(paste(lab.4),paste(lab.1),paste(lab.2)),
           x = c( 2 , 8, -7.5),
           y = c(8.5, 1, 1),
           parse=TRUE,
           colour = "grey40",
           size = 4,
           vjust = 0.5) 
p <- p + annotate("text",  x= -9,
                  y = 11, label = "(b)",
                  vjust = 1, hjust=0, size = 5)
p
ggsave("Alt_gull_predication2_track2_new.svg", width = 6, height = 4, units = "in")




# For murres ----


wind.side <- seq(0,10,.1)
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



v <- seq(-5, 8, 0.5) 
gg$z2 <- findInterval(gg$z, v)
gg$z3 <- v[gg$z2]


flight_alt_ok_gg <- data.frame(
  x = flight_alt_ok_murre$head_wind_10m_alt_filter,
  y = abs(flight_alt_ok_murre$cross_wind_10m_alt_filter),
  z = flight_alt_ok_murre$altitude_callib_extm,
  z2 = findInterval(flight_alt_ok_murre$altitude_callib_extm, v)
)

#expression("Vw"["c"]^"+")
lab.1 <- expression(atop("Vw"["s"]^"+"~"","Tail-wind"))
lab.2 <- expression(atop("Vw"["s"]^"-"~"","Head-wind"))
# lab.3 <- expression(atop("Vw"["c"]^"-"~"","To left"))
lab.4 <- expression(atop("Vw"["c"]~"","Cross-wind"))


p <- ggplot(gg,aes(x,y)) + 
  geom_raster(aes(fill=z3))+
  scale_fill_gradient2(low = muted("blue"), mid = "white",
                       high = muted("red"), midpoint = median(
                         flight_alt_ok_gg$z, na.rm = TRUE
                       ), space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
  scale_x_continuous(expand=c(0,0), breaks = seq(-10, 10, 2.5))+
  scale_y_continuous(expand=c(0,0), breaks = c(0,2.5,5,7.5,10), limits = c(0, 12))+
  # ?scale_y_continuous
  coord_fixed() +
  geom_point(data = flight_alt_ok_gg[complete.cases(flight_alt_ok_gg),],
             aes(fill = z), 
             shape=21, alpha=1,na.rm=T, size=2.5) +
  labs( x = expression("Vw"["s"]~~~~"Wind assitance ("~ms^{-1}~")"),
        y = expression("Vw"["c"]~~~~"Cross wind ("~ms^{-1}~")"),
        fill = "Altitude\n(m)",
        parse = TRUE) +
  # theme_new +
  theme_new_rt_legend +
  theme(legend.title = element_text(size = 14)) +
  annotate("text", label = c(paste(lab.4),paste(lab.1),paste(lab.2)),
           x = c( 2 , 8, -7.5),
           y = c(8.5, 1, 1),
           parse=TRUE,
           colour = "grey40",
           size = 4,
           vjust = 0.5) 
p <- p + annotate("text",  x= -9,
                  y = 11, label = "(c)",
                  vjust = 1, hjust=0, size = 5)
p
ggsave("Alt_murre_predication2_track2_new.svg", width = 6, height = 4, units = "in")


