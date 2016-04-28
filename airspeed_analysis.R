
# Analysis of determinants of airspeed


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

# Frequency distributions for Va for two species (fig. 6 in MS plan) ------
# Make in ggplot - adapt previous made one for WSC
# Maybe add dashed lines for Vg, or some such??
library(ggplot2)
library(cowplot)


# # Airspeed and ground-speed for two species
# # - for flights with good altitude data only + good quality locations
# ggplot(flight_alt_ok, aes(va_flt_ht_alt_filter,
#                           col = as.factor(species))) +
#   geom_line(stat="density", alpha = 0.6, lwd = 2) +
#   geom_line(stat="density", data = flight_alt_ok, aes(vg_alt_filter, col = as.factor(species)
#   ),alpha = 0.6, lty = 2, lwd = 2) +
#   theme_bw()
# 
# # Cross and head wind components for two species
# # - for flights with good altitude data only + good quality locations
# gg <- ggplot(flight_alt_ok, aes(cross_wind_flt_ht_alt_filter,
#                           col = as.factor(species))) +
#   geom_line(stat="density", alpha = 0.6, lwd = 2) +
#   geom_line(stat="density", data = flight_alt_ok, aes(
#     head_wind_flt_ht_alt_filter, col = as.factor(species)
#   ),alpha = 0.6, lty = 2, lwd = 2) +
#   theme_bw(base_size = 14, base_family = "serif")
# ?theme_bw
# 
# gg2 <- gg +
#   annotate("text",  x= layer_scales(gg)$x$range$range[1],
#            y = layer_scales(gg)$y$range$range[2], label = "(b)", vjust=1, hjust=1)
# #
# 
# layer_scales(gg)$y$range$range[2]
# 
# # library(cowplot)
# # gg
# # gg + draw_figure_label("(a)", position = "top.left")
# 
# hist(flight_alt$va_flt_ht[flight_alt$species == "gull"],  breaks = 20)
# hist(flight_alt$va_flt_ht[flight_alt$species == "murre"], breaks = 20)
# 
# 
# hist(flight_alt_ok$va_flt_ht[flight_alt_ok$species == "gull"],
#      breaks = 20)
# hist(flight_alt_ok$va_flt_ht[flight_alt_ok$species == "murre"], breaks = 20)
# 
# 



# *** Preparing a summary figure for flight behaviour recorded ------
# 3 plots:
# - 1. Altitude X species
# - 2. Va and Vg X species
# - 3. Vw-side and Vw-head

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
# theme_bw()
# 

# gg + theme_new

# 1. Altitude X species --------
# Histogram type thing - but flipped on side (altitude on y, frequency on x)
# Colour by species
p <-  ggplot(flight_alt_ok, aes(altitude_callib_extm,
                            col = sp_name,
                            fill = sp_name)) +
    # geom_line(stat="density", alpha = 0.6, lwd = 2) +
    geom_density(alpha = 0.4, lwd = 1) +
    coord_flip()
p <- p + theme_new
p <- p + labs(y = "Density", x = "Altitude (m)")
p <- p + scale_x_continuous(breaks = seq(-20,120,20), minor_breaks = seq(-50, 150, 10))
p <- p + geom_rug(alpha = 0.3, show.legend = FALSE)
# Add legend for species in top right corner
# see: https://rpubs.com/folias/A-simple-example-on-ggplot2-legend-options
# and: http://stackoverflow.com/a/10747608/1172358


# Add figure legend (a)
p <- p +
  annotate("text",  x= layer_scales(p)$x$range$range[2],
           y = layer_scales(p)$y$range$range[1], label = "(a)",
           vjust=0, hjust=0, size = 5)
plot_altitude <- p
ggsave("plot_altitude2.svg", plot = plot_altitude, width = 4, height = 8, units = "in")

# install.packages("svglite")
# 2. Va and Vg X species --------
library(reshape2)
# Make df in form for ggplot:
# - v  ~ species + type (Va or Vg)
df_va_vg <- melt(flight_alt_ok,
                 id.vars = "sp_name", measure.vars = c("va_flt_ht_alt_filter", "vg_alt_filter"))

# Histogram type thing
# Colour by species + line type by Va and Vg
p <-  ggplot(df_va_vg, aes(value,
                                col = sp_name)) +
  geom_line(aes(lty = variable), stat="density", alpha = 0.6, lwd = 2, show.legend = TRUE)
  # geom_density(alpha = 0.4, lwd = 1)
  # coord_flip()
# ?geom_line
p <- p + theme_new
p <- p + labs(y = "Density", x = expression("Speed ("~ms^{-1}~")"))
# p <- p + scale_x_continuous(breaks = seq(-20,120,20), minor_breaks = seq(-50, 150, 10))
p <- p + guides(colour=FALSE) 
p <- p + scale_linetype_discrete(breaks=levels(df_va_vg$variable),
                            labels=c("Va  Air-speed", "Vg  Ground-speed"))


# Add legend for line type only in top right corner
# Species already covered in left panel

# Add figure legend (b)
p <- p +
  annotate("text",  x= layer_scales(p)$x$range$range[1],
           y = layer_scales(p)$y$range$range[2], label = "(b)",
           vjust=1, hjust=0, size = 5)
plot_va_vg <- p
ggsave("plot_va_vg.svg", plot = plot_va_vg, width = 6, height = 4, units = "in")
# ?ggsave
# 3. Vw-side and Vw-head --------

# Make df in form for ggplot:
# - v  ~ species + type (Vwc or Vwa)
df_wind_comp <- melt(flight_alt_ok,
                 id.vars = "sp_name", measure.vars = c("cross_wind_10m_alt_filter", "head_wind_10m_alt_filter"))

# Histogram type thing
# Colour by species + line type by Vwc and VwA

p <-  ggplot(df_wind_comp, aes(value,
                           col = sp_name)) +
  geom_line(aes(lty = variable), stat="density", alpha = 0.6, lwd = 2, show.legend = TRUE)
# geom_density(alpha = 0.4, lwd = 1)
# coord_flip()
# ?geom_line
p <- p + theme_new
p <- p + labs(y = "Density", x = expression("Speed ("~ms^{-1}~")"))
# p <- p + scale_x_continuous(breaks = seq(-20,120,20), minor_breaks = seq(-50, 150, 10))
p <- p + guides(colour=FALSE) 
p <- p + scale_linetype_discrete(breaks=levels(df_wind_comp$variable),
                                 labels=c(
                                   expression("Vw"["c"]~"  Cross-wind"),
                                   expression("Vw"["s"]~"  Wind-assistance")
                                   ))


# Add legend for line type only in top right corner
# Species already covered in left panel

# Add figure legend (c)
p <- p +
  annotate("text",  x= layer_scales(p)$x$range$range[1],
           y = layer_scales(p)$y$range$range[2], label = "(c)",
           vjust=1, hjust=0, size = 5)
plot_winds <- p
ggsave("plot_winds.svg", plot = plot_winds, width = 6, height = 4, units = "in")

# plot_winds + geom_rug(alpha = 0.2)


# Explore how this differs using the different data sub-sets
# - maybe add some as suplementary


p <-  ggplot(flight_alt, aes(altitude_callib_extm_no_filter,
                                col = sp_name,
                                fill = sp_name)) +
  # geom_line(stat="density", alpha = 0.6, lwd = 2) +
  geom_density(alpha = 0.4, lwd = 1) +
  coord_flip()
p <- p + theme_new
p <- p + labs(y = "Density", x = "Altitude (m)")
p <- p + scale_x_continuous(breaks = seq(-20,120,20), minor_breaks = seq(-50, 150, 10))
p + geom_rug(alpha = 0.2)
# p + geom_rug()






p <-  ggplot(flight_alt_ok, aes(head_wind_flt_ht_alt_filter,
                               col = sp_name)) +
  geom_line(aes(lty = sp_name), stat="density", alpha = 0.6, lwd = 2, show.legend = TRUE)
# geom_density(alpha = 0.4, lwd = 1)
# coord_flip()
# ?geom_line
p <- p + theme_new
p <- p + labs(y = "Density", x = expression("Speed ("~ms^{-1}~")"))
# p <- p + scale_x_continuous(breaks = seq(-20,120,20), minor_breaks = seq(-50, 150, 10))
p <- p + guides(colour=FALSE) 
p



ggplot(flight_alt_ok, aes(y = va_flt_ht_alt_filter, x = head_wind_flt_ht_alt_filter,
                          col = sp_name)) +
  geom_point(alpha = 0.6, show.legend = TRUE)


# Statistical analysis -----
library(lme4)
library(arm)
library(lattice)
library(MuMIn)

# Make km version of distance
flight_alt_ok$trunc_seg_dist_a_b_km <- flight_alt_ok$trunc_seg_dist_a_b / 1000
hist(flight_alt_ok$trunc_seg_dist_a_b_km)

# Type of wind assistance
flight_alt_ok$head_wind_flt_ht_alt_filter_type <- sign(flight_alt_ok$head_wind_flt_ht_alt_filter)
summary(as.factor(flight_alt_ok$head_wind_flt_ht_alt_filter_type))
flight_alt_ok$head_wind_flt_ht_alt_filter_type <- factor(
  flight_alt_ok$head_wind_flt_ht_alt_filter_type, levels = c(-1, 1),
  labels = c("head", "tail")
)
summary(flight_alt_ok$head_wind_flt_ht_alt_filter_type)

# Wind assistance absolute value
flight_alt_ok$head_wind_flt_ht_alt_filter_abs<- abs(flight_alt_ok$head_wind_flt_ht_alt_filter)
hist(flight_alt_ok$head_wind_flt_ht_alt_filter_abs)


# Type of wind side
flight_alt_ok$cross_wind_flt_ht_alt_filter_type <- sign(flight_alt_ok$cross_wind_flt_ht_alt_filter)
summary(as.factor(flight_alt_ok$cross_wind_flt_ht_alt_filter_type))
flight_alt_ok$cross_wind_flt_ht_alt_filter_type <- factor(
  flight_alt_ok$cross_wind_flt_ht_alt_filter_type, levels = c(-1, 1),
  labels = c("to left", "to right")
)
summary(flight_alt_ok$cross_wind_flt_ht_alt_filter_type)

# Wind assistance absolute value
flight_alt_ok$cross_wind_flt_ht_alt_filter_abs<- abs(flight_alt_ok$cross_wind_flt_ht_alt_filter)
hist(flight_alt_ok$cross_wind_flt_ht_alt_filter_abs)

# Species subsets
flight_alt_ok_gull <- flight_alt_ok[flight_alt_ok$species == "gull",]
flight_alt_ok_murre <- flight_alt_ok[flight_alt_ok$species == "murre",]


# Fit possible models (18):
models.gull <- list()

models.gull[[14]] <- glmer(va_flt_ht_alt_filter ~
                       trunc_seg_dist_a_b_km +
                       cross_wind_flt_ht_alt_filter_abs*cross_wind_flt_ht_alt_filter_type +
                       head_wind_flt_ht_alt_filter_abs*head_wind_flt_ht_alt_filter_type +
                       + (1|ring_number),
                     data = flight_alt_ok_gull)

models.gull[[18]] <- glmer(va_flt_ht_alt_filter ~
                             
                             cross_wind_flt_ht_alt_filter_abs*cross_wind_flt_ht_alt_filter_type +
                             head_wind_flt_ht_alt_filter_abs*head_wind_flt_ht_alt_filter_type
                             + (1|ring_number),
                           data = flight_alt_ok_gull)


models.gull[[17]] <- glmer(va_flt_ht_alt_filter ~
                             
                             cross_wind_flt_ht_alt_filter_abs + cross_wind_flt_ht_alt_filter_type +
                             head_wind_flt_ht_alt_filter_abs*head_wind_flt_ht_alt_filter_type
                             + (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[16]] <- glmer(va_flt_ht_alt_filter ~
                             
                             cross_wind_flt_ht_alt_filter_type +
                             head_wind_flt_ht_alt_filter_abs*head_wind_flt_ht_alt_filter_type
                             + (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[15]] <- glmer(va_flt_ht_alt_filter ~
                             
                             head_wind_flt_ht_alt_filter_abs*head_wind_flt_ht_alt_filter_type
                             + (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[13]] <- glmer(va_flt_ht_alt_filter ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_flt_ht_alt_filter_abs+cross_wind_flt_ht_alt_filter_type +
                             head_wind_flt_ht_alt_filter_abs*head_wind_flt_ht_alt_filter_type +
                             + (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[12]] <- glmer(va_flt_ht_alt_filter ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_flt_ht_alt_filter_type +
                             head_wind_flt_ht_alt_filter_abs*head_wind_flt_ht_alt_filter_type +
                             + (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[11]] <- glmer(va_flt_ht_alt_filter ~
                             trunc_seg_dist_a_b_km +
                             
                             head_wind_flt_ht_alt_filter_abs*head_wind_flt_ht_alt_filter_type +
                             + (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[10]] <- glmer(va_flt_ht_alt_filter ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_flt_ht_alt_filter_abs*cross_wind_flt_ht_alt_filter_type
                             
                             + (1|ring_number),
                           data = flight_alt_ok_gull)


models.gull[[9]] <- glmer(va_flt_ht_alt_filter ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_flt_ht_alt_filter_abs+cross_wind_flt_ht_alt_filter_type +
                              (1|ring_number),
                           data = flight_alt_ok_gull)


models.gull[[8]] <- glmer(va_flt_ht_alt_filter ~
                             trunc_seg_dist_a_b_km +
                             cross_wind_flt_ht_alt_filter_type +
                              (1|ring_number),
                           data = flight_alt_ok_gull)


models.gull[[7]] <- glmer(va_flt_ht_alt_filter ~
                             head_wind_flt_ht_alt_filter_abs*head_wind_flt_ht_alt_filter_type +
                             + (1|ring_number),
                           data = flight_alt_ok_gull)


models.gull[[6]] <- glmer(va_flt_ht_alt_filter ~
                             
                             cross_wind_flt_ht_alt_filter_abs*cross_wind_flt_ht_alt_filter_type +
                             (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[5]] <- glmer(va_flt_ht_alt_filter ~
                             
                             cross_wind_flt_ht_alt_filter_abs+cross_wind_flt_ht_alt_filter_type 
                             + (1|ring_number),
                           data = flight_alt_ok_gull)


models.gull[[4]] <- glmer(va_flt_ht_alt_filter ~
                             cross_wind_flt_ht_alt_filter_type +
                             (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[3]] <- glmer(va_flt_ht_alt_filter ~
                             
                             cross_wind_flt_ht_alt_filter_abs
                             + (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[2]] <- glmer(va_flt_ht_alt_filter ~
                             trunc_seg_dist_a_b_km +
                              (1|ring_number),
                           data = flight_alt_ok_gull)

models.gull[[1]] <- glmer(va_flt_ht_alt_filter ~ 1 +
                             (1|ring_number),
                           data = flight_alt_ok_gull)
#

# Standardize models
models.gull.stdz <- list()
models.gull.stdz[[1]] <- models.gull[[1]]
for(i in 2:18){
  models.gull.stdz[[i]] <- standardize(models.gull[[i]], standardize.y=FALSE)
}

models.gull.aicc <- sapply(models.gull.stdz, AICc)
models.gull.aicc.dif <- models.gull.aicc-min(models.gull.aicc)
models.gull.r2m <- sapply(models.gull.stdz, r.squaredGLMM)
t(models.gull.r2m)

models.gull.fit.df <- cbind.data.frame(c(1:18),models.gull.aicc,
                                       models.gull.aicc.dif,
                                       t(models.gull.r2m))
names(models.gull.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")






















# Make model comparison tables for 3 data sub-sets for 2 species -----
# (table 4 in MS plan)
# Include:
# - AIC (or AICc) + delta AIC from best model
# - R2 - both components

# Calculate p values for dropping of terms from lowest AIC model


# Make forest plot to illustrate final model - fig 7 in ms plan -----
# Maybe do for all 3 data subsets depending on how much they differ
# Work out how to combine the two species in a single figure - different colours
# Maybe add vertical dashed lines for calculated max range and minimum power speeds



# Figures to illustrate model predictions - fig 8 in ms plan ------
# Similar style to fig 4 or sup file 4 in Sapir et al. 2014 study

# y - cross-wind, x - head-tail wind
# colour plot showing model predictions as colour field
# Actual points with same colour scale overplotted
# See http://docs.ggplot2.org/0.9.3.1/scale_gradient2.html
# - can use + scale_colour_gradient2(space="Lab")

# To make contour plot in ggplot with discreet categories
# - See: http://stackoverflow.com/a/27571412/1172358











