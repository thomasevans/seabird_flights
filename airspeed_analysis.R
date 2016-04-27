
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

# Airspeed and ground-speed for two species
# - for flights with good altitude data only + good quality locations
ggplot(flight_alt_ok, aes(va_flt_ht_alt_filter,
                          col = as.factor(species))) +
  geom_line(stat="density", alpha = 0.6, lwd = 2) +
  geom_line(stat="density", data = flight_alt_ok, aes(vg_alt_filter, col = as.factor(species)
  ),alpha = 0.6, lty = 2, lwd = 2) +
  theme_bw()

# Cross and head wind components for two species
# - for flights with good altitude data only + good quality locations
gg <- ggplot(flight_alt_ok, aes(cross_wind_flt_ht_alt_filter,
                          col = as.factor(species))) +
  geom_line(stat="density", alpha = 0.6, lwd = 2) +
  geom_line(stat="density", data = flight_alt_ok, aes(
    head_wind_flt_ht_alt_filter, col = as.factor(species)
  ),alpha = 0.6, lty = 2, lwd = 2) +
  theme_bw(base_size = 14, base_family = "serif")
?theme_bw

gg2 <- gg +
  annotate("text",  x= layer_scales(gg)$x$range$range[1],
           y = layer_scales(gg)$y$range$range[2], label = "(b)", vjust=1, hjust=1)
#

layer_scales(gg)$y$range$range[2]

# library(cowplot)
# gg
# gg + draw_figure_label("(a)", position = "top.left")

hist(flight_alt$va_flt_ht[flight_alt$species == "gull"],  breaks = 20)
hist(flight_alt$va_flt_ht[flight_alt$species == "murre"], breaks = 20)


hist(flight_alt_ok$va_flt_ht[flight_alt_ok$species == "gull"],
     breaks = 20)
hist(flight_alt_ok$va_flt_ht[flight_alt_ok$species == "murre"], breaks = 20)





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

# Add legend for species in top right corner
# see: https://rpubs.com/folias/A-simple-example-on-ggplot2-legend-options
# and: http://stackoverflow.com/a/10747608/1172358


# Add figure legend (a)
p <- p +
  annotate("text",  x= layer_scales(p)$x$range$range[2],
           y = layer_scales(p)$y$range$range[1], label = "(a)",
           vjust=1, hjust=0, size = 8)
plot_altitude <- p
ggsave("plot_altitude.svg", plot = plot_altitude, width = 4, height = 8, units = "in")


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
           vjust=1, hjust=0, size = 8)
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
           vjust=1, hjust=0, size = 8)
plot_winds <- p
ggsave("plot_winds.svg", plot = plot_winds, width = 6, height = 4, units = "in")




# Explore how this differs using the different data sub-sets
# - maybe add some as suplementary



# Statistical analysis -----
# Candidate list of models


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











