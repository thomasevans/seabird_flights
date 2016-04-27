
# Analysis of determinants of airspeed


# Load in data -----

# Load flight summary data
load("flight_details.RData")

# Make data subsets ----
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
flight_alt$vg <- sqrt(flight_alt$vg_v*flight_alt$vg_v + flight_alt$vg_u*flight_alt$vg_u)
ggplot(flight_alt, aes(va_flt_ht, fill = as.factor(species))) +
  geom_density(alpha = 0.2) +
  geom_density(data = flight_alt, aes(vg, fill = as.factor(species)
                                      ),alpha = 0.2, lty = 2)
# ?geom_density

hist(flight_alt$va_flt_ht[flight_alt$species == "gull"],
     breaks = 20)
hist(flight_alt$va_flt_ht[flight_alt$species == "murre"], breaks = 20)


hist(flight_alt_ok$va_flt_ht[flight_alt_ok$species == "gull"],
     breaks = 20)
hist(flight_alt_ok$va_flt_ht[flight_alt_ok$species == "murre"], breaks = 20)





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











