
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


unique(flight.details$species)

# WEAK CROSS-WINDS ---
no_cross <- flight.details$species == "gull" & ( abs(flight.details$track_cross_wind_10m) < 2)
summary(no_cross)
flights_c <- flight.details[no_cross,]


# Plot
plot(flights_c$vg~flights_c$track_head_wind_10m,
     col = as.factor(flights_c$ring_number))

plot(flights_c$vg~flights_c$track_head_wind_10m,
     col = as.factor(flights_c$ring_number),
     log = "xy")


birds <- read.csv("deployments_details_export.csv")
flights_bird <- merge(flights_c, birds, by = "ring_number", all = TRUE)
# ?merge

weight_range <- max(flights_bird$weight.kg., na.rm = TRUE) - min(flights_bird$weight.kg., na.rm = TRUE)
weight_min <- min(flights_bird$weight.kg., na.rm = TRUE)
col.alpha <- 0.3 + 0.5*(flights_bird$weight.kg. - weight_min)/weight_range
hist(col.alpha)


addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

keep <- !is.na(col.alpha)
# s

pdf("OF_plots.pdf")
plot(flights_bird$vg[keep]~flights_bird$track_head_wind_10m[keep],
     # col = addalpha(rep("red", sum(keep)), alpha = col.alpha[keep]),
     col = addalpha(as.factor(flights_c$ring_number[keep]), alpha = col.alpha[keep]),
     pch = 20, cex = 2)
# addalpha("red", alpha = 0.5) 
# range(col.alpha[keep])



plot(flights_c$altitude_callib_extm_05~flights_c$track_head_wind_10m,
     col = as.factor(flights_c$ring_number), pch = 20, cex = 2)


plot(flights_c$va_flt_ht~flights_c$track_head_wind_10m,
     col = as.factor(flights_c$ring_number), pch = 20, cex = 2)
dev.off()

# library(ggplot2)


