# Circular histograms of flight distributions

# Load in data -----
load("flight_details.RData")


# Packages needed -----
library(circular)
library(ggplot2)

# Colours -----
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols.new <- gg_color_hue(2)


# ** Plots ----

# Gulls geogrphic ----
gulls <- flight.details$species == "gull"
summary(gulls)

wind.dir.gull.circ <- circular(flight.details$ecmwf_wind_10m_dir[gulls],
                               type = "angles",
                               units = "degrees",
                               template = "geographics"
)
                               # ?circular
# ?atan2
# 
# dir.fun <- 
#   90-deg(atan2(1,-2))
#   

track.dir <- 90 - deg(atan2(flight.details$vg_v, flight.details$vg_u))
track.dir[track.dir<0] <- 360 + track.dir[track.dir<0]
# hist(track.dir)
track.dir.circ <- circular(track.dir,
                         type = "angles",
                         units = "degrees",
                         template = "geographics")

track.dir.gull.circ <- track.dir.circ[gulls]


plot(track.dir.gull.circ,
     stack = TRUE,
     sep = 0.015,
     shrink = 1.5,
     cex = 1,
     bin = 90,
     col = cols.new  [2])

# ticks.circular(circular(seq(0, 350, 10), units = "degrees"),
#                zero = pi/2, rotation = 'clock',
#                tcl = 0.075,
#                lwd = 1)
lines(density.circular(track.dir.gull.circ,
                       bw = bw.cv.ml.circular(track.dir.gull.circ)),
      col = cols.new  [2],
      lwd = 2,
      lty = 2)


rose.diag(wind.dir.gull.circ,
          bins = 16,
          col = cols.new  [2],
          cex = 1.5,
          prop = 1.6,
          add = TRUE,
          xlab = "",
          axes = FALSE)
# ?rose.diag

# Mures geographic ----

wind.dir.murre.circ <- circular(flight.details$ecmwf_wind_10m_dir[!gulls],
                               type = "angles",
                               units = "degrees",
                               template = "geographics"
)

track.dir.murre.circ <- track.dir.circ[!gulls]


plot(track.dir.murre.circ,
     stack = TRUE,
     sep = 0.015,
     shrink = 1.5,
     cex = 1,
     bin = 45,
     col = cols.new  [1])
# 
# ticks.circular(circular(seq(0, 350, 10), units = "degrees"),
#                zero = pi/2, rotation = 'clock',
#                tcl = 0.075,
#                lwd = 1)

lines(density.circular(track.dir.murre.circ,
                       bw = bw.cv.ml.circular(track.dir.murre.circ)),
      col = cols.new  [1],
      lwd = 2,
      lty = 2)

rose.diag(wind.dir.murre.circ,
          bins = 16,
          col = cols.new  [1],
          cex = 1.5,
          prop = 1.6,
          add = TRUE,
          xlab = "",
          axes = FALSE)


# Directions of Va (heading) with respect to wind -------
va.dir <- 90 - deg(atan2(flight.details$va_v_flt_ht,
                            flight.details$va_u_flt_ht))

fun2 <- function(x){
  if(is.na(x))return(NA) else{
    if(x<0)return(360 + x) else{
      return(x)
    }
  }
  
}
va.dir <- sapply(va.dir, fun2)
# hist(va.dir)
# va.dir[va.dir<0 & !is.na(va.dir)] <- 360 + va.dir[va.dir<0]

# *** need to fix this...
wind.dif <- flight.details$ecmwf_wind_10m_dir - va.dir
hist(wind.dif)
