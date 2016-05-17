# Circular histograms of flight distributions

# Load in data -----
load("flight_details.RData")


# Packages needed -----
# install.packages("circular")
library(circular)
# library(ggplot2)

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
     col = cols.new[2])

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
     sep = 0.045,
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
# warnings()
fun2 <- function(x){
  if(is.na(x))return(NA) else{
    if(x<0)return(360 + x) else{
      return(x)
    }
  }
}
# fun2(NA)
va.dir <- sapply(va.dir, fun2)
hist(va.dir)
range(va.dir, na.rm = TRUE)
# va.dir[va.dir<0 & !is.na(va.dir)] <- 360 + va.dir[va.dir<0]

range(flight.details$ecmwf_wind_10m_dir)

wind.dir.range <- flight.details$ecmwf_wind_10m_dir
wind.dir.range[flight.details$ecmwf_wind_10m_dir < 0] <- 360 + flight.details$ecmwf_wind_10m_dir[flight.details$ecmwf_wind_10m_dir < 0]

# *** need to fix this...
wind.dif <- wind.dir.range - va.dir
hist(wind.dif)

wind.dif.new <- sapply(wind.dif, fun2)
hist(wind.dif.new)

wind.dif.new.circ <- circular(wind.dif.new,
                                type = "angles",
                                units = "degrees",
                                template = "none"
)
# ?circular


Code for transparent colours ----
# Code from https://github.com/mylesmharrison/colorRampPaletteAlpha/blob/master/colorRampPaletteAlpha.R
# Hight-lighted by blog post: http://www.everydayanalytics.ca/2014/03/colorRampPalette-alpha-in-R.html
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}


plot(wind.dif.new.circ[gulls],
     # stack = TRUE,
     # sep = 0.015,
     shrink = 1.2,
     cex = 1,
     bin = 45,
     col = addalpha(cols.new[2], 0.3),
     zero = pi/2,
     rotation = "clock")
# ?plot.circular
# 
# hist(wind.dif.new[gulls], breaks = 40)
# points((x, pch = 16, cex = 1, stack = FALSE, sep = 0.025, 
#         shrink = 1, bins = NULL, col = NULL, next.points = NULL, 
#         plot.info = NULL, zero = NULL, rotation = NULL, ...))

# hist(wind.dif.new[!gulls], breaks = 20)

points(wind.dif.new.circ[!gulls],
     # stack = TRUE,
     # sep = 0.045,
     shrink = 1.2,
     cex = 1,
     bin = 45,
     col = addalpha(cols.new[1], 0.7),
     zero = pi/2,
     # new = FALSE,
     rotation = "clock",
     next.points = 0.02)

x <- wind.dif.new.circ[!gulls]
x <- x[!is.na(x)]
lines(density.circular(x,
                       bw = 30),
      col = addalpha(cols.new[1], 0.7),
      lwd = 3,
      # lty = 2,
      zero = pi/2,
      # new = FALSE,
      rotation = "clock",
      shrink = 0.7)


x <- wind.dif.new.circ[gulls]
x <- x[!is.na(x)]
lines(density.circular(x,
                       bw = 30),
      col = addalpha(cols.new[2], 0.7),
      lwd = 3,
      # lty = 2,
      zero = pi/2,
      # new = FALSE,
      rotation = "clock",
      shrink = 0.7)
# ?lines.circular
legend("topleft", "(e)", bty="n", cex = 1.2) 
# bw.cv.mse.circular()

# warnings()
# 
# data.1 <- rvonmises(n=100, mu=circular(0), kappa=3)
# data.2 <- rvonmises(n=100, mu=circular(pi/3), kappa=3) 
# res <- plot(data.1, stack=FALSE, col=1) 
# points(data.2, plot.info=res, col=2)
