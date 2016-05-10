

# Load in data ----
# GPS locations + calculated variables
load("points.detailed.incl.RData")
load("flights.detailed.incl.RData")


points.detailed <- merge(points.df, flights.df, "flight_id_combined")

points.murre <- points.detailed[(points.detailed$included_points == TRUE) &
                               (points.detailed$species == "murre") &
                            (points.detailed$include_flight == TRUE),]

points.gull <- points.detailed[(points.detailed$included_points == TRUE) &
                            (points.detailed$species == "gull") &
                           points.detailed$include_flight == TRUE,]


# Prepare basic map -----


# Code for transparent colours ----
# Code from https://github.com/mylesmharrison/colorRampPaletteAlpha/blob/master/colorRampPaletteAlpha.R
# Hight-lighted by blog post: http://www.everydayanalytics.ca/2014/03/colorRampPalette-alpha-in-R.html
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

# Base map with land on

# needed to plot maps
library(maps)
library(mapproj)
library(raster) 

# Plot base map
load("SWE_adm0.RData")


# Set limits
c.xlim <- range(points.detailed$longitude[points.detailed$included_points == TRUE])
dif    <- c.xlim[2] - c.xlim[1]
dif    <- dif *.05
c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))

c.ylim <- range(points.detailed$latitude[points.detailed$included_points == TRUE])
dif    <- c.ylim[2] - c.ylim[1]
dif    <- dif *.05
c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))

# Set global par
par(ps = 14, cex = 1.5, cex.lab = 2)

svg("map_test3.svg",
    width = 6, height = 6, family = "serif")

par(mfrow = c(1,1), cex = 1.0)
par(mar=c(2, 3, 1, 1) + 0.1)


range.x <-  c.xlim[2]-c.xlim[1]
range.y <-  c.ylim[2]-c.ylim[1]
dif.co <- max(c(range.x,range.y))

gadm_clip <- crop(gadm, extent(c.xlim[1]-1*(dif.co),
                               c.xlim[2]+1*(dif.co),
                               c.ylim[1]-1*(dif.co),
                               c.ylim[2]+1*(dif.co)))


plot(gadm_clip, xlim = c.xlim,
     ylim = c.ylim, col="grey", bg = "white",
     main = "")
grid()



# Add GPS tracked flights ----
flight_ids <- unique(points.gull$flight_id_combined)

for(i in 1:length(flight_ids)){
  fp <- points.gull$flight_id_combined == flight_ids[i]
  n <- sum(fp)
  if(!is.na(n) & n > 2){
    segments(points.gull$longitude[fp][1:(n-1)],
             points.gull$latitude[fp][1:(n-1)],
             points.gull$longitude[fp][-1],
             points.gull$latitude[fp][-1],
             col = addalpha("red", alpha = 0.1),
             lwd = 0.5
    )  
  }
  

}


dev.off()


# Label key locations -----


# Add scale, axis etc ------