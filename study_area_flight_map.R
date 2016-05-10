

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


# colours
library(ggplot2)
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols.new <- gg_color_hue(2)


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

# Set global par
par(ps = 14, cex = 1.5, cex.lab = 2)

svg("map_test3.svg",
    width = 12, height = 6, family = "serif")

par(mfrow = c(1,2), cex = 1.0)
par(mar=c(2, 3, 1, 1) + 0.1)


# Set limits
c.xlim <- range(points.gull$longitude[points.gull$included_points == TRUE])
dif    <- c.xlim[2] - c.xlim[1]
dif    <- dif *.05
c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))

c.ylim <- range(points.gull$latitude[points.gull$included_points == TRUE])
dif    <- c.ylim[2] - c.ylim[1]
dif    <- dif *.05
c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))


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
             col = addalpha(cols.new[2], alpha = 0.07),
             lwd = 1
    )  
  }
  

}




# Label key locations -----
karlso.cen.long   <-  17.972088
karlso.cen.lat    <-  57.284804

# SK.loc.proj <- mapproject(karlso.cen.long,karlso.cen.lat)

points(karlso.cen.long, karlso.cen.lat, pch = 4, cex = 2, col = "red",
       lwd = 2)

# Add scale, axis etc ------
map.scale(ratio = FALSE,
          col="black", col.lab="black",
          relwidth = 0.3)
box(col="black",lwd=2)
axis(side=(1), las=1, col="black", col.axis="black")
axis(side=(2), las=1, col="black", col.axis="black")

# ?map.scale

legend("topleft", "(a)", bty="n", cex = 1.2) 



# Murre map -----


# Set limits
c.xlim <- range(points.murre$longitude[points.murre$included_points == TRUE])
dif    <- c.xlim[2] - c.xlim[1]
dif    <- dif *.05
c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))

c.ylim <- range(points.murre$latitude[points.murre$included_points == TRUE])
dif    <- c.ylim[2] - c.ylim[1]
dif    <- dif *.05
c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))


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
flight_ids <- unique(points.murre$flight_id_combined)

for(i in 1:length(flight_ids)){
  fp <- points.murre$flight_id_combined == flight_ids[i]
  n <- sum(fp)
  if(!is.na(n) & n > 2){
    segments(points.murre$longitude[fp][1:(n-1)],
             points.murre$latitude[fp][1:(n-1)],
             points.murre$longitude[fp][-1],
             points.murre$latitude[fp][-1],
             col = addalpha(cols.new[1], alpha = 0.2),
             lwd = 1
    )  
  }
  
  
}




# Label key locations -----
karlso.cen.long   <-  17.972088
karlso.cen.lat    <-  57.284804

# SK.loc.proj <- mapproject(karlso.cen.long,karlso.cen.lat)

points(karlso.cen.long, karlso.cen.lat, pch = 4, cex = 2, col = "red",
       lwd = 2)

# Add scale, axis etc ------
map.scale(ratio = FALSE,
          col="black", col.lab="black",
          relwidth = 0.3)
box(col="black",lwd=2)
axis(side=(1), las=1, col="black", col.axis="black")
axis(side=(2), las=1, col="black", col.axis="black")

# ?map.scale

legend("topleft", "(b)", bty="n", cex = 1.2) 


dev.off()




# Extra area overview map ----
data(worldMapEnv)

# ?svg
svg("map_test_area.svg",
    width = 2, height = 2, family = "serif",
    bg = NA)
par(mfrow = c(1,1))
par(mar=c(0, 0, 0, 0))
map('world', xlim = c(5,25),
     ylim = c(52, 62), col= addalpha("white", 0.8), bg = NA,
     main = "", fill = TRUE)
rect(15.7, 56.2, 19.2, 58.1, angle = 45,
     col = addalpha("white", 0.5))
box(col = "white", lwd = 2)
# ?map
dev.off()