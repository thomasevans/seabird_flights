

gps_data <- read.csv("Antarctic petrel 3D flights, Svarthamaren, Antarctica (data from Tarroux et al. 2016).csv",
                     header = TRUE)


str(gps_data)


install.packages("plotKML")

library(plotKML)
library(sp)
library(rgdal)


gps_sp_df <- SpatialPointsDataFrame(cbind(gps_data$location.long, gps_data$location.lat),
                       gps_data)


# 
# test <- plotKML(gps_sp_df)

proj4string(gps_sp_df) <- CRS("+proj=longlat +datum=WGS84")

?CRS


# Need to modify the point styles (too large circles now)
# Plus should looop through the tracks to produce sepperate files for each flight
plotKML(gps_sp_df,
        shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png", 
        metadata = NULL, kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE,
        file.name = paste(folder.name, ".kml", sep=""))



summary(as.factor(gps_data$tag.local.identifier))
