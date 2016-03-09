# Script to sort and compile data for both murres and gulls

# Read in data ------

# Datbase functions
# Required library
library(RODBC)

# Establish a connection to the databases
gull.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

murre.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# See what tables are available
#sqlTables(gps.db)


# Get gull flight data -------
gull.flights <- sqlQuery(gull.db, query="SELECT lund_flights.flight_id, lund_flights.points, lund_flights.device_info_serial, lund_flights.trip_id, lund_flights.trip_flight_type, lund_flights.start_time, lund_flights.end_time, lund_flights.duration, lund_flights.dist_max, lund_flights.dist_total, lund_flights.interval_mean, lund_flights.interval_min, lund_flights.start_long, lund_flights.start_lat, lund_flights.end_long, lund_flights.end_lat, lund_flights.dist_nest_start, lund_flights.dist_nest_end, lund_flights.dist_nest_dif, lund_flights.dist_a_b, gps_ee_track_session_limited.ring_number, lund_trips.gotland_time_prop
FROM gps_ee_track_session_limited, lund_flights INNER JOIN lund_trips ON lund_flights.trip_id = lund_trips.trip_id
                         WHERE (((lund_flights.trip_flight_type)='Inward') AND ((lund_flights.start_time)>=[gps_ee_track_session_limited].[track_session_start_date]) AND ((lund_flights.end_time)<=[gps_ee_track_session_limited].[track_session_end_date]) AND ((gps_ee_track_session_limited.device_info_serial)=[lund_flights].[device_info_serial]));
                         ")

# Remove those flights where >1% trip is Gotland
hist(gull.flights$gotland_time_prop, breaks = 100)
hist(gull.flights$gotland_time_prop, breaks = 1000, xlim = c(0,0.1))

# Only include those flights from trips with less than 1% of time on Gotland
gull.flights <- gull.flights[gull.flights$gotland_time_prop <0.01,]

# Then remove Gotland column
gull.flights <- gull.flights[,1:21]

# Get murre flight data -----
murre.flights <- sqlQuery(murre.db, query="SELECT guillemots_gps_flights.flight_id, guillemots_gps_flights.n_points, guillemots_gps_flights.device_info_serial, guillemots_gps_flights.trip_id, guillemots_gps_flights.flight_type, guillemots_gps_flights.ring_number, guillemots_gps_flights.start_time, guillemots_gps_flights.end_time, guillemots_gps_flights.duration, guillemots_gps_flights.col_dist_max, guillemots_gps_flights.p2p2_dist, guillemots_gps_flights.interval_mean, guillemots_gps_flights.interval_min, guillemots_gps_flights.long_start, guillemots_gps_flights.lat_start, guillemots_gps_flights.long_end, guillemots_gps_flights.lat_end, guillemots_gps_flights.col_dist_start, guillemots_gps_flights.col_dist_end, guillemots_gps_flights.col_dist_dif, guillemots_gps_flights.dist_straight
FROM guillemots_gps_flights
WHERE (((guillemots_gps_flights.flight_type)='Final'));
")


# Combine the murre and gull data -----
names(gull.flights)
names(murre.flights)

# Which names are different
names2change <- names(murre.flights) %in% names(gull.flights)
# Rename these in the murre table to match the gull table names
names(murre.flights)[!names2change] <- c("points", "trip_flight_type",
                                         "dist_max", "dist_total",
                                         "start_long", "start_lat",
                                         "end_long", "end_lat",
                                         "dist_nest_start",
                                         "dist_nest_end",
                                         "dist_nest_dif",
                                         "dist_a_b"
                                         )

# Add column with species names before combining
murre.flights$species <- "murre"
gull.flights$species <- "gull"

# Reorder columns in murre table to match order of gull table.
murre.flights <- murre.flights[,names(gull.flights)]

# Combine the two species tables
all.flights <- rbind.data.frame(gull.flights,murre.flights)


# Do course filter of which flights to retain for further sorting then analysis ----

# Exclude those flights that have fewer than or equal to 5 points
all.flights.keep <- all.flights$points > 4

# Exclude those not travelling towards the island
hist(all.flights$dist_nest_dif, xlim = c(-10000, 10000), breaks = 1000)
all.flights.keep <- all.flights.keep & all.flights$dist_nest_dif <0

summary(all.flights.keep)

# Exclude very short flights
hist(all.flights$dist_a_b[all.flights.keep]/1000, xlim = c(0,100), breaks = 2000)
abline(v=3)
all.flights.keep <- all.flights.keep & all.flights$dist_a_b >3000

summary(all.flights.keep)

summary(as.factor(all.flights$species[all.flights.keep]))

hist(murre.flights$dist_a_b, breaks = 40)

summary(as.factor(murre.flights$ring_number))



# Further criteria to include -----


# Classify whether first point is on sea or land ----
library(sp)

# Swedish coast-line data
load("SWE_adm0.RData")
str(gadm)
all.flights2 <- all.flights

# Make into spatial points data frame
coordinates(all.flights2) <- c("start_long", "start_lat")

# Confirm that they have the same projection
proj4string(all.flights2) <- proj4string(gadm)

# Are points on land?
points_land <- !is.na(over(all.flights2, gadm , fn = NULL))[,1]

# See how this looks
# str(points_land)
x.lim <- range(all.flights2$start_long)
y.lim <- range(all.flights2$start_lat)
y.lim <- c(56,y.lim[2])
plot(gadm, col= "grey50", bg = "grey90", xlim = x.lim,
     ylim = y.lim
)
points(all.flights2)
points(all.flights2[points_land,], col = "red")
# str(all.flights)

# How many points are on land?
summary(points_land)

all.flights.keep <- all.flights.keep & !points_land
summary(all.flights.keep)
summary(as.factor(all.flights$species[all.flights.keep]))



# No long gaps in data
# Set limit for gaps between fixes (maybe 10 minutes??)
hist(all.flights$interval_min[all.flights.keep], breaks = 40)
abline(v=500)

summary(as.factor(all.flights$species[all.flights.keep & all.flights$interval_min <500]))

all.flights.keep <- all.flights.keep & all.flights$interval_min <500
summary(as.factor(all.flights$species[all.flights.keep]))


# maybe plot flights - start to end locations only (i.e. lots of lines)


x.lim <- range(all.flights$start_long[all.flights.keep])
y.lim <- range(all.flights$start_lat[all.flights.keep])

plot(gadm, col= "grey50", bg = "grey90", xlim = x.lim,
     ylim = y.lim
)

segments(all.flights$start_long[all.flights.keep],
         all.flights$start_lat[all.flights.keep],
         all.flights$end_long[all.flights.keep],
         all.flights$end_lat[all.flights.keep],
         col = as.factor(all.flights$species[all.flights.keep]))


# Save flights data to csv and Rdata object ------
flights <- all.flights[all.flights.keep,]

# Make new unique flight ID by combining species and original flight_id
new_id <- function(x,y){
  if(x == "murre"){return(paste("m",y, sep = ""))} else {
    return(paste("g",y, sep = ""))
  }
}

flights$flight_id_combined <- mapply(x = flights$species,
                                     y = flights$flight_id,
                                     FUN = new_id)

# Output to csv
write.table(flights, file = "flights_all.csv", col.names = TRUE,
            row.names = FALSE, sep = ",")

save(flights, file = "flights.RData")

