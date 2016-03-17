# Altitude correction script
# calculate mean/ median altitude for 'sea surface' GPS locations, then use
# as correction factor of recorded GPS altitude - to get 'true' altitude above
# mean sea level


# Set bounding box (sea only between Stora Karlsö and Öland) -----
# Latitude range
lat.lim <- c(56.9, 57.6)

# Longitude range
long.lim <- c(17.2, 17.9)


# Read in GPS point data (both IGU and UvA) -----
# Only data within bounding box where speed is < 1 ms-1
