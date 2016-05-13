# Script to extract Acc data for flights where this exists


# Load in data -----

# Load flight summary data
load("flight_details.RData")


# Make DB connection -----
# Datbase functions
# Required library
library(RODBC)

# Establish a connection to the databases
gull.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

murre.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# For each flight get info on where Acc records exist ----




# Combine into single table -----
