
# Prepare point data for inclusion as supplementary data

# GPS point data
load("points.detailed.RData")

# Names in full file
all.var.name <- names(points.detailed)

# to handle data efficiently
library(dplyr)

vars.to.include <- c("device_info_serial",
                     "flight_id_combined",
                     "date_time",
                     "latitude",
                     "longitude",
                     "altitude",
                     "speed_2d",
                     "direction_common",
                     "h_accuracy",
                     "v_accuracy",
                     "satellites_used",
                     "positiondop",
                     "ehpe",
                     "timeout",
                     "MSVs_QCN")

points.to.export <- select(points.detailed, one_of(vars.to.include))

str(points.to.export)

names(points.to.export) <- c("device_info_serial",
                             "flight_id",
                             "date_time",
                             "latitude",
                             "longitude",
                             "altitude_m",
                             "velocity_instantaneous_ms1",
                             "direction_instantaneous_ms1",
                             "h_accuracy",
                             "v_accuracy",
                             "satellites_used",
                             "positiondop",
                             "ehpe",
                             "timeout",
                             "MSVs_QCN")

# Sort data frame (flight_id,date_time)

points.to.export.sorted <- arrange(points.to.export, flight_id, date_time)


summary(points.to.export.sorted)

write.csv(points.to.export.sorted, "gps_points.csv",
          row.names = FALSE)
