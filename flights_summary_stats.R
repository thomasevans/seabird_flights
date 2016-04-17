# Calculate summary statistics for each flight for statistical analysis and general
# summary

# Get data -----

# Flight info
load("flights.detailed.incl.RData")

# GPS locations + calculated variables
load("points.detailed.incl.RData")



# Function ----
source("deg.dist.R")

# Calculate/ assemble general summary data ----
# e.g. distance travelled, bird_id, etc. for included segment (i.e. following truncation)

flight_ids <- flights.df$flight_id_combined[flights.df$include_flight]
n_flights <- length(flight_ids)

flight_summary_df <- data.frame(matrix(vector(),0,9,
                                          dimnames = list(c(),c("flight_id_combined",
                                     "trunc_seg_duration",
                                       "trunc_seg_lat_start",
                                       "trunc_seg_lat_end",
                                       "trunc_seg_long_start",
                                       "trunc_seg_long_end",
                                       "trunc_seg_date_time_start",
                                       "trunc_seg_date_time_end",
                                       "trunc_seg_dist_a_b"))
), stringsAsFactors = FALSE)

flight_summary_df[c(1:n_flights),"flight_id_combined"] <- flight_ids

# i <- 1
# Get following for each flight
for(i in 1:n_flights){
  
  # flight id
  f_id <- flight_summary_df$flight_id_combined[i]
  
  # get sub-set of points for flight, but only those included (after truncation)
  points.flight <- points.df[points.df$flight_id_combined == f_id &
                               points.df$included_points,]
  
  # How many points?
  n_points <- nrow(points.flight)
  
  # Start/ end latitude
  flight_summary_df$trunc_seg_lat_start[i] <- points.flight$latitude[1]
  flight_summary_df$trunc_seg_lat_end[i] <- points.flight$latitude[n_points]
  
  # Start/ end longitude
  flight_summary_df$trunc_seg_long_start[i] <- points.flight$longitude[1]
  flight_summary_df$trunc_seg_long_end[i] <- points.flight$longitude[n_points]
  
  # Start/ end date_time
  flight_summary_df$trunc_seg_date_time_start[i] <-
    points.flight$date_time[1]
  flight_summary_df$trunc_seg_date_time_end[i] <-
    points.flight$date_time[n_points]
  
  # flight segment duration
  flight_summary_df$trunc_seg_duration[i] <- flight_summary_df$trunc_seg_date_time_end[i] -
    flight_summary_df$trunc_seg_date_time_start[i]
  
  
  # flight segment distance
  flight_summary_df$trunc_seg_dist_a_b[i] <- deg.dist(flight_summary_df$trunc_seg_long_start[i],
                                 flight_summary_df$trunc_seg_lat_start[i],
                                 flight_summary_df$trunc_seg_long_end[i],
                                 flight_summary_df$trunc_seg_lat_end[i],
                                 km = FALSE)
  
  
  # trunc_seg_dist_p2p_original
  
}

# If we want straightness etc should do this too - ignore for now
# # To reduce scale dependence, recalculate for fixed time interval
# trunc_seg_dist_p2p_resample_100s
# # Here should calculate A to B distance again too, as will differ a bit from original locations
# trunc_seg_dist_straightness_resample_100s



# Mean/ median for wind conditions, and calculated wind dependent information
# e.g. alpha...


# Get airspeed (probably median??)


# Get altitude (include quality criteria in this selection)


# Output to new table ----
