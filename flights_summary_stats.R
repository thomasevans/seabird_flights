# Calculate summary statistics for each flight for statistical analysis and general
# summary

# Get data -----

# Flight info
load("flights.detailed.incl.RData")

# GPS locations + calculated variables
load("points.detailed.incl.RData")

load("points.detailed.RData")
# 
# points.detailed$va_1m
# points.detailed$va_v_1m
# points.detailed$va_u_1m
# points.detailed$va_flt_1m_bearing

points.df_new <- merge(points.df, points.detailed[,c("flight_id_combined",
                                                     "date_time",
                                                     "track_head_wind_1m",
                                                     "track_cross_wind_1m",
                                                     "va_1m",
                                                     "va_v_1m",
                                                     "va_u_1m",
                                                     "va_flt_1m_bearing")],
                       by = c("flight_id_combined", "date_time"))
# ?merge
# points.detailed$track_cross_wind_1m
points.df <- points.df_new

# Functions and packages ----
source("deg.dist.R")
library(CircStats)

# Calculate/ assemble general summary data ----
# e.g. distance travelled, bird_id, etc. for included segment (i.e. following truncation)

flight_ids <- flights.df$flight_id_combined[flights.df$include_flight]
n_flights <- length(flight_ids)

# Make an empty data frame for the data
df_names <- c("flight_id_combined",
              "n",
              "trunc_seg_duration",
              "trunc_seg_lat_start",
              "trunc_seg_lat_end",
              "trunc_seg_long_start",
              "trunc_seg_long_end",
              "trunc_seg_date_time_start",
              "trunc_seg_date_time_end",
              "trunc_seg_dist_a_b",
              "altitude_filter_n",
              "altitude_callib",
              "altitude_callib_extm",
              "altitude_callib_extm_05",
              "altitude_callib_no_filter",
              "altitude_callib_extm_no_filter",
              "altitude_callib_extm_05_no_filter",
              "ecmwf_wind_10m_dir",
              "ecmwf_wind_10m_speed",
              "ecmwf_wind_10m_speed_flt_ht",
              "ecmwf_wind_10m_speed_50m",
              "ecmwf_wind_10m_speed_1m",
              "ecmwf_wind_10m_speed_gradient_01_50_ratio",
              "ecmwf_wind_10m_speed_gradient_01_50_dif",
              "vg_v",
              "vg_u",
              "vg",
              "va_v_10m",
              "va_u_10m",
              "va_v_flt_ht",
              "va_u_flt_ht",
              "va_flt_ht",
              "va_10m",
              "va_v_1m",
              "va_u_1m",
              "va_1m",
              "va_flt_ht_bearing",
              "va_flt_10m_bearing",
              "alpha_flt_ht",
              "alpha_10m",
              "cross_wind_10m",
              "head_wind_10m",
              "cross_wind_flt_ht",
              "head_wind_flt_ht",
              "track_cross_wind_10m",
              "track_head_wind_10m",
              "track_cross_wind_1m",
              "track_head_wind_1m",
              "track_cross_wind_flt_ht",
              "track_head_wind_flt_ht",
              "wind_effect_10m",
              "wind_effect_flt_ht",
              "ecmwf_wind_10m_dir_alt_filter",
              "ecmwf_wind_10m_speed_alt_filter",
              "ecmwf_wind_10m_speed_flt_ht_alt_filter",
              "ecmwf_wind_10m_speed_50m_alt_filter",
              "ecmwf_wind_10m_speed_1m_alt_filter",
              "ecmwf_wind_10m_speed_gradient_01_50_ratio_alt_filter",
              "ecmwf_wind_10m_speed_gradient_01_50_dif_alt_filter",
              "vg_v_alt_filter",
              "vg_u_alt_filter",
              "vg_alt_filter",
              "va_v_10m_alt_filter",
              "va_u_10m_alt_filter",
              "va_v_1m_alt_filter",
              "va_u_1m_alt_filter",
              "va_1m_alt_filter",
              "va_v_flt_ht_alt_filter",
              "va_u_flt_ht_alt_filter",
              "va_flt_ht_alt_filter",
              "va_10m_alt_filter",
              "va_flt_ht_bearing_alt_filter",
              "va_flt_10m_bearing_alt_filter",
              "alpha_flt_ht_alt_filter",
              "alpha_10m_alt_filter",
              "cross_wind_10m_alt_filter",
              "head_wind_10m_alt_filter",
              "cross_wind_flt_ht_alt_filter",
              "head_wind_flt_ht_alt_filter",
              "track_cross_wind_10m_alt_filter",
              "track_head_wind_10m_alt_filter",
              "track_cross_wind_1m_alt_filter",
              "track_head_wind_1m_alt_filter",
              "track_cross_wind_flt_ht_alt_filter",
              "track_head_wind_flt_ht_alt_filter",
              "wind_effect_10m_alt_filter",
              "wind_effect_flt_ht_alt_filter"
              
)

flight_summary_df <- data.frame(matrix(vector(),0,length(df_names),
                                          dimnames = list(c(),
                                                          df_names)),
                                stringsAsFactors = FALSE)

flight_summary_df[c(1:n_flights),"flight_id_combined"] <- flight_ids

# i <- 100
# Get following for each flight
for(i in 1:n_flights){
  
  # flight id
  f_id <- flight_summary_df$flight_id_combined[i]
  
  # get sub-set of points for flight, but only those included (after truncation)
  points.flight <- points.df[points.df$flight_id_combined == f_id &
                               points.df$included_points,]
  
  # How many points?
  n_points <- nrow(points.flight)
  
  flight_summary_df$n[i] <- n_points
  
  
  # Non-flight points exclude
  fp <- points.flight$speed_2d > 3  & !is.na(points.flight$speed_2d)
  
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
  flight_summary_df$trunc_seg_dist_a_b[i] <- deg.dist(
    flight_summary_df$trunc_seg_long_start[i],
    flight_summary_df$trunc_seg_lat_start[i],
    flight_summary_df$trunc_seg_long_end[i],
    flight_summary_df$trunc_seg_lat_end[i],
    km = FALSE)
  
  
  # Various flight variables
  
  # Altitude variables
    # Altitude points_included
    alt_incl <- points.flight$altitude_filter_included == TRUE
    
    
    # How many altitude points included?
    flight_summary_df$altitude_filter_n[i] <- sum(alt_incl)
    
    # If points meeting altitude requirements get values (otherwise NAs)
    if(sum(alt_incl & fp) >0){
      flight_summary_df$altitude_callib[i] <-
        median(na.rm = TRUE, points.flight$altitude_callib[alt_incl & fp])
      
      flight_summary_df$altitude_callib_extm[i] <-
        median(na.rm = TRUE, points.flight$altitude_callib_extm[alt_incl & fp])
      
      flight_summary_df$altitude_callib_extm_05[i] <-
        median(na.rm = TRUE, points.flight$altitude_callib_extm_05[alt_incl & fp])
      
    }
    

    flight_summary_df$altitude_callib_no_filter[i] <-
      median(na.rm = TRUE, points.flight$altitude_callib[fp])
    
    flight_summary_df$altitude_callib_extm_no_filter[i] <-
      median(na.rm = TRUE, points.flight$altitude_callib_extm[fp])
    
    flight_summary_df$altitude_callib_extm_05_no_filter[i] <-
      median(na.rm = TRUE, points.flight$altitude_callib_extm_05[fp])
    

  # Wind variables
    flight_summary_df$ecmwf_wind_10m_dir[i] <-
      deg(circ.mean(rad(points.flight$ecmwf_wind_10m_dir[fp])))
    
    flight_summary_df$ecmwf_wind_10m_speed[i] <-
      median(na.rm = TRUE,points.flight$ecmwf_wind_10m_speed[fp])
      
    flight_summary_df$ecmwf_wind_10m_speed_flt_ht[i] <-
      median(na.rm = TRUE,points.flight$ecmwf_wind_10m_speed_flt_ht[fp])  
    
    flight_summary_df$ecmwf_wind_10m_speed_50m[i] <-
      median(na.rm = TRUE,points.flight$ecmwf_wind_10m_speed_50m[fp])  
    
    flight_summary_df$ecmwf_wind_10m_speed_1m[i] <-
      median(na.rm = TRUE,points.flight$ecmwf_wind_10m_speed_1m[fp])  
      
    flight_summary_df$ecmwf_wind_10m_speed_gradient_01_50_ratio[i] <-
      median(na.rm = TRUE,points.flight$ecmwf_wind_10m_speed_gradient_01_50_ratio[fp])  
    
    flight_summary_df$ecmwf_wind_10m_speed_gradient_01_50_dif[i] <-
      median(na.rm = TRUE,points.flight$ecmwf_wind_10m_speed_gradient_01_50_dif[fp])  
    
    
    
    
    # Calculate variables
    flight_summary_df$vg_v[i] <-
      median(na.rm = TRUE, points.flight$vg_v[fp])  
    
    flight_summary_df$vg_u[i] <-
      median(na.rm = TRUE, points.flight$vg_u[fp])
    
    vg <- NA
    vg <- sqrt(points.flight$vg_u[fp]*points.flight$vg_u[fp] +
                 points.flight$vg_v[fp]*points.flight$vg_v[fp])
    
    flight_summary_df$vg[i] <-
      median(na.rm = TRUE, vg)
    
    flight_summary_df$va_v_10m[i] <-
      median(na.rm = TRUE, points.flight$va_v_10m[fp])  
    
    flight_summary_df$va_u_10m[i] <-
      median(na.rm = TRUE, points.flight$va_u_10m[fp])  
    
    flight_summary_df$va_v_1m[i] <-
      median(na.rm = TRUE, points.flight$va_v_1m[fp])  
    
    flight_summary_df$va_u_1m[i] <-
      median(na.rm = TRUE, points.flight$va_u_1m[fp])  
    
    flight_summary_df$va_v_flt_ht[i] <-
      median(na.rm = TRUE, points.flight$va_v_flt_ht[fp])  
    
    flight_summary_df$va_u_flt_ht[i] <-
      median(na.rm = TRUE, points.flight$va_u_flt_ht[fp])  
    
    flight_summary_df$va_flt_ht[i] <-
      median(na.rm = TRUE, points.flight$va_flt_ht[fp])  
    
    flight_summary_df$va_10m[i] <-
      median(na.rm = TRUE, points.flight$va_10m[fp])  
    
    flight_summary_df$va_1m[i] <-
      median(na.rm = TRUE, points.flight$va_1m[fp])  
                                                                               
    flight_summary_df$va_flt_ht_bearing[i] <-
      median(na.rm = TRUE, points.flight$va_flt_ht_bearing[fp])  
    
    flight_summary_df$va_flt_10m_bearing[i] <-
      median(na.rm = TRUE, points.flight$va_flt_10m_bearing[fp])                                                                       
    flight_summary_df$alpha_flt_ht[i] <-
      median(na.rm = TRUE, points.flight$alpha_flt_ht[fp])  
    
    flight_summary_df$alpha_10m[i] <-
      median(na.rm = TRUE, points.flight$alpha_10m[fp])                                                              
    flight_summary_df$cross_wind_10m[i] <-
      median(na.rm = TRUE, points.flight$cross_wind_10m[fp])  
    
    flight_summary_df$head_wind_10m[i] <-
      median(na.rm = TRUE, points.flight$head_wind_10m[fp])                                                                  
    flight_summary_df$cross_wind_flt_ht[i] <-
      median(na.rm = TRUE, points.flight$cross_wind_flt_ht[fp])  
    
    flight_summary_df$head_wind_flt_ht[i] <-
      median(na.rm = TRUE, points.flight$head_wind_flt_ht[fp])                                              
                                                                  
    flight_summary_df$track_cross_wind_10m[i] <-
      median(na.rm = TRUE, points.flight$track_cross_wind_10m[fp])  
    
    flight_summary_df$track_head_wind_10m[i] <-
      median(na.rm = TRUE, points.flight$track_head_wind_10m[fp])  
    
    
    flight_summary_df$track_cross_wind_1m[i] <-
      median(na.rm = TRUE, points.flight$track_cross_wind_1m[fp])  
    # points.flight$track
    flight_summary_df$track_head_wind_1m[i] <-
      median(na.rm = TRUE, points.flight$track_head_wind_1m[fp])  
    
    flight_summary_df$track_cross_wind_flt_ht[i] <-
      median(na.rm = TRUE, points.flight$track_cross_wind_flt_ht[fp])  
    
    flight_summary_df$track_head_wind_flt_ht[i] <-
      median(na.rm = TRUE, points.flight$track_head_wind_flt_ht[fp])                                                     
    flight_summary_df$wind_effect_10m[i] <-
      median(na.rm = TRUE, points.flight$wind_effect_10m[fp])  
    
    flight_summary_df$wind_effect_flt_ht[i] <-
      median(na.rm = TRUE, points.flight$wind_effect_flt_ht[fp])                                          

    
    # With altitude filter thing
    # Wind variables
    flight_summary_df$ecmwf_wind_10m_dir_alt_filter[i] <-
      deg(circ.mean(rad(points.flight$ecmwf_wind_10m_dir[alt_incl & fp])))
    
    flight_summary_df$ecmwf_wind_10m_speed_alt_filter[i] <-
      median(na.rm = TRUE,points.flight$ecmwf_wind_10m_speed[alt_incl & fp])
    
    flight_summary_df$ecmwf_wind_10m_speed_flt_ht_alt_filter[i] <-
      median(na.rm = TRUE,points.flight$ecmwf_wind_10m_speed_flt_ht[alt_incl & fp])  
    
    flight_summary_df$ecmwf_wind_10m_speed_50m_alt_filter[i] <-
      median(na.rm = TRUE,points.flight$ecmwf_wind_10m_speed_50m[alt_incl & fp])  
    
    flight_summary_df$ecmwf_wind_10m_speed_1m_alt_filter[i] <-
      median(na.rm = TRUE,points.flight$ecmwf_wind_10m_speed_1m[alt_incl & fp])  
    
    flight_summary_df$ecmwf_wind_10m_speed_gradient_01_50_ratio_alt_filter[i] <-
      median(na.rm = TRUE,points.flight$ecmwf_wind_10m_speed_gradient_01_50_ratio[alt_incl & fp])  
    
    flight_summary_df$ecmwf_wind_10m_speed_gradient_01_50_dif_alt_filter[i] <-
      median(na.rm = TRUE,points.flight$ecmwf_wind_10m_speed_gradient_01_50_dif[alt_incl & fp])  
    
    
    
    
    # Calculate variables
    flight_summary_df$vg_v_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$vg_v[alt_incl & fp])  
    
    flight_summary_df$vg_u_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$vg_u[alt_incl & fp])  
    
    
    vg <- NA
    vg <- sqrt(points.flight$vg_u[alt_incl & fp]*points.flight$vg_u[alt_incl & fp] +
                 points.flight$vg_v[alt_incl & fp]*points.flight$vg_v[alt_incl & fp])
    
    flight_summary_df$vg_alt_filter[i] <-
      median(na.rm = TRUE, vg)
    
    flight_summary_df$va_v_10m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$va_v_10m[alt_incl & fp])  
    
    flight_summary_df$va_u_10m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$va_u_10m[alt_incl & fp])  
    
    
    flight_summary_df$va_v_1m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$va_v_1m[alt_incl & fp])  
    
    flight_summary_df$va_u_1m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$va_u_1m[alt_incl & fp])  
    
    flight_summary_df$va_v_flt_ht_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$va_v_flt_ht[alt_incl & fp])  
    
    flight_summary_df$va_u_flt_ht_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$va_u_flt_ht[alt_incl & fp])  
    
    flight_summary_df$va_flt_ht_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$va_flt_ht[alt_incl & fp])  
    
    flight_summary_df$va_10m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$va_10m[alt_incl & fp])  
    
    flight_summary_df$va_1m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$va_1m[alt_incl & fp]) 
    
    flight_summary_df$va_flt_ht_bearing_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$va_flt_ht_bearing[alt_incl & fp])  
    
    flight_summary_df$va_flt_10m_bearing_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$va_flt_10m_bearing[alt_incl & fp])                                                                       
    flight_summary_df$alpha_flt_ht_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$alpha_flt_ht[alt_incl & fp])  
    
    flight_summary_df$alpha_10m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$alpha_10m[alt_incl & fp])                                                              
    flight_summary_df$cross_wind_10m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$cross_wind_10m[alt_incl & fp])  
    
    flight_summary_df$head_wind_10m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$head_wind_10m[alt_incl & fp])                                                                  
    flight_summary_df$cross_wind_flt_ht_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$cross_wind_flt_ht[alt_incl & fp])  
    
    flight_summary_df$head_wind_flt_ht_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$head_wind_flt_ht[alt_incl & fp])                                              
    
    flight_summary_df$track_cross_wind_10m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$track_cross_wind_10m[alt_incl & fp])  
    
    flight_summary_df$track_head_wind_10m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$track_head_wind_10m[alt_incl & fp])      
    
    flight_summary_df$track_cross_wind_1m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$track_cross_wind_1m[alt_incl & fp])  
    # points.flight$track
    flight_summary_df$track_head_wind_1m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$track_head_wind_1m[alt_incl & fp])  
    
    
    
    
    flight_summary_df$track_cross_wind_flt_ht_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$track_cross_wind_flt_ht[alt_incl & fp])  
    
    flight_summary_df$track_head_wind_flt_ht_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$track_head_wind_flt_ht[alt_incl & fp])                                                     
    flight_summary_df$wind_effect_10m_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$wind_effect_10m[alt_incl & fp])  
    
    flight_summary_df$wind_effect_flt_ht_alt_filter[i] <-
      median(na.rm = TRUE, points.flight$wind_effect_flt_ht[alt_incl & fp])                                          
               
    
    }



# See how many are lacking altitude
summary(flight_summary_df$altitude_filter_n == 0)



# Combine summary table with essential columns from flights.df -----
# Merge tables (only columns required)

flight.details <- merge(flight_summary_df,
                        flights.df,
                        by = "flight_id_combined")

all(flight.details$flight_id_combined == flight_summary_df$flight_id_combined)


flight.details <- flight.details[order(flight.details$flight_id_combined),]


# str(flight.details)

# flight.details$trunc_seg_date_time_end <- as.POSIXct(
#   flight.details$trunc_seg_date_time_end, origin = "1970-01-01 00:00:00",
#            tz = "UTC")
# 
# 
# flight.details$trunc_seg_date_time_start <- as.POSIXct(
#   flight.details$trunc_seg_date_time_start, origin = "1970-01-01 00:00:00",
#   tz = "UTC")

# as.POSIXct()
flight.details <- flight.details[,-which(names(flight.details) %in% c("trunc_seg_date_time_start","trunc_seg_date_time_end"))]


# Output to new table ----

# Flight summary data
save(flight.details, file = "flight_details.RData")

# Output to csv
write.table(flight.details, file = "flight_details.csv", col.names = TRUE,
            row.names = FALSE, sep = ",")



# flight.details$track_head_wind_1m
