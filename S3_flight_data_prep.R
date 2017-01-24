
# Prepare flight data for inclusion as supplementary data

# flight data
load("flight_details.RData")


# Names in full file
all.var.name <- names(flight.details)

# Distance to km
flight_alt_ok$trunc_seg_dist_a_b_km <- flight_alt_ok$trunc_seg_dist_a_b / 1000


# to handle data efficiently
library(dplyr)



# To be edited if useing...



# Possible variables to keep:
# [1] "flight_id_combined"           
# [130] "device_info_serial"                                  
# [131] "trip_id"            
# [133] "start_time"                                          
# [134] "end_time" 
# [150] "species"  
# [2] "n"          
# 
# [3] "trunc_seg_duration"                                  
# [4] "trunc_seg_lat_start"                                 
# [5] "trunc_seg_lat_end"                                   
# [6] "trunc_seg_long_start"                                
# [7] "trunc_seg_long_end"                                  
# [8] "trunc_seg_dist_a_b"                                  
# 
# [9] "altitude_filter_n"                                   
# [10] "altitude_callib"                                     
# [11] "altitude_callib_extm"                                
# [12] "altitude_callib_extm_05"                             
# [13] "altitude_callib_no_filter"                           
# [14] "altitude_callib_extm_no_filter"                      
# [15] "altitude_callib_extm_05_no_filter"               
# 
# [16] "ecmwf_wind_10m_dir"                                  
# [17] "ecmwf_wind_10m_speed"                                
# [18] "ecmwf_wind_10m_speed_flt_ht"    
# 
# 
# [19] "ecmwf_wind_10m_speed_50m"                            
# [20] "ecmwf_wind_10m_speed_1m"                             
# [21] "ecmwf_wind_10m_speed_gradient_01_50_ratio"           
# [22] "ecmwf_wind_10m_speed_gradient_01_50_dif"             
# 
# [23] "vg_v"                                                
# [24] "vg_u"                                                
# [25] "vg"                                                  
# 
# [26] "va_v_10m"                                            
# [27] "va_u_10m"                                            
# [31] "va_10m"                                              
# 
# [28] "va_v_flt_ht"                                         
# [29] "va_u_flt_ht"                                         
# [30] "va_flt_ht"                                           
# 
# 
# [59] "goal_cross_wind_10m"                                 
# [60] "goal_head_wind_10m"                                  
# 
# [67] "goal_cross_wind_flt_ht"                              
# [68] "goal_head_wind_flt_ht"                               
# 
# 
# [78] "vg_v_alt_filter"                                     
# [79] "vg_u_alt_filter"                                     
# [80] "vg_alt_filter"                                       
# [81] "va_v_10m_alt_filter"                                 
# [82] "va_u_10m_alt_filter"                                 
# 
# [92] "va_v_flt_ht_alt_filter"                              
# [93] "va_u_flt_ht_alt_filter"                              
# [94] "va_flt_ht_alt_filter"                                
# 
# [95] "va_10m_alt_filter"                                   
# 
# [114] "goal_cross_wind_10m_alt_filter"                      
# [115] "goal_head_wind_10m_alt_filter"                       
# [116] "goal_cross_wind_1m_alt_filter"                       
# [117] "goal_head_wind_1m_alt_filter"                        
# [118] "goal_cross_wind_2m_alt_filter"                       
# [119] "goal_head_wind_2m_alt_filter"                        
# [120] "goal_cross_wind_5m_alt_filter"                       
# [121] "goal_head_wind_5m_alt_filter"                        
# [122] "goal_cross_wind_flt_ht_alt_filter"                   
# [123] "goal_head_wind_flt_ht_alt_filter"    


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
