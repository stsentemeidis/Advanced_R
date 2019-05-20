##################################################################################################
################################ CALCULATE DISTANCES FROM HOTSPOT ################################
distance_from_hotspot <- function(df){
  
  for (i in 1:nrow(hotspots_coordinates)){
    assign(paste0('distance_from_',hotspots_coordinates[i,1]),haversine_dist(df, 'long', 'lat', hotspots_coordinates[i,2], hotspots_coordinates[i,3]))
    hotspots_coordinates[i,4] <- paste0('distance_from_',hotspots_coordinates[i,1])
  }
  
  for (i in 1:nrow(hotspots_coordinates)){
    temp <- get(hotspots_coordinates[[i,4]])
    df <- cbind(df,temp)
    names(df)[names(df) == 'temp'] <- hotspots_coordinates[i,4]
  }
  
  return(df)
}

# df_train$distance_from_airport                <- haversine_dist(df_train, 'long', 'lat', -122.301659, 47.443546)
# df_train$distance_from_zoo_acquarium          <- haversine_dist(df_train, 'long', 'lat', -122.434110, 47.245885)
# df_train$distance_from_museum_of_glass        <- haversine_dist(df_train, 'long', 'lat', -122.43366, 47.24556)
# df_train$distance_from_train_station          <- haversine_dist(df_train, 'long', 'lat', -122.427778, 47.239722)
# df_train$distance_from_university_of_wash     <- haversine_dist(df_train, 'long', 'lat', -122.4378, 47.2448)
# df_train$distance_from_community_college      <- haversine_dist(df_train, 'long', 'lat', -122.521920, 47.244109)
# df_train$distance_from_university_puget_sound <- haversine_dist(df_train, 'long', 'lat', -122.482893, 47.263680)
# df_train$distance_from_cheney_stadium         <- haversine_dist(df_train, 'long', 'lat', -122.498138, 47.238098)
# df_train$distance_from_port                   <- haversine_dist(df_train, 'long', 'lat', -122.418487, 47.265181)
