# # ##################################################################################################
# # ################################ MAP OF THE LOCATIONS GOOGLE API  ################################
# #API SETTINGS
# register_google(key = 'AIzaSyCzcUX4czylxflM0J58r69BbLp6RgrnuhI')
# getOption("ggmap")
# 
# TacomaMap_terrain <- get_map(location = c(lon = -122.1, lat = 47.49),
#                      maptype = "terrain", source = 'google',scale = 2,
#                      zoom = 10,
#                      filename="data/TacomaMap_temp")
# 
# save(TacomaMap_terrain, file = 'data/TacomaMap_terrain.rda')
# 
# # ##################################################################################################
# TacomaMap_roadmap <- get_map(location = c(lon = -122.1, lat = 47.49),
#                              maptype = "roadmap", source = 'google',scale = 2,
#                              zoom = 10,
#                              filename="data/TacomaMap_temp")
# 
# save(TacomaMap_roadmap, file = 'data/TacomaMap_roadmap.rda')
# 
# # ##################################################################################################
# TacomaMap_satellite <- get_map(location = c(lon = -122.1, lat = 47.49),
#                              maptype = "satellite", source = 'google',scale = 2,
#                              zoom = 10,
#                              filename="data/TacomaMap_temp")
# 
# save(TacomaMap_satellite, file = 'data/TacomaMap_satellite.rda')

map1 <- ggmap(TacomaMap_roadmap) +
  labs(x = '', y = '') +
  theme(legend.position = 'none') +
  scale_color_hue() +
  scale_fill_hue() +
  geom_point(data = df_train, aes(x = long, y = lat), size = 0.3, color = 'blue')+
  geom_point(data = hotspots_coordinates, aes(x = long, y = lat), size = 5, color = 'red', alpha = 1/1.5)+
  geom_label_repel(
    aes(long, lat, label = name),
    data=hotspots_coordinates,
    family = 'Times', 
    size = 3, 
    box.padding = 0.2, point.padding = 0.6,
    segment.color = 'black')

map2 <- ggmap(TacomaMap_roadmap) + stat_density2d(
  aes(x = long, y = lat, fill = ..level.., alpha = 0.25),
  size = 0.01, bins = 30, data = df_train,
  geom = "polygon"
) +
  geom_point(aes(x = long, y = lat, stroke = 2), colour='red', data = hotspots_coordinates, size =1.5) + 
  geom_label_repel(
    aes(long, lat, label = name),
    data=hotspots_coordinates,
    family = 'Times', 
    size = 3, 
    box.padding = 0.2, point.padding = 0.3,
    segment.color = 'grey50')

map3 <- ggmap(TacomaMap_roadmap)+
  stat_density2d(
    aes(x = long, y = lat, fill = ..level.., alpha = 0.25),
    size = 0.1, bins = 40, data = df_train,
    geom = "polygon") +
  geom_density2d(data = df_train, 
                 aes(x = long, y = lat), size = 0.3) +
  geom_point(aes(x = long, y = lat, stroke = 2), colour='red', data = hotspots_coordinates, size =1.5)+ 
  geom_label_repel(
    aes(long, lat, label = name),
    data=hotspots_coordinates,
    family = 'Times', 
    size = 3, 
    box.padding = 0.2, point.padding = 0.3,
    segment.color = 'grey50') + 
  theme(legend.position="none")
