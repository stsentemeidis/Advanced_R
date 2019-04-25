##################################################################################################
################################ OPTIMAL DISTANCE VALUE ##########################################
distance_values <- seq(1000,5000,100)#seq(1000,3000,100)
distance_clusters_df <- data.frame(distance = numeric(),
                                   number_of_clusters = numeric())
for (i in distance_values){

  cluster_coords_test <- cluster_coords_hier(df_coords, i)                                 # distance in meters
  cluster_coords_test_df <- as.data.frame(cluster_coords_test)
  distance_clusters_df <- rbind(distance_clusters_df,c(i,length(unique(cluster_coords_test_df$clust)))  )              # number of clusters
  
}

saveRDS(distance_clusters_df,'data/distance_clusters_df.rds')

ggplot(count_clusters) + 
  geom_point(data = count_clusters, aes(x = radius, y = num_clusters))+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color2, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 8, family = font2, angle=90),
        legend.position = 'None',
        plot.background = element_rect(fill = color1))