##################################################################################################
################################ OPTIMAL DISTANCE VALUE ##########################################
# distance_values <- seq(1000,5000,100)
# distance_clusters_df <- data.frame(distance = numeric(),
#                                    number_of_clusters = numeric())
# for (i in distance_values){
# 
#   cluster_coords_test <- cluster_coords_hier(df_coords, i)                                                # distance in meters
#   cluster_coords_test_df <- as.data.frame(cluster_coords_test)
#   distance_clusters_df <- rbind(distance_clusters_df,c(i,length(unique(cluster_coords_test_df$clust)))  ) # number of clusters
#   
# }
# 
# colnames(distance_clusters_df) <- c('distance', 'number_of_clusters')
# saveRDS(distance_clusters_df,'data/distance_clusters_df.rda')

distance_clusters_df <- readRDS('data/distance_clusters_df.rda')

opt_dist_plot <- ggplot(distance_clusters_df) + 
  geom_point(data = distance_clusters_df, aes(x = distance, y = number_of_clusters, color = color3))+
  geom_hline(yintercept=639, linetype="dashed", color = "yellow3", size=0.5)+
  geom_text(aes(2250,639,label = 639, vjust = -1, size = 10, color = color3))+
  theme_tufte()+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color2, size = 12, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2, angle=90),
        legend.position = 'None',
        plot.background = element_rect(fill = color1))+
  scale_x_continuous(breaks = distance_clusters_df$distance)+
  scale_y_continuous(breaks = c(0,300,600,900,1200,1500, 1800))

# opt_dist_plot
# grid.text(unit(0.5, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
#           label="Optimal Distance for Hierarchical Clustering",
#           gp=gpar(col='yellow3', fontsize=16, fontfamily = font2))
