##################################################################################################
################################ CLUSTER POINTS BASED ON DISTANCES ###############################
library(sp)
library(rgdal)
library(geosphere)

cluster_coords_hier <- function(df, dist){
  # example data from the thread
  x <- df$long
  y <- df$lat
  
  # convert data to a SpatialPointsDataFrame object
  xy <- SpatialPointsDataFrame(
    matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  # use the distm function to generate a geodesic distance matrix in meters
  mdist <- distm(xy)
  
  # cluster all points using a hierarchical clustering approach
  hc <- hclust(as.dist(mdist), method="complete")
  
  # define the distance threshold, in this case 40 m
  d=dist
  
  # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
  xy$clust <- cutree(hc, h=d)
  return(xy)
}