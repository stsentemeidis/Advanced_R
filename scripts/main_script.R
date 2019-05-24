##################################################################################################
################################ LOADING DATASETS  ###############################################
df_test  <- read.csv('data/house_price_test.csv')
df_train <- read.csv('data/house_price_train.csv')

source('scripts/install_packages.R')
source('scripts/hotspot_coords.R')
source('scripts/analyze_correlations_plots.R')
source('scripts/rfe_feature_selection.R')
source('scripts/fct_plot_correlation.R')
source('scripts/fct_clusters_coord.R')
source('scripts/fct_cluster_coord_dist.R')
source('scripts/fct_haversine_dist.R')
source('scripts/fct_distance_from_hotspot.R')
source('scripts/fct_renovated_fixed_sft15.R')
source('scripts/fct_time_differences.R')
source('scripts/fct_turn_renovated_variable.R')
source('scripts/fct_one_hot_encoding.R')

##################################################################################################
################################ EXPLORATION OF THE DATASET ######################################
str(df_train)
str(df_test)
summary(df_train)
summary(df_test)

# Create column price
df_test$price <- 1000

# Keeping numeric as the common arithmetic type for the data columns.
for (i in colnames(df_train)){
  if (class(df_train[,i]) == 'integer'){
    df_train[,i] <- as.numeric(df_train[,i])
  }
}

for (i in colnames(df_test)){
  if (class(df_test[,i]) == 'integer'){
    df_test[,i] <- as.numeric(df_test[,i])
  }
}

# Converting the date column from factor to date type.
df_train[,'date'] <- as.Date(df_train[,'date'], format = "%m/%d/%Y")
df_test[,'date']  <- as.Date(df_test[,'date'],  format = "%m/%d/%Y")

# Subsetting numerical data.
numeric_data_train<-as.data.frame(data.table(df_train[, sapply(df_train,is.numeric)]))
numeric_data_train <- numeric_data_train[,-c(1)]
numeric_data_test <-as.data.frame(data.table(df_test[, sapply(df_test,is.numeric)]))
numeric_data_test <- numeric_data_test[,-1]

# Checking for missing values.
Amelia::missmap(numeric_data_train, col=c('black','white'))
Amelia::missmap(numeric_data_test , col=c('black','white'))


##################################################################################################
################################ EDA OF THE DATASET ##############################################
# Pairplot.
# ggpairs(data=df_train[,-c(1,2,3)])

# Correlation matrix with the response variable.
plot_correlation(numeric_data_train)

p1
grid.text(unit(0.7, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Histogram of Prices",
          gp=gpar(col=color3, fontsize=16, fontfamily = font2))

p2
grid.text(unit(0.1, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Relationship of Sqft_living | Price",
          gp=gpar(col=color3, fontsize=16, fontfamily = font2))

p3
grid.text(unit(0.1, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Relationship of Sqft_living | Sqft_above",
          gp=gpar(col=color3, fontsize=16, fontfamily = font2))
##################################################################################################
################################ MAP OF THE LOCATIONS OF THE HOUSES ##############################
load('data/TacomaMap_terrain.rda')
load('data/TacomaMap_roadmap.rda')
load('data/TacomaMap_satellite.rda')
source('scripts/map_api_script.R')

map1
map2
map3
##################################################################################################
################################ TRANSFORM THE DATASET WHEN NEEDEED ##############################

# Extracting date and month.
df_train$month <- format(as.Date(df_train$date), "%m")
df_test$month  <- format(as.Date(df_test$date), "%m")
df_train$year  <- format(as.Date(df_train$date), "%Y")
df_test$year   <- format(as.Date(df_test$date), "%Y")

# Time differences between
df_train <- time_differences(df_train)
df_test  <- time_differences(df_test)

# Reovated fixed
df_train <- renovated_fixed_sft15(df_train)
df_test  <- renovated_fixed_sft15(df_test)
yr_renov
grid.text(unit(0.5, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Summary of Year Renovated Variable",
          gp=gpar(col=color3, fontsize=16, fontfamily = font2))



# Changing some numeric to factor variables.
source('scripts/change_plot_ordinal_vars.R')

grid.arrange(grade_factor_plot, condition_factor_plot, view_factor_plot, nrow=1, ncol=3)
grid.text(unit(0.015, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Grade Variable",
          gp=gpar(col=color3, fontsize=10, fontfamily = font2))
grid.text(unit(0.33, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Condition Variable",
          gp=gpar(col=color3, fontsize=10, fontfamily = font2))
grid.text(unit(0.8, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="View Variable",
          gp=gpar(col=color3, fontsize=10, fontfamily = font2))

grid.arrange(year_factor_plot, month_factor_plot, nrow=1, ncol=2)
grid.text(unit(0.32, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Year Variable",
          gp=gpar(col=color3, fontsize=10, fontfamily = font2))
grid.text(unit(0.81, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Month Variable",
          gp=gpar(col=color3, fontsize=10, fontfamily = font2))

grid.arrange(years_price, months_price, nrow=1, ncol=2)
grid.text(unit(0.2, 'npc'), unit(0.97,"npc"), check.overlap = T,just = "left",
          label="Year | Price Distribution",
          gp=gpar(col=color3, fontsize=10, fontfamily = font2))
grid.text(unit(0.7, 'npc'), unit(0.97,"npc"), check.overlap = T,just = "left",
          label="Month | Price Distribution",
          gp=gpar(col=color3, fontsize=10, fontfamily = font2))

# Distances from hot spots (airport, attractions)
df_train <- distance_from_hotspot(df_train)
df_test  <- distance_from_hotspot(df_test)

# Clustering coordinates 
source('scripts/finding_optimal_distance.R')
df_coords <- rbind(df_train[,c('long','lat','id')], df_test[,c('long', 'lat','id')])
opt_dist_plot
grid.text(unit(0.5, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Optimal Distance for Hierarchical Clustering",
          gp=gpar(col='yellow3', fontsize=16, fontfamily = font2))

# cluster_coords_ids <- cluster_coords_hier(df_coords, 2200) # distance in meters
# cluster_coords_ids_df <- as.data.frame(cluster_coords_ids)
# length(unique(cluster_coords_ids_df$clust))                # number of clusters
# saveRDS(cluster_coords_ids_df, 'data/cluster_coords_ids_df.rds')

cluster_coords_ids_df <- readRDS('data/cluster_coords_ids_df.rds')

df_train_clusters <- cluster_coords_ids_df[1:17277,]
df_test_clusters  <- cluster_coords_ids_df[17278:21597,]

df_train <- cbind(df_train, df_train_clusters$clust )
setnames(df_train, old = 'df_train_clusters$clust', new = 'cluster')
df_test  <- cbind(df_test,  df_test_clusters$clust )
setnames(df_test, old = 'df_test_clusters$clust', new = 'cluster')

# Creating Variable if hte house is new or not 
df_train$new <- ifelse(df_train$year == df_train$yr_built, 'YES', 'NO')
df_test$new  <- ifelse(df_test$year  == df_test$yr_built, 'YES', 'NO')

# Creating total size of area of the house
df_train$total_area <- df_train$sqft_living + df_train$sqft_basement
df_test$total_area  <- df_test$sqft_living  + df_test$sqft_basement

##################################################################################################
############## CONVERTING CATEGORICAL | ORDINAL COLUMNS TO THE CORRECT DATA TYPE #################

factor_variables <- c('waterfront', 'view', 'condition', 'grade', 'yr_renovated', 
                      'month', 'year', 'new')

for (i in factor_variables){
  df_train[,i] <- as.character(df_train[,i])
}
for (i in factor_variables){
  df_test[,i] <- as.character(df_test[,i])
}

# Plot correlation matrix of all the variables that have been added to our initial dataset.
numeric_data_train_full<-as.data.frame(data.table(df_train[, sapply(df_train,is.numeric)]))
dev.off()
plot_correlation(numeric_data_train_full, size_p = 0.37, size_t = 0.7)

##################################################################################################
##################################### ONE HOT ENCODING ###########################################
df_train_full <- one_hot_encoding(df_train)
corr_full <- cor(df_train_full)
dev.off()
plot_correlation(df_train_full, size_p = 0.3, size_t = 0.6)
df_test_full  <- one_hot_encoding(df_test)
df_test_full$grade_3 <- 0

##################################################################################################
##################################### SCALING 0-1 ################################################
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

for (i in colnames(df_train_full[, -which(names(df_train_full) %in% c("price"))])){
  df_train_full[,i] <- range01(df_train_full[,i])
}

for (i in colnames(df_test_full[, -which(names(df_train_full) %in% c("price"))])){
  df_test_full[,i] <- range01(df_test_full[,i])
}

df_test_full$grade_3 <- 0

df_train_full$price <- log(df_train_full$price)
df_test_full$price  <- log(df_test_full$price)

##################################################################################################
######################### REMOVE MULTICOLLINEARITY WITH LASSO | VIF ##############################
source('scripts/remove_multicollinearity_vif.R')

df_train_full_vif <- df_train_full[,names(df_train_full) %in% signif_all]
df_test_full_vif  <- df_test_full[,names(df_test_full) %in% signif_all]

source('scripts/lasso_reg.R')

df_train_full_lasso <- df_train_full[, varsSelected]
df_test_full_lasso  <- df_test_full[,varsSelected]
numeric_data_train_full_lasso<-as.data.frame(data.table(df_train_full_lasso[, sapply(df_train_full_lasso,is.numeric)]))
dev.off()
plot_correlation(numeric_data_train_full_lasso, size_p = 0.5, size_t = 0.7)

df_test_full_lasso <- df_test_full_lasso[, names(df_test_full_lasso) %in% colnames(df_train_full_lasso)]


##################################################################################################
##################################### RECURSIVE FEATURE ELIMINATION ##############################
source('scripts/rfe_feature_selection.R')
rfe_predictors <- c('lat','sqft_living','distance_from_Seattle_college','distance_from_uni_of_washington','distance_From_airport',
                    'sqft_above','total_area','distance_from_ikea','sqft_living15','distance_from_capitol_hill_station','distance_from_lake',
                    'distance_from_woodland_zoo','distance_from_discovery_park','distance_from_lake_wilderness','distance_from_king_street_station',
                    'distance_from_us_football_stadium','distance_from_fun_center','distance_from_walmart','distance_from_baseball_stadium',
                    'distance_from_sodo_station','distance_from_lincol_park','distance_from_golf_course','long','distance_from_municipal_airport',
                    'grade_7','bathrooms','zipcode','sqft_lot15','sqft_lot','yr_built','age_when_sold','age','grade_9','grade_8','view_0','floors',
                    'grade_6','sqft_basement','grade_10','bedrooms','grade_11','cluster','view_4','waterfront_1','waterfront_0')

rfe_predictors_lasso <- c('lat','sqft_living','distance_from_uni_of_washington','distance_From_airport', 'distance_from_us_football_stadium',
                          'distance_from_ikea','distance_from_discovery_park','distance_from_lake_wilderness', 'distance_from_sodo_station',
                          'sqft_living15','total_area','distance_from_walmart','long','grade_7','bathrooms','zipcode','sqft_lot','sqft_lot15',
                          'yr_built','grade_8','floors','grade_6','grade_10','bedrooms','grade_11','view_4','waterfront1','condition_4')


##################################################################################################
########################################## MODELLING #############################################
source('scripts/xgboost.R')
source('scripts/ranger.R')

##################################################################################################
################################# EVALUATING AND SUBMITTING ######################################
source('scripts/final_metrics.R')























# # Clustering coordinates with radius
# clustering_coords <- clusters_coord(df_train[,c('id', 'lat', 'long')], 1, 'lat', 'long')
# unique(clustering_coords[[1]][,2])
# 
# df_train <- merge(df_train, unique(clustering_coords[[1]][,c(1,2)]))
# df_train <- df_train[order(df_train$id),]

# Clustering coordinates with distance
# df_train$coordinates <- paste0('(', df_train$long, ', ', df_train$lat, ')')
# df_test$coordinates  <- paste0('(', df_test$long, ', ', df_test$lat, ')')


# TESTING DIFFERENT GENERAL MODEL APPROACHES TO SEE THE PERFORMANCE OF ALGORITHMS OF INTEREST
# source('scripts/model_testing.R')
# # Reorder columns in df_test_full_clean
# colnames_df_test_full_clean <- c('sqft_living','sqft_basement','lat','age_when_sold','age','distance_from_airport',
#                                  'distance_from_ikea','distance_from_fun_center','distance_from_walmart','distance_from_seattle_college',
#                                  'distance_from_woodland_zoo','distance_from_lake','distance_from_us_football_stadium',
#                                  'distance_from_discovery_park','distance_from_lincol_park','distance_from_baseball_stadium',
#                                  'distance_from_king_street_station','distance_from_sodo_station','distance_from_capitol_hill_station',
#                                  'distance_from_golf_course','distance_from_lake_wilderness','total_area','waterfront_1','view_0','view_4',
#                                  'condition_3','condition_1','grade_7','grade_3','yr_renovated_NO','month_03','year_2014','year_2015','new_YES')
# 
# df_test_full_clean  <- df_test_full_clean[,colnames_df_test_full_clean]
