##################################################################################################
################################ LOADING DATASETS  ###############################################
df_test  <- read.csv('house_price_test.csv')
df_train <- read.csv('house_price_train.csv')

source('install_packages.R')
source('map_api_script.R')
source('hotspot_coords.R')
source('analyze_correlations_plots.R')
source('fct_plot_correlation.R')
source('fct_clusters_coord.R')
source('fct_haversine_dist.R')
source('fct_distance_from_hotspot.R')
source('fct_time_differences.R')
source('fct_turn_renovated_variable.R')


##################################################################################################
################################ EXPLORATION OF THE DATASET ######################################
str(df_train)
str(df_test)
summary(df_train)
summary(df_test)

# Familiriazing with the dataset.
ggpairs(data=df_train[,-c(1,2,3)])

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
load('TacomaMap.rda')

ggmap(TacomaMap) +
  labs(x = '', y = '') +
  theme(legend.position = 'none') +
  scale_color_hue() +
  scale_fill_hue() +
  geom_point(data = df_train, aes(x = long, y = lat), size = 0.3, color = 'blue')+
  geom_point(data = hotspots_coordinates, aes(x = long, y = lat), size = 5, color = 'red')

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

# Renovated or not
df_train <- turn_renovated_variable(df_train)
df_test  <- turn_renovated_variable(df_test)

yr_renov
grid.text(unit(0.5, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Summary of Year Renovated Variable",
          gp=gpar(col=color3, fontsize=16, fontfamily = font2))



# Changing some numeric to factor variables.
source('change_plot_ordinal_vars.R')

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


# Clustering coordinates with radius
clustering_coords <- clusters_coord(df_train[,c('id', 'lat', 'long')], 1, 'lat', 'long')
unique(clustering_coords[[1]][,2])

df_train <- merge(df_train, unique(clustering_coords[[1]][,c(1,2)]))
df_train <- df_train[order(df_train$id),]


# Detecting and fixing skewness. Acceptable test limits (-2,2)
for (i in colnames(numeric_data_train)){
  hist(numeric_data_train[,i], col = "tomato",main = i)
  skewness(numeric_data_train[,i])
}

# Scaling variables from 0 to 1 
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
for (i in colnames(numeric_data_train)){
  df_train[,i] <- range01(df_train[,i])
}

for (i in colnames(numeric_data_test)){
  df_test[,i] <- range01(df_test[,i])
}

# Detecting Outliers
