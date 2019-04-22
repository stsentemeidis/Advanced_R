##################################################################################################
################################ LOADING DATASETS  ###############################################
df_test  <- read.csv('house_price_test.csv')
df_train <- read.csv('house_price_train.csv')

##################################################################################################
################################ EXPLORATION OF THE DATASET ######################################
str(df_train)
str(df_test)
summary(df_train)
summary(df_test)

# Familiriazing with the dataset.
ggpairs(data=df_train[,-c(1,2,3)], )

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
numeric_data_train <- numeric_data_train[,-c(1,2)]
numeric_data_test <-as.data.frame(data.table(df_test[, sapply(df_test,is.numeric)]))
numeric_data_test <- numeric_data_test[,-1]

# Checking for missing values.
Amelia::missmap(numeric_data_train, col=c('black','white'))
Amelia::missmap(numeric_data_test , col=c('black','white'))


##################################################################################################
################################ EDA OF THE DATASET ##############################################

# Histogram of the response variable.

# Correlation matrix with the response variable.

# Analyze the 2 or 3 highest correlated variables.


##################################################################################################
################################ TRANSFORM THE DATASET WHEN NEEDEED ##############################

# Changing some numeric to factor variables.

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