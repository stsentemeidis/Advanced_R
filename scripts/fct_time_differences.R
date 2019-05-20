##################################################################################################
################################ CALCULATE TIME DIFFERENCES ######################################

time_differences <- function (df) {
  
  df$age_when_sold <- 0
  df$age <- 0
  df$year <- as.numeric(df$year)
  
  for (i in 1:nrow(df)){
    df[i,'age_when_sold'] <- df[i,'year'] - df[i,'yr_built']
  }
  
  for (i in 1:nrow(df)){
    df[i,'age'] <- 2019 - df[i,'yr_built']
  }
  return(df)
}
