##################################################################################################
################################ RENOVATED BASED ON DIFFERENCE SQFT15  ###########################
renovated_fixed_sft15 <- function(df){
  for (i in 1:nrow(df)){
    if((df[i,'sqft_living'] == df[i,'sqft_living15']) & (df[i,'sqft_lot'] == df[i,'sqft_lot15'])){
      df[i,'yr_renovated'] <- 'NO'
    }
    else{
      df[i,'yr_renovated'] <- 'YES'
    }
  }
  return(df)
}