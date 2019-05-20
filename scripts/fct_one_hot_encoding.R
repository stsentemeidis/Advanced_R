##################################################################################################
################################ ONE HOT ENCODING AND CORRPLOT  ##################################

one_hot_encoding <- function (df){
  df <- fastDummies::dummy_cols(df)
  df <- df[,!names(df) %in% factor_variables]
  df <- df[,-c(1,2)]
  return(df)
}