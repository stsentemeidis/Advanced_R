
options(digits = 4)
model_results <- data.frame(
  LASSO        = min(lasso_caret_best$results$RMSE),
  XGB          = min(xgb_caret$results$RMSE),
  XGB_LASSO    = min(xgb_caret_lasso$results$RMSE),
  RF           = min(ranger_rf$results$RMSE),
  RF_LASSO     = min(ranger_rf_lasso$results$RMSE)
)
## FINDING THE HOLD OUT SET JUST TO SEE AN ACTUAL SUBMISSION SCORE NOT FOR TRAINING!!!
df_real  <- read.csv('kc_house_data.csv')
df_real  <- as.data.table(df_real[,c('id','price')])
df_real <- df_real %>% 
  group_by(id) %>% 
  summarise(price = mean(price))

df_test_id <- as.data.table(df_test[,'id'])

df_test_real <- merge(df_test_id, df_real, by.x = "V1", by.y = "id")
colnames(df_test_real) <- c('id','price')

## GATHERING PREDICTIONS
predictions_df <- as.data.frame(df_test_id$V1)
predictions_df <- cbind(predictions_df, prediction_lasso)
predictions_df <- cbind(predictions_df, prediction_rf)
predictions_df <- cbind(predictions_df, prediction_rf_lasso)
predictions_df <- cbind(predictions_df, prediction_xgb)
predictions_df <- cbind(predictions_df, prediction_xgb_lasso)

colnames(predictions_df) <- c('id','LASSO','RF','RF_LASSO','XGB','XGB_LASSO')
predictions_df <- merge(as.data.table(predictions_df),as.data.table(df_test_real), by.x='id',by.y='id')

# Average of Three
predictions_df$MEAN_XGB_LASSO_RF <- rowMeans(subset(predictions_df, select = c(XGB, LASSO, RF)), na.rm = TRUE)

# Weighted Average
weight <- c(2.5,1,0.5)
predictions_df$MEAN_XGB_LASSO_RF_weighted <- apply(subset(predictions_df, select = c(XGB, LASSO, RF)), 1, function(d) weighted.mean(d, weight, na.rm = TRUE))

## NEW METRICS
model_results_test <- data.frame(matrix(c(
  RMSE(predictions_df$LASSO,predictions_df$price),
  RMSE(predictions_df$RF,predictions_df$price),
  RMSE(predictions_df$RF_LASSO,predictions_df$price),
  RMSE(predictions_df$XGB,predictions_df$price),
  RMSE(predictions_df$XGB_LASSO,predictions_df$price),
  RMSE(predictions_df$MEAN_XGB_LASSO_RF,predictions_df$price),
  RMSE(predictions_df$MEAN_XGB_LASSO_RF_weighted,predictions_df$price),
  
  MAE(predictions_df$LASSO,predictions_df$price),
  MAE(predictions_df$RF,predictions_df$price),
  MAE(predictions_df$RF_LASSO,predictions_df$price),
  MAE(predictions_df$XGB,predictions_df$price),
  MAE(predictions_df$XGB_LASSO,predictions_df$price),
  MAE(predictions_df$MEAN_XGB_LASSO_RF,predictions_df$price),
  MAE(predictions_df$MEAN_XGB_LASSO_RF_weighted,predictions_df$price),
  
  MAPE(predictions_df$LASSO,predictions_df$price),
  MAPE(predictions_df$RF,predictions_df$price),
  MAPE(predictions_df$RF_LASSO,predictions_df$price),
  MAPE(predictions_df$XGB,predictions_df$price),
  MAPE(predictions_df$XGB_LASSO,predictions_df$price),
  MAPE(predictions_df$MEAN_XGB_LASSO_RF,predictions_df$price),
  MAPE(predictions_df$MEAN_XGB_LASSO_RF_weighted,predictions_df$price)),
  
  nrow = 7,
  ncol = 3))

colnames(model_results_test) <- c('RMSE','MAE','MAPE')
rownames(model_results_test) <- c('LASSO','RF','RF_LASSO','XGB','XGB_LASSO','AVG_RF_XGB_LASSO','W_AVG_RF_XGB_LASSO')
