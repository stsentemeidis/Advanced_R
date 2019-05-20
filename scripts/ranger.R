##################################################################################################
######################## XGBOOST TRAINING - PREDICTION FUNCTION  #################################
df_train_ranger <- df_train_full[,names(df_train_full) %in% c(rfe_predictors,'price')]
df_test_ranger  <- df_test_full[,colnames(df_train_ranger)]

df_train_ranger_lasso <- df_train_full[,names(df_train_full) %in% c(rfe_predictors_lasso,'price')]
df_test_ranger_lasso  <- df_test_full[,colnames(df_train_ranger_lasso)]

# PARAMETER GRID SEARCH
ranger_control <-expand.grid(
  .mtry = 7,
  .splitrule = "variance",
  .min.node.size = 5
)

my_control <-trainControl(method="cv", number=5, verboseIter = T)

# run the RFE algorithm
# ranger_rf <- train(price~.,
#                      data=df_train_ranger, method = "ranger",
#                      tuneGrid = ranger_control,
#                      num.trees = 1000,
#                      importance = "permutation",
#                      trControl=my_control)
# 
# ranger_rf_lasso <- train(price~.,
#                    data=df_train_ranger_lasso, method = "ranger",
#                    tuneGrid = ranger_control,
#                    num.trees = 1000,
#                    importance = "permutation",
#                    trControl=my_control)

# saveRDS(ranger_rf, 'ranger_rf_model.rda')
# saveRDS(ranger_rf_lasso, 'data/ranger_rf_lasso_model.rda')
ranger_rf <- readRDS('data/ranger_rf_model.rda')
ranger_rf_lasso <- readRDS('data/ranger_rf_lasso_model.rda')
# predictions
prediction_rf <- predict(ranger_rf, df_test_ranger)
prediction_rf <- exp(prediction_rf)

prediction_rf_lasso <- predict(ranger_rf_lasso, df_test_ranger_lasso)
prediction_rf_lasso <- exp(prediction_rf_lasso)
