##################################################################################################
######################## XGBOOST TRAINING - PREDICTION FUNCTION  #################################
  # PARAMETER GRID SEARCH
  xgb_grid = expand.grid(
    nrounds = 1000,
    eta = c(0.1, 0.05, 0.01),
    max_depth = c(2, 3, 4, 5, 6),
    gamma = 0,
    colsample_bytree=1,
    min_child_weight=c(1, 2, 3, 4 ,5),
    subsample=1
  )
  
  # TRAINING CONTROL
  my_control <-trainControl(method="cv", number=5)
  
  # TRAINING BASELINE
  xgb_caret <- train(x=df_train_full_lasso[, -df_train_full_lasso$price], y=df_train_full_lasso$price, method='xgbTree', trControl= my_control, tuneGrid = xgb_grid) 
  
  # BEST PARAMETERS
  xgb_caret$bestTune
  
  # PLOTTING MODEL
  ggplot(xgb_caret)
  
  # TRAINING THE BEST TUNED MODEL
  xgb_caret_best <- train(x=df_train_full_lasso, y=df_train_full$price, method='xgbTree', trControl= my_control, tuneGrid = xgb_caret$bestTune) 
  
  # PREDICTING THE TEST SET
  prediction_xgb <- predict(xgb_caret_best, df_test_full_lasso)
  prediction_xgb <- exp(prediction_xgb)
