##################################################################################################
######################## NEURAL NETS TRAINING - PREDICTION FUNCTION  #############################

# TRAINING CONTROL
my_control <-trainControl(method="cv", number=5)

# PARAMETER GRID SEARCH
nnet_grid <- expand.grid(.decay = c(0.5, 0.1), 
                       .size = c(5, 6, 7))

# TRAINING BASELINE
nnet_caret <- train(x=df_train_full, y=df_train_full$price,
                      method = "nnet", maxit = 1000, tuneGrid = nnet_grid, trace = F, linout = 1, trControl = my_control)

# BEST PARAMETERS
nnet_caret$bestTune

# PLOTTING MODEL
ggplot(nnet_caret)

# TRAINING THE BEST TUNED MODEL
nnet_caret_best <- train(x=df_train_full, y=df_train_full$price, method='nnet', trControl= my_control, tuneGrid = nnet_caret$bestTune) 

# PREDICTING THE TEST SET
prediction_nnet <- predict(nnet_caret_best, df_test_full)
prediction_xgb <- exp(prediction_xgb)