##################################################################################################
######################## NEURAL NETS TRAINING - PREDICTION FUNCTION  #############################

df_train_nnet <- df_train_full[,names(df_train_full) %in% c(rfe_predictors,'price')]
df_test_nnet  <- df_test_full[,colnames(df_train_ranger)]

df_train_nnet_lasso <- df_train_full[,names(df_train_full) %in% c(rfe_predictors_lasso,'price')]
df_test_nnet_lasso  <- df_test_full[,colnames(df_train_ranger_lasso)]


# TRAINING CONTROL
my_control <-trainControl(method="cv", number=5)

# PARAMETER GRID SEARCH
#nnet_grid <- expand.grid(.decay = c(0.5, 0.1), 
#                      .size = c(5, 6, 7))

# TRAINING BASELINE
#nnet_caret <- train(x=df_train_full, y=df_train_full$price,
#                     method = "nnet", maxit = 1000, tuneGrid = nnet_grid, trace = F, linout = 1, trControl = my_control)

# BEST PARAMETERS
# nnet_caret$bestTune


# PREDICTING THE TEST SET
prediction_nnet <- predict(nnet_caret, df_test_nnet)
prediction_nnet <- exp(prediction_nnet)
