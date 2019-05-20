##################################################################################################
######################## LASSO REG TRAINING - PREDICTION FUNCTION  ###############################
# PARAMETER GRID SEARCH
lassoGrid <- expand.grid(alpha = 1, 
                         lambda = seq(0.001,0.1,
                         by = 0.0005)
                         )

# TRAINING CONTROL
my_control <-trainControl(method="cv", number=5)

df_lasso_train <- df_train_full[, -which(names(df_train_full) %in% c("price"))] # df_train_full
df_lasso_test  <- df_test_full[,  -which(names(df_test_full) %in% c("price"))]  # df_test_full
df_lasso_test  <- df_lasso_test[,colnames(df_lasso_train)]

# TRAINING BASELINE
lasso_caret <- train(x=df_lasso_train[,!names(df_lasso_train)%in%c('price')], y=df_train_full$price, method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 

# BEST PARAMETERS
lasso_caret$bestTune
min(lasso_caret$results$RMSE)

# VARIABLE IMPORTANCE | VARIABLES SELECTED
lassoVarImp <- varImp(lasso_caret,scale=F)
lassoImportance <- lassoVarImp$importance

varsSelected    <- which(lassoImportance$Overall!=0)
varsNotSelected <- which(lassoImportance$Overall==0)

cat('Lasso uses', length(varsSelected), 'variables in its model, and did not select', length(varsNotSelected), 'variables.')

# So lasso did what it is supposed to do: it seems to have dealt with multicolinearity well by not using about 
# 45% of the available variables in the model.

# TRAINING THE BEST TUNED MODEL
lasso_caret_best <- train(x=df_lasso_train[,varsSelected], y=df_train_full$price, method='glmnet', trControl= my_control, tuneGrid=lasso_caret$bestTune) 

# PREDICTING THE TEST SET
prediction_lasso <- predict(lasso_caret_best, df_lasso_test[,varsSelected])
prediction_lasso <- exp(prediction_lasso) #need to reverse the log to the real values

