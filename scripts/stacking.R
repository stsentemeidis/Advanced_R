##################################################################################################
######################## STACKING TRAINING - PREDICTION FUNCTION  ################################

df_train_stacking <- df_train_full[,names(df_train_full) %in% c(rfe_predictors,'price')]
df_test_stacking  <- df_test_full[,colnames(df_train_stacking)]
control <- trainControl(method="repeatedcv", number=5, savePredictions=TRUE,verboseIter = T,
                        index = createFolds(df_train_stacking$price, 5))
# create initial models
# algorithmList <- c('xgbLinear', 'glmnet', 'ranger')
#library(caretEnsemble)

# model_list <- caretList(x=df_train_stacking[,!names(df_train_stacking)%in%c('price')],
#                          y=df_train_stacking$price,
#                          trControl = control,
#                          tuneList=list(
#                            xgbT = caretModelSpec(method="xgbTree", tuneGrid=data.frame(nrounds = 1000,
#                                                                                          max_depth = 5,
#                                                                                          gamma = 0,
#                                                                                          eta = 0.05,
#                                                                                          colsample_bytree = 1,
#                                                                                          min_child_weight = 3,
#                                                                                          subsample = 1)),
#                            rf  = caretModelSpec(method="ranger",    tuneGrid=data.frame(mtry = 7,
#                                                                                         splitrule = "variance",
#                                                                                         min.node.size = 5)),
#                            lasso  = caretModelSpec(method="glmnet",    tuneGrid=data.frame(alpha=c(0.05,0.1,0.5),
#                                                                                            lambda=c(0.001,0.1,0.5)))),
#                          preProcess = c('center','scale'))
# 
# saveRDS(model_list, 'data/stacking_models.rda')
model_list <- readRDS('data/stacking_models.rda')

results <- resamples(model_list)
summary(results)
dotplot(results)
# correlation between results
modelCor(results)
splom(results)



#######################################################################################################################
stackControl <- trainControl(method="repeatedcv", number=10, savePredictions=TRUE, verboseIter = T)

# stacking using xgbTree
# stack.xgb    <- caretStack(model_list, method="xgbTree", metric="RMSE", trControl=stackControl)
# saveRDS(stack.xgb, 'data/stacked_xgb_model.rda')
stack.xgb <- readRDS('data/stacked_xgb_model.rda')
print(stack.xgb)

#######################################################################################################################
# stacking using random forest
# stack.rf     <- caretStack(model_list, method="ranger", metric="RMSE", trControl=stackControl)
# saveRDS(stack.rf, 'data/stacked_rf_model.rda')
stack.rf <- readRDS('data/stacked_rf_model.rda')
print(stack.rf)

#######################################################################################################################
#stacking using random forest
# stack.glm     <- caretStack(model_list, method="glmnet", metric="RMSE", trControl=stackControl)
# saveRDS(stack.glm, 'data/stacked_glm_model.rda')
stack.glm <- readRDS('data/stacked_glm_model.rda')
print(stack.glm)

#######################################################################################################################
# PREDICTING THE TEST SET
prediction_stacked_xgb  <- predict.caretStack(object= stack.xgb, newdata = df_test_stacking)
prediction_stacked_rf   <- predict.caretStack(stack.rf,  df_test_stacking)
prediction_stacked_glm  <- predict.caretStack(object=stack.glm, newdata=df_test_stacking)

#######################################################################################################################3
# options(digits = 3)
# model_results <- data.frame(
#   LASSO = min(model_list$lasso$results$RMSE),
#   SVM   = min(model_list$svmRadial$results$RMSE),
#   RF    = min(model_list$rf$results$RMSE),
#   XGBT  = min(model_list$xgbTree$results$RMSE),
#   XGBL  = min(model_list$xgb$results$RMSE),
#   STACK_XGB = min(stack.xgb$error$RMSE),
#   STACK_RF  = min(stack.rf$error$RMSE),
#   STACK_GLM = min(stack.glm$error$RMSE)
# )
# print(model_results)
