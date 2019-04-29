# rfe feature selection
# ensure the results are repeatable
set.seed(123)
# load the library
library(mlbench)
library(caret)

# define the control using a random forest selection function
rfe_control <- rfeControl(functions=rfFuncs, method="cv", number=10, saveDetails = T, returnResamp = 'all')
# run the RFE algorithm
rfe_results <- rfe(x=df_train_full_lasso[, -df_train_full_lasso$price], y=df_train_full_lasso$price, sizes=c(1:8,10,15,20,25), rfeControl=rfe_control)
# summarize the results
print(rfe_results)
# list the chosen features
predictors(rfe_results)
# plot the results
plot(results, type=c("g", "o"))
