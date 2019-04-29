 # load caret and DT the cars data set
 require(caret); require(DT);

 # get all model names just as example
 m <- unique(modelLookup()[modelLookup()$forReg,c(1)])

 # fill variable m with the fast working models
 m <- c(
        "avNNet", "bagEarth",
       "bayesglm", "BstLm" , "bstTree",
        "glm", "glmboost", "glmnet",
        "lasso", "lm",
        "ridge", "svmPoly","xgbLinear", "xgbTree"
        )


 # load all packages (does not really work due to other dependencies)
 suppressPackageStartupMessages(ll <-lapply(m, require, character.only = TRUE))

 # define x and y for regression
 y <- df_train_full_lasso$price; x <- df_train_full_lasso[, -df_train_full_lasso$price];

#  load all libraries
 library(doParallel); cl <- makeCluster(detectCores()); registerDoParallel(cl)

 # use lapply/loop to run everything
 t2 <- lapply(m,function(i)
 {cat("----------------------------------------------------","\n");
  set.seed(123); cat(i," <- loaded\n");
   t2 <- train(y=y, x=x, (i), trControl = trainControl(method = "boot632"))
 }
 )

 saveRDS(t2, 't2_testing_results.rda')
 t2 <- readRDS('t2_testing_results.rda')
 
 # use lapply to print the results
 r2 <- lapply(1:length(t2), function(i)
 {cat(sprintf("%-20s",(m[i])));
   cat(round(t2[[i]]$results$Rsquared[which.min(t2[[i]]$results$RMSE)],4),"\t");
   cat(round(t2[[i]]$results$RMSE[which.min(t2[[i]]$results$RMSE)],4),"\t")
   cat(t2[[i]]$times$everything[3],"\n")
 }
 )

 # stop the parallel processing and register sequential front-end
 stopCluster(cl); registerDoSEQ();

 # preallocate data types
i = 1;
 MAX = length(t2);
 x1 <- character() # Name
 x2 <- numeric()   # R2
 x3 <- numeric()   # RMSE
 x4 <- numeric()   # time [s]
 x5 <- character() # long model name

 # fill data and check indexes and NA
for (i in 1:length(t2)) {
   x1[i] <- t2[[i]]$method
   x2[i] <- as.numeric(t2[[i]]$results$Rsquared[which.min(t2[[i]]$results$RMSE)])
   x3[i] <- as.numeric(t2[[i]]$results$RMSE[which.min(t2[[i]]$results$RMSE)])
   x4[i] <- as.numeric(t2[[i]]$times$everything[3])
   x5[i] <- t2[[i]]$modelInfo$label
}

# coerce to data frame
df1 <- data.frame(x1,x2,x3,x4,x5, stringsAsFactors=FALSE)

# print all results to R-GUI
df1
saveRDS(df1, 'model_testing_results.rda')
library(DT)
df1 <- readRDS('data/model_testing_results.rda')
# call web browser output with sortable column names
datatable(df1,  options = list(
  columnDefs = list(list(className = 'dt-left', targets = c(0,1,2,3,4,5))),
  pageLength = MAX,
  order = list(list(2, 'desc'))),
  colnames = c('Num', 'Name', 'R^2', 'RMSE', 'time [s]', 'Model name'),
  caption = paste('Regression results from caret models',Sys.time()),
  class = 'cell-border stripe')  %>% 	       
  formatRound('x2', 3) %>%  
  formatRound('x3', 6) %>%
  formatRound('x4', 3) %>%
  formatStyle(2,
              background = styleColorBar(x2, 'steelblue'),
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center'
  )
