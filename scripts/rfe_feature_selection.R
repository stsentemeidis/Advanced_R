# rfe feature selection
devtools::install_github('topepo/caret/pkg/caret')
# ensure the results are repeatable
set.seed(123)
# load the library
library(mlbench)
library(caret)

# define the control using a random forest selection function
rfe_control <-expand.grid(
  .mtry = c(2:12),
  .splitrule = "variance",
  .min.node.size = c(5,10,15,20,5)
)
my_control <-trainControl(method="cv", number=5, verboseIter = T)

# run the RFE algorithm
# rfe_results <- train(price~.,
#                      data=df_train_full, method = "ranger", 
#                      tuneGrid = rfe_control,
#                      num.trees = 1000,
#                      importance = "permutation", 
#                      trControl=my_control)
# saveRDS(rfe_results, 'rfe_results_full.rda')
rfe_results <- readRDS('data/rfe_results_full.rda')
####################
# rfe_results_lasso <- train(price~.,
#                      data=df_train_full_lasso, method = "ranger",
#                      tuneGrid = rfe_control,
#                      num.trees = 800,
#                      importance = "permutation",
#                      trControl=my_control)
# saveRDS(rfe_results_lasso, 'rfe_results_lasso.rda')
rfe_results_lasso <- readRDS('data/rfe_results_lasso.rda')
# summarize the results
# print(rfe_results)
# # list the chosen features
# predictors(rfe_results)
# varImp(rfe_results)
# plot the results
rfe_grid_plot <- ggplot(rfe_results)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        legend.background = element_rect(fill = 'black'),
        legend.position = c(0.85,0.3),
        legend.title=element_text(size = 7, colour = 'white'),
        legend.text = element_text(colour = 'white', size = 7),
        axis.title=element_blank(),
        axis.text = element_text(colour = color2, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 8, family = font2),
        axis.text.y  = element_text(hjust = 1, size = 8, family = font2),
        plot.background = element_rect(fill = color1))
rfe_grid_plot

rfe_plot <- ggplot(varImp(rfe_results), aes(x=reorder(predictors(rfe_results),varImp(rfe_results)), y=varImp(rfe_results),fill=varImp(rfe_results)))+ 
  geom_bar(stat="identity", fill=color3)+ coord_flip()+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color2, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 8, family = font2),
        axis.text.y  = element_text(hjust = 1, size = 8, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color1))

############################################################################################################################################
rfe_grid_plot_lasso <- ggplot(rfe_results_lasso)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        legend.background = element_rect(fill = 'black'),
        legend.position = c(0.85,0.3),
        legend.title=element_text(size = 7, colour = 'white'),
        legend.text = element_text(colour = 'white', size = 7),
        axis.title=element_blank(),
        axis.text = element_text(colour = color2, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 8, family = font2),
        axis.text.y  = element_text(hjust = 1, size = 8, family = font2),
        plot.background = element_rect(fill = color1))
rfe_grid_plot_lasso

rfe_plot_lasso <- ggplot(varImp(rfe_results_lasso), aes(x=reorder(predictors(rfe_results_lasso),varImp(rfe_results_lasso)), y=varImp(rfe_results_lasso),fill=varImp(rfe_results_lasso)))+ 
  geom_bar(stat="identity", fill=color3)+ coord_flip()+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color2, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 8, family = font2),
        axis.text.y  = element_text(hjust = 1, size = 8, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color1))
rfe_plot_lasso
