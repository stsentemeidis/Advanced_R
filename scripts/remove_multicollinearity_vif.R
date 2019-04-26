##################################################################################################
######################## TREATING MULTICOLLINEARITY WITH VIF METHOD ##############################

model_vif <-lm(price~., data=df_train_full) 

lm_stats_vif <- summary(model_vif)

# After splitting our data in train and test we created our baseline model with all the variables. The results
# that we were given is an Adjusted R-Squared of 0.946 which is pretty good. However we have not tested yet our
# variables for multicollinearity. In order to do so, we are going to use the VIF (Variance Inflation Factor) method.

# As a general rule, if VIF is larger than 5, then multi collinearity is assumed to be high.
# As a result, each time we are going to calculate the VIF values, remove the biggest one,
# re-do the model until all the explanatory variables have a VIF below 5.
# Handle the above procedure with a WHILE loop.

#the linearly dependent variables
ld.vars <- attributes(alias(model_vif)$Complete)$dimnames[[1]]

#remove the linearly dependent variables variables
model_vif <-lm(price~.-sqft_basement-age-total_area-waterfront_1-view_4-condition_1-grade_3-yr_renovated_NO-month_03-year_2014-year_2015-new_YES, data=df_train_full) 

all_vifs <- car::vif(model_vif)

signif_all <- names(all_vifs)

while(any(all_vifs > 5)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the variable with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove this variable
  myForm <- as.formula(paste("price~ ", paste (signif_all, collapse=" + "), sep=""))  # design the new formula
  selectedMod <- lm(myForm, data=df_train_full)  # re-build model with new formula
  all_vifs <- car::vif(selectedMod)
}

print(all_vifs)
print(myForm)
