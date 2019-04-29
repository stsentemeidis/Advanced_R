##################################################################################################
################################ CHANGE RENOVATED VARIABLE #######################################
turn_renovated_variable <- function (df) {
  for (i in 1:nrow(df)){
    if (df[i,'yr_renovated'] == 0){
      df[i,'yr_renovated'] <- 'NO'
    }
    else{
      df[i,'yr_renovated'] <- 'YES'
    }
  }
  return(df)
}


test_df <- df_train
test_df$yr_renovated <- as.factor(test_df$yr_renovated)
table(test_df$yr_renovated)

yr_renov <- ggplot(test_df, aes(x=yr_renovated)) +
  geom_bar(fill = color3)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color2, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2, angle=90),
        legend.position = 'None',
        plot.background = element_rect(fill = color1))