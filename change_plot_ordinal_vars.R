##################################################################################################
################################ CHANGE SOME ORDINAL VARIABLES ###################################
ordinal_variables <- c('grade', 'view', 'condition', 'year', 'month')
for (i in ordinal_variables){
    df_train[, i] <- as.factor(df_train[,i])
    assign(paste0(i,'_factor_plot'), ggplot(df_train, aes_string(x=i)) +
                                       geom_bar(fill = color3)+
                                       theme_tufte(base_size = 5, ticks=F)+
                                       theme(plot.margin = unit(c(10,10,10,10),'pt'),
                                       axis.title=element_blank(),
                                       axis.text = element_text(colour = color2, size = 10, family = font2),
                                       axis.text.x = element_text(hjust = 1, size = 10, family = font2, angle=90),
                                       legend.position = 'None',
                                       plot.background = element_rect(fill = color1)))
  
}
##########################################################################################
years_price <- ggplot(df_train, aes(x=year, y=price)) +
  geom_bar(stat='summary', fun.y = "median", fill=color3)+
  scale_y_continuous(labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red")+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color2, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color1))

months_price <- ggplot(df_train, aes(x=month, y=price)) +
  geom_bar(stat='summary', fun.y = "median", fill=color3)+
  scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red")+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color2, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color1))


# 
# df_train$grade <- as.factor(df_train$grade)
# table(df_train$grade)
# df_train$view  <- as.factor(df_train$view)
# table(df_train$view)
# df_train$condition <- as.factor(df_train$condition)
# table(df_train$condition)
# df_train$year <- as.factor(df_train$year)
# table(df_train$year)
# df_train$month <- as.factor(df_train$month)
# table(df_train$month)
# 
# grade_f <- ggplot(df_train, aes(x=grade)) +
#   geom_bar(fill = color3)+
#   theme_tufte(base_size = 5, ticks=F)+
#   theme(plot.margin = unit(c(10,10,10,10),'pt'),
#         axis.title=element_blank(),
#         axis.text = element_text(colour = color2, size = 10, family = font2),
#         axis.text.x = element_text(hjust = 1, size = 10, family = font2, angle=90),
#         legend.position = 'None',
#         plot.background = element_rect(fill = color1))
# 
# view_f <- ggplot(df_train, aes(x=view)) +
#   geom_bar(fill = color3)+
#   theme_tufte(base_size = 5, ticks=F)+
#   theme(plot.margin = unit(c(10,10,10,10),'pt'),
#         axis.title=element_blank(),
#         axis.text = element_text(colour = color2, size = 10, family = font2),
#         axis.text.x = element_text(hjust = 1, size = 10, family = font2, angle=90),
#         legend.position = 'None',
#         plot.background = element_rect(fill = color1))
# 
# condition_f <- ggplot(df_train, aes(x=condition)) +
#   geom_bar(fill = color3)+
#   theme_tufte(base_size = 5, ticks=F)+
#   theme(plot.margin = unit(c(10,10,10,10),'pt'),
#         axis.title=element_blank(),
#         axis.text = element_text(colour = color2, size = 10, family = font2),
#         axis.text.x = element_text(hjust = 1, size = 10, family = font2, angle=90),
#         legend.position = 'None',
#         plot.background = element_rect(fill = color1))
# 
# year_f <- ggplot(df_train, aes(x=year)) +
#   geom_bar(fill = color3)+
#   theme_tufte(base_size = 5, ticks=F)+
#   theme(plot.margin = unit(c(10,10,10,10),'pt'),
#         axis.title=element_blank(),
#         axis.text = element_text(colour = color2, size = 10, family = font2),
#         axis.text.x = element_text(hjust = 1, size = 10, family = font2, angle=90),
#         legend.position = 'None',
#         plot.background = element_rect(fill = color1))
# 
# month_f <- ggplot(df_train, aes(x=month)) +
#   geom_bar(fill = color3)+
#   theme_tufte(base_size = 5, ticks=F)+
#   theme(plot.margin = unit(c(10,10,10,10),'pt'),
#         axis.title=element_blank(),
#         axis.text = element_text(colour = color2, size = 10, family = font2),
#         axis.text.x = element_text(hjust = 1, size = 10, family = font2, angle=90),
#         legend.position = 'None',
#         plot.background = element_rect(fill = color1))

