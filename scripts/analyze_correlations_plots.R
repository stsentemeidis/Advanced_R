##################################################################################################
################################ ANALYZE CORRELATIONS ############################################

# Histogram of the response variable.
p1<-ggplot(data=df_train, aes(x=price)) + 
  geom_histogram(col=color3,aes(fill=color3), fill = color3, binwidth = 10000) +
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color2, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color1)) + 
  scale_y_continuous(labels = comma)+
  scale_x_continuous(labels = comma)


# As you can see, the sale prices are right skewed. This was expected as few people can afford very expensive houses.
# I will keep this in mind, and take measures before modeling.

# Analyze the 2 or 3 highest correlated variables.
p2<-ggplot(data=df_train, aes(x=sqft_living, y=price)) + 
  geom_point(col=color3) + geom_smooth(method = "lm", se=FALSE, color=color2, aes(group=1)) +
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color2, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color1)) + 
  scale_y_continuous(labels = comma)+
  scale_x_continuous(labels = comma)

########################
p3<-ggplot(data=df_train, aes(x=sqft_above, y=sqft_living)) + 
  geom_point(col=color3) + geom_smooth(method = "lm", se=FALSE, color=color2, aes(group=1)) +
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color2, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color1)) + 
  scale_y_continuous(labels = comma)+
  scale_x_continuous(labels = comma)


