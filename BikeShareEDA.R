train <- vroom("BikeShare-IsaacR/bike-sharing-demand/train.csv")

plot_intro(train) 
plot_correlation(train)
plot_bar(train)
plot_histrograms(train)
plot_missing(train)
GGally::ggpairs(train)

temp <- ggplot(data=train, aes(x=temp, y=count)) +
  geom_point() +
  geom_smooth(se=FALSE)

weather <- ggplot(data=train, aes(x=weather, y=count)) +
  geom_col()

humidity <- ggplot(data=train, aes(x=humidity, y=count)) +
  geom_point() +
  geom_smooth(se=FALSE)

casual <- ggplot(data=train, aes(x=casual, y=count)) +
  geom_point() +
  geom_smooth(se=FALSE)

(temp + weather) / (humidity + casual)