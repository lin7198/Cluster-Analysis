######### Plot 1 ####################
iris_li <- read.csv("C:/Personal/80126902/Desktop/R_Shinny/App-2/data/play_data.csv")

attach(iris_li)
plot(Age, Var1, main="Plot1",
     xlab="Age ", ylab="Var1", pch=19)
abline(lm(Var1~Age), col="red") # regression line (y~x)
lines(lowess(Age,Var1), col="blue") # lowess line (x,y)


############# Plot 2 #################################
library(ggplot2)
library(dplyr)
iris_li$Gender <- as.factor(iris_li$Gender)
example_data <- iris_li %>% 
  group_by(Gender) %>% 
  summarise(mean_Distance = mean(Distance))

example_data %>% 
  ggplot(aes(x = Gender, y = mean_Distance)) +
  geom_col()