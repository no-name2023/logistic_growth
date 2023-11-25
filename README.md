# Script to estimate the model parameters using a linear approximation

### Installing and loading the packages

install.packages("dplyr")
library(dplyr)

### Loading in the data 

growth_data <- read.csv("experiment1.csv")

# Case 1. K >> N0, t is small

### Here I am setting the carrying capacity (K) to be much bigger than the population size (N) in order to estimate the starting population size (y-intercept) and the gradient (r). I was able to do this because the model acts as an expoential. The line of code below is filtering the data and creating a natural logarithm of the values in the N column to make the model linear. The model says that the y-intercept (No) = 6.903e+00 and the gradient (r) = 9.990e-03.  

data_subset1 <- growth_data %>% filter(t<1600) %>% mutate(N_log = log(N))

model1 <- lm(N_log ~ t, data_subset1)

summary(model1)

# Case 2. N(t) = K

### Here I am looking at the carying capacity because at N(t) = K the population size is equal to the carying capacity. In the lines of code below I am creating another subset of data where the values of t are greater than 2000 and then again fitting a linear model and looking at the intercept and gradient, as well as other information about the model. From the summary the model says K = 5.979e+10. 

data_subset2 <- growth_data %>% filter(t>2000)

model2 <- lm(N ~ 1, data_subset2)
summary(model2) 

# Script to plot the logistic growth data

### Below I am reading in the CSV file, installing and loading the ggplot2 package. 

growth_data <- read.csv("experiment1.csv")

install.packages("ggplot2")
library(ggplot2)

### Here I am creating a scatter plot with the variable N and t. 

ggplot(aes(t,N), data = growth_data) +
  
  geom_point() +
  
  xlab("t") +
  
  ylab("y") +
  
  theme_bw()

### Below I am performing a logarithmic transformation to help visualize the data more clearly. 

ggplot(aes(t,N), data = growth_data) +
  
  geom_point() +
  
  xlab("t") +
  
  ylab("y") +
  
  scale_y_continuous(trans='log10')
