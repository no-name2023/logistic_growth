# Script to estimate the model parameters using a linear approximation

### Installing and loading the packages

install.packages("dplyr")
library(dplyr)

### Loading in the data 

growth_data <- read.csv("experiment1.csv")

# Case 1. K >> N0, t is small

### Here we are setting the carrying capacity (K) to be much bigger than the population size (N) in order to estimate the starting population size (y-intercept) and the gradient (r). We are able to do this because the model acts as an expoential. The line of code below is filtering the data and creating a natural logarithm of the values in the N column to make the model linear. 

data_subset1 <- growth_data %>% filter(t<1600) %>% mutate(N_log = log(N))

model1 <- lm(N_log ~ t, data_subset1)
summary(model1)

#Case 2. N(t) = K

data_subset2 <- growth_data %>% filter(t>1600)

model2 <- lm(N ~ 1, data_subset2)
summary(model2) 
