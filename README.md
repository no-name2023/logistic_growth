# Script to estimate the model parameters using a linear approximation

### Installing and loading the packages

install.packages("dplyr")
library(dplyr)

### Loading in the data 

growth_data <- read.csv("experiment1.csv")

# Case 1. K >> N0, t is small

#### Here I am setting the carrying capacity (K) to be much bigger than the population size (N) in order to estimate the starting population size (y-intercept) and the gradient (r). I was able to do this because the model acts as an expoential. The line of code below is filtering the data and creating a natural logarithm of the values in the N column to make the model linear. The model says that the y-intercept (N0) = 6.903e+00 and the gradient (r) = 9.990e-03.  

data_subset1 <- growth_data %>% filter(t<1600) %>% mutate(N_log = log(N))

model1 <- lm(N_log ~ t, data_subset1)

summary(model1)

# Case 2. N(t) = K

#### Here I am looking at the carying capacity because at N(t) = K the population size is equal to the carying capacity. In the lines of code below I am creating another subset of data where the values of t are greater than 2000 and then again fitting a linear model and looking at the intercept and gradient, as well as other information about the model. From the summary the model says K = 5.979e+10. 

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

# Script to plot data and model

### Loading in the data.  

growth_data <- read.csv("experiment1.csv")

### Here I am defining the logistic growth model. 

logistic_fun <- function(t) {
  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  
  return(N)
  
}

### Here I am defining the parameters of the logistic growth model defined above. 

N0 <- 6.903e+00 #
  
r <- 9.990e-03 #
  
K <- 5.979e+10 #

### Here I am plotting the logistic growth model and comparing it agianst the data. 

ggplot(aes(t,N), data = growth_data) +
  
  geom_function(fun=logistic_fun, colour="red") +
  
  geom_point() +

  scale_y_continuous(trans='log10') 

# Results

#### Using the experiment1.csv file and the model shown above I have managed to estimate the initial popualtion size (N0), the population growth rate (r) and the carying capacity (K). The results are shown below: 

#### N0 = 6.903e+00

#### r = 9.990e-03

#### K = 5.979e+10

#### Overall, in this exercise I have shown that the data does align with the model. However, there is a slight difference due to differences in the values chosen and the actual population sizes. 

# Question 2: Calculating population size at t = 4980 

#### Given the estimates for initial population size and the population growth rate are: 

N0 = 6.903e+00
r = 9.990e-03

#### Using the exponential growth formula: N(t) = N0e^rt I can calculate the population size at t= 4980 by substituting these numbers into this formula. 

N(t) = 6.903e+00 x e^ ((9.990e-03) x 4980)

N0 <- 6.903e+00
r <- 9.990e-03
t <- 4980

N_4980_exp <- N0 * exp(r * t)

N(t) = 2.78788725821951e+22 

#### Then I looked at the population size at t = 4980 under a logistic growth model when the carying capacity (K) = 5.979e+10: 

N0 <- 6.903e+00
r <- 9.990e-03
t <- 4980
K <- 5.979e+10

N_4980_logistic <- K / (1 + ((K - N0) / N0) * exp(-r * t))

N(t) = 59,789,999,999.8718       

#### Population size at t = 4980 under an exponential growth model = 2.78788725821951e+22 

#### Population size at t = 4980 under a logistic growth model = 59789999999.8718

#### As we can see from these values, the population is much larger at time 4980 minutes under an exponential growth model comapred to a logitsic growth model. This is because under a logistic growth model the population can only grow up to a certain point (the carying capacity), whereas in the exponential growth model the population can keep growing exponentially. Under the logistic growth model the population has nearly reached carrying capacity by time 4980 minutes. Under the exponential growth model the popualtion has exceeded the carying capcity which is not a reasonable thing to predict. This is because in the real world the population would stop growing once it reaches the carying capacity because there are limited resources and a limited amount of space for a population to use.

# Question 3: making a graph to compare the exponential and logistic growth curves

### Link for the R code of how I created the graph: 
https://github.com/no-name2023/logistic_growth/blob/2a63ac1e6128d9cffbe150b76e31e2129f7a6815/Graphs.R

### Graph comparing exponential and logistic growth curves: 
![graph to show comparison of expoential and logisitc growth](https://github.com/no-name2023/logistic_growth/blob/ad51a3b929f8c6e139fdc358080e4948a06c45b0/graph%20comparing%20exponential%20and%20logstic%20growth%20.png)
