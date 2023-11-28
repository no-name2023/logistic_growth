#Script to estimate the model parameters using a linear approximation

#library(dplyr)

install.packages("dplyr")
library(dplyr)

growth_data <- read.csv("experiment1.csv")

#Case 1. K >> N0, t is small

data_subset1 <- growth_data %>% filter(t<1600) %>% mutate(N_log = log(N))

model1 <- lm(N_log ~ t, data_subset1)
summary(model1)

#Case 2. N(t) = K

data_subset2 <- growth_data %>% filter(t>2000)

model2 <- lm(N ~ 1, data_subset2)
summary(model2)

#Script to plot the logistic growth data

growth_data <- read.csv("experiment1.csv")

install.packages("ggplot2")
library(ggplot2)

ggplot(aes(t,N), data = growth_data) +
  
  geom_point() +
  
  xlab("t") +
  
  ylab("y") +
  
  theme_bw()

ggplot(aes(t,N), data = growth_data) +
  
  geom_point() +
  
  xlab("t") +
  
  ylab("y") +
  
  scale_y_continuous(trans='log10')

#Script to plot data and model

growth_data <- read.csv("experiment1.csv")

logistic_fun <- function(t) {
  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  
  return(N)
  
}

N0 <- 6.903e+00 #

r <- 9.990e-03 #

K <- 5.979e+10 #

ggplot(aes(t,N), data = growth_data) +
  
  geom_function(fun=logistic_fun, colour="red") +
  
  geom_point() +
  
  scale_y_continuous(trans='log10')

sink(file = "package-versions.txt")
sessionInfo()
sink()

N0 <- 6.903e+00
r <- 9.990e-03
t <- 4980

N_4980_exp <- N0 * exp(r * t)

N0 <- 6.903e+00
r <- 9.990e-03
t <- 4980
K <- 5.979e+10

N_4980_logistic <- K / (1 + ((K - N0) / N0) * exp(-r * t))

# Exponential growth
N_4980_exp

# Logistic growth
N_4980_logistic

#Script to make a graph comparing the exponential and logistic growth curves

# Loading the packages
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")

# Function to calculate logistic growth
logistic_growth <- function(t, N0, r, K) {
  return(K / (1 + ((K - N0) / N0) * exp(-r * t)))
}

#function to calculate exponential growth 
exponential_growth <- function(t, N0, r) {
  return(N0 * exp(r * t))
}

# Parameter estimates
N0 <- 6.903e+00
r <- 9.990e-03
K <- 5.979e+10

# Generate time values
t_values <- seq(0, 4980, by = 100)

# Calculate population sizes for exponential and logistic growth
N_exp <- N0 * exp(r * t_values)
N_logistic <- logistic_growth(t_values, N0, r, K)

# Create a data frame for plotting
data_plot <- data.frame(
  Time = rep(t_values, 2),
  Population = c(N_exp, N_logistic),
  Model = rep(c("Exponential", "Logistic"), each = length(t_values))
)

# Plotting
library(ggplot2)

ggplot(data_plot, aes(x = Time, y = Population, color = Model)) +
  geom_line() +
  labs(
    title = "Comparison of Exponential and Logistic Growth",
    x = "Time",
    y = "Population Size"
  ) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10', limits = c(6.903e+00, 1e12))  # Adjust limits as needed

