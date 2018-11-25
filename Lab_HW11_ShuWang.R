# Question 1:
rbinomial <- function (n,p) {
  x <- runif(n, min=0, max=1)
  return(sum(x < p))
}
rbinomial(10, 0.4)

# Question 2:
rbinom(1, 10, 0.4)
library(microbenchmark)
microbenchmark(rbinomial(10, 0.4), rbinom(1, 10, 0.4))

# Question 3:
library(tidyverse)
library(ggplot2)

set.seed(123)
beta_0 <- 15
beta_1 <- 0.4
e_i <- rnorm(50, 0, 3)
x_i <- runif(50, 20, 40)
y_i <- beta_0 + (beta_1 * x_i) + e_i

fitresult <- lm(y_i ~ x_i)
result <- data.frame(fitted = fitresult$fitted.values, residuals = fitresult$residuals)

result %>%
  ggplot(aes(x = fitted, y = residuals)) +
  geom_point() +
  labs(x = "fitted values", y = "residuals", title = "Plot for fitted values VS residuals")

# Question 4:
variates <- function (U1, U2) {
  R <- sqrt(-(2 * log(U1)))
  theta <- 2 * pi * U2
  
  var_X <- R * cos(theta)
  var_Y <- R * sin(theta)
  var <- data.frame(var_X, var_Y)
  
  return(var)
}

set.seed(123)
U1 <- runif(1000, 0, 1)
U2 <- runif(1000, 0, 1)
normal <- rnorm(1000, 0, 1)

boxmuller <- variates(U1, U2)
  
dataset_var %>%
  ggplot(aes(x = var_X)) +
  geom_histogram(color = "black", fill = "white", bins = 20) +
  geom_freqpoly(aes(x = normal),bins = 20, color = "red", lwd = 1) + 
  labs(x = "Variable X", y = "Count", title = "Variable_X VS Normal distribution")

dataset_var %>%
  ggplot(aes(x = var_Y)) +
  geom_histogram(color = "black", fill = "white", bins = 20) +
  geom_freqpoly(aes(x = normal), bins = 20, color = "blue", lwd = 1) + 
  labs(x = "Variable Y", y = "Count", title = "Variable_Y VS Normal distribution")

# It seems like the variable X and Y in Box Muller alogrithm are approximate to normal distribution.