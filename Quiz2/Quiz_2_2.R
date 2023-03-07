setwd('/Users/liuzikai/Desktop/3508/Quiz 2 data-20230214')
load('netdata.RData')
library(nnet)
netdata

# create the data frame
data <- netdata

# define the log-likelihood function
negloglik <- function(theta, Y, n, x) {
  beta0 = theta[1]
  beta1 = theta[2]
  a = c(1,2)
  lp <- as.numeric(a%*%theta)
  p <- plogis(lp)
  -sum(dbinom(Y, n , p, log = TRUE))
}

# set the initial values for the parameters
theta <- c(0, 0)

# extract the data for size class 2
y <- netdata[,2]
n <- netdata[,1]
x <- netdata[,3]

# optimize the negative log-likelihood function
fit <- optim(theta, negloglik, Y = y, n=n,x=x)

# extract the maximum likelihood estimates
theta_hat <- fit$par

# compute the probability of a fish of size class 2 escaping the net
lp <- theta_hat[1] + theta_hat[2] * 2
p <- plogis(lp)
p

