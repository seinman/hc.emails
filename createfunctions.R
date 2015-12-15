##################################
# PART 1: FUNCTIONS
###################################
# This file creates all the functions used throughout the analysis
# side of the project.

# Make a simple linear regression
lin.reg <- function(y, X) {
  beta <- solve(t(X) %*% X)%*%t(X)%*%y
  hat <- X %*% solve(t(X) %*% X) %*% t(X)
  e <- diag(1, nrow(X)) - hat
  #q <- nrow(X) * solve(t(e)%*%e)
  #covmat <- q * diag(1, nrow(X))
  return("beta" = beta)
}

# Create a lag function
create.lag <- function(vector, steps = 1, empty = NA) {
  obs <- length(vector)
  lag <- rep(0, obs)
  lag[1:steps] <- empty
  for(i in (steps+1):obs) {
    lag[i] <- vector[i - steps]
  }
  return(lag)
}

# this function calculates the likelihood for a univariate norm
like.norm <- function(input, mean, var) {
  normaliser <- 1/sqrt(pi * var * 2)
  inner <- -0.5 * ((input - mean) ** 2 ) / var
  return(normaliser * exp(inner))
}

# this function calculates the log likelihood for a multivariate norm
log.like.norm <- function(input, mean, var) {
  like <- 1
  obs <- length(input)
  for(i in 1:obs) {
    like <- like * like.norm(input[i], mean, var)
  }
  return(log(like))
}

# This function groups days into weeks
groupweek <- function(datevector) {
  jul <- julian(datevector)
  jul2 <- create.lag(jul, empty = 0) - jul
  jul3 <- cumsum(as.numeric(jul2))
  jul4 <- jul3 %/% 7
  
  return(jul4)
}

# This function creates an IWLS estimation for a Poisson model.
# THESE TWO FUNCTIONS ARE USED THROUGHOUT
poisson.mean <- function(eta) { exp(eta) }
poisson.var <- function(mu) { mu }

# This calculates the poisson iwls
poisson.iwls <- function(y, X) {
  max.iter <- 50
  beta <- rep(0, ncol(X))
  beta.prev <- beta
  beta.trace <- matrix(0, max.iter + 1, length(beta))
  eta <- X %*% beta
  mu <- (y + 0.5)/2
  beta.trace[1,] <- beta
  #return(eta)
  for(iter in 1:max.iter) {
    z <- eta + (y - mu) * (1/mu)
    w <- mu ** 2 / poisson.var(mu)
    W <- diag(as.numeric(w))
    beta <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z
    eta <- X %*% beta
    mu <- poisson.mean(eta)
    beta.trace[iter+1,] <- beta
    # Check for convergence
    if(sum(beta - beta.prev) ** 2 < 1e-4) {
      # Calculate variance of betas
      w <- poisson.var(mu)
      W <- diag(as.numeric(w))
      cov <- solve(t(X)%*% W %*% X)
      return (list(beta=beta, cov=cov))
      break
    }
    beta.prev <- beta
  }
}
# Create the EM algorithm
# ALGORITHM OF DOOM
# GREAT TERRIBLE DOOOOOOOOOOOOOM
# init.pi is the chance that an observation is an outlier
em <- function(t, x, inipi) {
  obs <- length(t)
  mu1 <- rep(max(t), length(t))
  mu2 <- rep(min(t), length(t))
  var1 <- var(t) / 3
  var2 <- var(t) / 2
  # these will be used for testing convergence
  mu1old <- mu1
  mu2old <- mu2
  pi <- inipi
  gamma1 <- rep(0, length(t))   
  for( i in 1:10) {
    cat(i)
    # E step
    for(observation in 1:length(t)) {
      #cat(observation)
      l1.1 <- like.norm(t[observation], mu1[observation], var1)
      l1 <- pi * l1.1
      l2.1<-like.norm(t[observation], mu2[observation], var2)
      l2 <-  (1-pi) * l2.1
      gamma1[observation] <- l1 / (l1 + l2)
    }
    gamma2 <- 1 - gamma1
     # M step
     n1 <- sum(gamma1)
     n2 <- sum(gamma2)
     # Today is not a good day.
     w1 <- diag(gamma1)
     beta1 <- solve(t(x) %*% w1 %*% x) %*% t(x) %*% w1 %*% t
     mu1 <- x %*% beta1
     w2 <- diag(gamma2)
     beta2 <- solve(t(x) %*% w2 %*% x) %*% t(x) %*% w2 %*% t
     mu2 <- x %*% beta2
     var1 <- 1/n1 * sum(gamma1 * (t - mu1)**2)
     var2 <- 1/n2 * sum(gamma2 * (t - mu2)**2)
     pi <- n1/(n1 + n2)
     # Check for convergence
     v <- sum(abs(mu1old - mu1), na.rm = TRUE) < 0.00001 & sum(abs(mu2old - mu2), na.rm = TRUE) < 0.0001
     if(v) { break }
     else { mu1old <- mu1
     mu2old <- mu2
     }
  }
    return(list("probabilities" = gamma1, "varexcept" = var1, "betaexcept" = beta1,
     "varnorm" = var2, "betanorm" = beta2))
}    


testing.list <- append(rnorm(90), rnorm(50, 3, 1.4))

# This is used to test the em algorithm
testing.matrix <- matrix(append(rnorm(200), rnorm(200, 5, 2)), 200, 2)
testing.matrix <- cbind(1, testing.matrix)
testing.list.2 <- testing.matrix[,2] + testing.matrix[,3]
testing.list.2[161:200] <- 1 + (testing.matrix[161:200,2] * 2)+ (3 * testing.matrix[161:200, 3])
eps <- rnorm(200)
testing.list.2 <- testing.list.2 + eps
##############################################################3
