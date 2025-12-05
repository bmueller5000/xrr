simdat <- function(n, sharex5, seed) {
  
  ## set seed
  set.seed(seed)
  
  ## variables
  data <- data.frame(
    x1 = runif(n, -1, 1),
    x2 = rnorm(n, 0, 1),
    x3 = rnorm(n, 0, 0.5),
    x4 = rexp(n, 5),
    x5 = runif(n, 0, 1)
    # x6 = runif(n, 0, 1)
  )
  data$x5[sample(1:n, ceiling(n*sharex5))] <- runif(ceiling(n*sharex5), 1, 1.2)
  vars <- colnames(data)
  nvars <- length(vars)
  
  ## effects
  fx1 <- function(x) x
  fx2 <- function(x) sin(x)
  fx3 <- function(x) (x^2) / 4
  fx4 <- function(x) 0.7 * (sin(pi * x) + exp(-(x - 1)^2))
  fx5 <- function(x) 0.1 * (x^12)
  # fx6 <- function(x) 0
  
  data$fx1 <- fx1(data$x1)
  data$fx2 <- fx2(data$x2)
  data$fx3 <- fx3(data$x3)
  data$fx4 <- fx4(data$x4)
  data$fx5 <- fx5(data$x5)
  # data$fx6 <- fx6(data$x6)
  
  ## centering
  center <- function(z) {z - mean(z)}
  data[paste0("fx", 1:nvars)] <- lapply(data[paste0("fx", 1:nvars)], center)
  
  ## generate y
  # data$y <- with(data, fx1 + fx2 + fx3 + fx4 + fx5 + fx6 + rnorm(n, 0, 1))
  data$y <- with(data, fx1 + fx2 + fx3 + fx4 + fx5 + rnorm(n, 0, 1))
  
  return(data)
}


