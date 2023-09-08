mvn_gen <- function(n, mu, sigma, factorization = "Cholesky") {
  # Generate a sample of size n from multivariate normal with
  # mean vector mu and covariance matrix sigma.
  # Argument factorization can be either "Cholesky" or "Spectral".
  d <- length(mu)
  Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
  if(factorization == "Cholesky") { Q <- chol(sigma) } else {
    if(factorization == "Spectral") {
      ev <- eigen(sigma)
      lambda <- ev$values
      P <- ev$vectors
      Q <- P %*% diag(sqrt(lambda)) %*% t(P) } else {
        stop("Arg factorization must be 'Cholesky' or 'Spectral'.")
      } }
  mu <- matrix(mu, nrow = d, ncol = 1)
  J = matrix(1, nrow = n, ncol = 1)
  X <- Z %*% Q + J %*% t(mu)
  return(data.frame(X))
}

sig <- matrix(c(1,0,0,0,0.2,0,0,0,0,0,
                0,1,0,0,0,0.9,0,0,0,0,
                0,0,1,0,0,0,0,0.2,0,0,
                0,0,0,1,0,0,0,0,0.9,0,
                0.2,0,0,0,1,0,0,0,0,0,
                0,0.9,0,0,0,1,0,0,0,0,
                0,0,0,0,0,0,1,0,0,0,
                0,0,0.2,0,0,0,0,1,0,0,
                0,0,0,0.9,0,0,0,0,1,0,
                0,0,0,0,0,0,0,0,0,1), 10, 10, byrow = TRUE)



dat_gen <- function(x=500){
  dat10 <- mvn_gen(n = x, mu = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), sigma = sig, factorization = "Cholesky")
  X11 <- c()
  for (i in 1:x){
    X11[i] <- (1 + exp(-(0 + dat10$X1[i] * 0.8 + dat10$X2[i] * (-0.25) + dat10$X3[i] * 0.6 + dat10$X4[i] * (-0.4) 
                        + dat10$X5[i] * (-0.8) + dat10$X6[i] * (-0.5) + dat10$X7[i] * 0.7 +(-0.25) * dat10$X2[i] * dat10$X2[i])))^(-1)
  }
  dich <- runif(x)
  A <- c()
  for (j in 1:x){
    if (dich[j] < X11[j]) {
      A[j] <- 1
    } else if (dich[j] > X11[j]){
      A[j] <- 0
    }
  }
  Y <- c() 
  for (k in 1:x){
    Y[k] <- 3.85 + 0.3 * dat10$X1[k] - 0.36 * dat10$X2[k] - 0.73 * dat10$X3[k] - 0.2 * dat10$X4[k] + 0.71 * dat10$X8[k] - 0.19 * dat10$X9[k] + 0.26 * dat10$X10[k] - 0.4 * A[k]
  }
  M <- cbind(dat10, X11, A, Y)
  return(M)
}
dat10 <- mvn_gen(n = 500, mu = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                 sigma = sig, factorization = "Cholesky")

cor(dat10)
rbinom(5,1)
rbinom(10, 1, 0.2)
