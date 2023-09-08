sig <- matrix(c(13.6, -14.5, -103.4, -14.5, 65.2, 154, -103.4, 154, 2702), 3, 3, byrow = TRUE)
error <- rnorm(1000,0,0.75^2)
mvn_gen <- function(n, mu, sigma, factorization = "Cholesky") {
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
set.seed(1211)



mv <- mvn_gen(n = 1000, mu = c(0.5, 0.3, 10), sigma = sig, factorization = "Spectral")
Y = 71 - 0.28 *mv$X1 + 0.05*mv$X2 - 0.007*mv$X3+error
data2 <- cbind(mv,Y)

model<-lm(Y ~ X1 +X2 + X3 ,data = data2)
summary(model)
model$residuals
library(tidyverse)
library(knitr)
library(generics)
model%>%
  tidy() %>%
  kable()
?tidy
