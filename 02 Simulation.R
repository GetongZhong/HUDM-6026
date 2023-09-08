# Generate sample of size 50 from normal distribution with mean
# 100 and sd = 15.
set.seed(4231)
sample1 <- rnorm(n = 50, mean = 100, sd = 15)

# Write a function using sum() and length() to compute the
# sample mean of a vector.
f_mean <- function(vec){
  
}

# Check function on sample1.
mean(sample1)
f_mean(sample1)

# Write a function to compute the sample variance of a vector.
f_var <- function(vec) {
  
}

# Check on sample1.
var(sample1)
f_var(sample1)

# Empirical cdf (ecdf) 
# Generate data from uniform on -1 to 1.
set.seed(8289)
v1 <- runif(n = 10, min = -1, max = 1)
plot(ecdf(v1))
v2 <- runif(n = 100, min = -1, max = 1)
plot(ecdf(v2))
v3 <- runif(n = 1000, min = -1, max = 1)
plot(ecdf(v3))

# Generate data from exponential(0.1).
set.seed(8289)
v1 <- rexp(n = 10, rate = .1)
plot(ecdf(v1))
v2 <- rexp(n = 100, rate = .1)
plot(ecdf(v2))
v3 <- rexp(n = 1000, rate = .1)
plot(ecdf(v3))

# Quantiles
# Estimate the q = .25 quantile from a sample from uniform on -1, 1.
# Generate again from uniform
set.seed(9832)
v1 <- runif(n = 10, min = -1, max = 1)
ecdf1 <- ecdf(v1)
plot(ecdf1)
# Heights at increments of 1/n.
# Cdf of Unif[-1,1] is (x-a)/(b-a) = (x - -1)/(1 - -1) = (x + 1)/2 = (1/2)x + 1/2
abline(a = 1/2, b = 1/2)
# Estimate .5 quantile via ecdf
# X_(.5) = inf{x : F(x) >= q}
# Greatest lower bound of set of x values for which the ecdf is 
# greater or equal to .5.
ecdf_tab <- round(cbind(sort(v1), seq(.1, 1, .1)), 3)
colnames(ecdf_tab) <- c("x", "F(x)")
ecdf_tab
points(ecdf_tab[5,1], ecdf_tab[5,2], pch = 3, col = 2, cex = 2, lwd = 2)
# What does R do by default?
quantile(x = v1, probs = .5)
points(quantile(x = v1, probs = .5), .5, pch = 4, col = 3, cex = 2, lwd = 2)
# Here there are n = 10 observations.
# R uses type = 7 which specifies that the kth order statistic is
# assigned cumulative probability of (k-1)/(n-1). Furthermore, 
# R help notes that "All sample quantiles are defined as weighted 
# averages of consecutive order statistics."
type7_tab <- round(cbind(sort(v1), seq(0, 1, 1/9)), 3)
colnames(type7_tab) <- c("x", "F(x)")
type7_tab
# Weighted avg of consecutive order statistics.
mean(type7_tab[5:6,1])

# Bias of s^2_{mle}
var_mle <- function(vec) {
  n <- length(vec)
  out <- ((n-1)/n)*var(vec)
  return(out)
}

# Generate N(0, 2^2) sample of size 4
# Sample variance s^2 is unbiased (i.e., bias = 0). 
# Bias is of var_mlw is sigma^2/n = 2^2/4 = 1.
set.seed(1838)
norm1 <- rnorm(n = 4, mean = 0, sd = 2)

# Calculate sample variance and mle variance estimates.
var(norm1)
var_mle(norm1)

# Do this 10000 times to estimate variance over 10000 replications.
set.seed(7488)
dat_list <- replicate(n = 10000, expr = rnorm(4, 0, 2), simplify = FALSE)
# Apply each function to dat_list
var_list <- sapply(X = dat_list, FUN = var, simplify = TRUE)
var_mle_list <- sapply(X = dat_list, FUN = var_mle, simplify = TRUE)
mean(var_list)
mean(var_mle_list)
# Estimate bias
mean(var_list) - 4
mean(var_mle_list) - 4

# What about variance of estimators?
# Variance of s^2 for normal(mu, sigma^2) is 2*sigma^4/(n-1).
#   = 2*sigma^4/(n-1) = 2(2^4)/(4-1) = 32/3 = 10.67
# Variance of sigma_hat^2 for normal(mu, sigma^2) is 2(n-1)*sigma^4/(n-1)^2.
#   = 2(n-1)*sigma^4/(n-1)^2 = 2(4-1)(2^4)/(4)^2 = 2*3 = 6
var(var_list)
var(var_mle_list)
h1 <- hist(var_list, breaks = 30, plot = FALSE)
h2 <- hist(var_mle_list, breaks = 30, plot = FALSE)
h2$counts <- -h2$counts
hmax = max(h1$counts)
hmin = min(h2$counts)
X = c(h1$breaks, h2$breaks)
xmax = max(X)
xmin = min(X)
plot(h1, ylim = c(hmin, hmax), col = "green", xlim = c(xmin, xmax))
lines(h2, col = "blue")
legend(x = "bottomright", 
       legend = c(expression(s^2), expression(hat(sigma)^2)),
       pch = 15, col = c("green", "blue"), cex = 2)
dat <- data.frame(Method = rep(c("Sample Variance", "MLE Variance"), each = 10000),
                  Estimate = c(var_list, var_mle_list))
boxplot(dat$Estimate ~ dat$Method,
        xlab = "Method", ylab = "Estimate", 
        main = "10000 samples from N(0, 2^2)")

# MSE of estimators is var + bias^2
# For sample variance, MSE = 10.67 + 0 = 10.67.
# For mle variance, MSE = 6 + 1 = 7.00.
mean((var_list - 4)^2)
mean((var_mle_list - 4)^2)

# Generate exponential data with lambda = 1/10 using the inverse
# transform method.
gen_exp <- function(n, lambda) { return(-log(runif(n)) / lambda) }
set.seed(2347)
samp1 <- gen_exp(n = 10000, lambda = 1/10)
plot(density(samp1), lwd = 2, col = 2)
points(density(samp2), lwd = 2, col = 3, type = "l")

# Sum (convolution) vs mixture.
set.seed(9921)
X1 <- rnorm(n = 10000, mean = 0, sd = 1)
X2 <- rnorm(n = 10000, mean = 3, sd = 1)
S <- X1 + X2
hist(S, breaks = 100)

# 50/50 mixture of univariate normals
set.seed(6327)
Z <- rbinom(n = 10000, size = 1, prob = 1/2)
M <- NULL
for (i in 1:length(Z)) {
  if(Z[i] == 0) { M[i] <- rnorm(1, mean = 0, sd = 1) } else {
    M[i] <- rnorm(1, mean = 3, sd = 1)
  }
}
hist(M, breaks = 100)

# Generate multivariate normal
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

sig <- matrix(c(1, .5, .8, .5, 1, 0, .8, 0, 1), 3, 3, byrow = TRUE)

set.seed(2381)
mv1 <- mvn_gen(n = 1000, mu = c(0, 1, 4), sigma = sig, factorization = "Cholesky")
plot(mv1)
mv2 <- mvn_gen(n = 1000, mu = c(0, 1, 4), sigma = sig, factorization = "Spectral")
plot(mv2)
