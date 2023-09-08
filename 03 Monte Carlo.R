# Generate some data from Uniform(0, 2) to demonstrate notation
set.seed(4328)
xs <- replicate(n = 5, expr = runif(n = 20, min = 0, max = 2), simplify = TRUE)
xs

# Function for trimmed mean
tr_mn <- function(vec) {
  n <- length(vec)
  vec_sort <- sort(vec)
  mn <- mean(vec_sort[2:(n-1)])
  return(mn)
}

# Apply function to each column of data
(theta_hats <- apply(X = xs, MARGIN = 2, FUN = tr_mn))


# SCR Example 7.1
# 1. Write a function for data generation.
dat_gen <- function() {return(rnorm(2))}
# 2. Replicate data generation R times.
R = 10000
set.seed(7667)
dat_list <- replicate(n = R,
                      expr = dat_gen(),
                      simplify = FALSE)
# 3. Write function to compute value of estimator.
theta_hat <- function(vec) {return(abs(diff(vec)))}
# 4. Apply the function to each data set in the data list.
theta_hats <- sapply(X = dat_list,
                     FUN = theta_hat,
                     simplify = TRUE)
# 5. Estimate the mean of the estimator.
hist(theta_hats)
mean(theta_hats)

# Use plug-in estimator for variance and SE.
(var_hat <- var(theta_hats))
(sd_hat <- sqrt(var_hat))
(se_hat <- sqrt(var_hat)/sqrt(R))

# MSE example
# 1. Write a function for data generation.
#    We actually can just use rnorm() here because the data generation
#    is simply an iid sample from N(0,1).
# 2. Replicate data generation R times.
R = 1000
set.seed(1820)
dat_list <- replicate(n = R,
                      expr = rnorm(n = 20, mean = 0, sd = 1),
                      simplify = FALSE)
# 3. Write a function to compute the value of estimator.
trimmed_mn <- function(vec, k = 1) {
  # k is the level of trimming; default is level-1 trimming
  n <- length(vec)
  if(2*k >= n) {stop("k is too large. Cannot trim off all observations.")}
  sorted <- sort(vec)
  tr_mn <- sum(vec[(k+1):(n-k)])/(n-2*k)
  return(tr_mn)
}
# 4. Apply the function to each data set in the list.
theta_hats <- sapply(X = dat_list,
                     FUN = trimmed_mn,
                     simplify = TRUE)
# 5. Estimate the MSE of the estimator.
hist(theta_hats)
(mse <- mean((theta_hats - 0)^2))
# The SE of the mean is calculated as follows.
sqrt(sum((theta_hats - mean(theta_hats))^2)) / R

# Upper confidence limit for sample variance.
# Generate R = 1000 samples
dat_list <- replicate(n = 1000,
                      expr = rnorm(n = 20, sd = 2),
                      simplify = FALSE)
ucl <- function(vec, alpha = .05) {
  n <- length(vec)
  ul <- (n-1)*var(vec) / qchisq(p = alpha, df = n - 1)
  return(ul)
}
ucls <- sapply(X = dat_list, FUN = ucl)
# Determine proportion of UCLs that exceed 4.
length(which(ucls > 4))/length(ucls)

# Repeat with data from chi-square(2).
dat_list <- replicate(n = 1000,
                      expr = rchisq(n = 20, df = 2),
                      simplify = FALSE)
ucls <- sapply(X = dat_list, FUN = ucl)
# Determine proportion of UCLs that exceed 4.
length(which(ucls > 4))/length(ucls)
