## Step 1 
dat_gen <- function(){
  
  return(runif(5))
  }
var(runif(5))
1/12
## Step 2
R = 1000
set.seed(1111)
dat_list <- replicate(n = R,
                      expr = dat_gen(),
                      simplify = FALSE)

## Step 3
mle <- function(vec) {
  n <- length(vec)
  sig.hat.sqr <- var(vec)*(n-1)/n
  return(sig.hat.sqr)
}


## Step 4
bias_hat <- sapply(X = dat_list,
                     FUN = mle,
                     simplify = TRUE)
## estimated bias   
mean(bias_hat)-1/12

## true bias
(-1/12)/5
