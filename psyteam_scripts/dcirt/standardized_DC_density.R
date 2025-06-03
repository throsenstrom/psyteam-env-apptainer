# standardized_DC_density.R
# Tom Rosenstrom / 2023-09-05
# In revision, TR added an option to return only mu and S / 2024-06-07
# Implements Eq. 13 from Zhang et al. (2021) 
# Multivariate Behavioral Research, 2021;56(5):703-723, 
# DOI: 10.1080/00273171.2020.1776096

standardized_DC_density <- function(x=NULL, dcfit, mean_var_only = FALSE){
  tmp <- coef(dcfit)$GroupPars
  k <- length(tmp) - 2 # infer k from the DC-IRT model object
  Phi <- tmp[3:(k+2)]  # collect Phi parameter vector 
  cc <- sin(Phi[1])
  if (k > 1){
    for (i in 1:(k-1)){cc <- c(cc, prod(cos(Phi[1:i]))*sin(Phi[i+1]))}
    cc <- c(cc, prod(cos(Phi[1:k])))
  } else {
    cc <- c(cc, cos(Phi[1]))
  }
  Mf <- function(s){ss <- s/2; ifelse((s)%%2 == 0, factorial(2*ss)/(2^ss*factorial(ss)), 0)}
  M <- matrix(0,k+1,k+1); for (i in 1:(k+1)){for (j in 1:(k+1)){ M[i,j] <- i+j-2}}
  M <- Mf(M)
  B <- chol(M)
  m <- solve(B) %*% t(t(cc))
  M_star <- matrix(0,k+1,k+1); for (i in 1:(k+1)){for (j in 1:(k+1)){ M_star[i,j] <- i+j-1}}
  M_star <- Mf(M_star)
  M_2stars <- matrix(0,k+1,k+1); for (i in 1:(k+1)){for (j in 1:(k+1)){ M_2stars[i,j] <- i+j}}
  M_2stars <- Mf(M_2stars)
  mu <- c(t(m) %*% M_star %*% m)
  S <- c(sqrt(t(m) %*% M_2stars %*% m - mu^2))
  if (is.null(x)|mean_var_only){
    return(list(mu = mu, S = S))
  } else {
    fout <- rep(0, length(x))
    for (i in 1:length(x)){
      fout[i] <- sum(c(m) * (x[i]*S+mu)^(0:k))^2 * dnorm(x[i]*S+mu) * S
    }
    return(fout)
  }
}
