# simulate_dda.R
# Simulation analyses for the paper / TR 24.3.2022, 16.6.2022, 31.8.2022
# The user needs to have Rtools installed, retrieve the package and
# install.package("KernelICA_0.1.0.tar.gz", repos = NULL, source = TRUE)
# and also
# install.packages(c("nimble", "parallel", "data.table", "Matrix", "expm", "dHSIC"))

source("direction_dependence_measures.R")
library(KernelICA)

###### parameters #######
n <- 1000                         # Number of patients
nR <- 2000                        # Number of replications
A <- expm::logm( matrix(c(0.5,0.0,0.5,0.5),2,2) ) # Lagged effects
I0 <- 0.5                         # Instantaneous (treatment) effect x -> y (1 -> 2)
#########################

eigen(A)$values # check that the dynamics are well-behaved (negative eigenvalues)
At <- Matrix::expm(A) # consider unit time steps

#######################################################################
##### Functions to test the methods in the simulation conditions ######
#######################################################################

# PRE- AND POST-THERAPY DATA WITHOUT INITIAL CORRELATIONS
prepost_simulate <- function(At, I, Nrep = 2000, Nt = 2, n = 1000){
  res <- data.frame( matrix(0,Nrep,2*6 + 3) )
  names(res) <- c(paste0(c("mxnt", "kgv", "tanh", "skew", "rskew","hsic"),"_Txy_I"),
                  paste0(c("mxnt", "kgv", "tanh", "skew", "rskew","hsic"),"_Txy_L"),
                  "Txy_C", "M_12", "M_21")
  for (j in 1:Nrep){
    X <- matrix(0,n,2)
    for (i in 1:Nt){
      X_old <- X
      X[,1] <- At[1,1]*X_old[,1] + At[1,2]*X_old[,2] + nimble::rdexp(n)
      X[,2] <- I0*X[,1] + At[2,2]*X_old[,2] + At[2,1]*X_old[,1] + nimble::rdexp(n)  
      
      if (i == 1){X_first <- X}
    }
    X <- scale(X); X_first <- scale(X_first)
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "mxnt")
    res[j,1] <- Txys$Txy_I
    res[j,1+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "kgv")
    res[j,2] <- Txys$Txy_I
    res[j,2+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "tanh")
    res[j,3] <- Txys$Txy_I
    res[j,3+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "skew")
    res[j,4] <- Txys$Txy_I
    res[j,4+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "rskew")
    res[j,5] <- Txys$Txy_I
    res[j,5+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "hsic")
    res[j,6] <- Txys$Txy_I
    res[j,6+6] <- Txys$Txy_L
    # confounded model
    M <- t( lm(X ~ X_first[,1] + X_first[,2])$coefficients[2:3,] )
    res[j, 13] <- abs(M[2,1]) - abs(M[1,2])
    res[j, 14] <- M[1,2]; res[j, 15] <- M[2,1]
  }
  return(res)
}

# PRE- AND POST-THERAPY DATA WITH INITIAL CORRELATIONS
prepost_simulate_correlated <- function(At, I, Nrep = 2000, Nt = 2, n = 1000){
  res <- data.frame( matrix(0,Nrep,2*6 + 3) )
  names(res) <- c(paste0(c("mxnt", "kgv", "tanh", "skew", "rskew","hsic"),"_Txy_I"),
                  paste0(c("mxnt", "kgv", "tanh", "skew", "rskew","hsic"),"_Txy_L"),
                  "Txy_C", "M_12", "M_21")
  for (j in 1:Nrep){
    X <- matrix(0,n,2)
    for (i in 1:Nt){
      X_old <- X
      X[,1] <- At[1,1]*X_old[,1] + At[1,2]*X_old[,2] + nimble::rdexp(n)
      X[,2] <- I0*X[,1] + At[2,2]*X_old[,2] + At[2,1]*X_old[,1] + nimble::rdexp(n)  
      if (i == 1){
        svec <- nimble::rdexp(n)
        X[,1] <- sqrt(0.5)*(X[,1] + svec)
        X[,2] <- sqrt(0.5)*(X[,2] + svec)
        X_first <- X
        }
    }
    X <- scale(X); X_first <- scale(X_first)
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "mxnt")
    res[j,1] <- Txys$Txy_I
    res[j,1+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "kgv")
    res[j,2] <- Txys$Txy_I
    res[j,2+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "tanh")
    res[j,3] <- Txys$Txy_I
    res[j,3+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "skew")
    res[j,4] <- Txys$Txy_I
    res[j,4+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "rskew")
    res[j,5] <- Txys$Txy_I
    res[j,5+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "hsic")
    res[j,6] <- Txys$Txy_I
    res[j,6+6] <- Txys$Txy_L
    # confounded model
    M <- t( lm(X ~ X_first[,1] + X_first[,2])$coefficients[2:3,] )
    res[j, 13] <- abs(M[2,1]) - abs(M[1,2])
    res[j, 14] <- M[1,2]; res[j, 15] <- M[2,1]
  }
  return(res)
}

# PRE- AND POST-THERAPY DATA WITH CONFOUNDING
prepost_simulate_confounded <- function(At, I, Nrep = 2000, Nt = 2, n = 1000){
  res <- data.frame( matrix(0,Nrep,2*6 + 3) )
  names(res) <- c(paste0(c("mxnt", "kgv", "tanh", "skew", "rskew","hsic"),"_Txy_I"),
                  paste0(c("mxnt", "kgv", "tanh", "skew", "rskew","hsic"),"_Txy_L"),
                  "Txy_C", "M_12", "M_21")
  for (j in 1:Nrep){
    X <- matrix(0,n,2)
    for (i in 1:Nt){
      X_old <- X
      X[,1] <- At[1,1]*X_old[,1] + At[1,2]*X_old[,2] + nimble::rdexp(n)
      X[,2] <- I0*X[,1] + At[2,2]*X_old[,2] + At[2,1]*X_old[,1] + nimble::rdexp(n)  
      if (i == 1){ X_first <- X }
      if (i == Nt){
        svec <- nimble::rdexp(n)
        # svec <- rnorm(n)
        X[,1] <- X[,1] + svec
        X[,2] <- X[,2] + svec
      }
    }
    X <- scale(X); X_first <- scale(X_first)
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "mxnt")
    res[j,1] <- Txys$Txy_I
    res[j,1+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "kgv")
    res[j,2] <- Txys$Txy_I
    res[j,2+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "tanh")
    res[j,3] <- Txys$Txy_I
    res[j,3+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "skew")
    res[j,4] <- Txys$Txy_I
    res[j,4+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "rskew")
    res[j,5] <- Txys$Txy_I
    res[j,5+6] <- Txys$Txy_L
    Txys <- lagged_direction_dependence(X1=X,X0=X_first, approximation = "hsic")
    res[j,6] <- Txys$Txy_I
    res[j,6+6] <- Txys$Txy_L
    # confounded model
    M <- t( lm(X ~ X_first[,1] + X_first[,2])$coefficients[2:3,] )
    res[j, 13] <- abs(M[2,1]) - abs(M[1,2])
    res[j, 14] <- M[1,2]; res[j, 15] <- M[2,1]
  }
  return(res)
}


# SKEWED DATA
skew_simulate <- function(Nrep = 2000, n = 1000){
  res <- data.frame( matrix(0,Nrep,6) )
  names(res) <- c("mxnt", "kgv", "tanh", "skew", "rskew","hsic")
  for (j in 1:Nrep){
    x <- rlnorm(n, sdlog = 0.25)
    y <- x + rlnorm(n, sdlog = 0.25)
    res[j,1] <- direction_dependence(x, y, approximation = "mxnt")
    res[j,2] <- direction_dependence(x, y, approximation = "kgv")
    res[j,3] <- direction_dependence(x, y, approximation = "tanh")
    res[j,4] <- direction_dependence(x, y, approximation = "skew")
    res[j,5] <- direction_dependence(x, y, approximation = "rskew")
    res[j,6] <- direction_dependence(x, y, approximation = "hsic")
  }
  return(res)
}

# BIMODEL DATA (MIXTURE OF 2 GAUSSIANS)
m2G_simulate <- function(Nrep = 2000, n = 1000){
  res <- data.frame( matrix(0,Nrep,6) )
  names(res) <- c("mxnt", "kgv", "tanh", "skew", "rskew","hsic")
  m2G <- function(Nsim = 1000, p1 = 0.75, m1 = 3, m2 = -4){
    isim <- runif(Nsim)<=p1
    return(rnorm(Nsim,mean = m1)*isim + rnorm(Nsim, mean = m2)*(!isim))
  }
  for (j in 1:Nrep){
    x <- scale(m2G(n))
    y <- x + scale(m2G(n))
    res[j,1] <- direction_dependence(x, y, approximation = "mxnt")
    res[j,2] <- direction_dependence(x, y, approximation = "kgv")
    res[j,3] <- direction_dependence(x, y, approximation = "tanh")
    res[j,4] <- direction_dependence(x, y, approximation = "skew")
    res[j,5] <- direction_dependence(x, y, approximation = "rskew")
    res[j,6] <- direction_dependence(x, y, approximation = "hsic")
  }
  return(res)
}

# KURTOTIC DATA
kurt_simulate <- function(Nrep = 2000, n = 1000){
  res <- data.frame( matrix(0,Nrep,6) )
  names(res) <- c("mxnt", "kgv", "tanh", "skew", "rskew", "hsic")
  for (j in 1:Nrep){
    x <- nimble::rdexp(n)
    y <- x + nimble::rdexp(n)
    res[j,1] <- direction_dependence(x, y, approximation = "mxnt")
    res[j,2] <- direction_dependence(x, y, approximation = "kgv")
    res[j,3] <- direction_dependence(x, y, approximation = "tanh")
    res[j,4] <- direction_dependence(x, y, approximation = "skew")
    res[j,5] <- direction_dependence(x, y, approximation = "rskew")
    res[j,6] <- direction_dependence(x, y, approximation = "hsic")
  }
  return(res)
}

####################################################################
#####                   Run the simulations                   ######
####################################################################

# Use half the available resources (I have 8 cores but using only 4)
# ncore <- ceiling(parallel::detectCores() / 2)
ncore <- parallel::detectCores() # or use all

# Divide replications to several cores, do PREPOST
set.seed(28790)
system.time({
  sres_part <- parallel::mclapply(
    rep(round(nR/ncore),ncore),
    function(x) prepost_simulate(At, Nrep = x, Nt = 2, n = n))
})
# Combine using a fast function
sres <- list(prepost = data.table::rbindlist(sres_part) )
save(sres, file = "simulation_results_revised.Rdata")

# PREPOST, CORRELATED X0
system.time({
  sres_part <- parallel::mclapply(
    rep(round(nR/ncore),ncore),
    function(x) prepost_simulate_correlated(At, Nrep = x, Nt = 2, n = n))
})

sres <- c(sres,list(ppcor = data.table::rbindlist(sres_part) ))
save(sres, file = "simulation_results_revised.Rdata")

# PREPOST, CONFOUNDED X1
system.time({
  sres_part <- parallel::mclapply(
    rep(round(nR/ncore),ncore),
    function(x) prepost_simulate_confounded(At, Nrep = x, Nt = 2, n = n))
})

sres <- c(sres,list(ppcon = data.table::rbindlist(sres_part) ))
save(sres, file = "simulation_results_revised.Rdata")


# SKEWED
sres_part <- parallel::mclapply(
  rep(round(nR/ncore),ncore),
  function(x) skew_simulate(Nrep = x, n = n))

sres <- c(sres, list(skewed = data.table::rbindlist(sres_part)))
save(sres, file = "simulation_results_revised.Rdata")

# MIXTURE OF GAUSSIANS
sres_part <- parallel::mclapply(
  rep(round(nR/ncore),ncore),
  function(x) m2G_simulate(Nrep = x, n = n))

sres <- c(sres, m2G = list(data.table::rbindlist(sres_part)))
save(sres, file = "simulation_results_revised.Rdata")

# KURTOTIC
sres_part <- parallel::mclapply(
  rep(round(nR/ncore),ncore),
  function(x) kurt_simulate(Nrep = x, n = n))

sres <- c(sres, list(kurtotic = data.table::rbindlist(sres_part)))
save(sres, file = "simulation_results_revised.Rdata")

#################################################################
############### Print results to csv-tables #####################
#################################################################

# A function to print all cross-sectional statistics to the same table
make_table <- function(res){
  res <- data.frame(res)
  tres <- matrix("", ncol(res), 3)
  for (i in 1:ncol(res)){
    tres[i,1] <- round( mean(res[,i]) , 3)
    tmpci <- round( quantile(res[,i], probs = c(0.025, 0.975)) , 3)
    tres[i,2] <- paste0("(",tmpci[1], ", ", tmpci[2], ")")
    tres[i,3] <- round( mean(res[,i] > 0) , 3)
  }
  rownames(tres) <- names(res)
  return(tres)
}

# A function to print selected lagged-model statistics
make_table_longit <- function(res){
  res <- data.frame(res)[,1:13]
  tres <- matrix("", ncol(res), 1)
  for (i in 1:ncol(res)){
    tres[i,1] <- round( mean(res[,i] > 0) , 3)
  }
  rownames(tres) <- names(res)
  return(tres)
}

# Cross-sectional simulation results table as a csv-file
write.csv(t( cbind(make_table(sres$kurtotic),
                   make_table(sres$skewed),
                   make_table(sres$m2G)) ), 
          file = "simulation_results_table_revised.csv")

# Longitudinal simulation results table as a csv-file
write.csv(cbind(make_table_longit(sres$prepost),
                make_table_longit(sres$ppcor),
                make_table_longit(sres$ppcon)),
          file = "simulation_results_longit_table_revised.csv")
