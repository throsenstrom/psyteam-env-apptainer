# direction_dependence_measures.R
# Functions implementing the statistics / TR 24.3.2022, 16.6.2022, 31.8.2022
# For KGV, user needs to have Rtools installed, retrieve the package and
# install.package("KernelICA_0.1.0.tar.gz", repos = NULL, source = TRUE)
# For HSIC, please install the package "dHSIC".

# Kernel Generalized Variance (KGV) approx. of mutual information (Bach and Jordan, 2002)
# Needs the package "KernelICA" to function
kgv_contrast <- function(x,sigma=ifelse(nrow(x)<=1000,2/100,2/1000),
                         eta=1e-04, kappaval=1e-02){
  centerpartial <- function(G1){
    return(G1 - matrix(rep(colMeans(G1),nrow(G1)),ncol = ncol(G1), byrow = T) )
  }
  N <- nrow(x) # number of data points
  m <- ncol(x) # number of components
  Us <- Drs <- vector("list", m)
  sizes <- rep(0,m)
  for (i in 1:m){
    G <- KernelICA::incomplete_cholesky(x[,i], kernel = "gaus", sigma = sigma)
    G <- centerpartial(G$L[G$perm,])
    # Regularization
    a <- eigen(t(G) %*% G, symmetric = TRUE)
    indexes <- which( (a$values >= N*eta) & sapply(a$values, is.numeric) )
    if (length(indexes)==0){ indexes <- 1 }
    D <- a$values[indexes]
    V <- G %*% (a$vectors[,indexes] %*% diag(sqrt(1/D)))
    Us[[i]] <- V
    Drs[[i]] <- D/(N*kappaval+D)
    sizes[i] <- length(D)
  }
  # Calculated Rkappa
  Rkappa <- diag(sum(sizes))
  starts <- cumsum(c(1,sizes))[-(m+1)]
  for (i in 2:m){
    for (j in 1:(i-1)){
      newbottom <- diag(Drs[[i]], nrow = sizes[i]) %*% 
        (t(Us[[i]]) %*% Us[[j]]) %*% diag(Drs[[j]], nrow = sizes[j])
      Rkappa[starts[i]:(starts[i]+sizes[i]-1),starts[j]:(starts[j]+sizes[j]-1)] <-
        newbottom
      Rkappa[starts[j]:(starts[j]+sizes[j]-1),starts[i]:(starts[i]+sizes[i]-1)] <-
        t(newbottom)
    }
  }
  return(-0.5*determinant(Rkappa, logarithm = TRUE)$modulus)
}


####### Pairwise direction dependence measures #######
####### T(x, y) > 0 when x -> y directed       #######

direction_dependence <- function(x, y, approximation = "mxnt"){
  Xtmp <- scale( na.omit(cbind(x, y)) )
  x <- Xtmp[,1]; y <- Xtmp[,2]
  # Maximum entropy approximation to likelihood ratio
  if (approximation == "mxnt"){
    mentappr <- function(z){
      # standardize
      z <- z - mean(z)
      zsd <- sd(z)
      z <- z/zsd
      # Constants we need
      k1 <- 36/(8*sqrt(3)-9)
      gamma <- 0.37457 
      k2 <- 79.047
      gaussianEntropy <- log(2*pi)/2+1/2
      # Negentropy
      negentropy <- k2*(mean(log(cosh(z)))-gamma)^2+k1*mean(z*exp(-z^2/2))^2
      entropy <- gaussianEntropy - negentropy + log(zsd);
      return(entropy)
    }
    ratio <- mentappr(y) + mentappr(lm(x~y)$residuals) - 
      mentappr(x) - mentappr(lm(y~x)$residuals)
    return(ratio)
  }
  # Kernel generalized variance approx of mutual information
  if (approximation == "kgv"){
    rx <- lm(y ~ x)$residuals
    ry <- lm(x ~ y)$residuals
    return( kgv_contrast( cbind(y, ry)) - kgv_contrast( cbind(x, rx)) )
  }
  # Tanh (kurtosis) based approximation to likelihood ratio
  if (approximation == "tanh"){
    return( cor(x,y)*mean(x*tanh(y) - tanh(x)*y) )
  }
  # Simple skewness based measure
  if (approximation == "skew"){
    x <- x*sign(moments::skewness(x)); y <- y*sign(moments::skewness(y))
    return( cor(x,y)*mean(x^2*y - x*y^2) )
  }
  # Skewness-based likelihood-ratio approximation
  if (approximation == "rskew"){
    return( cor(x,y)*mean( log(cosh(pmax(x,0)))*y - x*log(cosh(pmax(y,0))) ) )
  }
  if (approximation == "hsic"){
    rx <- lm(y ~ x)$residuals
    ry <- lm(x ~ y)$residuals
    return( dHSIC::dhsic( list(y, ry))$dHSIC - 
              dHSIC::dhsic( list(x, rx))$dHSIC )
  }
}

####### Pairwise direction dependence measures with lag included #######
####### Txy_I > 0 when instantaneous effect x -> y directed      #######
####### Txy_L > 0 when lagged effect is x -> y directed          #######

lagged_direction_dependence <- function(X1,X0, approximation = "mxnt"){
  X1 <- scale(X1); X0 <- scale(X0)
  mf <- lm(X1 ~ X0[,1] + X0[,2])
  M <- t( mf$coefficients[2:3,] )
  # mxnt
  Txy <- direction_dependence(mf$residuals[,1], mf$residuals[,2], 
                              approximation = approximation)
  if (Txy >= 0){
    B0 <- matrix(c(0,0,lm(mf$residuals[,2] ~ mf$residuals[,1])$coefficients[2],0),
                 2,2,byrow=T)
  } else {
    B0 <- matrix(c(0,lm(mf$residuals[,1] ~ mf$residuals[,2])$coefficients[2],0,0),
                 2,2,byrow=T)
  }
  B1 <- (diag(2) - B0) %*% M
  return( list(B0=B0, B1=B1, Txy_I=Txy, Txy_L = abs(B1[2,1]) - abs(B1[1,2])) )
}

#######################################################################
#### Test confounding or bidirectionality in instantaneous effects ####
#######################################################################

instantaneous_effect_confounding_test <- function(X1, X0, dontprint = FALSE){
  X1 <- scale(X1); X0 <- scale(X0)
  mf <- lm(X1 ~ X0[,1] + X0[,2])
  # Test direction V1 -> V2
  mf2 <- lm(mf$residuals[,2] ~ mf$residuals[,1])
  v1_to_v2 <- dHSIC::dhsic.test(mf$residuals[,1],mf2$residuals)
  # Test direction V2 -> V1
  mf2 <- lm(mf$residuals[,1] ~ mf$residuals[,2])
  v2_to_v1 <- dHSIC::dhsic.test(mf$residuals[,2],mf2$residuals)
  if (!dontprint){
    print("Test results for direction dependence V1 -> V2:")
    print(v1_to_v2)
    print("Test results for direction dependence V2 -> V1:")
    print(v2_to_v1)
    print("INTERPRETATION:")
    if ((v1_to_v2$p.value <= 0.05/2) & (v2_to_v1$p.value <= 0.05/2)){
      print("Confounding or reprocal directionality was present at family-wise significance level 0.05.")
    } else {
      print("No confounding or reprocal directionality present at family-wise significance level 0.05.")
    }
  }
  return(invisible(list(v1_to_v2 = v1_to_v2, v2_to_v1 = v2_to_v1)))
}
