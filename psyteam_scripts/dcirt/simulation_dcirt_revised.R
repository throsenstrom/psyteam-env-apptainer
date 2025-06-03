# simulation_dcirt.R

setwd("C:/LocalData/rosenstr/ohjaus/Talkkari_contd/revision_assessment/")
source("standardized_DC_density.R")

### Load packages ###
library(mirt)     # For DC-IRT analyses
# library(foreach)  # For parallel computing

### Set functions ###

# Skewed distribution with mu = 0, sigma = 1
gs <- function(x, w=0.9, m1=-0.2/0.9, m2=0.2/0.1, v1=0.450617283950617, v2=1.5){
  return( w * dnorm(x, m1, sqrt(v1)) + (1-w) * dnorm(x, m2, sqrt(v2)) )
}
gs_sim <- function(n, w=0.9, m1=-0.2/0.9, m2=0.2/0.1, v1=0.450617283950617, v2=1.5){
  ws <- (runif(n) <= w)*1
  return(ws * rnorm(n, m1, sqrt(v1)) + (1-ws) * rnorm(n, m2, sqrt(v2)))
}

# Bimodal distribution with mu = o, sigma = 1
gb <- function(x, w=0.5, m1=-0.8944, m2=0.8944, v1=0.2, v2=0.20009728){
  return( w * dnorm(x, m1, sqrt(v1)) + (1-w) * dnorm(x, m2, sqrt(v2)) )
}
gb_sim <- function(n, w=0.5, m1=-0.8944, m2=0.8944, v1=0.2, v2=0.20009728){
  ws <- (runif(n) <= w)*1
  return(ws * rnorm(n, m1, sqrt(v1)) + (1-ws) * rnorm(n, m2, sqrt(v2)))
}

### Set parameters ###
latdists <- c("Gaussian", "Skewed", "Bimodal")
simdists <- list(rnorm, gs_sim, gb_sim)
dendists <- list(dnorm, gs, gb)
n <- c(500, 1000, 2000, 3000, 4000, 5000, 6000, 
       7000, 8000, 9000, 10000, 11000, 12000)
dfreq <- read.table("frekvenssit.txt")
mfreq <- colMeans(dfreq[,2:5])
# qnorm(cumsum(mfreq))[1:3] # thresholds

cumth <- cumsum(mfreq[1:3])
quadists <- list(qnorm(cumth),
                 c(optimise(function(x) (integrate(gs, lower = -Inf, upper = x)$value - cumth[1])^2,c(-3,4))$minimum,
                   optimise(function(x) (integrate(gs, lower = -Inf, upper = x)$value - cumth[2])^2,c(-3,4))$minimum,
                   optimise(function(x) (integrate(gs, lower = -Inf, upper = x)$value - cumth[3])^2,c(-3,4))$minimum),
                 c(optimise(function(x) (integrate(gb, lower = -Inf, upper = x)$value - cumth[1])^2,c(-3,4))$minimum,
                   optimise(function(x) (integrate(gb, lower = -Inf, upper = x)$value - cumth[2])^2,c(-3,4))$minimum,
                   optimise(function(x) (integrate(gb, lower = -Inf, upper = x)$value - cumth[3])^2,c(-3,4))$minimum))

### Define the criterion ###

ISE <- function(g_hat, g){
  f <- function(x){(g_hat(x) - g(x))^2}
  integrate(f, lower = -Inf, upper = Inf)
}

### set data frames for collecting results ###
# rsres <- list(Gaussian=matrix(0,nsim,length(n)), Skewed=matrix(0,nsim,length(n)),
#               Bimodal=matrix(0,nsim,length(n))) # right-sampling results
# esres <- list(Gaussian=matrix(0,nsim,length(n)), Skewed=matrix(0,nsim,length(n)),
#               Bimodal=matrix(0,nsim,length(n))) # even sampling results
# rserr <- rsres; eserr <- esres # ISE numerical integration errors


### Simulate right-sampling questionnaire  ###
library(foreach)
library(doSNOW)
# cores <- parallel::detectCores()
cores <- 10
cl <- makeCluster(cores) #cl <- makeCluster(length(n))
nsim <- c(200, 50, 50)
registerDoSNOW(cl)

t0 <- proc.time()
# allres <- vector("list",3)
# for (isd in 1:length(simdists)){
for (isd in 3:3){
  print(paste0("Computing simdist ", isd, " at runtime ", (proc.time()-t0)[3]))
  # Compute different sample sizes in parallel
  # lres <- foreach(isim = 1:nsim[isd]) %dopar% {
  lres <- foreach(isim = 31:nsim[isd]) %dopar% {
    library(mirt)
    # library(targeted)
    # Simulate. Initialize results objects first:
    res_ISE <- res_ISE_AIC <- res_ISE_BIC <- rep(0,length(n))
    res_map_dcirt <- res_map_irt <- res_eap_dcirt <- res_eap_irt <- 
      res_eap_dcirt_aic <- res_eap_dcirt_bic <- 
      res_map_dcirt_aic <- res_map_dcirt_bic <- rep(0,length(n))
    res_par_dcirt <- res_par_irt <- res_par_dcirt_aic <- res_par_dcirt_bic <- 
      rep(0,length(n)*4*9)
    # Loop through sample sizes
    for (ii in 1:length(n)){
      x <- simdists[[isd]](n[ii]) # latent variable
      xx <- matrix(0,n[ii],9) # For item liabilities
      for (i in 1:9){xx[,i] <- x/sqrt(2) + rnorm(n[ii])/sqrt(2)}
      # Make items that sample high latent-trait values
      ds <- data.frame(xx)
      names(ds) <- paste0("V",1:9)
      # for (i in 1:9){ds[,i] <- cut(ds[,i], breaks = c(-Inf,0,0.5,1,1.5,Inf))}
      for (i in 1:9){ds[,i] <- cut(ds[,i], breaks = c(-Inf, as.numeric(quadists[[isd]]), Inf))}
      ds <- sapply(ds, function(x) as.numeric(ordered(x, levels = levels(ds[,1]))))
      # Fit DC-IRT models to items sampling positive end of the trait
      dav <- vector("list", 9)
      for (i in 1:9) dav[[i]] <- mirt(ds, 1, dentype = paste0('Davidian-',i+1),
                                       technical=list(NCYCLES = 10000))
      davHQs <- davAICs <- davBICs <- rep(0,9)
      for (i in 1:9){
        davHQs[i] <- extract.mirt(dav[[i]], what = "HQ")
        davAICs[i] <- extract.mirt(dav[[i]], what = "AIC")
        davBICs[i] <- extract.mirt(dav[[i]], what = "BIC")
      }
      # Evaluate HQ-best model
      dav_HQb <- dav[[which.min(davHQs)]]
      g_est <- function(x) standardized_DC_density(x, dcfit = dav_HQb)
      a <- ISE(dendists[[isd]], g_est)
      res_ISE[ii] <- a$value
      # Fit ordinary IRT too
      fit_irt <- mirt(ds, 1, technical=list(NCYCLES = 10000))
      # Evaluate parameter estimates
      res_par_dcirt[(1+(ii-1)*4*9):(ii*4*9)] <- unlist(coef(dav_HQb)[1:9])
      res_par_irt[(1+(ii-1)*4*9):(ii*4*9)] <- unlist(coef(fit_irt)[1:9])
      # Evaluate EAP and MAP scores
      # f_den <- density(fscores(dav_HQb)) # DC-IRT with EAP
      # g_den <- function(x) predict(f_den,x)
      # a <- ISE(dendists[[isd]], g_den)
      # res_eap_dcirt[ii] <- a$value
      res_eap_dcirt[ii] <- mean((fscores(dav_HQb) - x)^2) # DC-IRT with EAP
      res_eap_irt[ii] <- mean((fscores(fit_irt) - x)^2) # IRT with EAP
      res_map_dcirt[ii] <- mean((fscores(dav_HQb, type = "MAP") - x)^2) # DC-IRT with MAP
      res_map_irt[ii] <- mean((fscores(fit_irt, type = "MAP") - x)^2) # IRT with MAP
      # Repeat for AIC- and BIC-best DC-IRT
      dav_AICb <- dav[[which.min(davAICs)]]
      dav_BICb <- dav[[which.min(davBICs)]]
      g_est <- function(x) standardized_DC_density(x, dcfit = dav_AICb)
      a <- ISE(dendists[[isd]], g_est)
      res_ISE_AIC[ii] <- a$value
      g_est <- function(x) standardized_DC_density(x, dcfit = dav_BICb)
      a <- ISE(dendists[[isd]], g_est)
      res_ISE_BIC[ii] <- a$value
      res_par_dcirt_aic[(1+(ii-1)*4*9):(ii*4*9)] <- unlist(coef(dav_AICb)[1:9])
      res_par_dcirt_bic[(1+(ii-1)*4*9):(ii*4*9)] <- unlist(coef(dav_BICb)[1:9])
      res_eap_dcirt_aic[ii] <- mean((fscores(dav_AICb) - x)^2) # DC-IRT w AIC-EAP
      res_eap_dcirt_bic[ii] <- mean((fscores(dav_BICb) - x)^2) # DC-IRT with BIC & EAP
      # DC-IRT with AIC & MAP
      res_map_dcirt_aic[ii] <- mean((fscores(dav_AICb, type = "MAP") - x)^2)
      # DC-IRT with BIC & MAP
      res_map_dcirt_aic[ii] <- mean((fscores(dav_BICb, type = "MAP") - x)^2)
    }
    res <- list(ISE=res_ISE, ISE_aic=res_ISE_AIC, ISE_bic=res_ISE_BIC, 
                par_dcirt=res_par_dcirt, par_irt=res_par_irt,
                par_dcirt_aic=res_par_dcirt_aic, par_dcirt_bic=res_par_dcirt_bic,
                eap_dcirt=res_eap_dcirt, eap_irt=res_eap_irt,
                map_dcirt=res_map_dcirt, map_irt=res_map_irt,
                eap_dcirt_aic=res_eap_dcirt_aic, eap_dcirt_bic=res_eap_dcirt_bic,
                map_dcirt_aic=res_map_dcirt_aic, map_dcirt_bic=res_map_dcirt_bic)
    res
  }
  # allres[[isd]] <- lres
  allres[[isd]] <- c(allres[[isd]], lres)
  # allres_contd[[isd]] <- c(allres_contd[[isd]], lres)
  runtime <- proc.time() - t0
  save(isd, allres, runtime, file = "simulation_results_revised_contd.Rdata")
}
runtime <- proc.time() - t0 # 27.6 hours for isd=1 nsim=200 only
stopCluster(cl)

## Plot results for the latent Gaussian ###
library(mirt)
library(gridExtra)
library(ggplot2)

# load("../simulation_results_contd.Rdata")
load("simulation_results_revised_contd.Rdata")
plt1 <- plt2 <- plt3 <- vector("list", 3)
ylims <- c(50,200,1750)
for (k in 1:3){
  # HQ ISE
  # a <- allres_contd[[k]];
  # oise <- matrix(unlist(a),length(a),length(n),byrow = T) * 10^4
  oise <- matrix(0,length(allres[[k]]),length(n))
  for (i in 1:length(allres[[k]])){oise[i,] <- allres[[k]][[i]]$ISE  * 10^4 }
  ose <- apply(oise,2,sd)/sqrt(nrow(oise)) # observed ISE s.d.
  oise <- colMeans(oise) # observed ISE
  resframe <- data.frame(n=n, ISE = oise, lwse = oise - ose, upse = oise + ose,
                         lwci = oise - 1.96*ose, upci = oise + 1.96*ose)
  if (k == 1) {
  plt1[[k]] <- ggplot(resframe, aes(x = n)) + theme_classic(base_size = 14) +
    geom_line(aes(y = ISE)) + #ylab("ISE with HQ model selection") + 
    ylab(expression(paste("ISE" %*% 10^4, " (HQ, Gaussian)"))) + 
    ylim(c(-10, ylims[k])) + 
    geom_ribbon(aes(ymin=lwci,ymax=upci),alpha=0.3) + 
    geom_line(aes(y = lwse), linetype = "dotted") +
    geom_line(aes(y = upse), linetype = "dotted") +
    scale_x_continuous(breaks = c(0,2500,5000,7500,10000,12500), 
                       limits = c(0, 13000))
  } else {
    if (k == 2) {
      plt1[[k]] <- ggplot(resframe, aes(x = n)) + theme_classic(base_size = 14) +
        geom_line(aes(y = ISE)) + #ylab("ISE with HQ model selection") + 
        ylab(expression(paste("ISE" %*% 10^4, " (HQ, Skewed)"))) + 
        ylim(c(-10, ylims[k])) + 
        geom_ribbon(aes(ymin=lwci,ymax=upci),alpha=0.3) + 
        geom_line(aes(y = lwse), linetype = "dotted") +
        geom_line(aes(y = upse), linetype = "dotted") +
        scale_x_continuous(breaks = c(0,2500,5000,7500,10000,12500), 
                           limits = c(0, 13000))
    } else {
      plt1[[k]] <- ggplot(resframe, aes(x = n)) + theme_classic(base_size = 14) +
        geom_line(aes(y = ISE)) + #ylab("ISE with HQ model selection") + 
        ylab(expression(paste("ISE" %*% 10^4, " (HQ, Bimodal)"))) + 
        ylim(c(-10, ylims[k])) + 
        geom_ribbon(aes(ymin=lwci,ymax=upci),alpha=0.3) + 
        geom_line(aes(y = lwse), linetype = "dotted") +
        geom_line(aes(y = upse), linetype = "dotted") +
        scale_x_continuous(breaks = c(0,2500,5000,7500,10000,12500), 
                           limits = c(0, 13000))
    }
  }
  
  # load("simulation_results_revised.Rdata")
  # AIC ISE
  oise <- matrix(0,length(allres[[k]]),length(n))
  for (i in 1:length(allres[[k]])){oise[i,] <- allres[[k]][[i]]$ISE_aic  * 10^4 }
  ose <- apply(oise,2,sd)/sqrt(nrow(oise)) # observed ISE s.d.
  oise <- colMeans(oise) # observed ISE
  resframe <- data.frame(n=n, ISE = oise, lwse = oise - ose, upse = oise + ose,
                         lwci = oise - 1.96*ose, upci = oise + 1.96*ose)
  plt2[[k]] <- ggplot(resframe, aes(x = n)) + theme_classic(base_size = 14) +
    geom_line(aes(y = ISE)) + #ylab("ISE with AIC model selection") + 
    ylab(expression(paste("ISE" %*% 10^4, " (AIC)"))) + 
    ylim(c(-10, ylims[k])) + 
    geom_ribbon(aes(ymin=lwci,ymax=upci),alpha=0.3) + 
    geom_line(aes(y = lwse), linetype = "dotted") +
    geom_line(aes(y = upse), linetype = "dotted") +
    scale_x_continuous(breaks = c(0,2500,5000,7500,10000,12500), 
                       limits = c(0, 13000))
  
  # BIC ISE
  oise <- matrix(0,length(allres[[k]]),length(n))
  for (i in 1:length(allres[[k]])){oise[i,] <- allres[[k]][[i]]$ISE_bic * 10^4 }
  ose <- apply(oise,2,sd)/sqrt(nrow(oise)) # observed ISE s.d.
  oise <- colMeans(oise) # observed ISE
  resframe <- data.frame(n=n, ISE = oise, lwse = oise - ose, upse = oise + ose,
                         lwci = oise - 1.96*ose, upci = oise + 1.96*ose)
  plt3[[k]] <- ggplot(resframe, aes(x = n)) + theme_classic(base_size = 14) +
    geom_line(aes(y = ISE)) + #ylab("ISE with BIC model selection") + 
    ylab(expression(paste("ISE" %*% 10^4, " (BIC)"))) + 
    ylim(c(-10, ylims[k])) +
    geom_ribbon(aes(ymin=lwci,ymax=upci),alpha=0.3) + 
    geom_line(aes(y = lwse), linetype = "dotted") +
    geom_line(aes(y = upse), linetype = "dotted") +
    scale_x_continuous(breaks = c(0,2500,5000,7500,10000,12500), 
                       limits = c(0, 13000))
}

png("info_fig.png", width = 1200, height = 1200)
# postscript("prelim_info_fig.eps")
grid.arrange(plt1[[1]], plt2[[1]], plt3[[1]],
             plt1[[2]], plt2[[2]], plt3[[2]], 
             plt1[[3]], plt2[[3]], plt3[[3]], ncol = 3)
dev.off()

## Plot person- and item-parameter bias ##
mse_dcirt <- mse_irt <- mse_dcirt_bic <- vector("list",3)
for (k in 1:3){
  mse_dcirt[[k]] <- mse_irt[[k]] <- mse_dcirt_bic[[k]] <- 
    matrix(0, length(allres[[k]]), length(n))
  for (i in 1:length(allres[[k]])){
    mse_dcirt[[k]][i, ] <- allres[[k]][[i]]$eap_dcirt
    mse_dcirt_bic[[k]][i, ] <- allres[[k]][[i]]$eap_dcirt_aic # BIC because of (lucky) error
    mse_irt[[k]][i, ] <- allres[[k]][[i]]$eap_irt
  }
}

slope_dcirt <- slope_irt <- slope_dcirt_bic <- vector("list",3)
for (k in 1:3){
  slope_dcirt[[k]] <- slope_irt[[k]] <- slope_dcirt_bic[[k]] <- 
    matrix(0, length(allres[[k]]), length(n))
  for (i in 1:length(allres[[k]])){
    slope_dcirt[[k]][i, ] <- allres[[k]][[i]]$par_dcirt[1+(0:(length(n)-1)*4)]
    slope_dcirt_bic[[k]][i, ] <- allres[[k]][[i]]$par_dcirt_bic[1+(0:(length(n)-1)*4)]
    slope_irt[[k]][i, ] <- allres[[k]][[i]]$par_irt[1+(0:(length(n)-1)*4)]
  }
}


# plot person
postscript("person_and_item_params.eps")
par(mfrow=c(2,3))
ylims <- list(c(0.265,0.285), c(0.185, 0.205), c(0.245, 0.27))
ktitles <- c("Gaussian", "Skewed", "Bimodal")
for(k in 1:3){
  a <- mse_dcirt[[k]]; mse <- colMeans(a); # HQ
  mse_se <- apply(a, 2, function(x) sd(x)/sqrt(length(x)))
  if (k == 1){
    plot(n-100, mse, type = "b", ylim = ylims[[k]], 
         ylab = "MSE of person parameter", xlab = "n", col = "green", lwd = 2)
  } else {
    plot(n-100, mse, type = "b", ylim = ylims[[k]], 
         ylab = "", xlab = "n", col = "green", lwd = 2)
  }
  arrows(n-100, mse, n-100, mse - 1.96*mse_se, code = 2, angle = 90, 
         length = 0.05, col = "green", lwd = 2)
  arrows(n-100, mse, n-100, mse + 1.96*mse_se, code = 2, angle = 90, 
         length = 0.05, col = "green", lwd = 2)
  a <- mse_dcirt_bic[[k]]; mse <- colMeans(a); # BIC
  mse_se <- apply(a, 2, function(x) sd(x)/sqrt(length(x)))
  lines(n, mse, type = "b", ylim = ylims[[k]], lty = 3, pch = 8, 
        col = "magenta", lwd = 2)
  arrows(n, mse, n, mse - 1.96*mse_se, code = 2, angle = 90, 
         length = 0.05, lty = 1, col = "magenta", lwd = 2)
  arrows(n, mse, n, mse + 1.96*mse_se, code = 2, angle = 90, 
         length = 0.05, lty = 1, col = "magenta", lwd = 2)
  a <- mse_irt[[k]]; mse <- colMeans(a); # Classic IRT
  mse_se <- apply(a, 2, function(x) sd(x)/sqrt(length(x)))
  lines(n+100, mse, type = "b", lty = 2, pch = 15, col = "blue", lwd = 2)
  arrows(n+100, mse, n+100, mse - 1.96*mse_se, code = 2, angle = 90, 
         length = 0.05, lty = 1, col = "blue", lwd = 2)
  arrows(n+100, mse, n+100, mse + 1.96*mse_se, code = 2, angle = 90, 
         length = 0.05, lty = 1, col = "blue", lwd = 2)
  if (k == 1){
    legend("topright", legend = c("DC-IRT", "DC-IRT-BIC", "IRT"),
           lty = c(1,3,2), pch = c(1,8,15), lwd = 2, bty = "n", 
           col = c("green", "magenta", "blue"))
  }
  title(ktitles[k], adj = 0)
}
# Plot item slope
ylims <- list(c(1.5,2.4), c(1.5,2.4), c(1.5,2.4))
for(k in 1:3){
  a <- slope_dcirt[[k]]; mse <- colMeans(a); # HQ
  mse_se <- apply(a, 2, function(x) sd(x)/sqrt(length(x)))
  if (k == 1){
    plot(n-100, mse, type = "b", ylim = ylims[[k]], 
         # ylab = "Item parameter (slope)", xlab = "n", col = "green", lwd = 2)
         # revised 29.7.2024
         ylab = "Item parameter (discrimination)", xlab = "n", col = "green", lwd = 2)
  } else {
    plot(n-100, mse, type = "b", ylim = ylims[[k]], 
         ylab = "", xlab = "n", col = "green", lwd = 2)
  }
  arrows(n-100, mse, n-100, mse - 1.96*mse_se, code = 2, angle = 90, 
         length = 0.05, col = "green", lwd = 2)
  arrows(n-100, mse, n-100, mse + 1.96*mse_se, code = 2, angle = 90, 
         length = 0.05, col = "green", lwd = 2)
  a <- slope_dcirt_bic[[k]]; mse <- colMeans(a); # BIC
  mse_se <- apply(a, 2, function(x) sd(x)/sqrt(length(x)))
  lines(n, mse, type = "b", ylim = ylims[[k]], lty = 3, pch = 8, 
        col = "magenta", lwd = 2)
  arrows(n, mse, n, mse - 1.96*mse_se, code = 2, angle = 90, 
         length = 0.05, lty = 1, col = "magenta", lwd = 2)
  arrows(n, mse, n, mse + 1.96*mse_se, code = 2, angle = 90, 
         length = 0.05, lty = 1, col = "magenta", lwd = 2)
  a <- slope_irt[[k]]; mse <- colMeans(a); # Classic IRT
  mse_se <- apply(a, 2, function(x) sd(x)/sqrt(length(x)))
  lines(n+100, mse, type = "b", lty = 2, pch = 15, col = "blue", lwd = 2)
  arrows(n+100, mse, n+100, mse - 1.96*mse_se, code = 2, angle = 90, 
         length = 0.05, lty = 1, col = "blue", lwd = 2)
  arrows(n+100, mse, n+100, mse + 1.96*mse_se, code = 2, angle = 90, 
         length = 0.05, lty = 1, col = "blue", lwd = 2)
  if (k == 1){
    legend("topright", legend = c("DC-IRT", "DC-IRT-BIC", "IRT"),
           lty = c(1,3,2), pch = c(1,8,15), lwd = 2, bty = "n", 
           col = c("green", "magenta", "blue"))
  }
  title(ktitles[k], adj = 0)
  es <- 1.702*sqrt(0.5)/sqrt(1 - 0.5); lines(c(-100,20000), c(es,es))
}
dev.off()

## Plot item parameter bias - verify transformations first
# sapply(coef(fit)[1:9], function(x) (x[1]/1.702)/sqrt(1+(x[1]/1.702)^2)) # stick with this?
invisible(capture.output(a <- summary(fit)$rotF)); (L <- t(a))
sapply(coef(fit)[1:9], function(x) x[1]/sqrt(1.702^2+x[1]^2)) # this is the same!
1.702*L/sqrt(1 - L^2)
sapply(coef(fit)[1:9], function(x) x[1])
# sapply(coef(fit)[1:9], function(x) x[1]/(sqrt(S)*1.702*sqrt(1+(x[1]/1.702)^2)))

# Expected slope and loading parameters are:
es <- 1.702*sqrt(0.5)/sqrt(1 - 0.5)
elambda <- sqrt(0.5)

a <- aa <- matrix(0,200,length(n))
for (i in 1:200){
  a[i,] <- sapply(allres[[1]][[i]]$par_irt[1+(0:(length(n)-1)*4)],
                  function(x) (x/1.702)/sqrt(1+(x/1.702)^2))
  aa[i,] <- sapply(allres[[1]][[i]]$par_dcirt[1+(0:(length(n)-1)*4)],
                  function(x) (x/1.702)/sqrt(1+(x/1.702)^2))
}
c(mean(a), elambda)
mean((a - elambda)^2)
y <- colMeans((a - elambda)^2); y_se <- apply((a - elambda)^2, 2, sd)/sqrt(nrow(a))
yy <- colMeans((aa - elambda)^2); yy_se <- apply((aa - elambda)^2, 2, sd)/sqrt(nrow(aa))

plot(n, y, type="b", ylim = c(0,0.003), ylab = "Mean squared error", pch = 16)
arrows(n, y, n, y - 1.96*y_se, angle = 90, code = 2, length = 0.05)
arrows(n, y, n, y + 1.96*y_se, angle = 90, code = 2, length = 0.05)
lines(n+100, yy, type = "b", pch = 8, lty = 2)
arrows(n+100, yy, n+100, yy - 1.96*yy_se, angle = 90, code = 2, length = 0.05, lty = 2)
arrows(n+100, yy, n+100, yy + 1.96*yy_se, angle = 90, code = 2, length = 0.05, lty = 2)


colMeans(a)
mean(a)
sd(a)/sqrt(200*length(n))

# Do we need to simulate from an IRT model instead of a factor model?
a <- standardized_DC_density(dcfit = dav_HQb)
mean( abs( sapply(coef(dav[[2]])[1:9], function(x) (x[1]*sqrt(a$S)/(1.702))/sqrt(1+(x[1]*sqrt(a$S)/(1.702))^2)) - sqrt(0.5) ) )

mean( abs( sapply(coef(dav[[2]])[1:9], function(x) (x[1]*sqrt(a$S)/(1.702))/sqrt(1+(x[1]*sqrt(a$S)/(1.702))^2)) - sqrt(0.5) ) )












# Plot all results
load("simulation_results.Rdata")
load("simulation_results_contd.Rdata")
library(gridExtra)
library(ggplot2)
# join extra data for the Gaussian case
allres[[1]] <- allres_contd[[1]]
plts <- vector("list", 3)
for (i in 1:3){
  if (i == 1){a <- allres[[i]]; oise <- matrix(unlist(a),length(a),length(n),byrow = T) * 10^4}
  if (i == 2){a <- allres[[i]]; oise <- matrix(unlist(a),length(a),length(n),byrow = T) * 10^4}
  if (i == 3){a <- allres[[i]]; oise <- matrix(unlist(a),length(a),length(n),byrow = T) * 10^4}
  ose <- apply(oise,2,sd)/sqrt(nrow(oise)) # observed ISE s.d.
  oise <- colMeans(oise) # observed ISE
  resframe <- data.frame(n=n, ISE = oise, lwse = oise - ose, upse = oise + ose,
                         lwci = oise - 1.96*ose, upci = oise + 1.96*ose)
  # plts[[i]] <- ggplot(resframe, aes(n, ISE)) + theme_bw() +
  #   geom_line() + 
  #   geom_ribbon(data=resframe, aes(ymin=lwci,ymax=upci),alpha=0.3) + 
  #   geom_line(data=resframe, aes(n,lwse), linetype = "dotted") +
  #   geom_line(data=resframe, aes(n,upse), linetype = "dotted")
  plts[[i]] <- ggplot(resframe, aes(x = n)) + theme_classic(base_size = 14) +
    geom_line(aes(y = ISE)) + 
    geom_ribbon(aes(ymin=lwci,ymax=upci),alpha=0.3) + 
    geom_line(aes(y = lwse), linetype = "dotted") +
    geom_line(aes(y = upse), linetype = "dotted") + 
    scale_x_continuous(breaks = c(0,2500,5000,7500,10000,12500), 
                       limits = c(0, 13000)) # scale revised 29.7.2024
  if (i == 1) plts[[i]] <- plts[[i]] + ylab(expression(ISE %*% 10^4)) #+ ggtitle("Gaussian")
  if (i == 2) plts[[i]] <- plts[[i]] + ylab(expression(ISE %*% 10^4)) #+ ggtitle("Skewed")
  if (i == 3) plts[[i]] <- plts[[i]] + ylab(expression(ISE %*% 10^4)) #+ ggtitle("Bimodal")
}

# grid.arrange(plts[[1]], plts[[2]], plts[[3]], ncol = 3)

####### Simulation results figure ########

# make example fits

set.seed(2023)
exfs <- vector("list", 3) # example fits
exn <- 3000 # example sample size
for (isd in 1:3){
  x <- simdists[[isd]](exn) # latent variable
  xx <- matrix(0,exn,9) # For item liabilities
  for (i in 1:9){xx[,i] <- x/sqrt(2) + rnorm(exn)/sqrt(2)}
  # Make items that sample high latent-trait values
  ds <- data.frame(xx)
  names(ds) <- paste0("V",1:9)
  for (i in 1:9){ds[,i] <- cut(ds[,i], breaks = c(-Inf, as.numeric(quadists[[isd]]), Inf))}
  ds <- sapply(ds, function(x) as.numeric(ordered(x, levels = levels(ds[,1]))))
  # Fit DC-IRT models to items sampling positive end of the trait
  dav <- vector("list", 9)
  for (i in 1:9) dav[[i]] <- mirt(ds, 1, dentype = paste0('Davidian-',i+1),
                                  technical=list(NCYCLES = 10000))
  davHQs <- rep(0,9)
  for (i in 1:9) davHQs[i] <- extract.mirt(dav[[i]], what = "HQ")
  # Evaluate HQ-best model
  exfs[[isd]] <- dav[[which.min(davHQs)]]
  # exfs[[isd]] <- function(x) standardized_DC_density(x, dcfit = dav_HQb)
}

set.seed(2023)
sdat <- data.frame(s1=rep(0,exn),s2=rep(0,exn),s3=rep(0,exn))
for (isd in 1:3){
  x <- simdists[[isd]](exn) # latent variable
  xx <- matrix(0,exn,9) # For item liabilities
  for (i in 1:9){xx[,i] <- x/sqrt(2) + rnorm(exn)/sqrt(2)}
  # Make items that sample high latent-trait values
  ds <- data.frame(xx)
  names(ds) <- paste0("V",1:9)
  for (i in 1:9){ds[,i] <- cut(ds[,i], breaks = c(-Inf, as.numeric(quadists[[isd]]), Inf))}
  ds <- sapply(ds, function(x) as.numeric(ordered(x, levels = levels(ds[,1])))) - 1
  sdat[, isd] <- rowSums(ds)
}

x <- seq(-6, 6, length.out = 100)
pdat <- data.frame(x = x, d1 = standardized_DC_density(x, dcfit = exfs[[1]]),
                   d2 = standardized_DC_density(x, dcfit = exfs[[2]]),
                   d3 = standardized_DC_density(x, dcfit = exfs[[3]]),
                   s1 = dendists[[1]](x), s2 = dendists[[2]](x),
                   s3 = dendists[[3]](x))

plts[[4]] <- ggplot(pdat, aes(x = x)) + 
  geom_line(aes(y = s1, linetype = "Target")) + 
  geom_line(aes(y = d1, linetype = "Estimate")) + xlab(expression(theta)) +
  scale_linetype_manual(name = "", values = c("Target" = "solid", "Estimate" = "dashed")) +
  ylab("Density") + theme_classic(base_size = 14) + ggtitle("Gaussian") + labs() + 
  theme(legend.position = c(.15, .90))
plts[[5]] <- ggplot(pdat, aes(x = x)) + 
  geom_line(aes(y = s2, linetype = "Target")) + 
  geom_line(aes(y = d2, linetype = "Estimate")) + xlab(expression(theta)) +
  scale_linetype_manual(name = "", values = c("Target" = "solid", "Estimate" = "dashed")) +
  ylab("Density") + theme_classic(base_size = 14) + ggtitle("Skewed") + labs() + 
  theme(legend.position = c(.15, .90))
plts[[6]] <- ggplot(pdat, aes(x = x)) + 
  geom_line(aes(y = s3, linetype = "Target")) + 
  geom_line(aes(y = d3, linetype = "Estimate")) + xlab(expression(theta)) +
  scale_linetype_manual(name = "", values = c("Target" = "solid", "Estimate" = "dashed")) +
  ylab("Density") + theme_classic(base_size = 14) + ggtitle("Bimodal") + labs() + 
  theme(legend.position = c(.15, .90))
  

plts[[7]] <- ggplot(sdat, aes(s1)) + geom_histogram(bins = 20) + theme_classic(base_size = 14) + xlab("Sum score")
plts[[8]] <- ggplot(sdat, aes(s2)) + geom_histogram(bins = 20) + theme_classic(base_size = 14) + xlab("Sum score")
plts[[9]] <- ggplot(sdat, aes(s3)) + geom_histogram(bins = 20) + theme_classic(base_size = 14) + xlab("Sum score")

# Finally, plot the simulations figure
# postscript("simulations_fig.eps")
png("simulations_fig.png", res = 600, width = 9000, height = 9000)
grid.arrange(plts[[4]], plts[[5]], plts[[6]],
             plts[[7]], plts[[8]], plts[[9]],
             plts[[1]], plts[[2]], plts[[3]], ncol = 3)
dev.off()


# 
# # Plot some results
# # nsim <- 60
# oise <- matrix(unlist(allres[[1]]),nsim,length(n),byrow = T) * 10^4
# ose <- apply(oise,2,sd)/sqrt(nsim) # observed ISE s.d.
# oise <- colMeans(oise) # observed ISE
# plot(n, oise, type = "b", ylim = c(0,30), ylab = expression(ISE %*% 10^4))
# arrows(n,oise,n,oise-ose,angle = 90, length = 0.1)
# arrows(n,oise,n,oise+ose,angle = 90, length = 0.1)
# 
# x <- seq(-6,6,length.out=100)
# plot(x, g_est(x), type="l")
# 
# plot(n, colMeans(matrix(unlist(lres),nsim,length(n),byrow = T)), type = "b")
# 
# # plot(n, colMeans(rsres[[1]]), type = "b")
# # arrows(n, colMeans(rsres[[1]]), n, colMeans(rsres[[1]])-apply(rsres[[1]],2,sd)/sqrt(nsim), 
# #        code = 3, angle = 90, length = 0.1)
# # arrows(n, colMeans(rsres[[1]]), n, colMeans(rsres[[1]])+apply(rsres[[1]],2,sd)/sqrt(nsim), 
# #        code = 3, angle = 90, length = 0.1)
# 
# # dtmp <- dphq_cleaned[,2:10]+1
# # ib <- sample.int(nrow(dtmp), replace = T)
# # dboot <- dtmp[ib, ]
# # # dboot <- (dphq_cleaned[,2:10]+1)[sample.int(nrow(dphq_cleaned), replace = T),]
# # 
# # mfit <- mirt(dphq_cleaned[,2:10]+1, 1,
# #              dentype = paste0('Davidian-',4),
# #              technical=list(NCYCLES = 10000))



### Below is the rationale for finding standardized non-Gaussian distributions ###

n <- 10^4
m1 <- -0.2/0.9
m2 <- 0.2/0.1 # mu = 0.9*m1+0.1*m2 = 0
v2 <- 1.5
v1 <- (1 - 0.9*m1^2 - 0.1*m2^2 - 0.1*v2) / 0.9

# x <- 0.9 * rnorm(n,-0.25,0.61) + 0.1 * rnorm(n, 2.19, 1.04)
ws <- (runif(n) <= 0.9)*1
x <- ws * rnorm(n, m1, sqrt(v1)) + (1-ws) * rnorm(n, m2, sqrt(v2))
c(mean(x), sd(x))

# Skewed distribution with mu = 0, sigma = 1
gs <- function(x, w=0.9, m1=-0.2/0.9, m2=0.2/0.1, v1=0.450617283950617, v2=1.5){
  return( w * dnorm(x, m1, sqrt(v1)) + (1-w) * dnorm(x, m2, sqrt(v2)) )
}
gs_sim <- function(n, w=0.9, m1=-0.2/0.9, m2=0.2/0.1, v1=0.450617283950617, v2=1.5){
  ws <- (runif(n) <= w)*1
  return(ws * rnorm(n, m1, sqrt(v1)) + (1-ws) * rnorm(n, m2, sqrt(v2)))
}

x <- seq(-6,6,length.out=100)
plot(x, gs(x), type = "l")

# Bimodal distribution with mu = o, sigma = 1
n <- 10^4
m1 <- -0.8944
m2 <- 0.8944 # mu = 0.5*m1+0.5*m2 = 0
v2 <- 0.2
v1 <- (1 - 0.5*m1^2 - 0.5*m2^2 - 0.5*v2) / 0.5

gb <- function(x, w=0.5, m1=-0.8944, m2=0.8944, v1=0.2, v2=0.20009728){
  return( w * dnorm(x, m1, sqrt(v1)) + (1-w) * dnorm(x, m2, sqrt(v2)) )
}
gb_sim <- function(n, w=0.5, m1=-0.8944, m2=0.8944, v1=0.2, v2=0.20009728){
  ws <- (runif(n) <= w)*1
  return(ws * rnorm(n, m1, sqrt(v1)) + (1-ws) * rnorm(n, m2, sqrt(v2)))
}


x <- seq(-6,6,length.out=100)
plot(x, gb(x), type = "l")
x <- gb_sim(n); c(mean(x), sd(x))

a <- ISE(gs,dnorm)

dfreq <- read.table("frekvenssit.txt")
mfreq <- colMeans(dfreq[,2:5])
qnorm(cumsum(mfreq))[1:3] # thresholds

# # Check HQ-criterion term
# n <- 100:10000
# plot(n, log(n), type="l", ylim = c(0,10))
# lines(n, log(log(n)), lty = 2)
# plot(n, log(log(n)), type="l")
# 
# (log(log(6000)) - log(log(3000))) / (log(log(3000)) - log(log(100)))


######################################################################
################# Simulate without HQ-information ####################
######################################################################

### Simulate right-sampling questionnaire  ###
library(foreach)
library(doSNOW)
# cores <- parallel::detectCores()
cores <- 10
cl <- makeCluster(cores) #cl <- makeCluster(length(n))
nsim <- cores*5
registerDoSNOW(cl)

t0 <- proc.time()
for (isd in 2:2){
  # Compute different sample sizes in parallel
  lres <- foreach(isim = 1:nsim) %dopar% {
    library(mirt)
    # Simulate
    res <- reserr <- rep(0,length(n))
    for (ii in 1:length(n)){
      x <- simdists[[isd]](n[ii]) # latent variable
      xx <- matrix(0,n[ii],9) # For item liabilities
      for (i in 1:9){xx[,i] <- x/sqrt(2) + rnorm(n[ii])/sqrt(2)}
      # Make items that sample high latent-trait values
      ds <- data.frame(xx)
      names(ds) <- paste0("V",1:9)
      for (i in 1:9){ds[,i] <- cut(ds[,i], breaks = c(-Inf, as.numeric(quadists[[isd]]), Inf))}
      ds <- sapply(ds, function(x) as.numeric(ordered(x, levels = levels(ds[,1]))))
      # Fit DC-IRT models to items sampling positive end of the trait
      # dav <- vector("list", 9)
      # for (i in 1:9) dav[[i]] <- mirt(ds, 1, dentype = paste0('Davidian-',i+1),
      #                                 technical=list(NCYCLES = 10000))
      # davHQs <- rep(0,9)
      # for (i in 1:9) davHQs[i] <- extract.mirt(dav[[i]], what = "HQ")
      dav <- mirt(ds, 1, dentype = paste0('Davidian-',5),
                                      technical=list(NCYCLES = 10000))
      g_est <- function(x) standardized_DC_density(x, dcfit = dav)
      a <- ISE(dendists[[isd]], g_est)
      res[ii] <- a$value
      # reserr[ii] <- a$abs.error
    }
    res
    # rserr[isim] <- reserr
  }
  # allres[[isd]] <- lres
  nonHQI_res <- c(nonHQI_res,lres)
  # runtime <- proc.time() - t0
  runtime2 <- proc.time() - t0
  # save(nonHQI_res, runtime, runtime2, file = "simulation_results_nonHQI.Rdata")
}

# Plot results
load("simulation_results_nonHQI.Rdata")
plts <- vector("list",4)
library(ggplot2)
a <- nonHQI_res; oise <- matrix(unlist(a),length(a),length(n),byrow = T) * 10^4
ose <- apply(oise,2,sd)/sqrt(nrow(oise)) # observed ISE s.d.
oise <- colMeans(oise) # observed ISE
resframe <- data.frame(n=n, ISE = oise, lwse = oise - ose, upse = oise + ose,
                       lwci = oise - 1.96*ose, upci = oise + 1.96*ose)
plts[[1]] <- ggplot(resframe, aes(x = n)) + theme_classic(base_size = 14) +
  geom_line(aes(y = ISE)) + 
  geom_ribbon(aes(ymin=lwci,ymax=upci),alpha=0.3) + 
  geom_line(aes(y = lwse), linetype = "dotted") +
  geom_line(aes(y = upse), linetype = "dotted") +
  ggtitle("Without HQ information criterion") +
  ylim(c(-10,250))
plts[[1]] <- plts[[1]] + ylab(expression(ISE %*% 10^4)) #+ ggtitle("Skewed")

a <- nonHQI_res; oise <- matrix(unlist(a),length(a),length(n),byrow = T) * 10^4
# ose <- apply(oise,2,function(x) (quantile(x,0.75) - quantile(x,0.25))) #/sqrt(nrow(oise))) # observed ISE IQR
ose1 <- apply(oise,2,function(x) quantile(x,0.25))
ose2 <- apply(oise,2,function(x) quantile(x,0.75))
oise <- apply(oise,2,median) # observed ISE
# resframe <- data.frame(n=n, ISE = oise, lwse = oise - ose, upse = oise + ose)
resframe <- data.frame(n=n, ISE = oise, lwse = ose1, upse = ose2)
plts[[2]] <- ggplot(resframe, aes(x = n)) + theme_classic(base_size = 14) +
  geom_line(aes(y = ISE)) + 
  geom_ribbon(aes(ymin=lwse,ymax=upse),alpha=0.3) +
  ylim(c(0,365)) #ylim(c(0,110))
  # ylim(c(-60,90))
plts[[2]] <- plts[[2]] + ylab(expression("Median ISE" %*% 10^4)) #+ ggtitle("Skewed")

load("simulation_results.Rdata")
a <- allres[[2]]; oise <- matrix(unlist(a),length(a),length(n),byrow = T) * 10^4
ose <- apply(oise,2,sd)/sqrt(nrow(oise)) # observed ISE s.d.
oise <- colMeans(oise) # observed ISE
resframe <- data.frame(n=n, ISE = oise, lwse = oise - ose, upse = oise + ose,
                       lwci = oise - 1.96*ose, upci = oise + 1.96*ose)
plts[[3]] <- ggplot(resframe, aes(x = n)) + theme_classic(base_size = 14) +
  geom_line(aes(y = ISE)) + 
  geom_ribbon(aes(ymin=lwci,ymax=upci),alpha=0.3) + 
  geom_line(aes(y = lwse), linetype = "dotted") +
  geom_line(aes(y = upse), linetype = "dotted") +
  ggtitle("With HQ information criterion") +
  ylim(c(-10,250))
plts[[3]] <- plts[[3]] + ylab(expression(ISE %*% 10^4)) #+ ggtitle("Skewed")

a <- allres[[2]]; oise <- matrix(unlist(a),length(a),length(n),byrow = T) * 10^4
ose1 <- apply(oise,2,function(x) quantile(x,0.25))
ose2 <- apply(oise,2,function(x) quantile(x,0.75))
oise <- apply(oise,2,median) # observed ISE
# resframe <- data.frame(n=n, ISE = oise, lwse = oise - ose, upse = oise + ose)
resframe <- data.frame(n=n, ISE = oise, lwse = ose1, upse = ose2)
plts[[4]] <- ggplot(resframe, aes(x = n)) + theme_classic(base_size = 14) +
  geom_line(aes(y = ISE)) + 
  geom_ribbon(aes(ymin=lwse,ymax=upse),alpha=0.3) +
  ylim(c(0,365)) #ylim(c(0,110))
# ylim(c(-60,90))
plts[[4]] <- plts[[4]] + ylab(expression("Median ISE" %*% 10^4)) #+ ggtitle("Skewed")

png("simulations_supplement_fig.png", res = 600, width = 9000, height = 9000)
gridExtra::grid.arrange(plts[[1]], plts[[3]], plts[[2]], plts[[4]], 
                        ncol=2)
dev.off()

