library(tlverse)
library(sl3)
library(tmle3)
library(tmle3mopttx)
library(tmle3shift)


packageVersion("tlverse")
packageVersion("tmle3")
packageVersion("sl3")
packageVersion("tmle3mopttx")
packageVersion("tmle3shift")
packageVersion("origami")
packageVersion("delayed")


setwd()

# Simulaatiodata tmle3/sl3-testaamiseen:
# - useita confoundereita Z (jatkuva, binaari, kategorinen, laskuri)
# - moniluokkainen hoito A (K=4)
# - jatkuva Y
# - yhteydet: Z -> A, Z -> Y, A -> Y (sis. ei-lineaarisuutta + interaktioita)
#
# Palauttaa listan: data, truth (todelliset E[Y^a]), psi (ATE-kontrastit)

simulate_tmle3_sl3_data <- function(
    n = 2000,
    K = 4,                 # hoitoluokkien määrä
    sigma_y = 1.0,         # Y:n satunnaisvaihtelu
    seed = 1,
    truth_mc = 2e5         # Monte Carlo -koko totuuden approksimointiin
) {
  stopifnot(K >= 3)
  set.seed(seed)
  
  # --- Confounderit Z ---
  Z1 <- rnorm(n, 0, 1)                      # jatkuva
  Z2 <- rbinom(n, 1, plogis(0.2 + 0.7*Z1))  # binaari, korreloi Z1:n kanssa
  Z3 <- factor(sample(c("low","mid","high"), n, replace = TRUE,
                      prob = c(0.35, 0.45, 0.20)))  # kategorinen
  Z4 <- rpois(n, lambda = exp(-0.1 + 0.3*Z1 + 0.4*Z2)) # laskuri
  
  # one-hot Z3
  Z3_mid  <- as.integer(Z3 == "mid")
  Z3_high <- as.integer(Z3 == "high")
  
  # --- Hoitomekanismi: multinomiaalinen A | Z ---
  # Perusluokka a=0 referenssinä; mallinnetaan logitit luokille 1..(K-1)
  # Lisää ei-lineaarisuutta: sin(Z1), Z1^2, interaktio Z1*Z2, sekä Z4 vaikutus
  lin_mat <- matrix(0, nrow = n, ncol = K-1)
  for (k in 1:(K-1)) {
    lin_mat[, k] <-
      -0.4 + 0.5*k +
      (0.8 - 0.15*k)*Z1 +
      (0.6 + 0.10*k)*Z2 +
      (0.5 - 0.10*k)*Z3_mid +
      (0.9 - 0.20*k)*Z3_high +
      (0.20 + 0.05*k)*log1p(Z4) +
      0.35*sin(Z1) -
      0.15*(Z1^2) +
      (0.25 - 0.05*k)*(Z1*Z2)
  }
  
  # softmax: p(A=a|Z)
  logits <- cbind(0, lin_mat)                      # referenssiluokka 0
  logits <- logits - apply(logits, 1, max)         # stabilointi
  exp_logits <- exp(logits)
  pA <- exp_logits / rowSums(exp_logits)
  
  # näyte A ~ Multinomial(pA)
  A_num <- apply(pA, 1, function(pr) sample.int(K, 1, prob = pr) - 1L)
  A <- factor(A_num, levels = 0:(K-1))
  
  # --- Outcome-mekanismi: Y | A, Z ---
  # Z -> Y (useita), A -> Y (heterogeeninen vaikutus), interaktioita
  # Rakennetaan "treatment effect function" tau(a, Z)
  a <- A_num
  tau <- function(a, Z1, Z2, Z3_mid, Z3_high, Z4) {
    # hoitoluokkakohtainen perusvaikutus + heterogeenisyys
    base <- c(0.0, 0.6, 1.1, 1.5, 1.8, 2.0) # riittää K<=6; jos K>6 venytetään
    if (length(base) < (max(a)+1)) base <- c(base, seq(2.2, by = 0.2, length.out = max(a)+1 - length(base)))
    base[a+1] +
      0.25*a*Z1 - 0.20*(a >= 2)*Z2 +
      0.15*(a %% 2 == 1)*Z3_high -
      0.10*(a >= 3)*log1p(Z4)
  }
  
  mu_Y <- 1.0 +
    0.9*Z1 + 0.8*Z2 - 0.6*Z3_mid - 1.0*Z3_high +
    0.25*log1p(Z4) + 0.4*sin(Z1) - 0.2*(Z1^2) +
    tau(a, Z1, Z2, Z3_mid, Z3_high, Z4) +
    0.25*Z1*Z2 - 0.15*Z2*log1p(Z4)
  
  Y <- mu_Y + rnorm(n, 0, sigma_y)
  
  dat <- data.frame(
    Y = Y,
    A = A,
    A_num = A_num,
    Z1 = Z1,
    Z2 = Z2,
    Z3 = Z3,
    Z4 = Z4
  )
  
  set.seed(seed + 1000)
  m <- as.integer(truth_mc)
  
  Z1m <- rnorm(m, 0, 1)
  Z2m <- rbinom(m, 1, plogis(0.2 + 0.7*Z1m))
  Z3m <- factor(sample(c("low","mid","high"), m, replace = TRUE,
                       prob = c(0.35, 0.45, 0.20)))
  Z4m <- rpois(m, lambda = exp(-0.1 + 0.3*Z1m + 0.4*Z2m))
  Z3m_mid  <- as.integer(Z3m == "mid")
  Z3m_high <- as.integer(Z3m == "high")
  
  EY_a <- numeric(K)
  for (a0 in 0:(K-1)) {
    mu_a <- 1.0 +
      0.9*Z1m + 0.8*Z2m - 0.6*Z3m_mid - 1.0*Z3m_high +
      0.25*log1p(Z4m) + 0.4*sin(Z1m) - 0.2*(Z1m^2) +
      tau(a0, Z1m, Z2m, Z3m_mid, Z3m_high, Z4m) +
      0.25*Z1m*Z2m - 0.15*Z2m*log1p(Z4m)
    EY_a[a0+1] <- mean(mu_a)
  }
  names(EY_a) <- paste0("a", 0:(K-1))
  
  # Esimerkkikontrastit: ATE(a=k vs a=0)
  psi <- EY_a - EY_a[1]
  names(psi) <- paste0("ATE_a", 0:(K-1), "_vs_a0")
  
  list(
    data = dat,
    truth = EY_a,
    psi = psi,
    pA = pA # hyödyllinen diagnostiseen: propensiteetit havainnoille
  )
}

# --- Esimerkkiajo ---
sim <- simulate_tmle3_sl3_data(n = 8000, K = 4, seed = 123, sigma_y = 1.0)
data <- sim$data
# Nopea tarkistus:
table(data$A)
sim$truth
sim$psi
summary(data$Y)
rm(sim)

node_list <- list(
  W = c(
    "Z1", "Z2", "Z3", "Z4"),
  A = "A",
  Y = "Y"
)

data1<-data
n <- nrow(data1)
idx_miss <- sample(seq_len(n), size = floor(0.20 * n))

data1$Y[idx_miss] <- NA
psych::describe(data1)
processed <- process_missing(data1, node_list, complete_nodes=c("A"))
data_p <- processed$data
node_list1 <- processed$node_list
node_list1
head(data_p)
psych::describe(data1)
psych::describe(data_p)
table(data1$Z3)

str(data1$Z3)

interaction_formula <- Y ~ Z1 + Z2 + Z3 + Z4 +
  Z1:Z2 + Z1:Z3 + Z1:Z4 +
  Z2:Z3 + Z2:Z4 + Z3:Z4

#Q_LEARNER
lrn_mean_Q <- Lrnr_mean$new()
lrn_glm_Q <- Lrnr_glm_fast$new()
lrn_bayesglm_Q <- Lrnr_bayesglm$new()
#lrn_interaction_Q <-Lrnr_glm$new(formula = interaction_formula)
lrn_gam_Q <- Lrnr_gam$new()
lrn_lasso_Q <- Lrnr_glmnet$new(alpha = 1)
lrn_enet.5_Q <- Lrnr_glmnet$new(alpha = 0.5)
lrn_ridge_Q <- Lrnr_glmnet$new(alpha = 0)
lrn_polymars_Q <- Lrnr_polspline$new()
#lrn_xgboost_autotune_Q <- Lrnr_caret$new(method = "xgbTree", metric = "RMSE")
#lrn_nnet_autotune_Q <- Lrnr_caret$new(method = "nnet", name = "NNET_autotune")
#lrn_bart_Q <- Lrnr_dbarts$new(ndpost = 1000, verbose = FALSE)
lrn_hal_Q <- Lrnr_hal9001$new(max_degree = 2, num_knots = 3)
lrn_randomForest_Q<-Lrnr_randomForest$new()
lrn_ranger_Q<-Lrnr_ranger$new()
lrn_earth_Q<-Lrnr_earth$new()
lrn_xgboost_Q <- Lrnr_xgboost$new()
#lrn_lightgbm_Q <- Lrnr_lightgbm$new()
lrn_svm_Q <- Lrnr_svm$new()
#lrn_rpart_Q <- Lrnr_rpart$new()
lrn_gbm_Q <- Lrnr_gbm$new()
#lrn_grf_Q <- Lrnr_grf$new()
gc()


Q_learner <- Lrnr_sl$new(
  learners = list(
    lrn_mean_Q,
    lrn_glm_Q,
    lrn_bayesglm_Q,
    lrn_gam_Q,
    lrn_lasso_Q,
    lrn_enet.5_Q,
    lrn_ridge_Q,
    lrn_polymars_Q,
    lrn_hal_Q,
    lrn_randomForest_Q,
    lrn_ranger_Q,
    lrn_earth_Q,
    lrn_xgboost_Q,
    lrn_svm_Q,
    lrn_gbm_Q),
  metalearner = Lrnr_nnls$new()
)


data1$complete<-complete.cases(data1)
data_complete<-subset(data1, complete==TRUE)
data_complete$complete<-NULL

task_testi_Q <- make_sl3NULLtask_testi_Q <- make_sl3_Task(
  data = data_complete,
  outcome = "Y",
  covariates = c("A","Z1","Z2","Z3","Z4"),
  folds = origami::make_folds(n=nrow(data_complete),
                              origami::folds_vfold,
                              strata_ids = data_complete$A))

t0 <- proc.time()

set.seed(4197)
Q_learner_fit <- Q_learner$train(task = task_testi_Q)
(runtime_sl_fit_Q <- proc.time() - t0) 
save(Q_learner_fit, runtime_sl_fit_Q, file="iCBT_DEP_GAD_PAD_SAD/Q_learner_fit.Rdata")
round(Q_learner_fit$coefficients, 3)

#g learner
lrn_mean_A <- Lrnr_mean$new()
#lrn_interaction_A <-Lrnr_glm$new(formula = interaction_formula)
lrn_lasso_A <- Lrnr_glmnet$new(alpha = 1)
lrn_enet.5_A <- Lrnr_glmnet$new(alpha = 0.5)
lrn_ridge_A <- Lrnr_glmnet$new(alpha = 0)
#lrn_polymars_A <- Lrnr_polspline$new()#sama ongelma
#lrn_xgboost_autotune_A <- Lrnr_caret$new(method = "xgbTree", metric = "RMSE")
#lrn_nnet_autotune_A <- Lrnr_caret$new(method = "nnet", name = "NNET_autotune")
lrn_randomForest_A<-Lrnr_randomForest$new()
lrn_ranger_A<-Lrnr_ranger$new()
#lrn_solnp_A<-Lrnr_solnp$new()
lrn_xgboost_A <- Lrnr_xgboost$new()
#lrn_lightgbm_A <- Lrnr_lightgbm$new()
lrn_svm_A <- Lrnr_svm$new()
#lrn_rpart_A <- Lrnr_rpart$new()

# Define g learner
g_learner <- Lrnr_sl$new(
  learners = list(
    lrn_mean_A,
    lrn_lasso_A,
    lrn_enet.5_A,
    lrn_ridge_A,
    lrn_randomForest_A,
    lrn_ranger_A,
    lrn_xgboost_A,
    lrn_svm_A))


task_testi_g <- make_sl3_Task(
  data = data1,
  outcome_type = "categorical",
  outcome = "A",
  covariates = c("Z1","Z2","Z3","Z4"),
  folds = origami::make_folds(n=nrow(data1),
                              origami::folds_vfold,
                              strata_ids = data1$A))

t0 <- proc.time()

set.seed(13232)
g_learner_fit <- g_learner$train(task = task_testi_g)
(runtime_sl_fit_g <- proc.time() - t0) 
save(g_learner_fit, runtime_sl_fit_g, file="iCBT_DEP_GAD_PAD_SAD/g_learner_fit.Rdata")
round(g_learner_fit$coefficients, 3)
g_learner_fit$metalearner_fit()


#delta learner

lrn_mean_delta <- Lrnr_mean$new()
#lrn_glm_delta <- Lrnr_glm_fast$new()
lrn_bayesglm_delta <- Lrnr_bayesglm$new()
#lrn_interaction_delta <-Lrnr_glm$new(formula = interaction_formula)
#lrn_nnls_delta<- Lrnr_nnls$new()
lrn_gam_delta <- Lrnr_gam$new()
lrn_lasso_delta <- Lrnr_glmnet$new(alpha = 1,   family = "binomial")
lrn_enet.5_delta <- Lrnr_glmnet$new(alpha = 0.5,   family = "binomial")
lrn_ridge_delta <- Lrnr_glmnet$new(alpha = 0,   family = "binomial")
#lrn_polymars_delta <- Lrnr_polspline$new()
#lrn_xgboost_autotune_delta <- Lrnr_caret$new(method = "xgbTree", metric = "RMSE")
#lrn_nnet_autotune_delta <- Lrnr_caret$new(method = "nnet", name = "NNET_autotune")
#lrn_bart_delta <- Lrnr_dbarts$new(ndpost = 1000, verbose = FALSE)
lrn_hal_delta <- Lrnr_hal9001$new(max_degree = 2, num_knots = 3)
lrn_randomForest_delta<-Lrnr_randomForest$new()
lrn_ranger_delta<-Lrnr_ranger$new()
lrn_earth_delta<-Lrnr_earth$new()
lrn_xgboost_delta <- Lrnr_xgboost$new()
#lrn_lightgbm_delta <- Lrnr_lightgbm$new()
lrn_svm_delta <- Lrnr_svm$new()
#lrn_rpart_delta <- Lrnr_rpart$new()
lrn_gbm_delta <- Lrnr_gbm$new()
#lrn_grf_delta <- Lrnr_grf$new()
gc()

delta_learner <- Lrnr_sl$new(
  learners = list(
    lrn_mean_delta,
    lrn_bayesglm_delta,
    lrn_gam_delta,
    lrn_lasso_delta,
    lrn_enet.5_delta,
    lrn_ridge_delta,
    lrn_hal_delta,
    lrn_randomForest_delta,
    lrn_ranger_delta,
    lrn_earth_delta,
    lrn_xgboost_delta,
    lrn_svm_delta,
    lrn_gbm_delta))


learner_list_complex <- list(Y = Q_learner, A = g_learner, delta_Y=delta_learner)

psych::describe(data_p)

psych::describe(data_p)

#data_p$delta_Y <- factor(data_p$delta_Y)

task_testi_delta <- make_sl3_Task(
  data = data_p,
  outcome = "delta_Y",
  outcome_type = "binomial",
  covariates = c("A","Z1","Z2","Z3","Z4"),
  folds = origami::make_folds(n=nrow(data_p),
                              origami::folds_vfold,
                              strata_ids = data_p$A))

t0 <- proc.time()
set.seed(54197)
delta_learner_fit <- delta_learner$train(task = task_testi_delta)
(runtime_sl_fit_delta <- proc.time() - t0) 

save(delta_learner_fit, runtime_sl_fit_delta, file="iCBT_DEP_GAD_PAD_SAD/delta_learner_fit.Rdata")
round(delta_learner_fit$coefficients, 3)



str(data$A)

tsm_spec <- tmle_TSM_all()


learner_list_no_missing <- list(
  Y = Q_learner,
  A = g_learner)

psych::describe(data_complete)
t0 <- proc.time()
set.seed(54197)
tmle_fit_no_missing <- tmle3(tsm_spec, data_complete, node_list, learner_list_no_missing)
(runtime_tmle_fit_no_missing <- proc.time() - t0) 
save(tmle_fit_no_missing, runtime_tmle_fit_no_missing, file="iCBT_DEP_GAD_PAD_SAD/tmle_fit_no_missing.Rdata")



g_ml <- tmle_fit_no_missing$likelihood$factor_list$A$learner$metalearner_fit()
g_ml$name
g_ml$coefficients


Q_ml <- tmle_fit_no_missing$likelihood$factor_list$Y$learner$metalearner_fit()
Q_ml$coefficients
Q_ml


#######NOT WORKING BELOW

psych::describe(data1)

tsm_spec_1 <- tmle_TSM_all()


node_list_2 <- list(
  W = c(
    "Z1", "Z2", "Z3"),
  A = "A",
  Y = "Y"
)

hist(data1$Y)

table(data1$A)

table(is.na(data1$Y))
hist(data1$Z1)
hist(data1$Z2)
table(data1$Z3)
table(data1$Z4)

t0 <- proc.time()
set.seed(54197)
tmle_fit_with_missing <- tmle3(tsm_spec_1, data1, node_list_2, learner_list_complex)
(runtime_tmle_fit_missing <- proc.time() - t0) 
save(tmle_fit_missing, runtime_tmle_fit_missing, file="iCBT_DEP_GAD_PAD_SAD/tmle_fit_missing.Rdata")



