library(tlverse)
library(sl3)
library(tmle3)
library(tmle3mopttx)
library(tmle3shift)

schematic
data(schematic)

packageVersion("tlverse")#OK VS. CSC
packageVersion("tmle3")#OK VS. CSC
packageVersion("sl3")#OK VS. CSC
packageVersion("tmle3mopttx")#OK VS. CSC
packageVersion("tmle3shift")#OK VS. CSC
packageVersion("origami")# OK VS. CSC
packageVersion("delayed") #OK VS. CSC

library(data.table)
washb_data <- fread(
  paste0(
    "https://raw.githubusercontent.com/tlverse/tlverse-data/master/",
    "wash-benefits/washb_data.csv"
  ),
  stringsAsFactors = TRUE
)

data.table::fwrite(washb_data, "washb_data.csv")

washb_data$complete<-complete.cases(washb_data)
table(washb_data$complete)
data<-subset(washb_data, complete==TRUE)
data$complete<-NULL
table(data$tr)

node_list <- list(
  W = c(
    "month", "aged", "sex", "momage", "momedu",
    "momheight", "hfiacat", "Nlt18", "Ncomp", "watmin",
    "elec", "floor", "walls", "roof", "asset_wardrobe",
    "asset_table", "asset_chair", "asset_khat",
    "asset_chouki", "asset_tv", "asset_refrig",
    "asset_bike", "asset_moto", "asset_sewmach",
    "asset_mobile"
  ),
  A = "tr",
  Y = "whz"
)

data1<-data
n <- nrow(data1)
idx_miss <- sample(seq_len(n), size = floor(0.20 * n))

data1$whz[idx_miss] <- NA
psych::describe(data1)




library(superMICE)

?continuousSuperLearner
?mice.impute.SuperLearner
?continuousSuperLearner
?gaussianKernel
?localImputation
?binarySuperLearner
?jackknifeBandwidthSelection
?jackknifeVariance


# Multiple imputation
SuperLearner::listWrappers()

SL_lib <- c("SL.mean","SL.speedglm","SL.glmnet","SL.polymars", 
            "SL.earth","SL.xgboost", "SL.bayesglm","SL.randomForest", "SL.ranger") 


t0 <- proc.time()
set.seed(54197)
Imp_SL <- mice::mice(data1, maxit = 5, m = 5, method = "SuperLearner", print = T, 
                     SL.library = SL_lib, kernel = "gaussian", bw = 0.5)
(runtime_mice_ <- proc.time() - t0) 

save(Imp_SL, file = "imputation.Rdata")
plot(Imp_SL)


###### Pooled imputation estimates #######

library(tlverse)
library(sl3)
library(tmle3)
library(tmle3mopttx)
library(data.table)

load("imputation.Rdata")


# set up params
#Q_LEARNER
lrn_mean_Q <- Lrnr_mean$new()
lrn_glm_Q <- Lrnr_glm_fast$new()
lrn_bayesglm_Q <- Lrnr_bayesglm$new()#sama ongelma ku csc
lrn_gam_Q <- Lrnr_gam$new()
lrn_lasso_Q <- Lrnr_glmnet$new(alpha = 1)
lrn_enet.5_Q <- Lrnr_glmnet$new(alpha = 0.5)
lrn_ridge_Q <- Lrnr_glmnet$new(alpha = 0)
lrn_polymars_Q <- Lrnr_polspline$new()#sama ongelma
lrn_hal_Q <- Lrnr_hal9001$new(max_degree = 2, num_knots = 3)#sama ongelma ku csc
lrn_randomForest_Q<-Lrnr_randomForest$new()
lrn_ranger_Q<-Lrnr_ranger$new()
lrn_earth_Q<-Lrnr_earth$new()
lrn_xgboost_Q <- Lrnr_xgboost$new()
lrn_svm_Q <- Lrnr_svm$new()
lrn_gbm_Q <- Lrnr_gbm$new()
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
  metalearner = Lrnr_nnls$new())
  
# Define g learner
#g learner
  lrn_mean_A <- Lrnr_mean$new()
  lrn_lasso_A <- Lrnr_glmnet$new(alpha = 1)
  lrn_enet.5_A <- Lrnr_glmnet$new(alpha = 0.5)
  lrn_ridge_A <- Lrnr_glmnet$new(alpha = 0)
  lrn_randomForest_A<-Lrnr_randomForest$new()
  lrn_ranger_A<-Lrnr_ranger$new()
  lrn_xgboost_A <- Lrnr_xgboost$new()
  lrn_svm_A <- Lrnr_svm$new()
  
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
  
  learner_list <- list(
    Y = Q_learner,
    A = g_learner)
  
  
  # Combine results
  
  Q1 <- U1 <- cil1 <- ciu1 <- rep(0, 5) # Take CIs too for sanity checking
  Q2 <- U2 <- cil2 <- ciu2 <- rep(0, 5)
  Q3 <- U3 <- cil3 <- ciu3 <- rep(0, 5)
  Q4 <- U4 <- cil4 <- ciu4 <- rep(0, 5)
  Q5 <- U5 <- cil5 <- ciu5 <- rep(0, 5)
  Q6 <- U6 <- cil6 <- ciu6 <- rep(0, 5)
  Q7 <- U7 <- cil7 <- ciu7 <- rep(0, 5)
  Q8 <- U8 <- cil8 <- ciu8 <- rep(0, 5)
  Q9 <- U9 <- cil9 <- ciu9 <- rep(0, 5)
  Q10 <- U10 <- cil10 <- ciu10 <- rep(0, 5)
  
t0 <- proc.time()

for (i in 1:5){
    # set up complete data i
    dimp_i <- mice::complete(Imp_SL, i)
    
    tsm_spec <- tmle_TSM_all()
    tmle_task <- tsm_spec$make_tmle_task(dimp_i, node_list)
    
    initial_likelihood <- tsm_spec$make_initial_likelihood(
      tmle_task,
      learner_list)
    
    initial_likelihood$get_likelihoods(tmle_task)
    targeted_likelihood <- Targeted_Likelihood$new(initial_likelihood)
    
    all_tsm_params <- tsm_spec$make_params(tmle_task, targeted_likelihood)
    

    ate_param1_1 <- define_param(
      Param_delta, targeted_likelihood,
      delta_param_ATE,
      list(all_tsm_params[[1]], all_tsm_params[[4]])
    )
    
    ate_param2_1 <- define_param(
      Param_delta, targeted_likelihood,
      delta_param_ATE,
      list(all_tsm_params[[1]], all_tsm_params[[3]])
    )
    
    ate_param3_1 <- define_param(
      Param_delta, targeted_likelihood,
      delta_param_ATE,
      list(all_tsm_params[[1]], all_tsm_params[[2]])
    )
    
    ate_param4_1 <- define_param(
      Param_delta, targeted_likelihood,
      delta_param_ATE,
      list(all_tsm_params[[2]], all_tsm_params[[4]])
    )
    
    ate_param5_1 <- define_param(
      Param_delta, targeted_likelihood,
      delta_param_ATE,
      list(all_tsm_params[[2]], all_tsm_params[[3]])
    )
    
    ate_param6_1 <- define_param(
      Param_delta, targeted_likelihood,
      delta_param_ATE,
      list(all_tsm_params[[3]], all_tsm_params[[4]])
    )
    
    
    all_params <- c(all_tsm_params, ate_param1_1, ate_param2_1, ate_param3_1,
                    ate_param4_1, ate_param5_1, ate_param6_1)
    
    
    tmle_fit <- fit_tmle3(tmle_task, targeted_likelihood, all_params,
                          targeted_likelihood$updater)
    
    Q1[i] <- tmle_fit$summary$tmle_est[1]
    U1[i] <- tmle_fit$summary$se[1]
    cil1[i] <- tmle_fit$summary$lower[1]
    ciu1[i] <- tmle_fit$summary$upper[1]
    
    Q2[i] <- tmle_fit$summary$tmle_est[2]
    U2[i] <- tmle_fit$summary$se[2]
    cil2[i] <- tmle_fit$summary$lower[2]
    ciu2[i] <- tmle_fit$summary$upper[2]
    
    Q3[i] <- tmle_fit$summary$tmle_est[3]
    U3[i] <- tmle_fit$summary$se[3]
    cil3[i] <- tmle_fit$summary$lower[3]
    ciu3[i] <- tmle_fit$summary$upper[3]
    
    Q4[i] <- tmle_fit$summary$tmle_est[4]
    U4[i] <- tmle_fit$summary$se[4]
    cil4[i] <- tmle_fit$summary$lower[4]
    ciu4[i] <- tmle_fit$summary$upper[4]
    
    Q5[i] <- tmle_fit$summary$tmle_est[8]
    U5[i] <- tmle_fit$summary$se[8]
    cil5[i] <- tmle_fit$summary$lower[8]
    ciu5[i] <- tmle_fit$summary$upper[8]
    
    Q6[i] <- tmle_fit$summary$tmle_est[9]
    U6[i] <- tmle_fit$summary$se[9]
    cil6[i] <- tmle_fit$summary$lower[9]
    ciu6[i] <- tmle_fit$summary$upper[9]
    
    Q7[i] <- tmle_fit$summary$tmle_est[10]
    U7[i] <- tmle_fit$summary$se[10]
    cil7[i] <- tmle_fit$summary$lower[10]
    ciu7[i] <- tmle_fit$summary$upper[10]
    
    Q8[i] <- tmle_fit$summary$tmle_est[11]
    U8[i] <- tmle_fit$summary$se[11]
    cil8[i] <- tmle_fit$summary$lower[11]
    ciu8[i] <- tmle_fit$summary$upper[11]
    
    Q9[i] <- tmle_fit$summary$tmle_est[12]
    U9[i] <- tmle_fit$summary$se[12]
    cil9[i] <- tmle_fit$summary$lower[12]
    ciu9[i] <- tmle_fit$summary$upper[12]
    
    Q10[i] <- tmle_fit$summary$tmle_est[13]
    U10[i] <- tmle_fit$summary$se[13]
    cil10[i] <- tmle_fit$summary$lower[13]
    ciu10[i] <- tmle_fit$summary$upper[13]
  }
  
  runtime_pool <- proc.time() - t0
  
  
library(mice)
  

results_imp1<-mice::pool.scalar(Q1, U1^2, n = nrow(mice::complete(Imp_SL, 1)))
results_imp2<-mice::pool.scalar(Q2, U2^2, n = nrow(mice::complete(Imp_SL, 1)))
results_imp3<-mice::pool.scalar(Q3, U3^2, n = nrow(mice::complete(Imp_SL, 1)))
results_imp4<-mice::pool.scalar(Q4, U4^2, n = nrow(mice::complete(Imp_SL, 1)))
results_imp5<-mice::pool.scalar(Q5, U5^2, n = nrow(mice::complete(Imp_SL, 1)))
results_imp6<-mice::pool.scalar(Q6, U6^2, n = nrow(mice::complete(Imp_SL, 1)))
results_imp7<-mice::pool.scalar(Q7, U7^2, n = nrow(mice::complete(Imp_SL, 1)))
results_imp8<-mice::pool.scalar(Q8, U8^2, n = nrow(mice::complete(Imp_SL, 1)))
results_imp9<-mice::pool.scalar(Q9, U9^2, n = nrow(mice::complete(Imp_SL, 1)))
results_imp10<-mice::pool.scalar(Q10, U10^2, n = nrow(mice::complete(Imp_SL, 1)))

save(results_imp1,results_imp2, results_imp3, results_imp4, results_imp5,
     results_imp6, results_imp7, results_imp8, results_imp9, results_imp10,
     Q1, U1, cil1, ciu1, 
     Q2, U2, cil2, ciu2, 
     Q3, U3, cil3, ciu3,
     Q4, U4, cil4, ciu4,
     Q5, U5, cil5, ciu5,
     Q6, U6, cil6, ciu6,
     Q7, U7, cil7, ciu7,
     Q8, U8, cil8, ciu8,
     Q9, U9, cil9, ciu9,
     Q10, U10, cil10, ciu10,
     runtime_pool, file = "imputation_res1.Rdata")

c(results_imp1$qbar, sqrt(results_imp1$t))
c(results_imp1$qbar - qt(0.975, results_imp1$df)*sqrt(results_imp1$t), results_imp1$qbar + qt(0.975, results_imp1$df)*sqrt(results_imp1$t))


#dynamic treatment rule
lrn_xgboost_50 <- Lrnr_xgboost$new(nrounds = 50)
lrn_xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
lrn_xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
lrn_mean <- Lrnr_mean$new()
lrn_glm <- Lrnr_glm_fast$new()
learners <- list(lrn_xgboost_50, lrn_xgboost_100, lrn_xgboost_500, lrn_mean, lrn_glm)
b_learner_simple <- create_mv_learners(learners = learners)

learner_list2 <- list(Y = Q_learner, A = g_learner,  B = b_learner_simple)

tmle_spec_no_miss <- tmle3_mopttx_blip_revere(
  V = c("month", "aged", "sex", "momage", "momedu",
        "momheight", "hfiacat", "Nlt18", "Ncomp", "watmin",
        "elec", "floor", "walls", "roof", "asset_wardrobe",
        "asset_table", "asset_chair", "asset_khat",
        "asset_chouki", "asset_tv", "asset_refrig",
        "asset_bike", "asset_moto", "asset_sewmach",
        "asset_mobile"), type = "blip2",
  learners = learner_list2, maximize = TRUE, complex = TRUE,
  realistic = FALSE
)

Q_dyn <- U_dyn <- cil_dyn <- ciu_dyn <- rep(0, 5) # Take CIs too for sanity checking

t0 <- proc.time()
set.seed(1234)

for (i in 1:5){
dimp_i <- mice::complete(Imp_SL, 1)
# recode treatment to numeric-coded factor 1..7
dimp_i$tr_num <- factor(as.integer(dimp_i$tr), levels = 1:7)

node_list2 <- node_list
node_list2$A <- "tr_num"

tmle_fit <- tmle3(tmle_spec_no_miss, dimp_i, node_list2, learner_list2)

Q_dyn[i] <- tmle_fit$summary$tmle_est[1]
U_dyn[i] <- tmle_fit$summary$se[1]
cil_dyn[i] <- tmle_fit$summary$lower[1]
ciu_dyn[i] <- tmle_fit$summary$upper[1]
}
runtime_pool2 <- proc.time() - t0



results_imp_dyn<-mice::pool.scalar(Q_dyn, U_dyn^2, n = nrow(mice::complete(Imp_SL, 1)))

save(results_imp_dyn,
     Q_dyn, U_dyn, cil_dyn, ciu_dyn, 
     runtime_pool2,
    file = "imputation_res2.Rdata")

c(results_imp_dyn$qbar, sqrt(results_imp_dyn$t))
c(results_imp1$qbar - qt(0.975, results_imp_dyn$df)*sqrt(results_imp1$t), results_imp1$qbar + qt(0.975, results_imp1$df)*sqrt(results_imp1$t))



