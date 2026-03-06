library(tmle3)
library(sl3)
library(tmle3mopttx)
library(data.table)

data("data_cat_realistic")

node_list <- list(
  W = c("W1", "W2", "W3", "W4"),
  A = "A",
  Y = "Y"
)

data_missing <- data_cat_realistic
#Add some random missingless:
rr <- sample(nrow(data_missing), 100, replace = FALSE)
data_missing[rr,"Y"]<-NA

summary(data_missing$Y)

# Initialize few of the learners:
lrn_xgboost_50 <- Lrnr_xgboost$new(nrounds = 50)
lrn_xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
lrn_xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
lrn_mean <- Lrnr_mean$new()
lrn_glm <- Lrnr_glm_fast$new()

## Define the Q learner, which is just a regular learner:
Q_learner_simple <- Lrnr_sl$new(
  learners = list(lrn_xgboost_100, lrn_mean, lrn_glm),
  metalearner = Lrnr_nnls$new()
)

# Define the g learner, which is a multinomial learner:
# specify the appropriate loss of the multinomial learner:
mn_metalearner <- make_learner(Lrnr_solnp,
                               eval_function = loss_loglik_multinomial,
                               learner_function = metalearner_linear_multinomial
)

g_learner_simple <- make_learner(Lrnr_sl, list(lrn_xgboost_100, lrn_xgboost_500, lrn_mean), mn_metalearner)

delta_learner_simple <- Lrnr_sl$new(
  learners = list(lrn_mean, lrn_glm),
  metalearner = Lrnr_nnls$new()
)

learner_list_simple <- list(Y = Q_learner_simple, A = g_learner_simple, delta_Y=delta_learner_simple)


tsm_spec_1 <- tmle_TSM_all()
tmle_task_1 <- tsm_spec_1$make_tmle_task(data_missing, node_list)


initial_likelihood_1 <- tsm_spec_1$make_initial_likelihood(
  tmle_task_1,
  learner_list_simple
)

print(initial_likelihood_1)

initial_likelihood_1$get_likelihoods(tmle_task_1)
targeted_likelihood_1 <- Targeted_Likelihood$new(initial_likelihood_1)



all_tsm_params_1 <- tsm_spec_1$make_params(tmle_task_1, targeted_likelihood_1)
print(all_tsm_params_1)


#lisätään kontrastit
ate_param1_1 <- define_param(
  Param_delta, targeted_likelihood_1,
  delta_param_ATE,
  list(all_tsm_params_1[[1]], all_tsm_params_1[[3]])
)

ate_param2_1 <- define_param(
  Param_delta, targeted_likelihood_1,
  delta_param_ATE,
  list(all_tsm_params_1[[1]], all_tsm_params_1[[2]])
)

ate_param3_1 <- define_param(
  Param_delta, targeted_likelihood_1,
  delta_param_ATE,
  list(all_tsm_params_1[[2]], all_tsm_params_1[[3]])
)


all_params_1 <- c(all_tsm_params_1, ate_param1_1, ate_param2_1, ate_param3_1)


t0 <- proc.time()
set.seed(54197)
fit_cat_miss <- fit_tmle3(tmle_task_1, targeted_likelihood_1, all_params_1,
  targeted_likelihood_1$updater)
(runtime_fit_cat_miss<- proc.time() - t0) 


set.seed(54197)
fit_cat_miss_b<-tmle3(tsm_spec_1,data_missing, node_list, learner_list_simple)

learner_list_simple2 <- list(Y = Q_learner_simple, A = g_learner_simple)
set.seed(5597)
fit_cat_no_miss<-tmle3(tsm_spec_1, data_cat_realistic, node_list, learner_list_simple2)


save(fit_cat_miss,fit_cat_miss_b,fit_cat_no_miss, runtime_fit_cat_miss, file="fit_cat_miss.RData")

fit_cat_miss
fit_cat_miss_b
fit_cat_no_miss

fit_cat_miss$likelihood$factor_list$Y$learner
fit_cat_miss$likelihood$factor_list$A$learner
fit_cat_miss$likelihood$factor_list$delta_Y$learner

fit_cat_miss_b$likelihood$factor_list$Y$learner
fit_cat_miss_b$likelihood$factor_list$A$learner
fit_cat_miss_b$likelihood$factor_list$delta_Y$learner


fit_cat_no_miss$likelihood$factor_list$Y$learner
fit_cat_no_miss$likelihood$factor_list$A$learner
fit_cat_no_miss$likelihood$factor_list$delta_Y$learner

fit_cat_miss$likelihood$factor_list$Y$learner$metalearner_fit()
fit_cat_miss$likelihood$factor_list$A$learner$metalearner_fit()
fit_cat_miss$likelihood$factor_list$delta_Y$learner$metalearner_fit()
fit_cat_miss$tmle_task$npsem[["Y"]]$censoring_node$name
names(fit_cat_miss$likelihood$factor_list)


fit_cat_miss_b$likelihood$factor_list$Y$learner$metalearner_fit()
fit_cat_miss_b$likelihood$factor_list$A$learner$metalearner_fit()
fit_cat_miss_b$likelihood$factor_list$delta_Y$learner$metalearner_fit()
fit_cat_miss_b$tmle_task$npsem[["Y"]]$censoring_node$name
names(fit_cat_miss$likelihood$factor_list)

fit_cat_no_miss$likelihood$factor_list$Y$learner$metalearner_fit()
fit_cat_no_miss$likelihood$factor_list$A$learner$metalearner_fit()
fit_cat_no_miss$tmle_task$npsem[["Y"]]$censoring_node$name
names(fit_cat_no_miss$likelihood$factor_list)


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


#Q learner

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




initial_likelihood_2 <- tsm_spec_1$make_initial_likelihood(
  tmle_task_1,
  learner_list_complex
)

print(initial_likelihood_2)

initial_likelihood_2$get_likelihoods(tmle_task_1)
targeted_likelihood_2 <- Targeted_Likelihood$new(initial_likelihood_2)


all_tsm_params_2 <- tsm_spec_1$make_params(tmle_task_1, targeted_likelihood_2)
print(all_tsm_params_2)


#lisätään kontrastit
ate_param1_2 <- define_param(
  Param_delta, targeted_likelihood_2,
  delta_param_ATE,
  list(all_tsm_params_2[[1]], all_tsm_params_2[[3]])
)

ate_param2_2 <- define_param(
  Param_delta, targeted_likelihood_2,
  delta_param_ATE,
  list(all_tsm_params_2[[1]], all_tsm_params_2[[2]])
)

ate_param3_2 <- define_param(
  Param_delta, targeted_likelihood_2,
  delta_param_ATE,
  list(all_tsm_params_2[[2]], all_tsm_params_2[[3]])
)


all_params_2 <- c(all_tsm_params_2, ate_param1_2, ate_param2_2, ate_param3_2)


t0 <- proc.time()
set.seed(54197)
fit_cat_miss2 <- fit_tmle3(tmle_task_1, targeted_likelihood_2, all_params_2,
                           targeted_likelihood_2$updater)
(runtime_fit_cat_miss2<- proc.time() - t0) 

#lyhyempi tapa, molemmat nopeita ajoja tässä
set.seed(54197)
fit_cat_miss2_b<-tmle3(tsm_spec_1,data_missing, node_list, learner_list_complex)
#ei puuttuvuutta
learner_list_complex2 <- list(Y = Q_learner, A = g_learner)
set.seed(54197)
fit_cat_no_miss2<-tmle3(tsm_spec_1,data_cat_realistic, node_list, learner_list_complex2)

save(fit_cat_miss2,fit_cat_miss2_b,fit_cat_no_miss2, runtime_fit_cat_miss2, file="fit_cat_miss2.RData")

fit_cat_miss2
fit_cat_miss2_b
fit_cat_no_miss2

fit_cat_miss2$likelihood$factor_list$Y$learner
fit_cat_miss2$likelihood$factor_list$A$learner
fit_cat_miss2$likelihood$factor_list$delta_Y$learner

fit_cat_miss2_b$likelihood$factor_list$Y$learner
fit_cat_miss2_b$likelihood$factor_list$A$learner
fit_cat_miss2_b$likelihood$factor_list$delta_Y$learner

fit_cat_no_miss2$likelihood$factor_list$Y$learner
fit_cat_no_miss2$likelihood$factor_list$A$learner
fit_cat_no_miss2$likelihood$factor_list$delta_Y$learner

fit_cat_miss2$likelihood$factor_list$Y$learner$metalearner_fit()
fit_cat_miss2$likelihood$factor_list$A$learner$metalearner_fit()
fit_cat_miss2$likelihood$factor_list$delta_Y$learner$metalearner_fit()
fit_cat_miss2$tmle_task$npsem[["Y"]]$censoring_node$name
names(fit_cat_miss2$likelihood$factor_list)


fit_cat_miss2_b$likelihood$factor_list$Y$learner$metalearner_fit()
fit_cat_miss2_b$likelihood$factor_list$A$learner$metalearner_fit()
fit_cat_miss2_b$likelihood$factor_list$delta_Y$learner$metalearner_fit()
fit_cat_miss2_b$tmle_task$npsem[["Y"]]$censoring_node$name
names(fit_cat_miss2$likelihood$factor_list)

fit_cat_no_miss2$likelihood$factor_list$Y$learner$metalearner_fit()
fit_cat_no_miss2$likelihood$factor_list$A$learner$metalearner_fit()
fit_cat_no_miss2$tmle_task$npsem[["Y"]]$censoring_node$name
names(fit_cat_no_miss2$likelihood$factor_list)

#dynamic treatment rules

# Define the Blip learner, which is a multivariate learner:
learners <- list(lrn_xgboost_50, lrn_xgboost_100, lrn_xgboost_500, lrn_mean, lrn_glm)
b_learner_simple <- create_mv_learners(learners = learners)

learners2<-c(lrn_mean_Q,
lrn_bayesglm_Q,
lrn_gam_Q,
lrn_lasso_Q,
lrn_enet.5_Q,
lrn_ridge_Q,
lrn_hal_Q,
lrn_randomForest_Q,
lrn_ranger_Q,
lrn_earth_Q,
lrn_xgboost_Q)

b_learner <- create_mv_learners(learners = learners2)

learner_list_simple3 <- list(Y = Q_learner_simple, A = g_learner_simple,  B = b_learner_simple)

learner_list_complex3 <- list(Y = Q_learner, A = g_learner,  B = b_learner_simple)


tmle_spec_cat_no_miss <- tmle3_mopttx_blip_revere(
  V = c("W1", "W2", "W3", "W4"), type = "blip2",
  learners = learner_list_simple3, maximize = TRUE, complex = TRUE,
  realistic = FALSE
)

t0 <- proc.time()
set.seed(4343)
fit_cat_no_miss_dynamic <- tmle3(tmle_spec_cat_no_miss, data_cat_realistic, node_list, learner_list_simple3)
(runtime_fit_cat_no_miss_dynamic<- proc.time() - t0) 
fit_cat_no_miss_dynamic
table(tmle_spec_cat_no_miss$return_rule)

tmle_spec_cat_simple_no_miss <- tmle3_mopttx_blip_revere(
  V = c("W4", "W3", "W2", "W1"), type = "blip2",
  learners = learner_list_simple3,
  maximize = TRUE, complex = FALSE, realistic = FALSE
)

t0 <- proc.time()
set.seed(4343)
fit_cat_no_miss_dynamic_simple <- tmle3(tmle_spec_cat_simple_no_miss, data_cat_realistic, node_list, learner_list_simple3)
(runtime_fit_cat_no_miss_dynamic_simple<- proc.time() - t0) 
fit_cat_no_miss_dynamic_simple
#fit_cat_simple
#A tmle3_Fit that took 1 step(s)
#type                   param init_est tmle_est      se  lower  upper
#1:  TSM E[Y_{d(V=W4,W3,W2,W1)}]   0.5301   0.5497 0.05822 0.4356 0.6638
#psi_transformed lower_transformed upper_transformed
#1:          0.5497            0.4356            0.6638

table(tmle_spec_cat_simple_no_miss$return_rule)

tmle_spec_cat_realistic_no_miss <- tmle3_mopttx_blip_revere(
  V = c("W4", "W3", "W2", "W1"), type = "blip2",
  learners = learner_list_simple3,
  maximize = TRUE, complex = TRUE, realistic = TRUE
)

t0 <- proc.time()
set.seed(4343)
fit_cat_no_miss_dynamic_realistic <- tmle3(tmle_spec_cat_realistic_no_miss, data_cat_realistic, node_list, learner_list_simple3)
(runtime_fit_cat_no_miss_dynamic_realistic<- proc.time() - t0) 
fit_cat_no_miss_dynamic_realistic
table(tmle_spec_cat_realistic_no_miss$return_rule)
save(fit_cat_no_miss_dynamic_realistic, fit_cat_no_miss_dynamic_simple, fit_cat_no_miss_dynamic, runtime_fit_cat_no_miss_dynamic_realistic,
     runtime_fit_cat_no_miss_dynamic_simple, runtime_fit_cat_no_miss_dynamic, tmle_spec_cat_realistic_no_miss, tmle_spec_cat_simple_no_miss, 
     tmle_spec_cat_no_miss, file="fit_cat_no_miss_dynamic.RData")


tmle_spec_cat_no_miss2 <- tmle3_mopttx_blip_revere(
  V = c("W1", "W2", "W3", "W4"), type = "blip2",
  learners = learner_list_complex3, maximize = TRUE, complex = TRUE,
  realistic = FALSE
)

t0 <- proc.time()
set.seed(4343)
fit_cat_no_miss2_dynamic <- tmle3(tmle_spec_cat_no_miss2, data_cat_realistic, node_list, learner_list_complex3)
(runtime_fit_cat_no_miss2_dynamic<- proc.time() - t0) 
fit_cat_no_miss2_dynamic
table(tmle_spec_cat_no_miss2$return_rule)

tmle_spec_cat_simple_no_miss2 <- tmle3_mopttx_blip_revere(
  V = c("W4", "W3", "W2", "W1"), type = "blip2",
  learners = learner_list_complex3,
  maximize = TRUE, complex = FALSE, realistic = FALSE
)

t0 <- proc.time()
set.seed(4343)
fit_cat_no_miss2_dynamic_simple <- tmle3(tmle_spec_cat_simple_no_miss2, data_cat_realistic, node_list, learner_list_complex3)
(runtime_fit_cat_no_miss2_dynamic_simple<- proc.time() - t0) 
fit_cat_no_miss2_dynamic_simple
table(tmle_spec_cat_simple_no_miss2$return_rule)

tmle_spec_cat_realistic_no_miss2 <- tmle3_mopttx_blip_revere(
  V = c("W4", "W3", "W2", "W1"), type = "blip2",
  learners = learner_list_complex3,
  maximize = TRUE, complex = TRUE, realistic = TRUE
)

t0 <- proc.time()
set.seed(4343)
fit_cat_no_miss2_dynamic_realistic <- tmle3(tmle_spec_cat_realistic_no_miss2, data_cat_realistic, node_list, learner_list_complex3)
(runtime_fit_cat_no_miss2_dynamic_realistic<- proc.time() - t0) 
fit_cat_no_miss2_dynamic_realistic
table(tmle_spec_cat_realistic_no_miss2$return_rule)
save(fit_cat_no_miss2_dynamic_realistic, fit_cat_no_miss2_dynamic_simple, fit_cat_no_miss2_dynamic, runtime_fit_cat_no_miss2_dynamic_realistic,
     runtime_fit_cat_no_miss2_dynamic_simple, runtime_fit_cat_no_miss2_dynamic, tmle_spec_cat_realistic_no_miss2, tmle_spec_cat_simple_no_miss2, 
     tmle_spec_cat_no_miss2, file="fit_cat_no_miss2_dynamic.RData")




learner_list_simple4 <- list(Y = Q_learner_simple, A = g_learner_simple,  B = b_learner_simple, delta_Y=delta_learner_simple)


str(data_missing)
node_list

tmle_spec_cat_miss <- tmle3_mopttx_blip_revere(
  V = c("W1", "W2", "W3", "W4"), type = "blip2",
  learners = learner_list_simple4, maximize = TRUE, complex = TRUE,
  realistic = FALSE
)


fit_cat_miss <- tmle3(tmle_spec_cat_miss, data_missing, node_list, learner_list_simple4)
