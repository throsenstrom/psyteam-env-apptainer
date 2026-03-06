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

library(data.table)
washb_data <- fread(
paste0(
"https://raw.githubusercontent.com/tlverse/tlverse-data/master/",
"wash-benefits/washb_data.csv"
),
stringsAsFactors = TRUE
)

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
processed <- process_missing(data1, node_list, complete_nodes=c("A"))
data_p <- processed$data
node_list1 <- processed$node_list
node_list1
head(data_p)
psych::describe(data1)
psych::describe(data_p)

#Q_LEARNER
lrn_mean_Q <- Lrnr_mean$new()
lrn_glm_Q <- Lrnr_glm_fast$new()
lrn_bayesglm_Q <- Lrnr_bayesglm$new()#sama ongelma ku csc
lrn_gam_Q <- Lrnr_gam$new()
lrn_lasso_Q <- Lrnr_glmnet$new(alpha = 1)
lrn_enet.5_Q <- Lrnr_glmnet$new(alpha = 0.5)
lrn_ridge_Q <- Lrnr_glmnet$new(alpha = 0)
lrn_polymars_Q <- Lrnr_polspline$new()#sama ongelma
#lrn_xgboost_autotune_Q <- Lrnr_caret$new(method = "xgbTree", metric = "RMSE")
#lrn_nnet_autotune_Q <- Lrnr_caret$new(method = "nnet", name = "NNET_autotune")#, otin pois alempaa
#lrn_bart_Q <- Lrnr_dbarts$new(ndpost = 1000, verbose = FALSE)#sama ongelma ku csc, otin pois alempaa
lrn_hal_Q <- Lrnr_hal9001$new(max_degree = 2, num_knots = 3)#sama ongelma ku csc
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

node_list
task_testi_Q <- make_sl3NULLtask_testi_Q <- make_sl3_Task(
data = data_complete,
outcome = "whz",
covariates = c("tr","month", "aged", "sex", "momage", "momedu",
 "momheight", "hfiacat", "Nlt18", "Ncomp", "watmin",
 "elec", "floor", "walls", "roof", "asset_wardrobe",
 "asset_table", "asset_chair", "asset_khat",
 "asset_chouki", "asset_tv", "asset_refrig",
 "asset_bike", "asset_moto", "asset_sewmach",
 "asset_mobile"),
folds = origami::make_folds(n=nrow(data_complete),
origami::folds_vfold,
strata_ids = data_complete$A))

t0 <- proc.time()

#TOIMII
set.seed(4197)
Q_learner_fit <- Q_learner$train(task = task_testi_Q)
(runtime_sl_fit_Q <- proc.time() - t0) 
save(Q_learner_fit, file="Q_learner_fitwashb_data.Rdata")
round(Q_learner_fit$coefficients, 3)



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



task_testi_g <- make_sl3_Task(
data = data1,
outcome_type = "categorical",
outcome = "tr",
covariates = c("month", "aged", "sex", "momage", "momedu",
 "momheight", "hfiacat", "Nlt18", "Ncomp", "watmin",
 "elec", "floor", "walls", "roof", "asset_wardrobe",
 "asset_table", "asset_chair", "asset_khat",
 "asset_chouki", "asset_tv", "asset_refrig",
 "asset_bike", "asset_moto", "asset_sewmach",
 "asset_mobile"),
folds = origami::make_folds(n=nrow(data1),
origami::folds_vfold,
strata_ids = data1$A))

t0 <- proc.time()

set.seed(13232)
g_learner_fit <- g_learner$train(task = task_testi_g)
(runtime_sl_fit_g <- proc.time() - t0) 
save(g_learner_fit, runtime_sl_fit_g, file="g_learner_fitwashb_data.Rdata")
round(g_learner_fit$coefficients, 3)
g_learner_fit$metalearner_fit()


#delta learner

lrn_mean_delta <- Lrnr_mean$new()
#lrn_glm_delta <- Lrnr_glm_fast$new()
lrn_bayesglm_delta <- Lrnr_bayesglm$new()#sama ongelma ku csc
#lrn_interaction_delta <-Lrnr_glm$new(formula = interaction_formula)#JAAKKO LISÄNNYT
#lrn_nnls_delta<- Lrnr_nnls$new()#JAAKKO lisännyt
lrn_gam_delta <- Lrnr_gam$new()
lrn_lasso_delta <- Lrnr_glmnet$new(alpha = 1, family = "binomial")
lrn_enet.5_delta <- Lrnr_glmnet$new(alpha = 0.5, family = "binomial")
lrn_ridge_delta <- Lrnr_glmnet$new(alpha = 0, family = "binomial")
#lrn_polymars_delta <- Lrnr_polspline$new()#sama ongelma
#lrn_xgboost_autotune_delta <- Lrnr_caret$new(method = "xgbTree", metric = "RMSE")
#lrn_nnet_autotune_delta <- Lrnr_caret$new(method = "nnet", name = "NNET_autotune")#, otin pois alempaa
#lrn_bart_delta <- Lrnr_dbarts$new(ndpost = 1000, verbose = FALSE)#sama ongelma ku csc, otin pois alempaa
lrn_hal_delta <- Lrnr_hal9001$new(max_degree = 2, num_knots = 3)#sama ongelma ku csc
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
psych::describe(data_p)

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


psych::describe(data_p)

#data_p$delta_Y <- factor(data_p$delta_Y)

task_testi_delta <- make_sl3_Task(
data = data_p,
outcome = "delta_whz",
outcome_type = "binomial",
covariates = c("tr","month", "aged", "sex", "momage", "momedu",
 "momheight", "hfiacat", "Nlt18", "Ncomp", "watmin",
 "elec", "floor", "walls", "roof", "asset_wardrobe",
 "asset_table", "asset_chair", "asset_khat",
 "asset_chouki", "asset_tv", "asset_refrig",
 "asset_bike", "asset_moto", "asset_sewmach",
 "asset_mobile"),
folds = origami::make_folds(n=nrow(data_p),
origami::folds_vfold,
strata_ids = data_p$A))

gc()
t0 <- proc.time()
set.seed(54197)
delta_learner_fit <- delta_learner$train(task = task_testi_delta)
(runtime_sl_fit_delta <- proc.time() - t0) 

save(delta_learner_fit, runtime_sl_fit_delta,delta_learner, file="delta_learner_fitwashb_data.Rdata")
round(delta_learner_fit$coefficients, 3)


tsm_spec <- tmle_TSM_all()


learner_list_no_missing <- list(
Y = Q_learner,
A = g_learner)

#TOIMII
psych::describe(data_complete)
t0 <- proc.time()
set.seed(54197)
tmle_fit_no_missing <- tmle3(tsm_spec, data_complete, node_list, learner_list_no_missing)
(runtime_tmle_fit_no_missing <- proc.time() - t0) 
save(tmle_fit_no_missing, runtime_tmle_fit_no_missing,tsm_spec, file="tmle_fit_no_missingwashb_data.Rdata")

load("tmle_fit_no_missingwashb_data.Rdata")

tmle_fit_no_missing

g_ml <- tmle_fit_no_missing$likelihood$factor_list$A$learner$metalearner_fit()
g_ml$name
g_ml$coefficients


Q_ml <- tmle_fit_no_missing$likelihood$factor_list$Y$learner$metalearner_fit()
Q_ml$coefficients
Q_ml

psych::describe(data1)

tsm_spec_1 <- tmle_TSM_all()


#NOT WORKING

learner_list_complex <- list(Y = Q_learner, A = g_learner, delta_Y=delta_learner)


t0 <- proc.time()
set.seed(54197)
tmle_fit_with_missing <- tmle3(tsm_spec_1, data1, node_list, learner_list_complex)
(runtime_tmle_fit_missing <- proc.time() - t0) 
save(tmle_fit_missing, runtime_tmle_fit_missing, file="tmle_fit_missingwashb_data.Rdata")


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


#WORKING
psych::describe(data_complete)
t0 <- proc.time()
set.seed(54197)
tmle_fit_no_missing2 <- tmle3(tsm_spec, data_complete, node_list, learner_list_simple)
(runtime_tmle_fit_no_missing2 <- proc.time() - t0) 
save(tmle_fit_no_missing2, runtime_tmle_fit_no_missing2,tsm_spec, file="tmle_fit_no_missing_SIMPLE_washb_data.Rdata")

#NOT WORKING
psych::describe(data1)
t0 <- proc.time()
set.seed(54197)
tmle_fit_with_missing2 <- tmle3(tsm_spec_1, data1, node_list, learner_list_simple)
(runtime_tmle_fit_missing2 <- proc.time() - t0) 



#dynamic treatment rule
lrn_xgboost_50 <- Lrnr_xgboost$new(nrounds = 50)
lrn_xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
lrn_xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
lrn_mean <- Lrnr_mean$new()
lrn_glm <- Lrnr_glm_fast$new()
learners <- list(lrn_xgboost_50, lrn_xgboost_100, lrn_xgboost_500, lrn_mean, lrn_glm)
b_learner_simple <- create_mv_learners(learners = learners)

learner_list_complex3 <- list(Y = Q_learner, A = g_learner,  B = b_learner_simple)

tmle_spec_no_miss2 <- tmle3_mopttx_blip_revere(
  V = c("month", "aged", "sex", "momage", "momedu",
        "momheight", "hfiacat", "Nlt18", "Ncomp", "watmin",
        "elec", "floor", "walls", "roof", "asset_wardrobe",
        "asset_table", "asset_chair", "asset_khat",
        "asset_chouki", "asset_tv", "asset_refrig",
        "asset_bike", "asset_moto", "asset_sewmach",
        "asset_mobile"), type = "blip2",
  learners = learner_list_complex3, maximize = TRUE, complex = TRUE,
  realistic = FALSE
)

# recode treatment to numeric-coded factor 1..7
data$tr_num <- factor(as.integer(data$tr), levels = 1:7)

str(data$tr)
str(data$tr_num)
# update node_list... this solves the problem..
node_list2 <- node_list
node_list2$A <- "tr_num"

table(data$tr_num, useNA = "ifany")
anyNA(data$tr_num)

psych::describe(data)
t0 <- proc.time()
set.seed(4343)
fit_no_miss2_dynamic <- tmle3(tmle_spec_no_miss2, data, node_list2, learner_list_complex3)
(runtime_fit_no_miss2_dynamic<- proc.time() - t0) 
fit_no_miss2_dynamic
table(tmle_spec_no_miss2$return_rule)

table(data$tr)


tmle_spec_simple_no_miss2 <- tmle3_mopttx_blip_revere(
  V = c("month", "aged", "sex", "momage", "momedu",
        "momheight", "hfiacat", "Nlt18", "Ncomp", "watmin",
        "elec", "floor", "walls", "roof", "asset_wardrobe",
        "asset_table", "asset_chair", "asset_khat",
        "asset_chouki", "asset_tv", "asset_refrig",
        "asset_bike", "asset_moto", "asset_sewmach",
        "asset_mobile"), type = "blip2",
  learners = learner_list_complex3,
  maximize = TRUE, complex = FALSE, realistic = FALSE
)

t0 <- proc.time()
set.seed(4343)
fit_no_miss2_dynamic_simple <- tmle3(tmle_spec_simple_no_miss2, data, node_list2, learner_list_complex3)
(runtime_fit_no_miss2_dynamic_simple<- proc.time() - t0) 
fit_no_miss2_dynamic_simple
table(tmle_spec_simple_no_miss2$return_rule)

tmle_spec_realistic_no_miss2 <- tmle3_mopttx_blip_revere(
  V = c("month", "aged", "sex", "momage", "momedu",
        "momheight", "hfiacat", "Nlt18", "Ncomp", "watmin",
        "elec", "floor", "walls", "roof", "asset_wardrobe",
        "asset_table", "asset_chair", "asset_khat",
        "asset_chouki", "asset_tv", "asset_refrig",
        "asset_bike", "asset_moto", "asset_sewmach",
        "asset_mobile"), type = "blip2",
  learners = learner_list_complex3,
  maximize = TRUE, complex = TRUE, realistic = TRUE
)

t0 <- proc.time()
set.seed(4343)
fit_no_miss2_dynamic_realistic <- tmle3(tmle_spec_realistic_no_miss2, data, node_list2, learner_list_complex3)
(runtime_fit_no_miss2_dynamic_realistic<- proc.time() - t0) 
fit_no_miss2_dynamic_realistic
save(fit_no_miss2_dynamic_realistic, fit_no_miss2_dynamic, runtime_fit_no_miss2_dynamic_realistic,tmle_spec_realistic_no_miss2, tmle_spec_no_miss2, runtime_fit_no_miss2_dynamic, file="washb_data_fit_no_miss2_dynamic.RData")


table(tmle_spec_no_miss2$return_rule)
table(tmle_spec_realistic_no_miss2$return_rule)


print(fit_no_miss2_dynamic)


